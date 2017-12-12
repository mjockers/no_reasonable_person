library(caret)
library(dplyr)
library(readr)
library(pamr)
library(tidyr)
library(pROC)
library(ROCR)


# Load the wide form word frequencies data
load("data/wide_form_word_freqs.RData")

# load the metadata
load("data/metadata_word_punc.RData")

# Ignore very short documents: e.g. docs < 1000 wds
word_threshold <- 1000
long_docs <- filter(metadata, NumWords >= word_threshold) %>%
  mutate(ID=paste(Author, Text_ID, sep="_")) %>%
  select(Author, Text_ID, ID, NumWords)
long_doc_data <- wide_df[which(wide_df$ID %in% long_docs$ID),]

# Merge in the meta data
meta_full <- merge(long_docs, long_doc_data)

# Now winnow the data to high frequency words that are used by all authors.

# Calculate the means for winnowing
the_means <- colMeans(meta_full[, 5:ncol(meta_full)])

# First set a maximum number of possible features to retain
# We tested values from 100 to 1000.  Ideally the feature set
# should be small and limited to high frequency "context insensative"
# features.  Here we begin with 150 and then winnow further in the 
# next step
max_cols <- 150
if(length(the_means) < max_cols){
  max_cols <- length(the_means)
}

# We'll need to know the names of the meta columns
metacols <- colnames(meta_full)[1:4]

# we collect the 150 most frequent features into a vector "keepers"
keepers <- names(sort(the_means, decreasing = TRUE)[1:max_cols])

# form a new dataframe with just the metadata and feature columns
temp_data <- meta_full[,c(metacols, keepers)]

# now check that every feature appears at least once in every author
# and also in the unknown doc.  Second winnowing step
zero_value_test <- group_by(temp_data, Author) %>%
  select(-ID, -Text_ID, -NumWords) %>%
  summarise_each(funs(sum))

# reset any 0 values to NA, so we can use a function to find
# any columns containing an "NA" (i.e. zero value)
zero_value_test[zero_value_test == 0] <- NA

# This function will identify columns that have NA values
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}

# Sending zero_value_test to the function returns a set of features
# that were not present in all three authors and also in the 
# unknown file. we will remove these
remove <- nacols(zero_value_test)

# remove any features that are not common to all authors.
if(length(remove) > 0){
  classing_data_full <- temp_data[, -which(colnames(temp_data) %in% remove)]
} else {
  classing_data_full <- temp_data
}

# Balance the classes by undersampling
# Setting seed for repeatability during testing.
set.seed(8675309) # go Jenny!

# figure out which rows are which and then sample from the
# larger classes based on the size of the smaller class
r_ids <- which(classing_data_full$Author == "Rehnquist")
s_ids <- which(classing_data_full$Author == "Scalia")
t_ids <- which(classing_data_full$Author == "Thomas")
u_ids <- which(classing_data_full$Author == "Unknown")
small_class_size <- min(c(length(r_ids), length(s_ids), length(t_ids)))

r_keep <- sample(r_ids, small_class_size)
s_keep <- sample(s_ids, small_class_size)
t_keep <- sample(t_ids, small_class_size)

# a new data frame from the sampled data
classing_data <- classing_data_full[c(r_keep, s_keep, t_ids),]

# compare composition of authors before balancing . . . 
table(classing_data_full$Author)

# . . . to composition after Balancing
table(classing_data$Author)

################################################################################
# Classify the data using SVM and 3/4 of data for training
################################################################################

# TRAIN ON 3/4 OF DATA
trainIndex <- createDataPartition(factor(classing_data$Author), p = .75, list = FALSE, times = 1)
training <- classing_data[trainIndex,5:ncol(classing_data)]
testing  <- classing_data[-trainIndex,5:ncol(classing_data)]
unknown <- classing_data_full[u_ids, 5:ncol(classing_data_full)]

# 10-fold x-validation with 5 repeats
fitControl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = T)

# Build an SVM model from training data
svmFit <- train(x=training, y = factor(classing_data$Author[trainIndex]), method = "svmRadial", preProcess = c("center","scale"),trControl = fitControl)

# Examine the model
svmFit

# Examine how the training data was classified in x-validation
# This is not the best measure of performance.  We'll also look at
# the performace on the held out data (below).
training_data_class_pred <- predict(svmFit, newdata = training, type = "raw")
confusionMatrix(data = training_data_class_pred, reference = factor(classing_data$Author[trainIndex]))

################################################################################
# NOTE:
# Good explanation of the kappa statistic here:
# https://stats.stackexchange.com/questions/82162/cohens-kappa-in-plain-english
################################################################################

# Now make predictions using the unseen data and examine performance again
class_pred <- predict(svmFit, newdata = testing, type = "raw")
class_probs <- predict(svmFit, newdata = testing, type = "prob")
confusionMatrix(data = class_pred, reference = factor(classing_data$Author[-trainIndex]))

################################################################################
# NOTE:
# Some find the multi-class AUC a useful performance metric.
# This function builds multiple ROC curve to compute the multi-class 
# AUC as defined by Hand and Till. A multiclass AUC is a mean of AUCs.
# See David J. Hand and Robert J. Till (2001). A Simple Generalisation 
# of the Area Under the ROC Curve for Multiple Class Classification Problems. 
# Machine Learning 45(2), p. 171–186. DOI: 10.1023/A:1010920819831.
mx <- multiclass.roc(factor(classing_data$Author[-trainIndex]), as.numeric(factor(class_pred)), percent=TRUE)
mx$auc
################################################################################

# now classify the "unknown" Bush V. Gore Document
class_pred <- predict(svmFit, newdata = unknown, type = "raw")
class_probs <- predict(svmFit, newdata = unknown, type = "prob")

# Show final classification result and probabilities
class_probs

################################################################################
# Rerun the same test using all available data for model training  
################################################################################

trainIndex <- createDataPartition(factor(classing_data$Author), p = 1, list = FALSE, times = 1)
training <- classing_data[trainIndex,5:ncol(classing_data)]
testing  <- classing_data[-trainIndex,5:ncol(classing_data)]
unknown <- classing_data_full[u_ids, 5:ncol(classing_data_full)]

fitControl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = T)

svmFit <- train(x=training, y = factor(classing_data$Author[trainIndex]), method = "svmRadial", preProcess = c("center","scale"),trControl = fitControl)

svmFit # Examine the model

# Examine how the training data was classified in x-validation
training_data_class_pred <- predict(svmFit, newdata = training, type = "raw")
confusionMatrix(data = training_data_class_pred, reference = factor(classing_data$Author[trainIndex]))

# Predict the Bush V. Gore Doc.
class_pred <- predict(svmFit, newdata = unknown, type = "raw")
class_probs <- predict(svmFit, newdata = unknown, type = "prob")

# Show final classification result and probabilities
class_probs

training_data_class_pred <- predict(svmFit, newdata = training, type = "raw")

mx <- multiclass.roc(factor(classing_data$Author[trainIndex]), as.numeric(factor(training_data_class_pred)), percent=TRUE)
mx$auc

# Save the probabilities for each document?:
training_data_class_probs <- predict(svmFit, newdata = training, type = "prob")

################################################################################
# Rerun clasification USING NSC instead of SVM  
################################################################################
# TRAIN ON 3/4 OF DATA
set.seed(8675309) # Hi Jenny!
trainIndex <- createDataPartition(factor(classing_data$Author), p = .75, list = FALSE, times = 1)
training <- classing_data[trainIndex,5:ncol(classing_data)]
testing  <- classing_data[-trainIndex,5:ncol(classing_data)]
unknown <- classing_data_full[u_ids, 5:ncol(classing_data_full)]

# 10 x 10-fold x-validation
fitControl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = T)

# Build the NSC model
nscFit <- train(x=training, y = factor(classing_data$Author[trainIndex]), method = "pam", preProcess = c("center","scale"),trControl = fitControl)

# Examine the model
nscFit

# Examine how the training data was classified in x-validation
training_data_class_pred <- predict(nscFit, newdata = training, type = "raw")
confusionMatrix(data = training_data_class_pred, reference = factor(classing_data$Author[trainIndex]))

# Now make predictions using the unseen and examine
class_pred <- predict(nscFit, newdata = testing, type = "raw")
class_probs <- predict(nscFit, newdata = testing, type = "prob")
confusionMatrix(data = class_pred, reference = factor(classing_data$Author[-trainIndex]))

# now classify the Bush V. Gore Document
class_pred <- predict(nscFit, newdata = unknown, type = "raw")
class_probs <- predict(nscFit, newdata = unknown, type = "prob")

# Show final classification result and probabilities
class_probs

################################################################################
# Rerun USING out of the box NSC so that we can access the feature weights
################################################################################
# Author column is a factor and needs to be character vector for this algo
classing_data$Author <- as.character(classing_data$Author)

set.seed(8675309) # set see for repeatability during testing.
trainIndex <- createDataPartition(factor(classing_data$Author), p = .75, list = FALSE, times = 1)
training <- classing_data[trainIndex,5:ncol(classing_data)]
testing  <- classing_data[-trainIndex,5:ncol(classing_data)]
unknown <- classing_data_full[u_ids, 5:ncol(classing_data_full)]

feature_cols <- 5:ncol(classing_data)
train_data <- classing_data[trainIndex, feature_cols]
test_data <- classing_data[-trainIndex, feature_cols]
train_signal_colors <- classing_data[trainIndex, "Author"]
test_signal_colors <- classing_data[-trainIndex, "Author"]
unknown_data <- classing_data_full[u_ids, feature_cols]

features <- colnames(train_data)

data.train <- list(x=t(train_data), y=train_signal_colors, geneid=features)
data.test <- list(x=t(test_data), y=test_signal_colors, geneid=features)
data.unk <- list(x=t(unknown_data), y="UNK", geneid=features)

prior <- rep(1/length(levels(as.factor(data.train$y))), length(levels(as.factor(data.train$y))))
pamr.train.out <- pamr.train(data.train, prior=prior)
# pamr.cv.out <- pamr.cv(pamr.train.out, data.train)

new.scales <- pamr.adaptthresh(pamr.train.out)
pamr.train.out <- pamr.train(data.train, prior=prior, threshold.scale=new.scales)
pamr.cv.out <- pamr.cv(pamr.train.out, data.train)

thresh.row <- which(pamr.cv.out$error == min(pamr.cv.out$error))[1]
the.thresh <- pamr.cv.out$threshold[thresh.row]
tt <- pamr.confusion(pamr.cv.out,  threshold=the.thresh, FALSE)
tt1 <- tt
diag(tt1) <- 0
tt <- cbind(tt, apply(tt1, 1, sum)/apply(tt, 1, sum))
dimnames(tt)[[2]][ncol(tt)] <- "Class Error rate"
tt

# Held out data. . . 
pamr.test.pred <- pamr.predict(pamr.train.out, data.test$x, threshold=0, type="class")
theProbs <- as.data.frame(pamr.predict(pamr.train.out, data.test$x, threshold=0, type="posterior"))
signalskey <- c("Rehnquist", "Scalia", "Thomas")
predicted.class <- as.character(signalskey[as.numeric(pamr.test.pred)])
pred.results <- as.data.frame(cbind(Author=classing_data[-trainIndex, "Author"], predicted.class, theProbs))
colnames(pred.results)[3:5] <- c("Rehnquist", "Scalia", "Thomas")
confusionMatrix(data = pred.results$predicted.class, reference = pred.results$Author)

# Unknown Text 
pamr.test.pred <- pamr.predict(pamr.train.out, data.unk$x, threshold=0, type="class")
theProbs <- as.data.frame(pamr.predict(pamr.train.out, data.unk$x, threshold=0, type="posterior"))
signalskey <- c("Rehnquist", "Scalia", "Thomas")
predicted.class <- as.character(signalskey[as.numeric(pamr.test.pred)])
pred.results <- as.data.frame(cbind(Author="Unknown", predicted.class, theProbs))
colnames(pred.results)[3:5] <- c("Rehnquist", "Scalia", "Thomas")
pred.results
pamr.listgenes(pamr.train.out, data.train, the.thresh, pamr.cv.out)
