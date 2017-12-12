# get detailed info about: absurd*, implausible, unquestionably, inconceivable, precisely, surely, and impossible

library(dplyr)
library(readr)

# Load the wide form word frequencies data
load("data/wide_form_word_counts.RData")

# load the metadata
load("data/metadata_word_punc.RData")

# merge both
mdata <- mutate(metadata, ID=paste(Author, Text_ID, sep = "_"))
meta_full <- merge(mdata, wide_count_df)

# want to identify docs that have varients of "absurd*"
absurd <- colnames(meta_full)[grep("absur.*", colnames(meta_full))]
all_words <- c(absurd, "implausible", "unquestionably", "inconceivable", "precisely", "surely", "impossible")
meta_cols <- colnames(meta_full)[1:6]

rhetoric <- meta_full[, c(meta_cols, all_words)]

rhetoric_summary <- group_by(rhetoric, Author) %>%
  select(c("NumWords",all_words))  %>%
  summarize_all(funs(sum))

write_csv(rhetoric_summary,path = "data/rhetoric_summary.csv")

