# no_reasonable_person
This directory includes R Code and supporting data (inside the "data" subdirectory) for "'No Reasonable Person': The Rhetoric of Justice Scalia," an Essay by George H. Taylor, Matthew L. Jockers, and Fernando Nascimento.

Contents:

Code: 

1. "classify_with_words.R" -- this file loads the metadata and word frequency data then winnows the word frequency data before building and testing two classifiers, SVM and NSC.  Code is commented for clarity. 

2. "rhetorical_checks.R" -- this file loads the metadata and word count data and then produces a csv file containing information about a specified set of "rhetorical" words.

Data

1. "metadata_word_punc.RData" a data matrix of 2650 rows x 5 columns ("Author", "Text_ID", "NumWords", "NumPuncs", "File_name")

2. "wide_form_word_freqs.RData" a data frame of 2650 rows by 58575 columns in which cells contain the calculated relative frequencies of each word found in the corresponding column header.  The first column is the ID of the row which maps to the combined Author and ID fields in the data frame "metadata_word_punc.RData"

3. "word_counts.csv" a data frame of 2650 rows by 58575 columns in which cell values are the raw counts of each word found in the corresponding column header. The first column is the ID of the row which maps to the combined Author and ID fields in the data frame "metadata_word_punc.RData"





