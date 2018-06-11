remove(list = ls())

#install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages("pdftools")
# install.packages("tm")
# install.packages("googledrive")

library(SnowballC)
library(wordcloud)
library(tidyverse)
library(pdftools)
library(tm)
library(googledrive)

setwd("/Users/vmudivedu/OneDrive/OneDrive - Atimi Software Inc/Upgrad/case study/pdf_to_text")
# download the data from google drive
drive_download(as_dribble(as_id("https://drive.google.com/file/d/1gZCnlhwVMBIE0SugUUxDIgQrfVz-cDQR/")),overwrite = TRUE)
# store the data in the
java_basic_pdf_to_text <-  pdf_text(pdf = "JavaBasics-notes.pdf")
View(java_basic_pdf_to_text)
str(java_basic_pdf_to_text)

#----------------------------
# Data Cleaning
#----------------------------

# remove the "Java Basics" header
cleaned_java_basics <- str_replace_all(string = java_basic_pdf_to_text,pattern = "Java Basics\n|\\\n|\\•|[[:]]|© 1996-2003 jGuru.com. All Rights Reserved.|Java Basics -",replacement = "")

# java_basic_text <- str_split(string = java_basic_pdf_to_text,pattern = "Java Basics\n",simplify = T)
# View(java_basic_text)
# # convert the
# java_basic_text <- as.data.frame(java_basic_text,stringsAsFactors = F)
# java_basic_text <- java_basic_text[,-1]
# 
# str(java_basic_text)
# 
# # copy the data
# java_basic_text$V2[1] <- java_basic_text$V3[1]
# java_basic_text <- java_basic_text[,-2]
# 

# combining all the text from different pages.
reviewed_text <- paste(cleaned_java_basics,collapse = "")
reviewed_text

#
review_srce <- VectorSource(reviewed_text)
review_srce

# Corpora
crps_java_basics <- Corpus(review_srce)
crps_java_basics

# Further Cleaning of text

# Lower case
crps_cleaned <- tm_map(crps_java_basics,content_transformer(tolower))

# # special characters
# f <- content_transformer(function(x,pattern) gsub(pattern,"",x)) 
# crps_cleaned <- tm_map(crps_cleaned,f,"\\•")

# removing punctuations
crps_cleaned <- tm_map(crps_cleaned,removePunctuation)

# removing white spaces
crps_cleaned <- tm_map(crps_cleaned,stripWhitespace)

# removing numbers
crps_cleaned <- tm_map(crps_cleaned,removeNumbers)

# removing stopwords
crps_cleaned <- tm_map(crps_cleaned,removeWords,stopwords("english"))


# document term-matirx for the cleaned text
dtm <- DocumentTermMatrix(crps_cleaned)
dtm2 <- as.matrix(dtm)
head(dtm2)

# Most frequent terms are:
ft_java_basics <- findFreqTerms(x = dtm,lowfreq = 10,highfreq = Inf)
ft_java_basics

# frequency of the most frequent terms
freq_java_basic <- colSums(dtm2)
freq_java_basic <- sort(freq_java_basic,decreasing = TRUE)

head(freq_java_basic,n = 50)
# creating the word dataframe
word_df <- data.frame(words = names(freq_java_basic),freq = as.vector(freq_java_basic))

# Printing the hava Keywords in the text file.
library(RColorBrewer)
set.seed(100)
wordcloud(words = word_df$words,freq = word_df$freq,
          min.freq = 5,random.order = F,max.words = 200,
          rot.per = 0.35,colors = brewer.pal(n = 8,name = "Dark2"),
          random.color = F)
