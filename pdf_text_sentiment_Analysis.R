# install.packages("pdftools")
# install.packages("tm")
# install.packages("googledrive")
install.packages("Rpoppler")
library(Rpoppler)
library(tidyverse)
library(pdftools)
library(tm)
library(googledrive)

# pdf_file <- read.csv(file = "http://kmmc.in/wp-content/uploads/2014/01/lesson2.pdf")

# java_basics_pdf_file_text <- pdftools::pdf_data(pdf = "JavaBasics-notes.pdf")

text_from_pdf1 <-  pdf_text(pdf = "JavaBasics-notes.pdf")
View(text_from_pdf1)
str(text_from_pdf1)
#remove(text_from_pdf)

java_basic_text <- str_split(string = text_from_pdf1,pattern = "Java Basics\n",simplify = T) #%>% head()
View(java_basic_text)
java_basic_text <- as.data.frame(java_basic_text,stringsAsFactors = F)
java_basic_text <- java_basic_text[,-1]

str(java_basic_text)

# copy the data
java_basic_text$V2[1] <- java_basic_text$V3[1]

java_basic_text <- java_basic_text[,-2]


# combining all the text fro mdifferent pages.
reviewed_text <- paste(java_basic_text,collapse = "")
reviewed_text

review_srce <- VectorSource(reviewed_text)
review_srce

elmnt_java <- getElem(stepNext(x = review_srce))
elmnt_java
result <- readPlain(elem = elmnt_java,language = "",id = "id1")
meta(result)


crps_java_basics <- Corpus(review_srce)
crps_java_basics


# cleaning of text

crps_cleaned <- tm_map(crps_java_basics,content_transformer(tolower))

f <- content_transformer(function(x,pattern) gsub(pattern,"",x)) 
crps_cleaned <- tm_map(crps_cleaned,f,"\\•")


crps_cleaned <- tm_map(crps_cleaned,removePunctuation)
crps_cleaned <- tm_map(crps_cleaned,stripWhitespace)
crps_cleaned <- tm_map(crps_cleaned,removeNumbers)
#crps_cleaned <- tm_map(crps_cleaned,content_transformer(removeSparseTerms))
crps_cleaned <- tm_map(crps_cleaned,removeWords,stopwords("english"))



#crp_cleaned <- tm_map(crps_cleaned,FUN = 
# str_replace_all(string = crps_cleaned, pattern = "[^[:punct:]]",replacement = "")
# str_remove_all(string = crps_cleaned,pattern = "\\•|\\©|[^[:punct:]]|[^[:punct:]]")


# document term-matirx
dtm <- DocumentTermMatrix(crps_cleaned)
dtm2 <- as.matrix(dtm)
dtm

# frequency of the most frequent terms

freq_java_basic <- colSums(dtm2)
freq_java_basic <- sort(freq_java_basic,decreasing = T)
#freq_java_basic
View(freq_java_basic)
# 
# stopwords(kind = "en")
