getwd()
setwd("C:/Users/joshi/Documents/R")
sms_spam_df<- read.csv(file = "C:/Users/joshi/Documents/R/sms_spam.csv", stringsAsFactors = F)
str(sms_spam_df)
library(tm)
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)
sms_corpus <- Corpus(VectorSource(sms_spam_df$text))
summary(sms_corpus)
View(sms_corpus)
inspect(sms_corpus[1:3])
#cleaning the corpus
#translate all letterss to lowercase
clean_corpus <- tm_map(sms_corpus, tolower)
#remove numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)
#remove punctuation
clean_corpus <- tm_map(clean_corpus, removePunctuation)
stopwords()[1:10]
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
#inspect the clean corpus
inspect(clean_corpus[1:4])
#Tokenize the corpus
sms_dtm <- DocumentTermMatrix(clean_corpus)
inspect(sms_dtm[1:4, 30:38])
#indices of spam and ham
spam_indices <- which(sms_spam_df$type == "spam")
spam_indices[1:3]
ham_indices <- which(sms_spam_df$type == "ham")
ham_indices[1:3]
#wordcloud for ham
library(wordcloud)
wordcloud(clean_corpus[ham_indices], min.freq = 40)
#wordcloud for spam
wordcloud(clean_corpus[spam_indices], min.freq = 40)
#Building a spam filter using Naive Bayes Classifier
#training dataset to be 75% and test dataset to be 25% 
sms_raw_train <- sms_spam_df[1:4169,]
sms_raw_test <- sms_spam_df[4170:5574,]
#document term matrix and clean corpus
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5574,]
sms_corpus_train <- clean_corpus[1:4169]
sms_corpus_tes <- clean_corpus[4170:5574]
#separate training data to spam and ham
spam <- subset(sms_raw_train, type == 'spam')
ham <- subset(sms_raw_test, type == 'ham')
#Identify frequently used words
five_times_words <- findFreqTerms(sms_dtm_train, 5)
length(five_times_words)
five_times_words[1:5]
#Create Document term matrices using frequent words
sms_train <- DocumentTermMatrix(sms_corpus_train, control = list(dictionary = five_times_words))
sms_test <- DocumentTermMatrix(sms_corpus_tes, control = list(dictionary = five_times_words))
#convert count information to "Yes", "No"
convert_count <- function(x) {
  y<- ifelse(x>0,1,0)
  y<-factor(y, levels=c(0,1), labels = c("No","Yes"))
  y
}
#convert document term matrices
sms_train <- apply(sms_train, 2, convert_count)
sms_test <- apply(sms_test, 2, convert_count)
#NaiveBayes function
library(e1071)
#creating a  Naive Bayes classifier object on training data
sms_classifier <- naiveBayes(sms_train, factor(sms_raw_train$type))
class(sms_classifier)
View(sms_raw_train)
#evaluate the performance on test data
sms_test_pred <- predict(sms_classifier, newdata = sms_test)
View(sms_train)
View(sms_test_pred)
#generate the prediction table
table(sms_test_pred, sms_raw_test$type)
