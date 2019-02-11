### LOADING LIBRARIES FOR WORD CLOUD AND WORD FREQUENCY GENERATOR  ######
rm(list = ls())
library(tm) # for text mining
library(SnowballC) # for text stemming
library(wordcloud) # word-cloud generator 
library(RColorBrewer) # color palettes
library(data.table)
library(sentimentr)

### READING THE AMAZON REVIEW DATA FOR ALEXA ####

df <- read.table("~/R Projects/Sentiment-Analysis/source/amazon_alexa.tsv", 
                 header = T, 
                 sep = "\t", 
                 check.names = F, 
                 stringsAsFactors = F, 
                 fill = T,                 # Some rows may not have all values, and this may cause issue while reading file. Fill NA for such missing values
                 quote = "")               # This is to disable quoting 

names(df) <- c("rating", "date", "variation", "verified_reviews", "feedback")

review_data <- df$verified_reviews

##### READING THE REVIEW DATA IN TO CORPUS FORMAT FOR TEXT MINING ###

corpData <- Corpus(VectorSource(review_data))

### CLEANING DATA BEFORE MINING ####

toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
corpData <- tm_map(corpData, toSpace, "/")
corpData <- tm_map(corpData, toSpace, "@")
corpData <- tm_map(corpData, toSpace, "\\|")

# CONVERT THE TEXT TO LOWER CASE

corpData <- tm_map(corpData, content_transformer(tolower))

# REMOVE NUMBERS

corpData <- tm_map(corpData, removeNumbers)

# REMOVE COMMON STOP WORDS

corpData <- tm_map(corpData, removeWords, stopwords("english"))

# REMOVE CUSTOM STOP WORDS

corpData <- tm_map(corpData, removeWords, c("is", "the", "alexa", "echo", "dot"))

# REMOVE PUNCTUATIONS

corpData <- tm_map(corpData, removePunctuation)

# ELIMINATE EXTRA WHITE SPACES

corpData <- tm_map(corpData, stripWhitespace)

# TEXT STEMMING

corpData <- tm_map(corpData, stemDocument)

#### BUILDING TERM DOCUMENT MATRIX ####

dtm <- TermDocumentMatrix(corpData)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = T)
frequencyTable <- data.frame(word = names(v), frequency = v)
View(head(frequencyTable,100))

### SUBSETTING DATA FOR TRAIN AND TEST ####
manualReviews <- c(1,1,1,1,0,0,1, 1,1,1,0, 1, 1, 1,1,1,1,1,1, 1,1, 1, 1, 1, 1, 1, 1, 1, 1,0,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,1,1,0,1,1,0,1,1,0,1,1,1,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1)
negativeReviews <- rep(-1,30)
validationReviews <- c(manualReviews, negativeReviews)

df$SNo <- 1:nrow(df)
names(df)[1] <- "rating"

validation <- df[1:68,]
negative <- df[df$rating<2,][1:30,]
validation <- rbind(validation, negative)
validation$Sentiment <- validationReviews
validation$Sentiment <- ifelse(validation$Sentiment == 1, "Positive", ifelse(validation$Sentiment == 0, "Neutral", "Negative"))

test <- anti_join(df, validation)

review_data <- validation$verified_reviews

### SENTIMENT ANALYSIS ####

# GET THE CUSTOM POLARITY DICT

custom_polarity_dict <- fread("~/R Projects/Sentiment-Analysis/source/custom_polarity_dict.csv")
setkey(custom_polarity_dict)

review_data <- test$verified_reviews # nrow(df)
review_data <- get_sentences(review_data)

# SENTIMENT ANALYSIS USING THE IN BUILT DIST

sentiment_analysis <- sentiment_by(review_data,
                                polarity_dt = lexicon::hash_sentiment_jockers_rinker,
                                valence_shifters_dt = lexicon::hash_valence_shifters,
                                hyphen = " ",
                                amplifier.weight = 0.8,
                                n.before = 4,
                                n.after = 3,
                                adversative.weight = 0.7,
                                neutral.nonverb.like = T)

# sentiment_analysis_summarized <- sentiment_analysis %>%
#   group_by(element_id) %>% summarise(sentiment_analysis_avg = mean(sentiment))
# 
# sentiment_analysis_summarized

sentiment_analysis_avg <- sentiment_analysis$ave_sentiment

new_df <- cbind(test, sentiment_analysis_avg)
#new_df$sentiment_analysis_avg_scaled <- scales::rescale(new_df$sentiment_analysis_avg, to=c(1,5))
new_df$Predicted <- ifelse(new_df$sentiment_analysis_avg < 0, "Negative", ifelse(new_df$sentiment_analysis_avg>0, "Positive", "Neutral"))
table(new_df$Sentiment, new_df$Predicted)


# SENTIMENT ANALYSIS USING CUSTOM DICT

sentiment_analysis <- sentiment_by(review_data,
                                   polarity_dt = custom_polarity_dict,
                                   valence_shifters_dt = lexicon::hash_valence_shifters,
                                   hyphen = " ",
                                   amplifier.weight = 0.5,
                                   n.before = 4,
                                   n.after = 3,
                                   adversative.weight = 0.5,
                                   neutral.nonverb.like = T)

# sentiment_analysis_summarized <- sentiment_analysis %>%
#   group_by(element_id) %>% summarise(sentiment_analysis_avg = mean(sentiment))
# 
# sentiment_analysis_summarized

sentiment_analysis_avg_cust <- sentiment_analysis$ave_sentiment

new_df <- cbind(test, sentiment_analysis_avg_cust)
#new_df$sentiment_analysis_avg_scaled <- scales::rescale(new_df$sentiment_analysis_avg, to=c(1,5))
new_df$Predicted_cust <- ifelse(new_df$sentiment_analysis_avg_cust < 0, "Negative", ifelse(new_df$sentiment_analysis_avg_cust>0, "Positive", "Neutral"))
table(new_df$Predicted_cust, new_df$Predicted)



#CHECK
check <- new_df[new_df$Sentiment == "Neutral" & new_df$Predicted == "Positive",] # View(check)
review_data <- get_sentences(check$verified_reviews)


sentiment_by(review_data,
             polarity_dt = custom_polarity_dict,
             valence_shifters_dt = lexicon::hash_valence_shifters,
             hyphen = " ",
             amplifier.weight = 0.1,
             n.before = 4,
             n.after = 3,
             adversative.weight = 0.5,
             neutral.nonverb.like = T) %>% highlight()







cor(new_df$ï..rating, new_df$sentiment_analysis_avg_scaled)

new_df$Sentiment_feedback <- ifelse(new_df$sentiment_analysis_avg < 0, "Negative", ifelse(new_df$sentiment_analysis_avg <= 0.5, "Neutral","Positive"))

library(magrittr)
library(dplyr)
set.seed(2)

hu_liu_cannon_reviews %>%
  #  filter(review_id %in% sample(unique(review_id), 3)) %>%
  mutate(review = get_sentences(text)) %$%
  sentiment_by(review) %>%
  highlight()














