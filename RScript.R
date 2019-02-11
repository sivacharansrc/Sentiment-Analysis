## SETTING UP ENVIRONMENT ###
rm(list = ls())
options(scipen = 999)
library(sentimentr)
library(dplyr)
library(scales)
library(data.table)


### COMBINING GOOGLE AND JOCKERS LEXICON

google_lexicon <- lexicon::hash_sentiment_socal_google
jockers_rinker_lexicon <- lexicon::hash_sentiment_jockers_rinker

gLexicon <- google_lexicon$x
jLexicon <- jockers_rinker_lexicon$x

key <- c(google_lexicon$x, jockers_rinker_lexicon$x)
key <- unique(key)

polarity_lexicon <- data.frame(x = key)
polarity_lexicon <- left_join(polarity_lexicon, google_lexicon, by = c("x" = "x"), all.x=T)
polarity_lexicon_g <- polarity_lexicon[!is.na(polarity_lexicon$y),]
polarity_lexicon_j <- polarity_lexicon[is.na(polarity_lexicon$y),]
polarity_lexicon_j <- rename(polarity_lexicon_j, delCol = y)
polarity_lexicon_j <- left_join(polarity_lexicon_j, jockers_rinker_lexicon, by = c("x" = "x"), all.x=T)
polarity_lexicon_j <- polarity_lexicon_j %>% select(-delCol)


polarity_lexicon <- rbind(polarity_lexicon_g, polarity_lexicon_j)
polarity_lexicon <- data.table(polarity_lexicon)
setkey(polarity_lexicon,x)

### READ AMAZON REVIEW DATA ###

df <- read.delim("~/R Projects/Sentiment-Analysis/source/amazon_alexa.tsv", 
                 header = T, 
                 stringsAsFactors = F, sep = "\t")

review_data <- df$verified_reviews # nrow(df)
review_data <- get_sentences(review_data)

sentiment_analysis <- sentiment(review_data,
                                polarity_dt = polarity_lexicon,
                                valence_shifters_dt = lexicon::hash_valence_shifters,
                                hyphen = " ",
                                amplifier.weight = 0.5,
                                n.before = 4,
                                n.after = 3,
                                adversative.weight = 0.5,
                                neutral.nonverb.like = F)

sentiment_analysis_summarized <- sentiment_analysis %>%
  group_by(element_id) %>% summarise(sentiment_analysis_avg = mean(sentiment))

sentiment_analysis_avg <- sentiment_analysis_summarized$sentiment_analysis_avg

new_df <- cbind(df, sentiment_analysis_avg)
new_df$sentiment_analysis_avg_scaled <- scales::rescale(new_df$sentiment_analysis_avg, to=c(1,5))

cor(new_df$ï..rating, new_df$sentiment_analysis_avg_scaled)

new_df$Sentiment_feedback <- ifelse(new_df$sentiment_analysis_avg < 0, "Negative", ifelse(new_df$sentiment_analysis_avg <= 0.5, "Neutral","Positive"))


table(new_df$Sentiment_feedback)


View(new_df[614,])
polarity_lexicon_g[polarity_lexicon_g$x %like% "great",]
polarity_lexicon[polarity_lexicon$x %like% "dissatisfaction",]
View(new_df[new_df$ï..rating == 1 & new_df$Sentiment_feedback == "Positive",])



View(new_df)
# new_df <- new_df[,c("verified_reviews", "sentiment_analysis_avg")]
# table(new_df$ï..rating, new_df$Sentiment_feedback)
# table(new_df$ï..rating,as.integer( new_df$google_avg_scaled))
# cor(new_df$ï..rating, new_df$google_avg)


View(head(new_df,200))
