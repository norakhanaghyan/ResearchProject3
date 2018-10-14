library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(sentimentr)
reviews<-read_csv('reviews_final.csv')
stock<-read_excel('stock.xlsx')
reviews<-reviews[-c(1)]
reviews$date<-as.Date(parse_date_time(reviews$date, c('mdy', 'ymd_hms')))
reviews$date<-as.character(reviews$date)
stock$Date<-as.character(stock$Date)

all_data<-inner_join(reviews, stock, by=c('date'='Date'))
data1<-na.omit(all_data)
data1<-data1[-c(3)]


# tokenize

library(tidytext)
library(stringr)
data("stop_words")
my_stop_words <- tibble(word = c("flight","united", "airlines", 0:100, "told"))
all_stop_words <- stop_words %>%bind_rows(my_stop_words)

tidy_data<-data1 %>% unnest_tokens(word, reviews)
tidy_data <-tidy_data[grepl("[a-zA-Z']{3,}", tidy_data$word),]

tidy_data <- tidy_data%>%anti_join(all_stop_words)

tidy_data %>%count(word, sort = TRUE)


tidy_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# sentiments

## afinn

contributions <- tidy_data %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))


contributions
get_sentiments('afinn')

contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()


## bing
bing_word_counts <- tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

## wordclouds

tidy_data %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
# sentences
library(syuzhet)
convert <- iconv(data1$reviews,to="utf-8")
my_text<-sentimentr::get_sentences(convert)
#my_text1<-syuzhet::get_sentences(convert)
#afin_sent<-as.data.frame(syuzhet::get_sentiment(my_text1, method='afinn'))
my_sentiments<-sentimentr::sentiment(my_text)
#my_sentiments$afin_score<-afin_sent$`syuzhet::get_sentiment(my_text1, method = "afinn")`
freq_table<-as.data.frame(table(my_sentiments$element_id))

for (i in nrow(freq_table)){
  new<-my_sentiments %>%
    group_by(element_id) %>%
    summarise(weighted_sum=(sum(sentiment)/freq_table[i,2]))
}

library(dplyr)
my_sentiments %>% group_by(element_id) %>% sum(sentiment) %>% max() 


data1$afin_scores<-new$weighted_sum

data1$rev_length<-NA

for (i in 1:nrow(data1)){
  data1$rev_length[i]<-nchar(convert[i])
}


hist(data1$rev_length)
hist(data1$afin_scores)

#LDA

library(topicmodels)
library(RTextTools)
library(tm)
library(SnowballC)
index<-sample(1:nrow(data1), 0.8*nrow(data1))
train<-data1[index,]
test<-data1[-index,]

#train
convert_1 <- iconv(train$reviews,to="utf-8")
docs<-VCorpus(VectorSource(convert_1))
summary(docs)
inspect(docs[1])
writeLines(as.character(docs[2]))

## Preprocessing
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

docs <- tm_map(docs, removeWords, c(stopwords("english"), "flight", "united", "airlines", "will", "can", "told", "airline" ))
docs <- tm_map(docs, PlainTextDocument)

docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

## Stage Data
dtm <- DocumentTermMatrix(docs)  
iter <- 450
seed <-700
best <- TRUE
k<-5

ldaOut <-LDA(dtm,k, method="Gibbs", control=list(seed = seed, best=best, iter = iter))

lda5_topics <- ldaOut %>% 
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, - beta) 

lda5_topics %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ldaOut10 <-LDA(dtm,10, method="Gibbs", control=list(seed = seed, best=best, iter = iter))

lda10_topics <- ldaOut10 %>% 
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, - beta)


lda10_topics %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ldaOut15 <-LDA(dtm,15, method="Gibbs", control=list(seed = seed, best=best, iter = iter))

lda15_topics <- ldaOut15 %>% 
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, - beta)

lda15_topics %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#test
convert_2 <- iconv(test$reviews,to="utf-8")
docs1<-VCorpus(VectorSource(convert_2))
summary(docs1)
inspect(docs1[1])
writeLines(as.character(docs1[2]))

## Preprocessing
docs1 <- tm_map(docs1,removePunctuation)
docs1 <- tm_map(docs1, removeNumbers) 
docs1 <- tm_map(docs1, content_transformer(tolower))
docs1 <- tm_map(docs1, PlainTextDocument)
DocsCopy1 <- docs1

docs1 <- tm_map(docs1, removeWords, c(stopwords("english"), "flight", "united", "airlines","â€™", "will", "can", "told", "airline"))
docs1 <- tm_map(docs1, PlainTextDocument)
docs1 <- tm_map(docs1, stripWhitespace)
docs1 <- tm_map(docs1, PlainTextDocument)

dtm1 <- DocumentTermMatrix(docs1)

## perplexity
perplexity(ldaOut, dtm1)
perplexity(ldaOut10, dtm1)
perplexity(ldaOut15, dtm1)

#Analysis

volume<-as.data.frame(table(data1$date))
volume$Var1<- as.Date(volume$Var1)
data1$date<-as.Date(data1$date)
data1 %>% ggplot(aes(x=date, y=Close))+geom_line(col='light blue')+ labs(x='Date')+
  ggtitle('Stock price over time')

stock$Date<-as.Date(stock$Date)
fin<-data1 %>% group_by(date) %>% summarise(mean_afin=mean(afin_scores))
final_df<-left_join(fin, stock, by=c('date'='Date'))
final_df$date<-as.Date(final_df$date)
review_numb<-as.data.frame(table(data1$date))
final_df$rev_numb<-review_numb$Freq

final_df %>% ggplot(aes(x=date, y=rev_numb))+geom_line(col='light blue')+ labs(x='Date')+
  ggtitle('Number of reviews per day over time')


library(forecast)
library(tseries)
library(zoo)

#only stocks

stock1<-read_excel('stock1.xlsx')
names(stock1)<-c('Date', 'Close')
stock1$Date<-as.Date(stock1$Date)
stock1<-stock1[order(nrow(stock1):1),]

stock1_train<-stock1[1:2900,]
stock1_test<-stock1[2901:3081,]

only_stock<-auto.arima(ts(stock1$Close, frequency = 365), D=1)
stock_pred<-forecast(only_stock, h=181, level=95)
df1_only<-data.frame(date=as.Date(stock1_train$Date),   M=stock1_train$Close, isin='observations')
df2_only<-data.frame(date=as.Date(stock1_test$Date),  M=stock_pred$mean, isin='forecast')
df3_only<-data.frame(date=as.Date(stock1_test$Date), M=stock_pred$lower, isin='lower')
df4_only<-data.frame(date=as.Date(stock1_test$Date), M=stock_pred$upper, isin='upper')
df5_only<-data.frame(date=as.Date(stock1_test$Date), M=stock1_test$Close, isin='actual')

names(df3_only)<-c( 'date','M', 'isin')
names(df4_only)<-c( 'date', 'M', 'isin')

plot(stock_pred)
df6_only<-rbind(df1_only, df2_only, df3_only, df4_only, df5_only)

ggplot(df6_only, aes(x=df6_only$date, y = M, color = isin)) + geom_line() + 
  scale_colour_manual(values=c(observations='blue', forecast='red', actual='orange', upper='black', lower='black'))


# combined
complete<-left_join(stock1, final_df, by=c('Date'='date'))
complete$rev_numb[which(is.na(complete$rev_numb))]<-0
plot(complete$Close.x~complete$Date)
complete<-complete[-c(4:5)]

library(mice)
md.pattern(complete)
imputed<-mice(complete[, 2:4])
summary(imputed)
imputed$imp$mean_afin
densityplot(imputed)
final2<-with(data=imputed)
final1<-complete(imputed,3)
hell$date<-complete$Date
acf(ts(diff(final1$Close.x)))

complete_train<-imputed[1:2900,]
complete_test<-final1[2901:3081,]


with_scores<-auto.arima(ts(complete_train$Close.x, frequency = 365), D=1, xreg=as.matrix(ts(complete_train$mean_afin, frequency = 365)))
forecasting<-forecast(with_scores, xreg=as.matrix(ts(complete_test$mean_afin, frequency=365)), h=181, level=95)
summary(with_scores)
plot(forecasting)
df1_test<-data.frame(date=as.Date(complete_train$date),   M=complete_train$Close.x, isin='observations')
df2_test<-data.frame(date=as.Date(complete_test$date),  M=forecasting$mean, isin='forecast')
df3_test<-data.frame(date=as.Date(complete_test$date), M=forecasting$lower, isin='lower')
df4_test<-data.frame(date=as.Date(complete_test$date), M=forecasting$upper, isin='upper')
df5_test<-data.frame(date=as.Date(complete_test$date), M=complete_test$Close.x, isin='actual')

names(df3_test)<-c( 'date','M', 'isin')
names(df4_test)<-c( 'date', 'M', 'isin')

df8<-rbind(df1_test, df2_test, df3_test, df4_test, df5_test)
ggplot(df8, aes(x=df8$date, y = M, color = isin)) + geom_line() + 
  scale_colour_manual(values=c(observations='blue', forecast='red', actual='orange', upper='black', lower='black'))


##Stationarity(seasonality) tests
adf.test(final1$Close.x)# no stationarity
adf.test(diff(final1$Close.x))# stationarity
adf.test(final1$mean_afin)# stationarity

#Autocorrelations and residuals

qqnorm(with_scores$residuals)
qqline(with_scores$residuals)
plot(with_scores$residuals)
acf(final1$Close.x)
pacf(final1$Close.x)
tsdiag(with_scores)
plot(complete_test$Close.x,forecasting$mean)
MSE1<-mean((stock1_test$Close-stock_pred$mean)^2)
MSE2<-mean((forecasting$mean-complete_test$Close.x)^2)

