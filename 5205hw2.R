library(readr)

baby_reviews = read_csv("baby_reviews.csv")
data = baby_reviews

####################Section 1 
#1 reviews in dataset
sum(is.na(data$review))
summary(data$review)

#2 average review rating
mean(data$review_rating)

#3 average number of characters in a review
mean_char = mean(nchar(data$review)); mean_char

#4 relationship between review length (characters) and rating
cor(nchar(data$review),data$review_rating)
cor.test(nchar(data$review),data$review_rating)

#5 median number of words in a review
median_words = median(str_count(string = data$review,pattern = '\\S+')); 
median_words

#6-7 how many words in longest/shortest review
summary(str_count(string = data$review,pattern = '\\S+'))

#8 top 10 common words
install.packages('tidytext')
library(tidytext)
library(dplyr)
data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)

data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

#9 top 10 words after removing stopwords
data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)

data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

####################Section 2
#1 tokenize reviews & total number of words in all reviews
data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output = word,input=review)%>%
  ungroup()%>%
  count()

#2 total number of positive words 
data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()

#3 proportion of positive words
data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#4 which rating has highest proportion of positive words
#method 1
data%>%
  group_by(review_rating)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(review_rating)%>%
  summarize(positivity = sum(sentiment=='positive')/n())
#method 2
data %>%
  select(id,review,review_rating)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(review_rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#5-6 use 'nrc' dictionary, how many wrods reflect surprice & anticipation
library(tidytext)
library(remotes)
library(lexicon)
nrc = get_sentiments('nrc')
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',
                 header = F,
                 col.names = c('word','sentiment','num'),
                 sep = '\t',
                 stringsAsFactors = F)
nrc = nrc[nrc$num!=0,]
nrc$num = NULL

data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = review)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()

#7,9  using 'Afinn', mininun sentiment score & average sentiment score
afinn = get_sentiments('afinn')
afinn = read.table('https://raw.githubusercontent.com/pseudorational/data/master/AFINN-111.txt',
                   header = F,
                   quote="",
                   sep = '\t',
                   col.names = c('word','value'), 
                   encoding='UTF-8',
                   stringsAsFactors = F)
data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))

#8 check following numbers, whom has lowest sentiment score
c_ids <- c(146, 91, 238,1432,2598)
data %>%
  select(id,review)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  filter(id%in%c_ids)%>%
  summarize(reviewSentiment = mean(value))


####################Section 3
# Create a corpus from the variable 'review'
library(tm)
corpus = Corpus(VectorSource(data$review))
# transform text to lower case
corpus = tm_map(corpus,FUN = content_transformer(tolower))
# remove punctuation
corpus = tm_map(corpus,FUN = removePunctuation)
# remove English stopwords 
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
# remove whitespace
corpus = tm_map(corpus,FUN = stripWhitespace)
# Create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$review))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))
# Use tm_map to stem words
corpus = tm_map(corpus,FUN = stemDocument)
# Create a DocumentTermMatri
dtm = DocumentTermMatrix(corpus)
dtm

#1 how many terms does document term matrix contain
dtm = DocumentTermMatrix(corpus)
dtm

#2 how many times does 'amazon' appear while inspecting 100 of the documents
inspect(dtm[100,'amazon'])

#3 how many teerms remain after rremoving sparse terms (at least 10% of documents)
xdtm = removeSparseTerms(dtm,sparse = 0.9); xdtm

#4 which term appears  most frequently
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
sort(colSums(xdtm),decreasing = T)

#5 thrid most frequently occuring word among reviews with rating of 5
data2 = cbind(review_rating = data$review_rating,xdtm)
rating5 = subset(data2, review_rating == 5)
sort(colSums(rating5),decreasing = T)

####################Section 4
#1-4 CART model
set.seed(1031)
split = sample(1:nrow(data2),size = 0.7*nrow(data2))
train = data2[split,]
test = data2[-split,]

library(rpart); library(rpart.plot)
tree = rpart(review_rating~.,train)
rpart.plot(tree)

#5
reg = lm(review_rating~.,train)
summary(reg)

#6 CART MDOEL rmse
pred_tree = predict(tree,newdata=test)
rmse_tree = sqrt(mean((pred_tree - test$review_rating)^2)); rmse_tree

#7 linear regression rmse
pred_reg = predict(reg, newdata=test)
rmse_reg = sqrt(mean((pred_reg-test$review_rating)^2)); rmse_reg