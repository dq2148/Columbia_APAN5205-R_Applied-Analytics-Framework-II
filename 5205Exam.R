#SECTION 1
setwd('~/Desktop/APAN5205notes')
fastfood = read.csv('fastfood_survey_Exam.csv')

# 2
sum(is.na(fastfood))

#4
fastfood_std = scale(fastfood)
fastfood_std[1,4]

#6
distances = dist(fastfood_std, method = 'euclidean')
clusters = hclust(d = distances, method = 'ward.D2')
plot(as.dendrogram(clusters))

#8
cor(cophenetic(clusters),distances)

#10
h_clusters = cutree(tree = clusters,k=3)
table(h_clusters)

#12
data2=cbind(fastfood,h_clusters)
  str(data2)
  library(dplyr); library(ggplot2); library(tidyr)
  
  data2 %>%
    select(speed_of_service:taste_burgers,h_clusters)%>%
    group_by(h_clusters)%>%
    summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
    gather(key = var,value = value,speed_of_service:taste_burgers)%>%
    ggplot(aes(x=var,y=value,fill=factor(h_clusters)))+
    geom_col(position='dodge')+
    coord_flip()

#SECTION 2
library(recommenderlab)
data(MovieLense)

#15
nratings(MovieLense['717',])

#17
summary(getRatings(MovieLense['717']))

#19
set.seed(1706)
split = sample(nrow(MovieLense),size = 0.8*nrow(MovieLense))
train = MovieLense[split,]
test = MovieLense[-split,]
dim(train)

#21
recom_ubcf = Recommender(data = train,
                         method='UBCF',
                         parameter = list(method='cosine',nn=10,normalize='center'))
pred_ubcf = predict(recom_ubcf,newdata=test, method='topNList', n = 3);pred_ubcf
getList(pred_ubcf)['717']


#23
pred_ubcf_1 = predict(recom_ubcf, newdata=test, type='ratings')
getList(pred_ubcf_1)['717']

#25
recom_pop = Recommender(train,method='POPULAR')
pred_pop = predict(recom_pop,newdata = test, method='topNList', n = 3)
getList(pred_pop)['33']

#Section 3
#28
ausbeer = readRDS('ausbeer.RDS')
train = window(ausbeer,end=c(2006,04))
test = window(ausbeer, start=c(2007,01))
length(test)
length(train)

#30
average_model = meanf(train,h = 14)
window(average_model$mean,c(2008,01))

#31
accuracy(average_model)

#33
accuracy(average_model, x=test)

#36
auto_arima_model = auto.arima(train)
summary(auto_arima_model)


#39
auto_arima_forecast = forecast(auto_arima_model,h=14)
window(auto_arima_forecast$mean,c(2008,01))

#41
accuracy(auto_arima1_model,x=test)


#44
model1 = h2o.deeplearning(x=1:999,
                          y = 1000,
                          training_frame = train_h2o,
                          hidden = c(25,25,25),
                          seed=10)
