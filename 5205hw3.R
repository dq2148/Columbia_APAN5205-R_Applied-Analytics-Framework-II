install.packages('arules')
library(arules)
library(arulesViz)

#1 Number of transaction in Groceries
data(Groceries)
dim(Groceries)

#2 top 5 frequently occuring items in dataset
summary(Groceries)

#3 support 0.01, conofidence 0.01, how many rules were generated
rules1 = apriori(Groceries,parameter = list(support = 0.01, confidence = 0.01))
summary(rules1)

#4 support 0.001, confidence 0.001
rules2 = apriori(Groceries,parameter=list(support=0.001,confidence=0.001))
summary(rules2)

#5-6
summary(rules1)

#7 whom have the highest confidence
rules3 = apriori(Groceries,parameter = list(support = 0.01, confidence = 0.01,minlen=2,maxlen=2))
summary(rules3)
inspect(rules3)
x = inspect(rules3)
x = x[x$count!=0,]
x[order(x$confidence,decreasing = T),]
inspect(subset(rules3,subset = confidence > 0.497))

#8 What is support for the rule, soda => whole milk?
x[order(x$support,decreasing = T),]
inspect(subset(rules3, lhs %ain% 'soda'& rhs %ain% 'whole milk'))

#10 a shopper picked up yogurt, what most likely item will also buy
inspect(sort(subset(rules3, lhs %ain% 'yogurt'),decreasing = T,by='confidence'))

####################Section 2
product_ratings_data = read_csv("product_ratings_data.csv")
data = product_ratings_data
#1 What format is the data in
dim(data)
summary(data)

#2 create a realRatingMatrix, how many ratings contain
library(recommenderlab)
ratings_matrix = as(data,"realRatingMatrix")
ratings_matrix

#3 what rating id u10023 give to prodd_14
ratings_matrix['u10023','prod_14']@data

#4 how many rows in train sample
set.seed(1031)
split = sample(nrow(ratings_matrix),size = 0.9*nrow(ratings_matrix))
train = ratings_matrix[split,]
test = ratings_matrix[-split,]
dim(train)

#5 How many products (prod) did user 20150 (u20150) rate?
train['u20150']

#6 How many user ratings did product 25 (prod_25) receive?
train[,'prod_25']

#7 What is the most common rating in the train sample?
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(train@data@x)

#8 What is the average rating for product 100 (prod_100) in the train sample?
colMeans(train[,'prod_100'])

#9 What is the average rating for product 100 (prod_100) in the normalized train sample?
train1 = normalize(train,method='center', row = TRUE)
colMeans(train1[,'prod_100'])

#10 cosine similarity Which of the following pairs is most similar?
similarity(train1[1:5,],method = 'cosine')

###################Section 3
#1 User-based collaborative recommender foor toplist 5 products for u10139 (in test data)
recom1 = Recommender(train,'UBCF')
pred1 = predict(recom1, newdata=test,method='topNList', n = 5);pred1
getList(pred1)$u10139

#2 Based on the recommender created above, what is the predicted rating of (prod_1) by (u10139)?
pred_ubcf = predict(recom1,newdata=test,type='ratings')
as(pred_ubcf,'matrix')['u10139','prod_1']

#3 item-based collaborative filtering recommender: the list of top 5 recommended products
recom_ibcf = Recommender(train, method='IBCF')
recom_ibcf
pred_ibcf_top = predict(recom_ibcf,newdata=test,method='topNList',n=5)
getList(pred_ibcf_top)$u10139

#4 predicted rating of (prod_1) by (u10139)?
pred_ibcf= predict(recom_ibcf,newdata=test,type='ratings')
as(pred_ibcf,'matrix')['u10139','prod_1']

#5 What is the RMSE for the item-based collaborative filtering recommender?
set.seed(1031)
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)
recom = Recommender(getData(es,'train'),method='IBCF')
pred_ibcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ibcf = calcPredictionAccuracy(x = pred_ibcf,data = getData(es,'unknown'))
accuracy_ibcf

#6 What is the RMSE for the user-based collaborative filtering recommender?
set.seed(1031)
recom = Recommender(getData(es,'train'),method='UBCF')
pred_ubcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf = calcPredictionAccuracy(x = pred_ubcf,data = getData(es,'unknown'))
accuracy_ubcf

#7 What is the RMSE for this modified user-based collaborative filtering recommender?
# recommenderRegistry$get_entries()$UBCF_realRatingMatrix to check
set.seed(1031)
recom = Recommender(getData(es,'train'),method='UBCF',parameter = list(nn=100))
pred_ubcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf = calcPredictionAccuracy(x = pred_ubcf,data = getData(es,'unknown'))
accuracy_ubcf

#8 non-personalized recommender: What is the RMSE for this non-personalized recommender?
recom_pop = Recommender(getData(es,'train'),method='POPULAR')
pred_pop = predict(recom_pop,newdata = getData(es,'known'),type='ratings')
accuracy_pop = calcPredictionAccuracy(x = pred_pop,data = getData(es,'unknown'))
accuracy_pop
