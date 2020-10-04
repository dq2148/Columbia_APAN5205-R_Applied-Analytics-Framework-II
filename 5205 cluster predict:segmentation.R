wine=read.table('wine.csv',header = TRUE, sep=';')
str(wine)

install.packages('psych')
library(stats)
library(ggplot2)
library(psych)
library(factoextra)
library(mclust)

#split Data
library(lattice)
library(ggplot2)
library(caret)
set.seed(1706)
split=createDataPartition(y=wine$quality,p=0.7,list=F,groups=100)
train=wine[split,]
test=wine[-split,]

#predict
linear=lm(quality~.,train)
summary(linear)

seeLinear=sum(linear$residuals^2);seeLinear

predLinear = predict(linear,newdata=test)
sseLinear = sum((predLinear-test$quality)^2); sseLinear

#cluster then Predict using Regression
trainMinusDV = subset(train,select=-c(quality))
testMinusDV = subset(test,select=-c(quality))

#prepare data for clustering
library(caret)
preproc = preProcess(trainMinusDV)
trainNorm = predict(preproc,trainMinusDV)
testNorm = predict(preproc,testMinusDV)
mean(trainNorm$chlorides)

mean(testNorm$chlorides)

#Hierarchical Cluster Analysis
distances = dist(trainNorm,method = 'euclidean')
clusters = hclust(d = distances,method = 'ward.D2')
library(dendextend)
plot(color_branches(as.dendrogram(clusters),k = 2,groupLabels = F))

cluserGroups=cutree(tree=clusters,k=2)
table(clusterGroups)

#Visualize
library(psych)
temp = data.frame(cluster = factor(clusterGroups),
                  factor1 = fa(trainNorm,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(trainNorm,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

#k-means clustering
set.seed(1706)
km = kmeans(x = trainNorm,centers = 2,iter.max=10000,nstart=100)
#km$centers
mean(km$cluster==clusterGroups) # %match between results of hclust and kmeans














