RNGversion(vstr = 3.6)
#Section 1 Preparing Data for Clustering

setwd('~/Desktop/APAN5205notes')
data = read.csv('fastfood_survey.csv')

#1 How many variables in dataset
str(data)

#2 how many variables does data_cluster have
data_cluster = data[,1:11]
data_cluster

#3 How many missing values for variable cleanliness
summary(data_cluster)

#4 How many rows of data would be left 
#if rows corresponding to missing values on any of the eleven variables 
#were removed? 
install.packages('data.table')
library(data.table)dd
dcp = data.table(data_cluster)
dcp_na = na.omit(dcp); dcp_na
str(dcp_na)

#5 What is the imputed value of observation 10 
#for the variable cleanliness?
library(mice)
set.seed(1706)
data_cluster = mice::complete(mice(data_cluster))
head(data_cluster, n = 10)

#6 What is the value of observation 10 
#for the variable cleanliness? 
data_cluster = scale(data_cluster)
head(data_cluster, n = 10)

#Section 2 Clustering Techniques

#1 How many elements are in the distance matrix/ Compute Euclidean distance
d = dist(x = data_cluster,method = 'euclidean')
str(d)

#2 Conduct a Hierarchical cluster analysis using the method, 'ward.D2'. 
# What is the Cophenetic correlation coefficient?
clusters = hclust(d = d, method='ward.D2')
cor(cophenetic(clusters),d)

#3 Which is best cluster solution based on shown ddistances
plot(clusters)
rect.hclust(tree=clusters,k = 2,border='tomato')
rect.hclust(tree=clusters,k = 3,border='tomato')

#4 How many observatioons be in smaller of 2 clusters
h_segments_2 = cutree(tree = clusters,k=2)
table(h_segments_2)

#5 ~ in smaller of 3 clusters
h_segments_3 = cutree(tree = clusters,k=3)
table(h_segments_3)

#6 How many observations in smaller cluster
set.seed(1706)
km_2 = kmeans(x = data_cluster,centers = 2,iter.max=100)
table(km_2$cluster)

#7 How many observations in smaller cluster
set.seed(1706)
km_3 = kmeans(x = data_cluster,centers = 3,iter.max=100)
table(km_3$cluster)

#8 Total within cluster sum of squares for a 3-cluster solution
set.seed(1706)
within_ss = sapply(2:10,FUN = function(x) kmeans(x = data_cluster,centers = 3,iter.max = 100)$tot.withinss)
within_ss

#9 Ratio of between sum of squares and total sum of squares
ratio_ss = sapply(2:10,FUN = function(x) {km = kmeans(x = data_cluster,centers = 3,iter.max = 100)
km$betweenss/km$totss} )
ratio_ss

#10 which oof following are good cluster solutions
#line graph of clusters
library(ggplot2)
within_ss = sapply(1:10,FUN = function(x) kmeans(x = data_cluster,centers = x,iter.max = 100)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#11 average Silhouette width for 2-cluster solution
library(cluster)
pam(data_cluster,k = 2)$silinfo$avg.width

#12
library(cluster)
pam(data_cluster,k = 3)$silinfo$avg.width

#13
silhoette_width = sapply(2:10,FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

#14 Model-based clusering technique
library(mclust)
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)

#15 model-based cluuserting to oa 2-cluster solution.
#how many observations are in the smallest cluster
clusters_mclust_2 = Mclust(data_cluster,G=2)
summary(clusters_mclust_2)

#16 compare 2-cluster solutions from hierarchical to k-means
k_segments_2=km_2$cluster
min(sum(h_segments_2==k_segments_2),
    sum(h_segments_2!=k_segments_2))

#17 compare 2-cluster solutions for k-means to Model-based
m_segments_2=clusters_mclust_2$classification
min(sum(k_segments_2==m_segments_2),
    sum(k_segments_2!=m_segments_2))

#Section 3 Interpreting Clusters

#1-4
set.seed(1706)

#k_segments
km_3 = kmeans(x = data_cluster,centers = 3,iter.max=100)
k_segments = km_3$cluster
table(k_segments)

#h_segments
h_segments = cutree(tree = clusters,k=3)
table(h_segments)

#m_segments
m_clusters = Mclust(data = data_cluster,G = 3)
m_segments = m_clusters$classification
table(m_segments)

library(dplyr)
library(tidyr)
library(ggplot2)
data2 = cbind(data_cluster,h_segments, k_segments,m_segments)
View(data2)
data2 = data.table(data2)

data2 %>%
  select(speed_of_service:taste_burgers,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,speed_of_service:taste_burgers)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

#5-7
prop.table(table(data2$k_segments,data[,20]),1)
lapply(12:21,function(x) round(prop.table(table(data2$k_segments,data[,x]),1),2)*100)


