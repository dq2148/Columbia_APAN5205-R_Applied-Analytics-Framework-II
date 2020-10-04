###################cluster_iris_story###################
install.packages('factoextra')
install.packages('ggplot')
install.packages('mclust')
dat = iris[1:100, c(1,2,5)]
head(dat)

#visualize 2 Species in terms of Sepal.Length and Sepal.Width
library(ggplot2)
ggplot(dat, aes(x=Sepal.Length, y=Sepal.Width, color=Species))+
  geom_point()

#Hierarchical Cluster Analysis
dat[,1:2] = scale(dat[,1:2])
clusters = hclust(d = dist(dat[,1:2],method = 'euclidean'),method = 'ward.D2')
library(factoextra)
fviz_dend(clusters, k=2)

  #extract cluster assignments and plot them
h_clusters = cutree(clusters,k = 2)
ggplot(data=cbind(dat,clusters = h_clusters), aes(x=Sepal.Length,y=Sepal.Width,color=factor(clusters)))+
  geom_point()

#K-means
library(ggplot2)
set.seed(617)
km=kmeans(x=dat[,1:2], centers=2, iter.max=1000, nstart=25)
k_cluster=km$cluster
ggplot(data=cbind(dat, clusters=k_cluster), aes(x=Sepal.Length, y=Sepal.Width, color=factor(clusters)))+
  geom_point()

#Model-based
library(mclust)
m_cluster = Mclust(dat[,1:2])$classification
ggplot(data=cbind(dat,clusters = m_cluster), aes(x=Sepal.Length,y=Sepal.Width,color=factor(clusters)))+
  geom_point()


###################clusterDemo###################

#create data
library(ggplot2)
set.seed(1031)
data=data.frame(x1=sample(1:10,10,replace=T),
                x2=sample(1:10,10,replace=T))
rownames(data)=1:10
data

#scatter plot
ggplot(data=data, aes(x=x1,y=x2))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F)+
  geom_text(aes(label=rownames(data)),hjust=-1.5,vjust=0.5)

#Hierarchical Clustering
distances=round(dist(data,method ='euclidean'),2)
distances
#cluster data based on distances
clust=hclust(distances, method='ward.D2')
plot(clust)

plot(clust)
rect.hclust(tree=clust, k=2)

library(dendextend)
plot(color_branches(as.dendrogram(clust),k=2))

clusters=cutree(clust, k=2)
clusters

plot(clust, label=clusters)

clust$height

#visualize the plot of Clusters
data2=cbind(data,clusters)
ggplot(data=data2, aes(x=x1,y=x2,color=factor(clusters)))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10), breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F, color=F)+
  geom_text(aes(label=rownames(data2)),,hjust=-1.5,vjust=0.5)

#try a 4-clster solution
plot(clust)
rect.hclust(tree=clust,k=4)

#color clusters in dendrogram
library(dendextend)
plot(color_branches(as.dendrogram(clust), k=4))

#cuut adta at 4 clusters
clusters=cutree(clust,k=4)
clusters

#visualize the plot of a 4-Cluster Solution
data2 = cbind(data,clusters)
ggplot(data=data2,aes(x=x1,y=x2,color=factor(clusters)))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F,color=F)+
  geom_text(aes(label=rownames(data2)),hjust=-1.5,vjust=0.5)


#K-Means Clustering
set.seed(100)
km=kmeans(data, centers=4, iter.max=1000)
km

within_ss = sapply(X = 1:9, 
                   FUN = function(x) kmeans(data,centers = x,iter.max = 100)$tot.withinss)

ratio_ss = sapply(X = 1:9, 
                  FUN = function(x) {
                    km = kmeans(data,centers = x,iter.max = 100)
                    ratio = km$betweenss/km$totss
                    return(ratio)
                  })
dat = data.frame(clusters=1:9,within_ss, ratio_ss)

# Elbow in Within_ss
library(ggplot2)
ggplot(dat,aes(x=clusters,y=within_ss))+
  geom_line(color='steelblue',size=1.4)+
  scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
  geom_vline(xintercept=4)

# Elbow in Ratio
ggplot(dat,aes(x=clusters,y=ratio_ss))+
  geom_line(color='steelblue',size=1.4)+
  scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
  geom_vline(xintercept=4)

library(cluster)
sil = sapply(2:9,FUN=function(x){
  pam(data,k=x)$silinfo$avg.width
})

ggplot(data.frame(clusters=2:9,avg_silhouette_width = sil),aes(x=clusters,y=avg_silhouette_width))+
  geom_line(color='steelblue',size=1.4)+
  scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
  geom_vline(xintercept=4)

km$centers
km$cluster
#visualize the solution
data3 = cbind(data2,kmc=km$cluster)
ggplot(data=data3,aes(x=x1,y=x2,color=factor(kmc)))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F,color=F)+
  geom_text(aes(label=rownames(data3)),hjust=-1.5,vjust=0.5)

#Moel-Base Clustering
library(mclust)
clus = Mclust(data)
summary(clus)

clus4 = Mclust(data,G = 4)
summary(clus4)

bic = sapply(1:9,FUN=function(x){
  -Mclust(data,G=x)$bic
})
dat = data.frame(clusters=1:9,bic)
ggplot(dat,aes(x=clusters,y=bic))+
  geom_line(color='steelblue',size=1.4)+
  scale_x_continuous(breaks=1:9,minor_breaks = 1:9)

mcluster = Mclust(data,G=4)

data4 = cbind(data3,mclust=mcluster$classification)
ggplot(data=data4,aes(x=x1,y=x2,color=factor(mclust)))+
  geom_point(aes(size=1.2))+
  scale_x_continuous(limits=c(1,10),breaks=1:10)+
  scale_y_continuous(limits=c(1,10),breaks=1:10)+
  guides(size=F,color=F)+
  geom_text(aes(label=rownames(data3)),hjust=-1.5,vjust=0.5)
