mydata=read.csv(file.choose())
View(mydata)
my_data=mydata[,(2:14)]
View(my_data)

# PCA clustering
pca_wine=princomp(my_data,cor = TRUE,scores=TRUE,covmat = NULL)
summary(pca_wine)
loadings(pca_wine)
plot(pca_wine)
z=pca_wine$scores[,1:3] # Top 3 PCA Scores which represents the whole data
# preparing data for clustering (considering only pca scores as they represent the entire data)
plot(pca_wine$scores[,1:3],col="blue",cex=0.2)


norm_clus<-scale(z) # Scale function is used to normalize data

#Hierarchial clustering
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# Clustering the data using hclust function --> Hierarchical
hierarchy<-hclust(dist1,method="complete") # method here is complete linkage
plot(hierarchy)
plot(hierarchy,hang=-1) #common base
rect.hclust(hierarchy,k=6,border="red") # red border


#K-means clustering
km=kmeans(norm_clus,6) #k means
km$withinss

wss = (nrow(norm_clus)-1)*sum(apply(norm_clus, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:10) wss[i] = sum(kmeans(norm_clus, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

#Animation
#install.packages("animation")
l#ibrary(animation)
#km <- kmeans.ani(norm_clus, 4)

###6 clusters are ideal with pca[1:3]



############
###########
##Now clustering analysis by considering the whole pca
#z1=pca_wine$scores[,1:13] 
#norm_clus1=scale(z1)

#Hierarchial clustering
#dist2=dist(norm_clus1,method="euclidean")
#hierarchy2=hclust(dist2,method="complete")
#plot(hierarchy2)
#plot(hierarchy,hang=-1) #common base
#rect.hclust(hierarchy,k=6,border="red") # red border

#K-means clustering
#km1=kmeans(norm_clus1,6) #k means
#km1$withinss
#wss = (nrow(norm_clus1)-1)*sum(apply(norm_clus1, 2, var))		 # Determine number of clusters by scree-plot 
#for (i in 2:12) wss[i] = sum(kmeans(norm_clus1, centers=i)$withinss)
#plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
#title(sub = "K-Means Clustering Scree-Plot")

#km <- kmeans.ani(norm_clus1, 6)



##################
############
#by considering the whole pca we can say that we need 8 or 10 clusters

normalized_data=scale(my_data)  #To normalize the data

#Hierarichial clustering on whole data
d=dist(normalized_data,method="euclidean") #To find distance between points
d
fit=hclust(d,method="complete") # hierarchy clustering
plot(fit)
plot(fit,hang=-1) #common base
groups<-cutree(fit,k=6) #5 clusters
rect.hclust(fit,k=6,border="red") # red border

#K means clustering on whole data
kclus=kmeans(normalized_data,6)
kclus$withinss

#elbow curve & k ~ sqrt(n/2) to decide the k value
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:10) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss) #12 0r 15
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")


##With PCA and by considering the whole data both can have equal number of clusters(6) as checked with the scree-plot