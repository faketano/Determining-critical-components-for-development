#Load Packages
library(ggplot2)
library(gtools)
library(factoextra)
library(dplyr)
library(ClusterR)
library(cluster)
library(gridExtra)
library(purrr)
library(tidyverse)
library(cluster)    
library(factoextra) 
library(dendextend)


# INFO FROM WHERE THE DATASET WAS TAKEN FROM
#https://solability.com/the-global-sustainable-competitiveness-index/the-index/natural-capital/

#Loading Dataset
# Dataset with 5 features (Natural Capital, Resource Intensity, Social Capital, Intellectual Capital and Governance)
# Each of these features is already an INDEX from 0 to 100. 
# Each index contains numerous measures. The whole 5 indexes are made with more than 100 different indicators.
# Dataset already between 0 and 100. 
# No NANS in the dataset nor atypical values. 
df <- read.csv("UNSUPERVISED_DF.csv", row.names = 1)


# Let's get the distance between observations for only 25 countries.
# We do this in order to get an idea of the countries characteristics.
# Also, we want to take a glance to the data and see if it matches what we know from the real world
# Countries in RED are not simmilar with eachother while countries in Blue are more simmilar. 
distance <- get_dist(sample_n(df, 25))
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


# Now let's do some clustering and see how it cathegorizes each country.
# THIS DOES PCA TO BE ABLE TO GRAPH BECAUSE WE HAVE MORE THAN 2 DIMENSIONS. 
# The thing with CLUSTERING is that we NEED to specify the number of clusters.
k_means <- kmeans(df, centers = 3, nstart = 25)
str(k_means)

#GRAPH OF THE CLUSTERING. Since we have more than 2 dimensions, when performing clustering, it does PCA automatically
fviz_cluster(k_means, data = df, , labelsize = 9,  pointsize = 0.9, alpha=0.8,main='Clustering with k=3')
# We see here that if we wanted to have 3 clusters for different types of countries.
# Developed, in the way of development and undeveloped then we would get this clusters.
# We see that since we have 5 features (that are containing different measures)
# The algorithm for clustering already performs PCA and it shows that: 
# The first dimention explains 52% of variability and second dimention 22% of variability
# Which is quite good, because, above 75% of variability is explained by the two main dimensions.
# However, since we are here talking about DEVELOPMENT of countries, it is not useful to use PCA.
# The main thing we want is to have INTERPRETABILITY. We want to know what contributes to the development.
# Why? Because we are interested in POLICY. Numbers have to tell a story.


# That said, we would like to know what should be the right amount of clusters.
# We have the following hipothesis. 
# n=2 (developed, undeveloped)
# n=3 (developed, in way of development, undeveloped)
# n=4 Highly developed (expecting Scandinavia, Australia, Singapore, Switzerland, etc), moderately developed (Argentina, Uruguay, Bulgaria),
# Moderately undeveloped (Egypt, Philippines, etc), Highly Undeveloped (Sierra Leone, Burundi, Zimbawe)
# And so on... Hence, the more clusters, the more granularity we could have.

# So even though we have the economic theory behind, we will use different methods to find out the optimum number of clusters. 

# ELBOW METHOD.
# The scope of clustering is getting internal homogeneity and external heterogeneity. 
# Hence total intra-cluster variation (known as total within-cluster variation or total within-cluster sum of square WSS) is minimized.
# We compute it and when graphing it, the location of a bend (knee) in the plot is the indicator of the appropriate number of clusters.

# Optimun number of Clusters
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss", linecolor = 'red', k.max=5)
# We limit the number of clusters to 5 to match economic theory and also to make analysis understandable.
# We need to find the "ELBOW" or where the curve breaks. 
# It seems that the optimum number of clusters is 2.

# SILUETTE METHOD.
fviz_nbclust(df, kmeans, method = "silhouette", linecolor='red', )
# The optimal number of clusters is the one that maximizes the average silhouette.
# Here, clearly the optimum number of clusters is 2. 
# 2 clusters maximize the average silhouette values with 4 clusters coming in as second best.

# GAP Statistic.
# It compares the total intracluster variation for different values of k.
# The estimate of the optimal clusters k will be the value that maximizes the GAP.
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
#Visualization
fviz_gap_stat(gap_stat, linecolor='red')
# It tells us that the optimum number of clusters is 5

# We choose to do it with 2. Since 2 out of 3 told us that 2 was the optimum number of clusters.

set.seed(333)
final <- kmeans(df, 2, nstart = 25)
print(final)


#We see the following:
final[1] #1 is DEVELOPED and 2 is UNDEVELOPED

final[2] #CENTROIDS

#WHOLE GRAPH
fviz_cluster(final, data = df, , labelsize = 9,  pointsize = 0.9, alpha=0.8,main='Clustering with k=2')

# When we analize the information we see the difference between the means of the clusters.
# We see that the heterogenieties are concentrated in mainly three cathegories (differences)
# 1) Intelectual Capital
# 2) Social Capital
# 3) Governance


# Cluster 1 DEVELOPED does WORSE in Resource Intensity than UNDEVELOPED

# We are going to analize this separatelly in order to avoid the algorith to perform PCA 
# We want to be able to CONCLUDE and to INTERPRET the results. 

# 2^3 possible combinations but relevant one are the following
# 1) Intelectual Capital and Social capital
# 2) Intelectual Capital and Governance
# 3) Social Capital and Governance

# If we only choose 2 dimensions.
# We always choose Governance because it is 
df1 <- df[c(3,4)] # Social Capital and Intellectual Capital
df2 <- df[c(3,5)] # Social Capital and Governance
df3 <- df[c(4,5)] # Intellectual Capital and Governance

#CLUSTER 0. PCA
k_means0 <- kmeans(df, centers = 2, nstart = 25)
fviz_cluster(k_means0, data = df, labelsize = 9,  pointsize = 0.9, alpha=0.8, labels = c('Str','as'))

#CLUSTER 1. Social Capital and Intellectual Capital
k_means1 <- kmeans(df1, centers = 2, nstart = 25)
fviz_cluster(k_means1, data = df1, labelsize = 9,  pointsize = 0.9, alpha=0.8, labels = c('Str','as'))

#CLUSTER 2. Social Capital and Governance
k_means2 <- kmeans(df2, centers = 2, nstart = 25)
fviz_cluster(k_means2, data = df2, labelsize = 9,  pointsize = 0.9, alpha=0.8, labels = c('Str','as'))

#CLUSTER 3. Intellectual Capital and Governance
k_means3 <- kmeans(df3, centers = 2, nstart = 25)
fviz_cluster(k_means3, data = df3,labelsize = 9,  pointsize = 0.9, alpha=0.8, labels = c('Str','as'))


# We call variables and we will create a graph
p0 <- fviz_cluster(k_means0, geom = "point",  data = df) + ggtitle("5 features with PCA")
p1 <- fviz_cluster(k_means1, geom = "point", data = df1,labelsize = 9,  pointsize = 0.9, alpha=0.8, labels = c('Str','as')) + ggtitle("Social Capital and Intellectual Capital")
p2 <- fviz_cluster(k_means2, geom = "point",  data = df2,labelsize = 9,  pointsize = 0.9, alpha=0.8, labels = c('Str','as')) + ggtitle("Social Capital and Governance")
p3 <- fviz_cluster(k_means3, geom = "point",  data = df3,labelsize = 9,  pointsize = 0.9, alpha=0.8, labels = c('Str','as')) + ggtitle("Intellectual Capital and Governance")

# Grid of graphs
grid.arrange( p1, p2, p3, nrow = 3)


#Hierarquical Clustering
#We can perform agglomerative HC with hclust.
#First we compute the dissimilarity values with dist and then feed these values into hclust
# then we specify the agglomeration method to be used (i.e. “complete”, “average”, “single”, “ward.D”).
#We can then plot the dendrogram.

# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
#Maximum or complete linkage: 
#It tends to produce more compact clusters.
hc1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.35, hang = -1, cex.axis = 1,cex.lab = 0.2)
#Cut it to 2 CLUSTERS (developed and undeveloped)
cut_1 <- cutree(hc1, k = 2)
rect.hclust(hc1 , k = 2, border = 2:6)
abline(h = 2, col = 'red')


# Hierarchical Clustering Using AVERAGE
#Mean or average linkage: The distance between two clusters is defined as the average distance between the elements in cluster 1 and the elements in cluster 2.
hc2 <- hclust(d, method = "average" )
# Plot the obtained dendrogram
plot(hc2, cex = 0.35, hang = -1, cex.axis = 1,cex.lab = 0.2)
#Cut it to 2 CLUSTERS (developed and undeveloped)
cut_2 <- cutree(hc2, k = 2)
rect.hclust(hc2 , k = 2, border = 2:6)
abline(h = 0, col = 'blue')

#Other methods (single, centroid, ward) would't give clear clusters.

#Computing accuracy of clustering
# Compute cophentic distance
cophe1 <- cophenetic(hc1)
# Correlation between cophenetic distance and the original distance
cor(d, cophe1) #0.58

cophe2 <- cophenetic(hc2)
cor(d,cophe2) #0.61


#Hierarchical Clustering using Agglomerative Nesting (AGNES)
hc_agnes <- agnes(df, method = "complete")
# Agglomerative coefficient
hc_agnes$ac

#Different methods
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)
#average    single  complete      ward 
#0.7766799 0.5503107 0.8952382 0.9675572 

#This allows us to find certain hierarchical clustering methods that can identify stronger clustering structures. 
# Here we see that Ward’s method identifies the strongest clustering structure of the four methods assessed.

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.37, hang = -1, main = "Agglomerative Clustering") 

#Divisive Hierarchical Clustering
# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram

pltree(hc4, cex = 0.35, hang = -1, main = "Dendrogram of diana")
#In the dendrogram displayed above, each leaf corresponds to one observation. 
# As we move up the tree, observations that are similar to each other are combined into branches, which are themselves fused at a higher height.

#The height of the fusion, provided on the vertical axis, indicates the (dis)similarity between two observations.
#The higher the height of the fusion, the less similar the observations are.
#Note that, conclusions about the proximity of two observations can be drawn only based on the height where branches containing those two observations first are fused. 
# We cannot use the proximity of two observations along the horizontal axis as a criteria of their similarity.

#The height of the cut to the dendrogram controls the number of clusters obtained. 
# It plays the same role as the k in k-means clustering. 
# In order to identify sub-groups (i.e. clusters), we can cut the dendrogram with cutree:

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 2 groups
sub_grp <- cutree(hc5, k = 6)

# Number of members in each cluster
table(sub_grp)
## sub_grp
##  1  2  3  4 
##  33 34 42 71

df %>%
  mutate(cluster = sub_grp)


plot(hc5, cex = 0.3)
rect.hclust(hc5, k = 5, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp))

