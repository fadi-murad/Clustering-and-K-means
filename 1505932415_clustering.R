# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

##install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function


df<-scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(df, nc=15, seed=1234){
	              wss <- (nrow(df)-1)*sum(apply(df,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(df, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)



# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

##we can say that 3 cluster soluton could be a good answer for our question
##we can notice that there is a big drop till cluster no. 3 after that the drop decreased


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

##also here the 3 cluster model could be the best solution becuase 14 out of 24 criterias suggest 3 cluster solution.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df,centers = 3, iter.max = 1000)


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster,wine$Type)
##it's very obvious that:
##all the wine type 1 listed in the cluster no. 3
##all the wine type 3 listed in the cluster no. 1
## 65 out of 71 records of wine type 2 listed in cluster no.2 and 6 records listed in the other 2 clusters
# we can say that the clustering model is a good model (only 6 out of 178 records inserted in unappropriate cluster)

#also we can check the relation between type and cluster
#using an adjusted Rank index randIndex [-1 not good to 1 very good]
library(flexclust)
randIndex(table(fit.km$cluster,wine$Type))
#0.89 very good



# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
clusplot(wine,fit.km$cluster )
#in general we can say it's a good clustering, although the distance between some values on the 1st and 2nd clusters are very small 
