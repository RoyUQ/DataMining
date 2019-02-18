# Jialuo Ding 44732024
# This file aim to cluster the data

# Extract data
bcw = readRDS("./data/bcw_processed.Rda")

# Select first 9 vars (i.e. remove Class var)
bcw2 <- bcw[,1:9]
nrow(bcw2)
ncol(bcw2)

# For reproducible result
set.seed(2024)


# Cluster with K-means into nclust clusters
nclust = 2
(kmeans.result <- kmeans(bcw2,nclust))

# Plot the results of the clusters
jpeg(filename = "./plots/K2.jpeg")
plot(bcw2[, c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans.result$cluster)
title(paste("k = ", nclust, sep = ""))
points(kmeans.result$centers[, c("Clump.Thickness","Uniformity.of.Cell.Size")], 
       col=1:nclust, pch = 8, cex = 2)
dev.off()

# Plot the results of the Class column
jpeg(filename = "./plots/class.jpeg")
palette(c("red","black"))
plot(bcw[, c("Clump.Thickness","Uniformity.of.Cell.Size")], col = bcw$Class)
title(paste("Class"))
dev.off()
palette("default")

# Cluster the data into more than 2 clusters
jpeg(filename = "./plots/K3.jpeg")
nclust = 3
(kmeans.result <- kmeans(bcw2,nclust))
plot(bcw[, c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans.result$cluster)
title(paste("k = ", nclust, sep = ""))
points(kmeans.result$centers[, c("Clump.Thickness","Uniformity.of.Cell.Size")], 
       col=1:nclust, pch = 8, cex = 2)
dev.off()

jpeg(filename = "./plots/K4.jpeg")
nclust = 4
(kmeans.result <- kmeans(bcw2,nclust))
plot(bcw[, c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans.result$cluster)
title(paste("k = ", nclust, sep = ""))
points(kmeans.result$centers[, c("Clump.Thickness","Uniformity.of.Cell.Size")], 
       col=1:nclust, pch = 8, cex = 2)
dev.off()

jpeg(filename = "./plots/K5.jpeg")
nclust = 5
(kmeans.result <- kmeans(bcw2,nclust))
plot(bcw[, c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans.result$cluster)
title(paste("k = ", nclust, sep = ""))
points(kmeans.result$centers[, c("Clump.Thickness","Uniformity.of.Cell.Size")], 
       col=1:nclust, pch = 8, cex = 2)
dev.off()

# Apply hierarchical clustering to the data 
set.seed(2024)

# Generate a subset of the data for a clear image and lower cost in hierarchical clustering
n = nrow(bcw2)
idx <- sample(1:n, 40)
bcwSample <- bcw2[idx,]
jpeg(filename = "./plots/h2.jpeg")
hc <- hclust(dist(bcwSample))
plot(hc, hang = -1, labels = bcw$Class[idx])

# Cut the dendrogram into 2 clusters
nclust2 = 2
rect.hclust(hc, k = nclust2)
group <- cutree(hc, k=nclust2)
dev.off()

# Create a function for cutting the dendrogram
hcluster <- function(n){
  set.seed(2024)
  hc <- hclust(dist(bcwSample))
  plot(hc, hang = -1, labels = bcw$Class[idx])
  rect.hclust(hc, k = n)
  group <- cutree(hc, k=n)
}

nclust3 = 3
nclust4 = 4
nclust5 = 5

# 3 cluster
jpeg(filename = "./plots/h3.jpeg")
hcluster(nclust3)
dev.off()

# 4 cluster
jpeg(filename = "./plots/h4.jpeg")
hcluster(nclust4)
dev.off()

# 5 cluster
jpeg(filename = "./plots/h5.jpeg")
hcluster(nclust5)
dev.off()

# Try different agglomeration methods in hierarchical clustering
hcluster2 <- function(n, myMethod){
  set.seed(2024)
  hc <- hclust(dist(bcwSample), method = myMethod)
  plot(hc, hang = -1, labels = bcw$Class[idx])
  rect.hclust(hc, k = n)
  group <- cutree(hc, k=n)
}


# single
jpeg(filename = "./plots/h2_single.jpeg")
hcluster2(2,"single")
dev.off()
jpeg(filename = "./plots/h3_single.jpeg")
hcluster2(3,"single")
dev.off()
jpeg(filename = "./plots/h4_single.jpeg")
hcluster2(4,"single")
dev.off()
jpeg(filename = "./plots/h5_single.jpeg")
hcluster2(5,"single")
dev.off()

# complete
jpeg(filename = "./plots/h2_complete.jpeg")
hcluster2(2,"complete")
dev.off()
jpeg(filename = "./plots/h3_complete.jpeg")
hcluster2(3,"complete")
dev.off()
jpeg(filename = "./plots/h4_complete.jpeg")
hcluster2(4,"complete")
dev.off()
jpeg(filename = "./plots/h5_complete.jpeg")
hcluster2(5,"complete")
dev.off()

# average
jpeg(filename = "./plots/h2_ave.jpeg")
hcluster2(2,"average")
dev.off()
jpeg(filename = "./plots/h3_ave.jpeg")
hcluster2(3,"average")
dev.off()
jpeg(filename = "./plots/h4_ave.jpeg")
hcluster2(4,"average")
dev.off()
jpeg(filename = "./plots/h5_ave.jpeg")
hcluster2(5,"average")
dev.off()

