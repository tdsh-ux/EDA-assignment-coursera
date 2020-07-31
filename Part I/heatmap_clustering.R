# Hierarchical clustering
library(tidyverse)

set.seed(1234)
par(mar = c(5,5,5,5))

x <- rnorm(12, mean = rep(1:3, each = 4), sd = .2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = .2)
plot(x, y, col = "blue", pch = 19, cex = 2)

text(x + .05, y +.05, labels = as.character(1:12))
df <- data.frame(x = x, y = y)
dist(df)   # gives the distance matrix

# choose the closer points; they form a cluster.
# do it again;
# eventually, you get a hierarchical set of clusters.

distxy <- dist(df)
hClustering <- hclust(distxy)
plot(hClustering)


heatmap(as.matrix(df))
# organize the table with the hierarclical clustering algorithm.
