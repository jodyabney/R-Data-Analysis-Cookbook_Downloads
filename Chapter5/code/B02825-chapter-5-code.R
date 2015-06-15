#Code snippets for Chapter 5
#===========================

#Recipe: Performing Cluster analysis using k-means clustering
#----------------------------------------------------------
auto <- read.csv("auto-mpg.csv")

rdacb.scale.many <- function (dat, column_nos) {
  nms <- names(dat)
  for (col in column_nos) {
    name <- paste0(nms[col], "_z")
    dat[name] <- scale(dat[, col])
  }
  cat(paste("Scaled", length(column_nos), "variable(s)\n"))
  dat
}


auto <- rdacb.scale.many(auto, 2:7)

names(auto)

set.seed(1020)
fit <- kmeans(auto[, 10:15], 5)
fit

pairs(auto[,2:7], col=c(1:5)[fit$cluster])

library(cluster)
clusplot(auto[,10:15], fit$cluster, color = TRUE, shade = TRUE, labels=0, lines=0)

rdacb.kmeans.plot <- function (data, num_clust = 15, seed = 9876) 
{
  set.seed(seed)
  ss <- numeric(num_clust)
  ss[1] <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:num_clust) {
    ss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:num_clust, ss, type = "b", pch = 18, xlab = "# Clusters", 
       ylab = "Total within_ss across clusters")
}


rdacb.kmeans.plot(auto[,10:15])


#Recipe: Performing Cluster analysis using hierarchical clustering
#--------------------------------------------------------------

auto <- read.csv("auto-mpg.csv")

rdacb.scale.many <- function (dat, column_nos) {
  nms <- names(dat)
  for (col in column_nos) {
    name <- paste0(nms[col], "_z")
    dat[name] <- scale(dat[, col])
  }
  cat(paste("Scaled", length(column_nos), "variable(s)\n"))
  dat
}

auto <- rdacb.scale.many(auto, 2:7)

names(auto)

dis <- dist(auto[,10:15], method = "euclidean")

fit <- hclust(dis, method = "ward")

plot(fit, labels = FALSE, hang = 0)

rect.hclust(fit, k=4, border="blue")

cluster <- cutree(fit, k=4)

cluster

#Recipe: Reducing dimensionality with Principal Component Analysis (PCA)
#---------------------------------------------------------------------

bh <- read.csv("BostonHousing.csv") 

round(cor(bh[,-14]),2)

plot(bh[,-14])

bh.pca <- prcomp(bh[,-14], scale = TRUE)

print(bh.pca)

summary(bh.pca)

plot(bh.pca)

plot(bh.pca, type = "lines")

biplot(bh.pca, col = c("gray", "black"))

head(bh.pca$x, 3)

bh.pca$rotation
bh.pca$sdev







