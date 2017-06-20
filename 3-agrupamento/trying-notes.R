small_iris <- sample_n(iris, 50)

iris_hclust <- small_iris %>% 
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  dist() %>% 
  hclust(method = "complete")

plot(iris_hclust, labels = small_iris$Species)

###

(clusters <- cutree(iris_hclust, 3))

ggplot(small_iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color = factor(clusters)))

###

small_iris %>% 
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  dist() %>% 
  hclust(method = "single") %>% 
  plot(labels = small_iris$Species)

###

data(ruspini, package="cluster")
ruspini <- ruspini[sample(1:nrow(ruspini)),]
plot(ruspini)

ruspini_scaled <- scale(ruspini)
plot(ruspini_scaled)

###

d <- dist(ruspini_scaled)
hc <- hclust(d, method="complete")
plot(hc)
rect.hclust(hc, k=4)

plot(as.dendrogram(hc), leaflab="none") # plot dendrogram without leaf labels

cluster_complete <- cutree(hc, k=4)
plot(ruspini_scaled, col=cluster_complete)