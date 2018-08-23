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

###

plot(agrupamento_h_2d_dendograma, type = "triangle")

###  As Phylogenetic trees

library(ape)
plot(as.phylo(agrupamento_h_2d), cex = 0.9, label.offset = 1)

### CLADOGRAM

plot(as.phylo(agrupamento_h_2d), type = "cladogram", cex = 0.9, label.offset = 1)

### UNROOTED

plot(as.phylo(agrupamento_h_2d), type = "unrooted")

### FAN

plot(as.phylo(agrupamento_h_2d), type = "fan")

### RADIAL

plot(as.phylo(agrupamento_h_2d), type = "radial")
# add colors randomly
plot(as.phylo(agrupamento_h_2d), type = "fan", 
     tip.color = hsv(runif(15, 0.65, 0.95), 1, 1, 0.7), 
     edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), 
     edge.width = runif(20, 0.5, 3), 
     use.edge.length = TRUE, col = "gray80")

# colors
mypal = c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")
# cutting dendrogram in 5 clusters
clus5 = cutree(agrupamento_h_2d, 5)
op = par(bg = "#E8DDCB")
# Size reflects miles per gallon
plot(as.phylo(agrupamento_h_2d), type = "fan", tip.color = mypal[clus5], label.offset = 1, 
     cex = log(mtcars$mpg, 10), col = "red")


###

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R") # load code of A2R function
op = par(bg = "#EFEFEF")
A2Rplot(agrupamento_h_2d, k = 4, boxes = FALSE, col.up = "gray50", 
        col.down = c("#FF6B6B", "#4ECDC4", "#556270", "blue", "red", "pink"))

###

ggdendrogram(agrupamento_h_2d, rotate = TRUE, size = 4, theme_dendro = FALSE, color = "tomato")

### VISUALIZANDO..


agrupamento_h_2d_dendograma = as.dendrogram(agrupamento_h_2d)
labelColors = c("#CDB380", "#036564", "#EB6841", "#EDC951")
clusMember = cutree(agrupamento_h_2d, 4)

colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

clusDendro = dendrapply(agrupamento_h_2d_dendograma, colLab)
plot(clusDendro, main = "Cool Dendrogram", type = "triangle")
