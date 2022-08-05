library(vegan)
data(dune)
data(dune.env)
table(dune.env$Management)

bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))

library(cluster)
b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")

par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)

par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)

par(mfrow = c(1, 1))

### PCA ------------------------------------------------------------------------

is(chord_distance)
norm <- decostand(dune,"norm")
pca <- rda(norm)

plot(pca)
summary(pca)

#Let's do a pca of the enviromental matrix
names(dune.env)
apply(dune.env, 2, class)


library(dplyr)

dune.env$A1 <- as.numeric(dune.env$A1)
dune.env$Moisture <- as.numeric(dune.env$Moisture)
dune.env$Manure <- as.numeric(dune.env$Manure)

pca_env <- rda(dune.env[, c("A1", "Moisture", "Manure")])

plot(pca_env)
cor(dune.env)
