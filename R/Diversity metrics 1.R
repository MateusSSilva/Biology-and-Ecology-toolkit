library(vegan)

community_A <- c(10, 6, 4, 1)
community_B <- c(17, rep(1,7))

diversity(community_A, "shannon")
diversity(community_B, "shannon")
diversity(community_A, "invsimpson")
diversity(community_B, "invsimpson")

ren_comA <- renyi(community_A)
ren_comB <- renyi(community_B)
ren_AB <- rbind(ren_comA, ren_comB)
matplot(t(ren_AB), type = "l", axes = F)
box()
axis(side = 2)
axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "inf"), at = 1:11)
legend("topright", legend = c("Community A", "Community B"), lty = c(1, 2), col(1, 2))
