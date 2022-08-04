#---Back to cestes data------------------------------------------------------

# We will apply the function to list all files in the directory `data` with the extension `.csv`.
files_path <- list.files(path = "data/raw/cestes",
                         pattern = ".csv",
                         full.names = TRUE)

# The `files_path` object is a vector of five elements (after all, there are five files) containing the full name of the file. Let's use the contents of this vector in the `read.csv()` function. We will use the a loop to read all data at once.
file_names <- gsub(".csv", "", basename(files_path), fixed = TRUE)
for (i in 1:length(files_path)) {
  data <- read.csv(files_path[[i]])
  assign(file_names[i], data)
}

head(comm)[,1:6]
head(traits)[,1:6]
rownames(comm)[1:6]

rownames(comm) <- paste0("Site", comm[,1])
rownames(traits) <- traits$Sp
comm <- comm[,-1]
traits <- traits[,-1]
head(comm)[,1:6]
head(traits)[,1:6]

# Species richness can be calculated with the vegan package:

library(vegan)
richness <- vegan::specnumber(comm)

# Taxonomic measures can be calculated using diversity() function:

shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")

# Gower distance is a common distance metric used in trait-based ecology.

library(cluster)
library(FD)
gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)
#implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?

class(gow) #different classes
class(gow2)
plot(gow, gow2, asp = 1) #same values

# Using package SYNCSA
library(SYNCSA)
tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1)

#install.packages("FD")
library(FD)
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)

#We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)

library(taxize)
classification_data <- classification(sp_list$TaxonName, db =  "ncbi")
str(classification)
length(classification_data)

classification_data$"Arisarum vulgare"
classification_data[[1]]
classification_data[[4]]

library(dplyr)
table_ex <- classification_data[[1]] %>%
  filter(rank == "family") %>%
  select(name)

families <- list()
for(i in 1:length(classification_data)) {
  families[[i]] <- xxxxxxx
}
