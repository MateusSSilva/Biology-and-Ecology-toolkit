
# loading needed packages
library(tidyverse)
library(reshape2)
library(dplyr)

# Reading the data in R ----

# The files are all in .csv format, separated by commas. So let's use the `read.csv()` function. We could also use `read.table()` with the arguments `sep = ","` and `header = TRUE`
# So let's read the five sets of data. A very useful function for reading data is the `list.files()` from the **base** package. This function lists files in a directory, based on a pattern.

# We will apply the function to list all files in the directory `data` with the extension `.csv`.
files_path <- list.files(path = "data/raw/cestes",
                         pattern = ".csv",
                         full.names = TRUE)

files_path

# The `files_path` object is a vector of five elements (after all, there are five files) containing the full name of the file. Let's use the contents of this vector in the `read.csv()` function. We will use the a loop to read all data at once.
file_names <- gsub(".csv", "", basename(files_path), fixed = TRUE)
for (i in 1:length(files_path)) {
  data <- read.csv(files_path[[i]])
  assign(file_names[i], data)
}

commSum = colSums(comm)

commSorted = sort(colSums(comm), decreasing = TRUE)[2:6]



shannon_diversity <- function(comm) {
  sp <- ncol(comm)-1
  H <- 0
  N <- sum(comm[,2:sp+1])

  for(i in seq(1,sp)){
    p <- sum(comm[,i+1])/N
    Hi <- p*log(p)
    H <- H - Hi
  }
  return(H)
}
