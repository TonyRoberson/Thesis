# Import cleaned and merged thesis data set
# Need to set appropriate working directory
# where dataset lives

thesis <- read.csv(file = "thesis_merged.csv", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)