## Script for loading data file and reformatting into a long-format dataframe
##
## DEPENDENCIES: ggplot2
##
## You need to chage the file name to wherever you have saved your data file 
## and then just run this (1) and section (2) together that will format your data
## from wide-format as it is currently to a long-format dataframe for plotting. 
##
## Note: if you have dplyr (or tidyverse) already installed, or have time to install 
## these now, it is more straightforward to use the other script provided that does 
## this using that package

# packages
library(ggplot2)


## (1) read in csv file (wide-format)
dat_wf <- read.csv("files/completed_datatable.csv", header = T, 
                   colClasses = c(rep("integer", 11), "character"))

# Check it: does it look like you expect?
View(dat_wf)

# convert to a long-format dataframe
col_index <- c(which(names(dat_wf) == "point_1"), 
               which(names(dat_wf) == "point_10"))
dat_lf <- expand.grid(id_species = dat_wf$Morphospecies, point = 1:10)
dat_lf$abundance <- unlist(dat_wf[,col_index[1]:col_index[2]])


## (2) calculate point-level statistics
# abundance
dat_summary <- data.frame(point = 1:10, 
           habitat = c(rep("Forest", 5), rep("Pasture", 5)), 
           point_habitat = rep(1:5, 2))

dat_summary$abundance <- aggregate(abundance ~ point, data = dat_lf, 
                                   FUN = sum)$abundance

# species of richness
# note: this is just counting how many non-NA values there are for each point
dat_summary$n_species <- aggregate(abundance ~ point, data = dat_lf, 
                                   FUN = function(x) sum(!is.na(x)))$abundance

# calculate cumulative abundance and species richness in each habitat
dat_summary$c_abu <- with(dat_summary, c(cumsum(abundance[1:5]), 
                                         cumsum(abundance[6:10])))

dat_summary$c_sp <- with(dat_summary, c(cumsum(n_species[1:5]), 
                                        cumsum(n_species[6:10])))


## (3) plotting
## note: dat_summary contains all the information you need for your plot


# task 1: produce plot of point-level species richness by habitat type
# e.g. histogram, dotplot, points (+horizontal jittering)

# task 2: species accumulation curve