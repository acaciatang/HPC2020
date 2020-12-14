# CMEE 2020 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice 
graphics.off()

source("/rds/general/user/tst116/home/tang_HPC_2020_main.R")
SIZE <- c(500, 1000, 2500, 5000)
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
#for (iter in 1:4){
    set.seed(iter)
    size <- SIZE[iter%%4 + 1]
    name <- paste("Simulation", iter, ".rda", sep = "")
    cluster_run (0.0021381, size, 690, 1, size/10, 8*size, name)
#}