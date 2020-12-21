# CMEE 2020 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice 
graphics.off()

source("/rds/general/user/tst116/home/tst116_HPC_2020_main.R") # source file, for HPC
#source("tst116_HPC_2020_main.R") # source file, for PC
SIZE <- c(500, 1000, 2500, 5000) # set community sizes
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) # for HPC
#for (iter in 5:8){ # for running on PC
    set.seed(iter) # set seed
    s <- SIZE[iter%%4 + 1] # select community size
    name <- paste("Simulation", iter, ".rda", sep = "") # name for output file
    cluster_run (speciation_rate=0.0021565, size=s, wall_time=690, interval_rich=1, interval_oct=s/10, burn_in_generations=8*s, output_file_name=name) # cluster_run from tst116_HPC_2020_main.R
#}  #for running on PC