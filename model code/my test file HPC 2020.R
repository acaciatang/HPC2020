# CMEE 2020 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice in this instance
source("tst116_HPC_2020_main.R")
# it should take a faction of a second to source your file
# if it takes longer you're using the main file to do actual simulations
# it should be used only for defining functions that will be useful for your cluster run and which will be marked automatically

# do what you like here to test your functions (this won't be marked)
# for example
species_richness(c(1,4,4,5,1,6,1))
# should return 4 when you've written the function correctly for question 1

# you may also like to use this file for playing around and debugging
# but please make sure it's all tidied up by the time it's made its way into the main.R file or other files.

speciation_rate = 0.1
size = 100
wall_time = 5
interval_rich = 1
interval_oct = 10
burn_in_generations = 20
output_file_name = "my_test_file.rda"



#put files
sftp tst116@login.cx1.hpc.ic.ac.uk
#[enter imperial password]
put tst116_HPC_2020_main.R
put tst116_HPC_2020_cluster.R
put clusterRun.sh
exit

#set up
Ssh -l tst116 login.cx1.hpc.ic.ac.uk
#[enter imperial password]
#mkdir tst116_HPC_2020
#cd tst116_HPC_2020
#cat tst116_HPC_2020_cluster.R
#module load anaconda3/personal
#anaconda-setup
#conda install r

#run
qsub -J 1-100 clusterRun.sh

#get files
mv clusterRun.sh.e* error
mv clusterRun.sh.o* output
cd tst116_HPC_2020
tar czvf Simulation.tgz *
cd ../error
tar czvf Error.tgz *
cd ../output
tar czvf Output.tgz *

sftp tst116@login.cx1.hpc.ic.ac.uk
#[enter imperial password]
get tst116_HPC_2020/Simulation.tgz
get error/Error.tgz
get output/Output.tgz
exit

# name abundance bins according to the length of an octaves vector
abundance_bin_names <- function(octaves_vector) {
    abundance_bins <- c("1")
    for (n in (1:length(octaves_vector))) {
    abundance_bins <- append(abundance_bins, paste(2^n, "-", 2^(n+1)-1, sep = ""))}
    abundance_bins <- abundance_bins[-(length(abundance_bins))]
    return(abundance_bins)
}
# Challenge question D
coalescence_sim <- function(J=100, v=0.1){ # coalescence simulation, returns abundances arranged into octaves
  lineages <- rep(1, times = J) # vector of length J with all entries being 1
  abundances <- c() # initialise empty abundance vector
  N = J # initilise N
  theta <- v*(J-1)/(1-v)
  while(N > 1){ 
    j <- sample(c(1:N), 1) # choose an index
    randnum <- runif(1, min = 0, max = 1)
    if(randnum < theta/(theta+N-1)){ # speciation caused all individuals of this species
      abundances <- c(abundances, lineages[j]) # count number of individuals of species, put into abundance
    }
    else if(randnum >= theta/(theta+N-1)){ #coalescence
      i <- sample(c(1:N)[-j], 1) # choose index i that is not j
      lineages[i] = lineages[i] + lineages [j] # make individual i the same as individual j
    }
    lineages <- lineages[-j] # remove individual that has been accounted for
    N <- N-1 # decrease counter
  }
  abundances <- c(abundances, lineages) # put the remaining 1 item in lineages into abundances
  return(abundances) 
}

Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  #graphics.off()
  
  # timed coalescence simulations with community size 500
  set.seed(500)
  start_coalesc_500 <- proc.time()[[3]]
  oct_coalesc_500 <- replicate(250, octaves(coalescence_sim(500, personal_speciation_rate)))
  avg_oct_coalesc_500 <- Reduce(sum_vect, oct_coalesc_500)/250
  runtime_coalesc_500 <- proc.time()[[3]] - start_coalesc_500
  
  # timed coalescence simulations with community size 1000
  set.seed(1000)
  start_coalesc_1000 <- proc.time()[[3]]
  oct_coalesc_1000 <- replicate(250, octaves(coalescence_sim(1000, personal_speciation_rate)))
  avg_oct_coalesc_1000 <- Reduce(sum_vect, oct_coalesc_1000)/250
  runtime_coalesc_1000 <- proc.time()[[3]] - start_coalesc_1000
  
  # timed coalescence simulations with community size 2500
  set.seed(2500)
  start_coalesc_2500 <- proc.time()[[3]]
  oct_coalesc_2500 <- replicate(250, octaves(coalescence_sim(2500, personal_speciation_rate)))
  avg_oct_coalesc_2500 <- Reduce(sum_vect, oct_coalesc_2500)/250
  runtime_coalesc_2500 <- proc.time()[[3]] - start_coalesc_2500
  
  # timed coalescence simulations with community size 5000
  set.seed(5000)
  start_coalesc_5000 <- proc.time()[[3]]
  oct_coalesc_5000 <- replicate(250, octaves(coalescence_sim(5000, personal_speciation_rate)))
  avg_oct_coalesc_5000 <- Reduce(sum_vect, oct_coalesc_5000)/250
  runtime_coalesc_5000 <- proc.time()[[3]] - start_coalesc_5000
  
  octaves_coalesc_sims <- list(size500=avg_oct_coalesc_500, size1000=avg_oct_coalesc_1000, size2500=avg_oct_coalesc_2500, size5000=avg_oct_coalesc_5000)
  runtimes_coalesc_sims <- list(size500=runtime_coalesc_500/10, size1000=runtime_coalesc_1000/10, size2500=runtime_coalesc_2500/10, size5000=runtime_coalesc_5000/10)
  
  # load("plot_cluster_results.rda")
  # par(mfrow=c(1,2))
  # plot_cluster_results()
  barplot(octaves_coalesc_sims$size5000, main = "Relative Abundance in Varying Community Sizes Using Coalescence Simulations",
          xlab = "Species Abundance Bins", ylab = "Number of Unique Species",
          names.arg = abundance_bin_names(octaves_coalesc_sims$size5000),
          ylim = c(0, 30), col = 2)
  barplot(octaves_coalesc_sims$size2500, col = 3, add = T)
  barplot(octaves_coalesc_sims$size1000, col = 4, add = T)
  barplot(octaves_coalesc_sims$size500, col = 5, add = T)
  legend("topright", legend = c("Size 500", "Size 1000", "Size 2500", "Size 5000"), col = c(2:5), pch = 15)
  
  return("type your written answer here")
}