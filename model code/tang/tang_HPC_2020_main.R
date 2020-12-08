# CMEE 2020 HPC excercises R code main proforma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Tsz So Acacia Tang"
preferred_name <- "Acacia"
email <- "tst116@ic.ac.uk"
username <- "acaciatang"

# please remember *not* to clear the workspace here, or anywhere in this file. If you do, it'll wipe out your username information that you entered just above, and when you use this file as a 'toolbox' as intended it'll also wipe away everything you're doing outside of the toolbox.  For example, it would wipe away any automarking code that may be running and that would be annoying!

# Question 1
species_richness <- function(community){
  return(length(unique(community)))
}

# Question 2
init_community_max <- function(size){
  #return(c(1:size))
  return(seq(from = 1, to = size, by = 1))
}

# Question 3
init_community_min <- function(size){
  return(rep(1, times = size))
}

# Question 4
choose_two <- function(max_value){
  return(sample(c(1:max_value), size = 2, replace = F))
}

# Question 5
neutral_step <- function(community){
  chosenOnes <- choose_two(length(community))
  community[chosenOnes[1]] <- community[chosenOnes[2]]
  return(community)
}

# Question 6
neutral_generation <- function(community){
  steps <- round((length(community)/2 + runif(1, min = -0.1, max = 0.1)))
  for (i in 1:steps){
    community <- neutral_step(community)
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community, duration)  {
  communities <- rep(list(community), duration)
  
  for (i in 2: duration){
    communities[[i]] <- neutral_generation(communities[[i-1]])
  }
  
  timeSeries <- sapply(1:duration, function(i) species_richness(communities[[i]]))
  return(timeSeries)
}

# Question 8
question_8 <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  p <- plot(
    c(1:200),
    neutral_time_series(init_community_max(100), 200),
    type = "l",
    col = "red",
    main = "Time Series Graph of Neutral Model Simulation",
    ylab = "Species Richness",
    xlab = "Time (generations)"
  )
  print(p)
  return("It will always converge upon species homogeneity, where the community is composed of individuals belonging to only one species. This is because during each step one individual is replaced by an individual of the same or different existing species. Also, once the last individual of a species is replaced, the species disappears from the community forever. So, without speciation, the number of species can only remain constant or decrease. Given enough time then, the community will eventually reach the lowest possible species richness where all individuals are from the same species (.")
}

# Question 9
neutral_step_speciation <- function(community,speciation_rate)  {
  x <- runif(1, min = 0, max = 1)
  if(x <= speciation_rate){
    chosenOne <- sample(c(1:length(community)), size = 1)
    community[chosenOne] <- max(community, length(community)) + 1
    return(community)
  } else{
    return(neutral_step(community))
  }
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  steps <- round((length(community)/2 + runif(1, min = -0.1, max = 0.1)))
  for (i in 1:steps){
    community <- neutral_step_speciation(community, speciation_rate)
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  communities <- rep(list(community), duration)
  
  for (i in 2: duration){
    communities[[i]] <- neutral_generation_speciation(communities[[i-1]], speciation_rate)
  }
  
  timeSeries <- sapply(1:duration, function(i) species_richness(communities[[i]]))
  return(timeSeries)
}

# Question 12
question_12 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  p <- plot(
    c(1:200),
    neutral_time_series_speciation(init_community_max(100), 0.1, 200),
    type = "l",
    col = "red",
    main = "Time Series Graph of Neutral Model Simulation With Speciation",
    ylab = "Species Richness",
    xlab = "Time (generations)"
  )
  print(p)
  return("Adding speciation to the model allows the system to reach dynamic equilibrium instead of always reaching the stable equilibrium of species homogeneity.")
}

# Question 13
species_abundance <- function(community)  {
  return(as.vector(sort(table(community), decreasing = T)))
}

# Question 14
octaves <- function(abundance_vector) {
  return(tabulate(floor(log(abundance_vector, base = 2))+1))
}

# Question 15
sum_vect <- function(x, y) {
  diff = length(x) - length(y)
  if(diff < 0){
    x <- c(x, rep(0, times = diff*-1))
  } else if(length(y) < length(x)){
    y <- c(y, rep(0, times = diff))
  }
  return(x+y)
}

# Question 16 
question_16 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
    
  community <- init_community_max(100)
  
  for (i in 1:200){
    community <- neutral_generation_speciation(community, 0.1)
  }
  
  abundance <- species_abundance(community)
  abunOctave <- octaves(community)
  sum <- abunOctave

  for (i in 1:2000){
    if(i%%20 == 0){
      community <- neutral_generation_speciation(community, 0.1)
        
      abundance <- species_abundance(community)
      abunOctave <- octaves(community)
      sum <- sum_vect(sum, abunOctave)
    }
    else{
      community <- neutral_generation_speciation(community, 0.1)
    }
  }
  average <- sum/101
  barplot(
    average,
    main = "Average Species Abundance Distribution",
    xlab = "Octaves"
    ylab = "Frequency"
    names.arg = c(1:length(average))
  )
  return("type your written answer here")
}

# Question 17
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {
  community <- init_community_min(size)
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  combined_results <- list() #create your list output here to return
  # save results to an .rda file
  
}

plot_cluster_results <- function()  {
    # clear any existing graphs and plot your graph within the R window
    # load combined_results from your rda file
    # plot the graphs
    
    return(combined_results)
}

# Question 21
question_21 <- function()  {
    
  return("type your written answer here")
}

# Question 22
question_22 <- function()  {
    
  return("type your written answer here")
}

# Question 23
chaos_game <- function()  {
  # clear any existing graphs and plot your graph within the R window
  
  return("type your written answer here")
}

# Question 24
turtle <- function(start_position, direction, length)  {
    
  return() # you should return your endpoint here.
}

# Question 25
elbow <- function(start_position, direction, length)  {
  
}

# Question 26
spiral <- function(start_position, direction, length)  {
  
  return("type your written answer here")
}

# Question 27
draw_spiral <- function()  {
  # clear any existing graphs and plot your graph within the R window
  
}

# Question 28
tree <- function(start_position, direction, length)  {
  
}

draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window

}

# Question 29
fern <- function(start_position, direction, length)  {
  
}

draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window

}

# Question 30
fern2 <- function(start_position, direction, length)  {
  
}
draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window

}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
}

# Challenge question B
Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
}

# Challenge question C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
}

# Challenge question D
Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  return("type your written answer here")
}

# Challenge question G should be written in a separate file that has no dependencies on any functions here.


