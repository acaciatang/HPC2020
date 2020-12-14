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
  communities <- rep(list(community),duration)
  
  for (i in 2:duration){
    communities[[i]] <- neutral_generation_speciation(communities[[i-1]], speciation_rate)
  }
  
  timeSeries <- sapply(1:duration, function(i) species_richness(communities[[i]]))
  return(timeSeries)
}
# Question 12
question_12 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  plot(
    c(1:200),
    neutral_time_series_speciation(init_community_max(100), 0.1, 200),
    type = "l",
    col = "red",
    main = "Time Series Graph of Neutral Model Simulation With Speciation",
    ylab = "Species Richness",
    xlab = "Time (generations)",
    ylim = c(0, 100)
  )
  lines(
    c(1:200),
    neutral_time_series_speciation(init_community_min(100), 0.1, 200),
    type = "l",
    col = "blue",
    main = "Time Series Graph of Neutral Model Simulation With Speciation",
    ylab = "Species Richness",
    xlab = "Time (generations)"
  )
  legend(47, 100, 
  legend = c("initial community with maximum species richness", "initial community with minimum species richness"),
  fill = c("red", "blue"))
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
  abunOctave <- octaves(abundance)
  sum <- abunOctave

  for (i in 1:2000){
    if(i%%20 == 0){
      community <- neutral_generation_speciation(community, 0.1)
      abundance <- species_abundance(community)
      abunOctave <- octaves(abundance)
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
    xlab = "Octaves",
    ylab = "Frequency",
    names.arg = c("1", "2-3", "4-7", "8-15", "16-31", "32-63")
  )
  return("type your written answer here")
}

# Question 17
cluster_run <- function(speciation_rate = 0.1, size = 100, wall_time = 5, interval_rich = 1, interval_oct = 10, burn_in_generations = 20, output_file_name = "my_test_file.rda")  {
  community <- init_community_min(size)
  start <- proc.time()[3]
  loop = 0
  richness = c()
  abundance = c()
  while (proc.time()[3] - start < wall_time*60){
    community <- neutral_generation_speciation(community, speciation_rate)
    while (loop <= burn_in_generations){
      if (loop %% interval_rich == 0){
        richness <- c(richness, species_richness(community))
      }
      loop = loop + 1
     }

    if (loop %% interval_oct == 0){
      oct <- list(octaves(species_abundance(community)))
      abundance <- c(abundance, oct)
    }
    loop = loop + 1
  }
  abundance <- abundance[-1]
  time <- proc.time()[3] - start
  parameters <- c(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations)
  save(richness, abundance, community, time, parameters, file = output_file_name)
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  s500 = s1000 = s2500 = s5000 = c()
  results <- list(s500, s1000, s2500, s5000) #create your list output here to return
  combined_results <- list(rep(NA, 4))
  for (i in 1:100){
    load(paste("Simulation/Simulation", i, ".rda", sep = ""))
    size <- i%%4 + 1
    sum = c()
    for (j in abundance){
      sum <- sum_vect(sum, j)
    }
    average <- list(sum/length(abundance))
    results[[size]] <- c(results[[size]], average)
  }
  c = 0
  for (a in results){
    c = c+1
    SUM = 0
    for (b in a){
      SUM <- sum_vect(SUM, b)
    }
    AVERAGE <- list(SUM/25)
    combined_results[[c]] <- AVERAGE
  }
  
  # save results to an .rda file
  save(combined_results, file = "CombinedResults.rda")
}

plot_cluster_results <- function()  {
    # clear any existing graphs and plot your graph within the R window
    graphics.off()
    # load combined_results from your rda file
    load("CombinedResults.rda")
    oct <- c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127", "128-255", "256-511", "512-1023", "1024-2047", "2048-4095")
    # plot the graphs
    par(mfrow=c(4,1))
    barplot(
    combined_results[[1]][[1]],
    main = "Average Species Abundance Distribution (Population Size = 500)",
    xlab = "Octaves",
    ylab = "Frequency",
    names.arg = oct[1:length(combined_results[[1]][[1]])],
    cex.names=0.7
    )
    
    barplot(
    combined_results[[2]][[1]],
    main = "Average Species Abundance Distribution (Population Size = 1000)",
    xlab = "Octaves",
    ylab = "Frequency",
    names.arg = oct[1:length(combined_results[[2]][[1]])],
    cex.names=0.7
    )
    
    barplot(
    combined_results[[3]][[1]],
    main = "Average Species Abundance Distribution (Population Size = 2500)",
    xlab = "Octaves",
    ylab = "Frequency",
    names.arg = oct[1:length(combined_results[[3]][[1]])],
    cex.names=0.5
    )

    barplot(
    combined_results[[4]][[1]],
    main = "Average Species Abundance Distribution (Population Size = 5000)",
    xlab = "Octaves",
    ylab = "Frequency",
    names.arg = oct[1:length(combined_results[[4]][[1]])],
    cex.names=0.5
    )
    return(combined_results)
}

# Question 21
question_21 <- function()  {
  dim <- log(8)/log(3)
  answer <- "The object is made up of 8 repeated parts arranged in a 3*3 manner with the middle kept empty, so to make it 3 times as wide would require 8 times the material."
  return(list(dim, answer))
}

# Question 22
question_22 <- function()  {
  dim <- log(20)/log(3)
  answer <- "The object is made up of 20 repeated parts arranged in a 3*3*3 manner with the middle of each face and the middle of the whole object kept empty, so to make it 3 times as wide would require 20 times the material."
  return(list(dim, answer))
}

# Question 23
chaos_game <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(0:4, 0:4, type = "n", xlab="", ylab="")
  xs <- c(0, 3, 4)
  ys <- c(0, 4, 1)
  points(xs, ys, col = "#CC0033", pch = 4)
  text(c(-0.08, 3.08, 4.08), c(0, 4, 1), labels=c("A", "B", "C"), cex=0.9, font=1)
  x = y = 0
  Xs = Ys = 0
  for (i in 1:10000){
    choose <- sample(3, 1)
    newX = (x + xs[choose])/2
    newY = (y + ys[choose])/2
    x = newX
    y = newY
    Xs <- c(Xs, newX)
    Ys <- c(Ys, newY)
  }
  points(Xs, Ys, col = "black", pch = 19, cex = 0.3)
  return("The position of X resembles that of a Sierpiński triangle. The triangle is skewed, because the positions of A, B, and C define the vertices of the triangle.")
}

# Question 24
turtle <- function(start_position, direction, length)  {
  x1 <- start_position[1]
  y1 <- start_position[2]
  angle <- pi/2 + direction
  x2 <- x1 + length*sin(angle)
  y2 <- y1 - length*cos(angle)
  lines(c(x1, x2), c(y1, y2))
  return(c(x2, y2)) # you should return your endpoint here.
}

# Question 25
elbow <- function(start_position, direction, length)  {
  start <- turtle(start_position, direction, length)
  turtle(start, angle, length*0.95)
  return(c(x2, y2)) # you should return your endpoint here.
}

# Question 26
spiral <- function(start_position, direction, length)  {
  start <- turtle(start_position, direction, length)
  if(length > 0.00001){
  spiral(start, direction + pi/4, length*0.95)
  }
  return("The function never stops running.")
}

# Question 27
draw_spiral <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(-4:4, -4:4, type = "n", xlab="", ylab="")
  spiral(c(-2, -3), 0, 3)
}

# Question 28
tree <- function(start_position, direction, length)  {
  if (length > 0.01) {
    start <- turtle(start_position, direction, length)
    tree(start, direction-pi/4, length*0.65)
    tree(start, direction+pi/4, length*0.65)
  }
}

draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(-4:4, -4:4, type = "n", xlab="", ylab="")
  tree(c(0, -4),pi/2, 2)
}

# Question 29
fern <- function(start_position, direction, length)  {
  if (length > 0.01) {
    start <- turtle(start_position, direction, length)
    fern(start, direction+pi/4, length*0.38)
    fern(start, direction, length*0.87)
  }
}

draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(-4:4, -4:4, type = "n", xlab="", ylab="")
  fern(c(0, -4),pi/2, 1)
}

# Question 30
fern2 <- function(start_position, direction, length, dir)  {
  if (length > 0.01) {
    start <- turtle(start_position, direction, length)
    fern2(start, direction + pi/4*dir, length*0.38, -dir)
    fern2(start, direction, length*0.87, -dir)
  }
}
draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  plot(-4:4, -4:4, type = "n", xlab="", ylab="")
  fern2(c(0, -4),pi/2, 1, 1)
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


