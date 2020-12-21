# CMEE 2020 HPC excercises R code main proforma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Tsz So Acacia Tang"
preferred_name <- "Acacia"
email <- "tst116@ic.ac.uk"
username <- "tst116"
personal_speciation_rate <- 0.0021565

# please remember *not* to clear the workspace here, or anywhere in this file. If you do, it'll wipe out your username information that you entered just above, and when you use this file as a 'toolbox' as intended it'll also wipe away everything you're doing outside of the toolbox.  For example, it would wipe away any automarking code that may be running and that would be annoying!

# Question 1
species_richness <- function(community){ # returns species richness of a community
  return(length(unique(community))) # which is the number of unique species within a community
}

# Question 2
init_community_max <- function(size){ # returns community of maximum initial species richness
  #return(c(1:size)) # an alterantive solution
  return(seq(from = 1, to = size, by = 1)) #returns vector of 1, 2, 3... of length size
}

# Question 3
init_community_min <- function(size){ # returns community of minimum initial species richness
  return(rep(1, times = size)) # returns vector of 1, 1, 1... of length size
}

# Question 4
choose_two <- function(max_value){ # returns two different numbers from 1 to max_value
  return(sample(c(1:max_value), size = 2, replace = F)) # replace = F so the same number will not be chosen twice
}

# Question 5
neutral_step <- function(community){ # one individual dies, a new individual is born
  chosenOnes <- choose_two(length(community)) # 
  community[chosenOnes[1]] <- community[chosenOnes[2]] # first one dies, second one reproduces/determines species of new individual
  return(community)
}

# Question 6
neutral_generation <- function(community){ # returns community after one generation (involves several steps)
  steps <- round((length(community)/2 + runif(1, min = -0.1, max = 0.1))) # dividing by 2 and either adding or subtracting 0.1, if even this will not affect round, but if it is odd it will be round up 50% of the time nad down 50% of the time
  for (i in 1:steps){
    community <- neutral_step(community) # do however many steps a generation is
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community, duration)  { #returns vector of species richness of a community over time
  communities <- rep(list(community), duration) # pre-allocating list to hold all communities over time
  
  for (i in 2: duration){
    communities[[i]] <- neutral_generation(communities[[i-1]]) # apply neutral_generation() on the previous community
  }
  
  timeSeries <- sapply(1:duration, function(i) species_richness(communities[[i]])) # apply species_richness() on each element in communities, and store results in the vector timeSeries
  return(timeSeries)
}

# Question 8
question_8 <- function() { # plots time series graph of neutral model simulation
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  
  plot(
    c(1:200), # x-values
    neutral_time_series(init_community_max(100), 200), #y-values
    type = "l", # line graph
    col = "red", #make line red
    main = "Time Series Graph of Neutral Model Simulation", # title
    ylab = "Species Richness", # y-axis label
    xlab = "Time (generations)" # x-axis label
  )
  return("It will always converge upon species homogeneity. This is because during each step one individual is replaced by an individual of the same or different existing species. Also, once the last individual of a species is replaced, the species disappears from the community forever. So, without speciation, the number of species can only remain constant or decrease. Given enough time then, the community will eventually reach the lowest possible species richness where all individuals are from the same species.")
}

# Question 9
neutral_step_speciation <- function(community,speciation_rate)  { # simulates one neutral step with speciation
  x <- runif(1, min = 0, max = 1) # pick a number from 0 to 1
  if(x <= speciation_rate){ # speciation occurs
    chosenOne <- sample(c(1:length(community)), size = 1) # choose one position where individual dies
    community[chosenOne] <- max(community, length(community)) + 1 # new individual
    return(community) 
  } else{ # no speciation
    return(neutral_step(community)) # without speciation, is the same as neutral_step()
  }
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate) { # returns community after one generation (with speciation)
  steps <- round((length(community)/2 + runif(1, min = -0.1, max = 0.1))) # see neutral_speciation: if even, divides by 2, if odd, divides by 2 and randomly rounds up or down
  for (i in 1:steps){
    community <- neutral_step_speciation(community, speciation_rate) # do however many steps are in a generation
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration){ #returns vector of species richness of a community over time (with speciation)
  communities <- rep(list(community),duration) # pre-allocating list to hold all communities over time
  
  for (i in 2:duration){
    communities[[i]] <- neutral_generation_speciation(communities[[i-1]], speciation_rate) # apply neutral_generation_speciation() on the previous community
  }
  
  timeSeries <- sapply(1:duration, function(i) species_richness(communities[[i]])) # apply species_richness() on each element in communities, and store results in the vector timeSeries
  return(timeSeries)
}

# Question 12
question_12 <- function()  { # plots time series graph of neutral model simulation with speciation
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  
  plot( # plot time series starting from maximum species richness
    c(1:200), # x values
    neutral_time_series_speciation(init_community_max(100), 0.1, 200), # y values
    type = "l", # make it a line graph
    col = "red", # make the line red
    main = "Time Series Graph of Neutral Model Simulation With Speciation", # title
    ylab = "Species Richness", # y-axis label
    xlab = "Time (generations)", # x-axis label
    ylim = c(0, 100) # make sure all y-values can be shown
  )
  lines( # plot time series starting from minimum species richness
    c(1:200),
    neutral_time_series_speciation(init_community_min(100), 0.1, 200),
    type = "l",
    col = "blue",
  )
  legend(47, 100, # add legend
  legend = c("initial community with maximum species richness", "initial community with minimum species richness"),
  fill = c("red", "blue"))
  return("Adding speciation to the model allows the system to reach dynamic equilibrium instead of always reaching the stable equilibrium of species homogeneity. At equilibrium, the rate of extinction is the same as the rate of speciation, so the species richness remains constant even though the species present in the community change.")
}

# Question 13
species_abundance <- function(community)  { # returns vector of abundance of species in decreasing order
  return(as.vector(sort(table(community), decreasing = T))) # table to count occurance of each species, sort with decreasing to put in order, as.vector to make it into a vector
}

# Question 14
octaves <- function(abundance_vector) { # bins abundance into octave classes
  return(tabulate(floor(log(abundance_vector, base = 2))+1)) # log base 2 so octave classes are powers of 2, floor rounds down to nearest integer, tabulate counts and gives the vector
}

# Question 15
sum_vect <- function(x, y) { # sums two vectors, filling the shorter one with 0s 
  diff = length(x) - length(y) # get difference
  if(diff < 0){ # if x is shorter
    x <- c(x, rep(0, times = diff*-1)) # add appropriate number of 0s
  } else if(length(y) < length(x)){ # if y is shorter
    y <- c(y, rep(0, times = diff)) # add appropriate number of 0s
  }
  return(x+y) # sum two vectors as normal
}

# Question 16 
question_16 <- function()  { # plots mean species abundance distribution from specified simulation as bar plot
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  
  community <- init_community_max(100) # make initial community with 100 individuals and maximum species richness
  
  for (i in 1:200){ # burn in
    community <- neutral_generation_speciation(community, 0.1) # 200 generations without recording
  }
  
  abundance <- species_abundance(community) # then record species abundance
  abunOctave <- octaves(abundance) # convert to octaves
  sum <- abunOctave

  for (i in 1:2000){ # recording data for 2000 generations after burn in
    if(i%%20 == 0){ # every 20 generations
      community <- neutral_generation_speciation(community, 0.1) # make one generation pass
      abundance <- species_abundance(community) # record abundance
      abunOctave <- octaves(abundance) # convert to octaves
      sum <- sum_vect(sum, abunOctave) # add to existing sum of all octaves
    }
    else{
      community <- neutral_generation_speciation(community, 0.1) # even if no recording, generation needs to pass
    }
  }
  average <- sum/101 # take average
  names <- c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-128") # all possible octaves
  barplot( # make a bar plot
    average, # data to plot
    main = "Average Species Abundance Distribution", # title
    xlab = "Octaves", # x-axis label
    ylab = "Frequency", # y-axis label
    names.arg = names[1:length(average)], # label bars
    ylim = c(0, ceiling(max(average))+1)
  )
  return("The initial composition of the community does not matter in that the same average species abundance will be reached at equilibrium. This is because the dynamic equilibrium is determined by the speciation rate and community size, and initial community composition just affects the path the community takes to reach the equilibrium.")
}

# Question 17
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  { # code to run simulation, to be used with HPC. Default values of parameters for testing.
  start <- proc.time()[3] # start timing
  community <- init_community_min(size) # create initial community with minimum species richness
  loop = 0 # set counter to 0
  richness <- species_richness(community) # set richness to initial species richness
  abundance <- list(octaves(species_abundance(community))) # set abundance to initial species abundance, grouped by octaves
  
  while (proc.time()[3] - start < wall_time*60){ # before time runs out, wall_time is multiplied by 60 because it is in minutes
    
    while (loop < burn_in_generations){ # during burn-in period
      community <- neutral_generation_speciation(community, speciation_rate) # one generation later
      if (loop %% interval_rich == 0){ # during each interval_rich
        richness <- c(richness, species_richness(community)) # append species richness of current community to vector containing previous species richnesses
        oct <- list(octaves(species_abundance(community))) # get species abundance, grouped by octaves
        abundance <- c(abundance, oct)
      }
      loop = loop + 1 # increase counter by 1
     }
    #after burn in
    community <- neutral_generation_speciation(community, speciation_rate) # one generation later
    if (loop %% interval_oct == 0){ # if it is time to record species abundance
      oct <- list(octaves(species_abundance(community))) # get species abundance, grouped by octaves
      abundance <- c(abundance, oct) # record abundance by appending to list
    }
    loop = loop + 1 # increase counter by 1
  }
  time <- proc.time()[3] - start # stop timer
  parameters <- c(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations) # save outputs
  save(richness, abundance, community, time, parameters, file = output_file_name) # save output to file
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  { # takes in .rda files from simulation and saves mean abundance data after burn-in in new .rda output
  s500 = s1000 = s2500 = s5000 = c()
  results <- list(s500, s1000, s2500, s5000) #create your list output here to return
  combined_results <- list(rep(NA, 4)) # pre-allocate empty list to store results
  for (i in c(1:100)){ # cycle through all files
    load(paste("Simulation/Simulation", i, ".rda", sep = "")) # load result from simulation
    size <- i%%4 + 1 # the community size
    sum = c() # vector to hold sum of octave vectors
    for (j in abundance[parameters[6]:length(abundance)]){ # for each octave
      sum <- sum_vect(sum, j) # sum them all
    }
    average <- list(sum/(length(abundance)-parameters[6])) # take the average
    results[[size]] <- c(results[[size]], average) # append to list element containing other mean abundance octaves from simulations with the same community size
  }
  c = 0 # counter of which population size it is
  for (a in results){ # for each community size
    c = c+1 # increase counter by 1
    SUM = 0 # set SUM to 0
    for (b in a){ # for each mean abundance octave vector from community
      SUM <- sum_vect(SUM, b) # sum up all mean abundance octave vectors from same community size
    }
    AVERAGE <- list(SUM/25) # take average 
    combined_results[[c]] <- AVERAGE[[1]] # save octave output to combined_results
  }
  
  # save results to an .rda file
  save(combined_results, file = "CombinedResults.rda") # save octave outputs to file
}

plot_cluster_results <- function()  { # plots mean species abundance distribution for each community size
    # load combined_results from your rda file
    load("CombinedResults.rda") # load processed data for plotting
    oct <- c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127", "128-255", "256-511", "512-1023", "1024-2047", "2048-4095") # all possible octaves
    # plot the graphs
    combined_results <- lapply(1:4, function(i) c(combined_results[[i]], rep(0, length(combined_results[[4]]) - length(combined_results[[i]]) )))
    ToPlot <- matrix(unlist(combined_results), nrow = 4, ncol = length(combined_results[[4]]), byrow = T)
    rownames(ToPlot) <- c("500", "1000", "2500", "5000")
    # clear any existing graphs and plot your graph within the R window
    graphics.off()
    barplot(
      ToPlot, # data is automatically grouped by row
      beside = TRUE, # makes grouped bar plot instead of stacked
      col = c(1, 2, 3, 4), # colour bars
      main = "Average Species Abundance Distribution", # title
      xlab = "Octaves", # x-axis label
      ylab = "Frequency", # y-axis label
      ylim= c(0,12), # make y-axis longer than data frequency
      names.arg = oct[1:length(combined_results[[4]])], # select appropriate labels for bars
      cex.names=0.4, # adjust font size so all labels are shown
      legend = rownames(ToPlot), # add legend
      args.legend=list(title="Community Size", horiz = T) # add title to legend and make legend horizontal
    )
    
    return(combined_results) # return plotted data
}

# Question 21
question_21 <- function()  {
  dim <- log(8)/log(3) # dimension of object
  answer <- "The object is made up of 8 repeated parts arranged in a 3*3 manner with the middle kept empty, so to make it 3 times as wide would require 8 times the material."
  return(list(dim, answer))
}

# Question 22
question_22 <- function()  {
  dim <- log(20)/log(3) # dimension of object
  answer <- "The object is made up of 20 repeated parts arranged in a 3*3*3 manner with the middle of each face and the middle of the whole object kept empty, so to make it 3 times as wide would require 20 times the material."
  return(list(dim, answer))
}

# Question 23
chaos_game <- function()  { #
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  plot(0:4, 0:4, type = "n", xlab="", ylab="") # make a new 4*4 blank plot 
  xs <- c(0, 3, 4) # x-coordinates of A, B, C
  ys <- c(0, 4, 1) # y-coordinates of A, B, C
  points(xs, ys, col = "#CC0033", pch = 4) # mark A, B, C on plot
  text(c(-0.08, 3.08, 4.08), c(0, 4, 1), labels=c("A", "B", "C"), cex=0.9, font=1) # label A, B, C
  x = y = 0 # initial coordinates of X
  Xs = Ys = 0 # to store coordinates of X
  for (i in 1:10000){ # repeat 10000 times
    choose <- sample(3, 1) # choose one of A, B, C
    newX = (x + xs[choose])/2 # find new coordinates of X
    newY = (y + ys[choose])/2
    x = newX # move X
    y = newY
    Xs <- c(Xs, newX) #store coordinates of X as vector
    Ys <- c(Ys, newY)
  }
  points(Xs, Ys, col = "black", pch = 19, cex = 0.3) # plots all positions of X as points
  return("The position of X resembles that of a Sierpiński triangle. The triangle is skewed, because the positions of A, B, and C define the vertices of the triangle.")
}

# Question 24
turtle <- function(start_position, direction, length)  { # draws a line of specific length from start_position in specific direction
  x1 <- start_position[1] # store initial x-coordinate
  y1 <- start_position[2] # store initial y-coordinate
  x2 <- x1 + length*cos(direction) # calculate final x-coordinate
  y2 <- y1 + length*sin(direction) # calculate final y-coordinate
  lines(c(x1, x2), c(y1, y2)) # draw a line
  return(c(x2, y2)) # you should return your endpoint here.
}

# Question 25
elbow <- function(start_position, direction, length)  {
  start <- turtle(start_position, direction, length) # plot first line as specified
  turtle(start, direction - pi/4, length*0.95) # make second line pi/4 to the right and 5% shorter
  return(c(x2, y2)) # you should return your endpoint here.
}

# Question 26
spiral <- function(start_position, direction, length)  {
  start <- turtle(start_position, direction, length) # draw first line
  if(length > 0.00001){ # if the line is longer than 0.00001
  spiral(start, direction - pi/4, length*0.95) # call self: draws shorter and shorter lines pi/4 to the right of one before
  }
  return("Without the if, the function never stops running, because the lengths just get shorter and shorter infinitely.")
}

# Question 27
draw_spiral <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  plot(-4:4, -4:4, type = "n", xlab="", ylab="") # make a new blank plot
  spiral(c(-2, 3), 0, 3) # run spiral()
}

# Question 28
tree <- function(start_position, direction, length)  { # draws a tree
  if (length > 0.01) { # if the stem is longer than 0.01
    start <- turtle(start_position, direction, length) # draw lines as specified
    tree(start, direction-pi/4, length*0.65) # right branch
    tree(start, direction+pi/4, length*0.65) # left branch
  }
}


draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  plot(-4:4, -4:4, type = "n", xlab="", ylab="") # create blank plot
  tree(c(0, -4), pi/2, 1) # call tree()
}

# Question 29
fern <- function(start_position, direction, length)  {
  if (length > 0.01) { # if the stem is longer than 0.01
    start <- turtle(start_position, direction, length) # draw lines as specified
    fern(start, direction+pi/4, length*0.38) # branch to the left
    fern(start, direction, length*0.87) #stem
  }
}

draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  plot(-4:4, -4:4, type = "n", xlab="", ylab="") # create blank plot
  fern(c(0, -4), pi/2, 0.7) #draw fern with fern()
}

# Question 30
fern2 <- function(start_position, direction, length, dir)  {
  if (length > 0.01) { # if the stem is longer than 0.01
    start <- turtle(start_position, direction, length) # draw line as specified
    fern2(start, direction + pi/4*dir, length*0.38, -dir) # branches
    fern2(start, direction, length*0.87, -dir) # stem
  }
}

draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  plot(-4:4, -4:4, type = "n", xlab="", ylab="") # make new plot
  fern2(c(0, -4), pi/2, 0.7, 1) # draw fern
}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window
  communityMax <- init_community_max(100) # set initial community with maximum species richness
  resultsMax <- replicate(20, neutral_time_series_speciation(communityMax,0.1,2200), simplify=T) # store vectors of species richness over time for 20 simulations in matrix
  averageMax <- apply(resultsMax, 1, mean) # calculate average species richness at every generation across simulations
  sdMax <- apply(resultsMax, 1, sd) # calculate standard deviation of species richness at every generation across simulations
  minMax <- averageMax - qnorm(0.986)*sdMax/sqrt(20) # lower limit of confidence intervals
  maxMax <- averageMax + qnorm(0.986)*sdMax/sqrt(20) # upper limit fo confidence intervals

  #same thing, but start with minimum species richness
  communityMin <- init_community_min(100)
  resultsMin <- replicate(20, neutral_time_series_speciation(communityMin,0.1,2200), simplify=T) 
  averageMin <- apply(resultsMin, 1, mean)
  sdMin <- apply(resultsMin, 1, sd)
  minMin <- averageMin - qnorm(0.986)*sdMin/sqrt(20)
  maxMin <- averageMin + qnorm(0.986)*sdMin/sqrt(20)
  
  graphics.off() # clear exiting graphs
  plot( #plot average species richness for initial community with maximum species richness
    c(1:2200), # x values: time in generations
    averageMax, # y values: average species richness from 20 simulations
    log = "x", # put x-axis in logarithmic scale so it is easier to read
    type = "l", # make a line plot
    col = "red", # make line red
    main = "Time Series Graph of Neutral Model Simulation With Speciation",
    ylab = "Species Richness", # y-axis label
    xlab = "Time (generations)", # x-axis label
    ylim = c(1, 100) # make sure all species richness values can be shown
  )
  polygon( #97.2% confidence interval for community starting at maximum species richness
    c(c(1:2200), c(2200:1)), c(minMax, rev(maxMax)), border = NA, col = adjustcolor("red",alpha.f=0.2) # rev because the points are supposed to go around the polygon
  )
  lines( #add to same plot average species richness for initial community with minimum species richness
    c(1:2200),
    averageMin,
    type = "l",
    col = "blue"
  )
  polygon( #97.2% confidence interval for community starting at minimum species richness
    c(c(1:2200), c(2200:1)), c(minMin, rev(maxMin)), border = NA, col = adjustcolor("blue",alpha.f=0.2) 
  )
  legend(6, 100, #add legend to graph
    legend = c("initial community with maximum species richness", "initial community with minimum species richness"),
    fill = c(adjustcolor("red",alpha.f=0.2), adjustcolor("blue",alpha.f=0.2) ), #colours of confidence intervals
    lwd = 1, # we also have lines
    col = c("red", "blue"), # colour of lines
    seg.len=0.6, # make line as large as filled box
    merge = T #merge line and fill
  )
}

# Challenge question B
richnessCommunity <- function(richness){ #returns community with specified species richness
  species <- c(1:richness) # First n individuals are of different species, number depends on richness specified. This ensures that the communtiy will always have the species richness specified
  community <- c(species, sample(species, 100-richness, replace = T)) # The rest of the individuals are randomly assigned one of n species
  return(community)
}

plot_time_series <- function(richnesscommunity){ # adds to existing plot a line of average species richness over time for a given initial communtiy aka output of richnessCommuntiy()
  timeSeries <- replicate(20, neutral_time_series_speciation(richnesscommunity,0.1,250)) # store species richness over time for 20 simulations with the starting initial communtiy (given as parameter) in a list
  averageTimeSeries <- apply(timeSeries, 1, mean) # calculate mean species richness at each generation
  lines( # adds to existing graph a line of species richness over time
    c(1:250),
    averageTimeSeries,
    col = colours()[averageTimeSeries[1] + 25] # makes it easier to tell between different lines
  )
}

Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  minSeries <- replicate(20, neutral_time_series_speciation(init_community_min(100),0.1,250)) # run 20 simulations with initial community that has maximum species richness and store species richness time series in list
  averageMinSeries <- apply(minSeries, 1, mean) # calculate mean species richness at each generation
  
  plot( # plot average species richenss over time
    c(1:250),
    averageMinSeries,
    log = "x",
    type = "l",
    col = colours()[26],
    main = "Time Series Graph of Neutral Model Simulation With Speciation",
    ylab = "Species Richness",
    xlab = "Time (generations)",
    ylim = c(1, 100)
  )
  sapply(seq(5, 100, 5), function(i) plot_time_series(richnessCommunity(i))) # add to plot species richness over time starting with initial species richness of 1, 5, 10, 15...95
  legend(6, 100, #add legend to graph
    title = "Initial Community Species Richness",
    legend = as.character(c(1, seq(5, 100, 5))),
    fill = colours()[c(26, seq(30, 125, 5))], #colours of confidence intervals
    ncol = 5
  )
}

# Challenge question C
time_series_plot <- function(id, results){ # plots species richness over time, same as in challenge B
 size = c(500, 1000, 2500, 5000) # size of community
 ticks = c(1, 10, 100, 1000, 10000, 100000) # x-axis ticks
 limit = c(1000, 1500, 2000, 2000) # end of burn-in
 plot( # plot species richness over time
    c(1:length(results[[id]])), # x values
    results[[id]], # y values
    log = "x", # make x-axis log scale
    type = "l", # make plot line graph
    col = 2, # colour of line
    main = paste("Species Richness Over Time With Community Size", as.character(size[id]), sep = " "), #title of plot
    cex.main = 0.85, # decrease font of title so everything fits
    ylab = "Species Richness", # y-axis label
    xlab = "Time (generations)", # x-axis label
    ylim = c(0, 10*ceiling((max(results[[id]])/10))), # make axes cover all values, largest y value is the smallest multiple of 10 larger than max y value
    xlim = c(1,10^(floor(log(length(results[[id]]),10))+1)), # make axes cover all values, largest y value is the smallest power of 10 larger than max y value
    axes = FALSE # do not plot default axes
  )
  axis(side = 1, at=ticks[1:10^(floor(log(length(results[[id]]),10))+1)]) # make x-axis ticks every power of 10
  axis(side = 2) # y-axis as default
  
  lines(c(limit[id], limit[id]), c(0, 10*ceiling((max(results[[id]])/10)))) # add line to indicate end of burn-in
}

Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window
  a500 = a1000 = a2500 = a5000 = c()
  combined_results <- list(a500, a1000, a2500, a5000) #create your list output here to return
  for (i in c(1:100)){ # cycle through all files
    load(paste("Simulation/Simulation", i, ".rda", sep = "")) # load result from simulation
    size <- i%%4 + 1 # the community size
    combined_results[[size]] <- sum_vect(combined_results[[size]], richness) # append to list element containing other mean abundance octaves from simulations with the same community size
  }
  combined_results <- lapply(1:4, function(i) combined_results[[i]]/25) # obtain average species richness over time for each community size, store as list elements

  graphics.off() # clear exiting graphs
  par(mfrow=c(2,2)) # make a 2*2 subplot
  sapply(1:4, function(i) time_series_plot(i, combined_results)) # plot average species richness over time for each community size
  return(combined_results) # return average species richness over time for each community size as list
}

# Challenge question D
coalescence_run <- function(J=100, v=0.1){ # coalescence simulation, returns abundances arranged into octaves
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
  SIZE <- c(500, 1000, 2500, 5000) # set community sizes
  results <- vector("list", 4)
  start <- proc.time()[3]
  for (iter in 1:100){
    set.seed(iter) # set seed
    size <- SIZE[iter%%4 + 1] # select community size
    results[[iter%%4 + 1]] <- sum_vect(results[[iter%%4 + 1]], octaves(coalescence_run(size, 0.0021565)))
  }
  timer <- as.character(proc.time()[3]-start)
  combined_results <- lapply(results, function(i) i/25)
  
  # clear any existing graphs and plot your graph within the R window
  combined_results <- lapply(1:4, function(i) c(combined_results[[i]], rep(0, length(combined_results[[4]]) - length(combined_results[[i]]) )))
  ToPlot <- matrix(unlist(combined_results), nrow = 4, ncol = length(combined_results[[4]]), byrow = T)
  rownames(ToPlot) <- c("500", "1000", "2500", "5000")
  oct <- c("1", "2-3", "4-7", "8-15", "16-31", "32-63", "64-127", "128-255", "256-511", "512-1023", "1024-2047", "2048-4095") # all possible octaves
  graphics.off()
    barplot(
      ToPlot, # data is automatically grouped by row
      beside = TRUE, # makes grouped bar plot instead of stacked
      col = c(1, 2, 3, 4), # colour bars
      main = "Average Species Abundance Distribution", # title
      xlab = "Octaves", # x-axis label
      ylab = "Frequency", # y-axis label
      ylim= c(0,12), # make y-axis longer than data frequency
      names.arg = oct[1:length(combined_results[[4]])], # select appropriate labels for bars
      cex.names=0.4, # adjust font size so all labels are shown
      legend = rownames(ToPlot), # add legend
      args.legend=list(title="Community Size", horiz = T) # add title to legend and make legend horizontal
    )

  answer <- "The coalescence model is more stochastic since it does not average out the species abundance for a community over generations. This is why the results are different from that of the cluster run. However, coalescence is faster because it only follows the ancestry of the individuals found in the stable community, where sharing ancestry implies individuals are of the same species. Many lineages/species are lost in the history of the communtiy, and by ignoring the ones that have gone exitinct, less simulations need to be carried out. Also, coalescence does not require a burn-in period."
  return(paste("The coalescence simulation took ", timer, " seconds to run 100 simulations in sequence. ", answer, sep = "" ))
}

# Challenge question E
chaos_game_start <- function(start)  { # chaos game, but you can choose where to start
  # clear any existing graphs and plot your graph within the R window
  xs <- c(0, 4, 2) # x-coordinates of A, B, C
  ys <- c(0, 0, 4*cos(pi/6)) # y-coordinates of A, B, C
  points(xs, ys, col = "#CC0033", pch = 4) # mark A, B, C on plot
  x = start[1]
  y = start[2] # initial coordinates of X
  Xs = Ys = c()
  for (i in 1:1000){ # repeat 10000 times
    choose <- sample(3, 1) # choose one of A, B, C
    newX = (x + xs[choose])/2 # find new coordinates of X
    newY = (y + ys[choose])/2
    Xs <- c(Xs, x) #store coordinates of X as vector
    Ys <- c(Ys, y)
    x = newX # move X
    y = newY
  }
  points(Xs[11:1000], Ys[11:1000], pch = 19, cex = 0.1, col = adjustcolor("grey",alpha.f=0.4) ) # plot rest of the positions of x in grey
  text(Xs[1:10], Ys[1:10], labels = as.character(c(1:10)), font=1) # number the first 10 positions of x to demonstrate the movement of x into a position on the Sierpiński triangle
}

chaos_game_general <- function(pointsX, pointsY, distance){ # chaos game, but you can choose how many points you want to bound the movement of X by (and where to put them), and how much to move x by towards a point
  points(pointsX, pointsY, col = "#CC0033", pch = 4)
  Xs = 0
  Ys = 0
  for (i in 2:1000){ # repeat 10000 times
    choose <- sample(length(pointsX), 1) # choose one of A, B, C
    newX = (Xs[i-1] + pointsX[choose])*distance # find new coordinates of X
    newY = (Ys[i-1] + pointsY[choose])*distance
    x = newX # move X
    y = newY
    Xs <- c(Xs, newX) #store coordinates of X as vectors
    Ys <- c(Ys, newY)
  }
  points(Xs, Ys, pch = 19, cex = 0.1, col = round(1/distance)) # point first 10000 positions of X
}

Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  par(mfrow=c(2,2)) # split plot into 2*2 subplots
  plot(0:4, 0:4, type = "n", xlab="", ylab="") # make a new 4*4 blank plot 
  chaos_game_start(c(0, 3)) # different starting point

  plot(0:4, 0:4, type = "n", xlab="", ylab="")
  chaos_game_general(c(0, 4, 2), c(0, 0, 4*cos(pi/6)), 1/2.2) # change ratio
  chaos_game_general(c(0, 4, 2), c(0, 0, 4*cos(pi/6)), 1/3) 
  chaos_game_general(c(0, 4, 2), c(0, 0, 4*cos(pi/6)), 1/5)
  chaos_game_general(c(0, 4, 2), c(0, 0, 4*cos(pi/6)), 1/9)

  plot(0:4, 0:4, type = "n", xlab="", ylab="")
  chaos_game_general(c(0, 4, 0, 4), c(0, 0, 4, 4), 1/2) # change vertices to 4
  chaos_game_general(c(0, 4, 0, 4), c(0, 0, 4, 4), 1/3)

  plot(0:4, 0:4, type = "n", xlab="", ylab="")
  chaos_game_general(c(0, 4, 0, 4, 2), c(0, 0, 2, 2, 4), 1/2) # change vertices to 5
  chaos_game_general(c(0, 4, 0, 4, 2), c(0, 0, 2, 2, 4), 1/3)

  return("When changing the starting position of X, the plot remains (largely) the same, with the exception of some points that fall outside the Sierpiński triangle for some starting positions. This is because whatever the starting position, X eventually falls within the points on the Sierpiński triangle defined by the three points A, B, C, and once it reaches a point on the Sierpiński triangle, it will only travel to other points on the Sierpiński triangle. When X is moved less towards the vertice chosen, the triangle becomes smaller, and the repeating units relatively further apart.When the number of vertices are increased (and distance X is moved by is unchanged), the chaos game no longer returns a fractal, but instead X occupies the entire space defined by the vertices. However, when the distance X is moved by is decreased, a fractual of repeating units whose shape and arrangement mirrors that of the vertices is formed")
}

# Challenge question F
line <- function(start_position, direction, length, colour){
  x1 <- start_position[1] # store initial x-coordinate
  y1 <- start_position[2] # store initial y-coordinate
  x2 <- x1 + length*cos(direction) # calculate final x-coordinate
  y2 <- y1 + length*sin(direction) # calculate final y-coordinate
  lines(c(x1, x2), c(y1, y2), col = colour) # draw a line from (x1, y1) to (x2, y2) of the colour specified
  return(c(x2, y2)) # you should return your endpoint here.
}

TREE <- function(start_position=c(0, -12), direction=pi/2, length=8, threshold=0.1, colour=1, change = 1)  { # draws a tree starting at start_position, with the first branch pointing at the angle direction (in radians), of specified length, do not plot lines shorter than threshold, colour of first branch is colour, each subsequent loop colour changes by change
  if (length > threshold) { # if the stem is longer than 0.01
    start <- line(start_position, direction, length, colour) # draw lines as specified
    TREE(start, direction-pi/4, length*0.65, threshold, colour+change, change) # right branch
    TREE(start, direction+pi/4, length*0.65, threshold, colour+change, change) # left branch
  }
}

FERN <- function(start_position, direction, length, dir, threshold, colour, change1, change2, curl)  { # draws a fern starting at start_position, with the first branch pointing at the angle direction (in radians), of specified length, dir is for flipping branch direction but can be used to change angle branches have, do not plot lines shorter than threshold, colour of first branch is colour, each subsequent loop colour of line in different direction changes by change1, one of same direction changes by change2, curl changes line that was in same direction before
  if (length > threshold) { # if the stem is longer than threshold
    start <- line(start_position, direction, length, colour) # draw line as specified
    FERN(start, direction + pi/4*dir, length*0.5, -dir, threshold, colour+change1, change1, change2, curl) # branches
    FERN(start, direction + curl, length*0.87, -dir, threshold, colour+change2, change1, change2, curl) # stem
  }
}

Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off() # clear exiting graphs
  plot(-12:12, -12:12, type = "n", xlab="", ylab="") # make new plot
  TREE() #tree
  FERN(start_position=c(-10, -12), direction = pi/2, length = 0.7, dir=1, threshold=0.05,colour=1, change1=0, change2=1, curl=-pi/80) #three ferns in the corner
  FERN(start_position=c(-8.5, -12), direction = pi/2, length = 0.4, dir=1, threshold=0.05,colour=1, change1=0, change2=2, curl=-pi/40)
  FERN(start_position=c(-6, -12), direction = pi/2, length = 0.5, dir=1, threshold=0.05,colour=1, change1=3, change2=3, curl=-pi/60)
  FERN(start_position=c(10, -6), direction=-pi/3, length=0.5, dir=0.5, threshold=0.03, colour=1, change1=1, change2=0, curl = pi/20) #curly fern to demonstrate impact of threshold
  return("The smaller the threshold, the (exponentially) longer the code takes to run and the more filled in the image is.")
}
# Challenge question G should be written in a separate file that has no dependencies on any functions here.