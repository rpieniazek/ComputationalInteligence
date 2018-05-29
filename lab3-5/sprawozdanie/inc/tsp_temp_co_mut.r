#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
#install.packages("TSP")
library(GA)
library(globalOptTests)
library(TSP)

drill <- read_TSPLIB("~/Documents/gr120.tsp")
D <- as.matrix(drill)

# given a tour, calculate the total distance
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}

# inverse of thetotal distance is the fitness
tpsFitness <- function(tour, ...) 1/tourLength(tour, ...)

#########################################
## crossover/mutation
crossoverSizes = seq(0, 1.0, by = 0.1) 
mutationSizes = seq(0, 1.0, by = 0.1)
crossoverSizesLength <- length(crossoverSizes)
mutationSizesLength <- length(mutationSizes)

tempMat <- matrix(0, crossoverSizesLength, mutationSizesLength)

for(crossover in seq(1,crossoverSizesLength )){
  for(mutation in seq(1,mutationSizesLength )){
    B <- 10
    fitnessMat <- matrix(0, B, 2)
    for (b in seq(1, B)) {
      # run a GA algorithm
      GA.rep <- ga(type = "permutation", fitness = tpsFitness, distMatrix = D, 
                   min = 1, max = nrow(D), popSize = 10, maxiter = 50, run = 100, 
                   keepBest=TRUE,
                   pmutation = mutationSizes[mutation],
                   pcrossover = crossoverSizes[crossover],
                   monitor = NULL)
      
      fitnessMat[b, 1] <- GA.rep@summary[GA.rep@iter]
      fitnessMat[b, 2] <- GA.rep@summary[GA.rep@iter]
    }
    
    tempMat[crossover,mutation] <- GA.rep@fitnessValue
  }
}

filled.contour(crossoverSizes,
               mutationSizes,
               tempMat,
               color.palette = bl2gr.colors,
               xlab="Crossover Probability",
               ylab="Mutation Probability",
               plot.axes = {
                 axis(1)
                 axis(2)
               })


plot(GA.rep, main = "Best and Avg at 50th iteration over 100 simulations")
points(rep(50, B), fitnessMat[, 1], pch = 16, col = "lightgrey")
points(rep(55, B), fitnessMat[, 2], pch = 17, col = "lightblue")

