#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
#install.packages("TSP")
library(GA)
library(globalOptTests)
library(TSP)

drill <- read_TSPLIB("~/Documents/gr17.tsp")
D <- as.matrix(drill)

# given a tour, calculate the total distance
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}

# inverse of thetotal distance is the fitness
tpsFitness <- function(tour, ...) 1/tourLength(tour, ...)

##################################
## pop size / iteration

populationSizes = seq(50, 250, by = 50)
iterationSizes = seq(10, 100, by = 20)

populationSizesLength <- length(populationSizes)
iterationSizesLength <- length(iterationSizes)

tempMat <- matrix(0, populationSizesLength, iterationSizesLength)

for(population in seq(1,populationSizesLength )){
  for(iteration in seq(1,iterationSizesLength )){
    B <- 10
    fitnessMat <- matrix(0, B, 2)
    for (b in seq(1, B)) {
      # run a GA algorithm
      GA.rep <- ga(type = "permutation", fitness = tpsFitness, distMatrix = D, 
                   min = 1, max = nrow(D), run = 100, 
                   keepBest=TRUE,
                   popSize = populationSizes[population],
                   maxiter = iterationSizes[iteration],
                   monitor = NULL)
      
      fitnessMat[b, 1] <- GA.rep@summary[GA.rep@iter]
      fitnessMat[b, 2] <- GA.rep@summary[GA.rep@iter]
    }
    
    tempMat[population,iteration] <- GA.rep@fitnessValue
  }
}

filled.contour(populationSizes,
               iterationSizes,
               tempMat,
               color.palette = bl2gr.colors,
               xlab="Population Size",
               ylab="Generation Amount",
               plot.axes = {
                 axis(1)
                 axis(2)
               })

plot(GA.rep, main = "Best and Avg at 50th iteration over 100 simulations")
points(rep(50, B), fitnessMat[, 1], pch = 16, col = "lightgrey")
points(rep(55, B), fitnessMat[, 2], pch = 17, col = "lightblue")

