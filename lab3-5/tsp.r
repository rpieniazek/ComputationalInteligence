#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
#install.packages("TSP")
library(GA)
library(globalOptTests)
library(TSP)

defaultPopSize = 50
defaultIterationSize = 100
defaultCrossover = 0.8
defaultMutation = 0.1
testInstances = 10
path = "~/Desktop/ga.nosync/"

populationSizes = seq(50, 250, by = 50)
iterSizes = seq(50, 250, by = 50)
crossoverSizes = seq(0, 1.0, by = 0.25)
mutationSizes = seq(0, 1.0, by = 0.25)

# given a tour, calculate the total distance
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}

# inverse of thetotal distance is the fitness
tpsFitness <- function(tour, ...) 1 / tourLength(tour, ...)

calculateGA <- function(fileName, popSize, iterationSize, crossover, mutation) {
  drill <- read_TSPLIB(sprintf("~/Documents/%s", fileName))
  D <- as.matrix(drill)
  fitnessMat <- matrix(0, testInstances, 2)
  
  for (instanceIndex in seq(1, testInstances)) {
    # run a GA algorithm
    GA.rep <-
      ga(
        type = "permutation",
        fitness = tpsFitness,
        distMatrix = D,
        min = 1,
        max = nrow(D),
        popSize = popSize,
        keepBest = TRUE,
        pmutation = mutation,
        pcrossover = crossover,
        maxiter = iterationSize,
        run = iterationSize,
        monitor = FALSE
      )
    
    fitnessMat[instanceIndex, 1] <- GA.rep@summary[GA.rep@iter]
    fitnessMat[instanceIndex, 2] <- GA.rep@summary[GA.rep@iter]
  }
  
  name = sprintf("%s pop-%s iter-%s cross-%s", fileName, popSize, iterationSize, crossover, mutation)
  jpeg(file = sprintf("%s%s.jpg", path, name))
  plot(GA.rep, main = name)
  points(rep(50, testInstances), fitnessMat[, 1], pch = 16, col = "lightgrey")
  points(rep(55, testInstances), fitnessMat[, 2], pch = 17, col = "lightblue")
  dev.off()
}

invoke <- function(fileName) {
  
  #zmiana wartosci krzyzowania
  for (pcrossover in crossoverSizes) {
    calculateGA(
      fileName,
      defaultPopSize,
      defaultIterationSize,
      pcrossover,
      defaultMutation
    )
  }
  
  #zmiana wartosci liczby iteracji
  for (iterationSize in iterSizes) {
    calculateGA(
      fileName,
      defaultPopSize,
      iterationSize,
      defaultCrossover,
      defaultMutation
    )
  }
  
  #zmiana liczby popopulacji
  for (popSize in populationSizes) {
    calculateGA(
      fileName,
      popSize,
      defaultIterationSize,
      defaultCrossover,
      defaultMutation
    )
  }
}

invoke('bays29.tsp')
invoke('gr17.tsp')
invoke('gr120.tsp')
