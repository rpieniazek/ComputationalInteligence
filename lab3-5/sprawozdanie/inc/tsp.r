#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
#install.packages("TSP")
library(GA)
library(globalOptTests)
library(TSP)
library(igraph)


#domyslne wartosci GA
defaultPopSize = 50
defaultIterationSize = 100
defaultCrossover = 0.8
defaultMutation = 0.1

#domyslne wartosci HGA
defaultMethod <- "L-BFGS-B";
defaultPoptim <- 0.05
defaultPressel <- 0.5

#badane parametry HGA
poptims = seq(0.05, 1, 0.2)
pressels = seq(0, 1, 0.25)

#badane parametry GA
populationSizes = seq(50, 250, by = 50)
iterSizes = seq(50, 250, by = 50)
crossoverSizes = seq(0, 1.0, by = 0.25)
mutationSizes = seq(0, 1.0, by = 0.25)

#ilosc przebiegow
testInstances = 1

#sciezka zapisu
path = "~/Desktop/ga.nosync/"

# obliczenie calkowitej dlugoeci
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}

# odwrotnoec calkowitej odlegloeci 
tpsFitness <- function(tour, ...) 1 / tourLength(tour, ...)

#funkcja obliczajaca GA i zapisujaca wykresy
calculateGA <- function(fileName, popSize, iterationSize, crossover, mutation) {
  #wczytanie pliku *.tsp
  drill <- read_TSPLIB(sprintf("~/Documents/%s", fileName))
  
  #konwersja pliku do postaci macierzowej
  D <- as.matrix(drill)
  
  #przygotowanie macierzy wyjsciowej (do obliczania wartosci srednich)
  fitnessMat <- matrix(0, testInstances, 2)
  
  # 2-d coordinates
  mds <- cmdscale(D)
  x <- mds[, 1]
  y <- -mds[, 2]
  n <- length(x)
  
  #petla odpowiedzialna za ilosc przebiegow funkcji GA
  for (instanceIndex in seq(1, testInstances)) {
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
    
    #dodawanie rozwiazan do macierzy wynikowej
    fitnessMat[instanceIndex, 1] <- GA.rep@summary[GA.rep@iter]
    fitnessMat[instanceIndex, 2] <- GA.rep@summary[GA.rep@iter]
  }
  
  #generowanie nazwy pliku/wykresu
  name = sprintf("%s-pop-%s-iter-%s-cross-%s", fileName, popSize, iterationSize, crossover, mutation)
  
  #zapis wykresu
  jpeg(file = sprintf("%s%s.jpg", path, name))
  plotName = sprintf("%s%s.jpg", path, fileName)
  
  #generowane wykresu
  plot(GA.rep, main = name)
  points(rep(50, testInstances), fitnessMat[, 1], pch = 16, col = "lightgrey")
  points(rep(55, testInstances), fitnessMat[, 2], pch = 17, col = "lightblue")
  dev.off()
}

#funkcja obliczajaca HGA i zapisujaca wykresy
calculateHGA <- function(fileName, method, poptim, pressel) {
  drill <- read_TSPLIB(sprintf("~/Documents/%s", fileName))
  D <- as.matrix(drill)
  fitnessMat <- matrix(0, testInstances, 2)
  
  
  
  for (instanceIndex in seq(1, testInstances)) {
    
    optimArgs = list(method = defaultMethod, 
                     poptim = poptim,
                     pressel = pressel,
                     control = list(fnscale = -1, maxit = 100))
    
    # run a HGA algorithm
    GA.rep <-
      ga(
        type = "permutation",
        fitness = tpsFitness,
        distMatrix = D,
        min = 1,
        max = nrow(D),
        monitor = FALSE,
        optimArgs = optimArgs
      )
    
    fitnessMat[instanceIndex, 1] <- GA.rep@summary[GA.rep@iter]
    fitnessMat[instanceIndex, 2] <- GA.rep@summary[GA.rep@iter]
  }
  
  
  #generowanie nazwy pliku/wykresu
  name = sprintf("hga-%s-method-%s-poptim-%s-pressel-%s", fileName, method, poptim, pressel)
  
  #zapis wykresu
  jpeg(file = sprintf("%s%s.jpg", path, name))
  
  #generowane wykresu
  plot(GA.rep, main = name)
  points(rep(50, testInstances), fitnessMat[, 1], pch = 16, col = "lightgrey")
  points(rep(55, testInstances), fitnessMat[, 2], pch = 17, col = "lightblue")
  dev.off()
}

#funkcja uruchamiajace GA dla roznych parametrow
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

#funkcja uruchamiajace HGA dla roznych parametrow
invokeHGA <- function(fileName) {
  
  #zmiana wartosci prawdopodobienstwa przeszukiwania lokalnego
  for (poptim in poptims) {
    calculateHGA(
      fileName,
      defaultMethod,
      poptim,
      defaultPressel
    )
  }
  
  #zmiana wartosci selective pressure
  for (pressel in pressels) {
    calculateHGA(
      fileName,
      defaultMethod,
      defaultPoptim,
      pressel
    )
  }
}

invokeHGA('bays29.tsp')
invoke('bays29.tsp')
invoke('gr17.tsp')
invoke('gr120.tsp')

