#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
library(GA)
library(globalOptTests)

fnNames = c("AluffiPentini", "Bohachevsky1", "Branin")
path = '/Users/evelan/Desktop/'

#true = wykonuje się tylko raz dla domyslnych parametrów dla 3 funkcji powyżej
isDebug <- 0 #0 - false, 1 - true

populationSizes = seq(50, 250, by = 50)
iterSizes = seq(50, 300, by = 50)
crossoverSizes = seq(0, 1.0, by = 0.2)
mutationSizes = seq(0, 1.0, by = 0.2)
elitePopulationSizes = seq(0, 1.0, by = 0.25)
testInstances = 40

defaultPopSize = 50
defaultCrossover = 0.8
defaultMutation = 0.1
defaultElitePopulation = 0.05
defaultIterationSize = 100

if (isDebug) {
  populationSizes = c(defaultPopSize)
  crossoverSizes = c(defaultCrossover)
  mutationSizes = c(defaultMutation)
  elitePopulationSizes = c(defaultElitePopulation)
  iterSizes = c(defaultIterationSize)
  testInstances = 1
}

#PLOT DRAWING --- 3d plot użytej funkcji
showFunction3dPlot <- function(x1, x2, f) {
  persp3D(x1,
          x2,
          f,
          theta = -50,
          phi = 20,
          color.palette = bl2gr.colors)
}

#PLOT DRAWING --- kontur z użytej funkcji
showFunctionContour <- function(x1, x2, f) {
  filled.contour(x1, x2, f, color.palette = bl2gr.colors)
}

#PLOT DRAWING --- kontur z zaznaczonym wynikiem
showFunctionContourWithResult <- function(x1, x2, f, GA) {
  filled.contour(x1,
                 x2,
                 f,
                 color.palette = bl2gr.colors,
                 plot.axes = {
                   axis(1)
                   axis(2)
                   
                   points(
                     GA@solution[, 1],
                     GA@solution[, 2],
                     pch = 3,
                     cex = 2,
                     col = "white",
                     lwd = 2
                   )
                 })
}

#PLOT DRAWING --- nie dziala
showSummaryPlot <- function(GASummary) {
  matplot(
    rownames(GASummary),
    GASummary,
    type = 'l',
    xlab = 'generations',
    ylab = 'fitnes',
    col = 1:6
  )
  legend(
    'bottomright',
    inset = .05,
    legend = colnames(GASummary),
    pch = 1,
    horiz = TRUE,
    col = 1:6
  )
}


#GA CALCULATIONS --- liczy GA i zapisuje/wyswietla wykresy
calculateGA <-
  function(functionName,
           popSize,
           iterationSize,
           elitsimPercentage,
           pcrossover,
           pmutation) {
    #wrapper funkcji
    testFunctionWrapper <- function(x1, x2)
    {
      goTest(par = c(x1, x2) , fnName = functionName)
    }
    
    #rozpatrywana przestrzeń
    x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
    f <- outer(x1, x2, Vectorize(testFunctionWrapper))
    
    jpeg(file = sprintf("%s%s-3dplot.jpg", path, functionName))
    showFunction3dPlot(x1, x2, f)
    dev.off()
    
    jpeg(file = sprintf("%s%s-contour.jpg", path, functionName))
    showFunctionContour(x1, x2, f)
    dev.off()
    
    #oblicza liczbę elitarnetj populacji
    elitsim = round(popSize * elitsimPercentage)
    
    #6 wartosci (kolumn) x liczba iteracji (wiersze) 
    tmpGASummary = matrix(0, iterationSize, 6)
    #ilosc uruchomień testu
    for (test in 1:testInstances) {
      #minimizacja GA:
      GA <- ga(
        type = "real-valued",
        fitness =  function(x)
          - Vectorize(testFunctionWrapper(x[1], x[2])),
        # uwaga na minusa, bo szukamy glob. minimum
        min = c(-5.12,-5.12),
        max = c(5.12, 5.12),
        popSize = popSize,
        maxiter = iterationSize,
        elitism = elitsim,
        pcrossover = pcrossover,
        pmutation = pmutation,
        run = 100
      )
      tmpGASummary <- GA@summary + tmpGASummary
    }
    #obliczenie sredniej dla 6 wartosci wyjsciowych z GA
    tmpGASummary <- tmpGASummary / testInstances
    #showSummaryPlot(tmpGASummary) - nie dziala
    
    #nazwa pliku z uzytymi parametrami
    name <- sprintf(
      "%s-p%03d-i%03d-c%.2f-m%.2f-e%.2f",
      functionName,
      popSize,
      iterationSize,
      pcrossover,
      pmutation,
      elitsimPercentage
    )
    
    jpeg(file = sprintf("%sgenerations-%s.jpg", path, name))
    plot(GA) #wykres wg generacji
    dev.off()
    
    jpeg(file = sprintf("%sresult-%s.jpg", path, name))
    showFunctionContourWithResult(x1, x2, f, GA)
    dev.off()
  }

#INVOKING GA WITH DIFFERENT PARAMERERS 
invokeTestsWithFunction <- function(functionName) {
  print(sprintf("Test function name %s", functionName))
  
  #zmiana wartosci populacji elitarnej
  for (elitsimPercentage in elitePopulationSizes) {
    calculateGA(
      functionName,
      defaultPopSize,
      defaultIterationSize,
      elitsimPercentage,
      defaultCrossover,
      defaultMutation
    )
  }
  
  #zmiana wartosci mutacji
  for (pmutation in mutationSizes) {
    calculateGA(
      functionName,
      defaultPopSize,
      defaultIterationSize,
      defaultElitePopulation,
      defaultCrossover,
      pmutation
    )
  }
  
  #zmiana wartosci krzyzowania
  for (pcrossover in crossoverSizes) {
    calculateGA(
      functionName,
      defaultPopSize,
      defaultIterationSize,
      defaultElitePopulation,
      pcrossover,
      defaultMutation
    )
  }
  
  #zmiana wartosci liczby iteracji
  for (iterationSize in iterSizes) {
    calculateGA(
      functionName,
      defaultPopSize,
      iterationSize,
      defaultElitePopulation,
      defaultCrossover,
      defaultMutation
    )
    
  }
  
  #zmiana liczby popopulacji
  for (popSize in populationSizes) {
    calculateGA(
      functionName,
      popSize,
      defaultIterationSize,
      defaultElitePopulation,
      defaultCrossover,
      defaultMutation
    )
  }
  #write.csv(GA@solution, file=sprintf("%s%s%s%s", path, "ga-",functionName, ".csv"))

}


#SZTART
for (fnName in fnNames) {
  invokeTestsWithFunction(fnName)
}