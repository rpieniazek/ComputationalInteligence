#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
library(GA)
library(globalOptTests)
#install.packages("magrittr")
library(magrittr)

#fnNames = c("AluffiPentini", "Bohachevsky1", "Branin")
fnNames = c("AluffiPentini")
path = '/Users/evelan/Desktop/'

populationSizes = c(20, 80, 150)
iterSizes = c(80, 160, 240)
crossoverSizes = c(0.0, 0.2, 0.8)
mutationSizes = c(0.0, 0.2, 0.8)
elitePopulationSizes = c(0.0, 0.2, 0.8)


testMethod <- function(functionName) {
  
  tmpGASummary = matrix(0, 50, 6)
  for(iter in seq(1:5)){
  print(sprintf("Test function name %s", functionName))
  #optymalizacja w dwoch wymiarach:
  testFunctionWrapper <- function(x1, x2)
  {
    goTest(par = c(x1, x2) , fnName = functionName)
  }
  
  x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
  f <- outer(x1, x2, Vectorize(testFunctionWrapper))
  #jpeg(file = sprintf("%s%s%s", path, functionName, '.jpg'))
  #persp3D(x1,x2,f,theta = -50,phi = 20,color.palette = bl2gr.colors)
  #dev.off()
  
  #filled.contour(x1, x2, f, color.palette = bl2gr.colors)
  
  popSize <- 50
  elitsimPercentage <- 0.05
  pcrossover <- 0.8
  pmutation <- 0.1
  
  elitsim = round(popSize * elitsimPercentage)
  #minimizacja GA:
  GA <- ga(
    type = "real-valued",
    fitness =  function(x)
      - Vectorize(testFunctionWrapper(x[1], x[2])),
    # uwaga na minusa, bo szukamy glob. minimum
    min = c(-5.12,-5.12),
    max = c(5.12, 5.12),
    popSize = popSize,
    maxiter = 50,
    elitism = elitsim,
    pcrossover = pcrossover,
    pmutation = pmutation,
    run = 100
  )
  
  tmpGASummary <- GA@summary + tmpGASummary
  #print(GA)
  #view(GA)
  #write.csv(GA@solution, file=sprintf("%s%s%s%s", path, "ga-",functionName, ".csv"))
  #plot(GA)
  
  #print("dupa")
  #tmp <- GA@solution[, 1] + tmp
  #print("dupa2")
  
  #rysowanie wyniku:
  #filled.contour(x1, x2, f, color.palette = bl2gr.colors,
  #  plot.axes = {
  #    axis(1)
  #    axis(2)
  #    
  #    points(
  #      GA@solution[, 1],
  #      GA@solution[, 2],
  #      pch = 3,
  #      cex = 2,
  #      col = "white",
  #      lwd = 2
  #    )
  #  })
  }
  
  matplot(rownames(tmpGASummary), tmpGASummary, type='l', xlab='generations', ylab='fitnes', col=1:5)
  legend('bottomright', inset=.05, legend=colnames(tmpGASummary), 
         pch=1, horiz=TRUE, col=1:5)
  
}


for(fnName in fnNames) {
  testMethod(fnName)
}