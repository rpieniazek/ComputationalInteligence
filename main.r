#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
library(GA)
library(globalOptTests)

#fnNames = c("AluffiPentini", "Bohachevsky1", "Branin")
fnNames = c("AluffiPentini")
path = '/Users/evelan/Desktop/'

isTest <- 0 #0 - false, 1 - true

populationSizes = seq(20, 200, by = 20)
iterSizes = seq(20, 200, by = 20)
crossoverSizes = seq(0, 1.0, by = 0.2)
mutationSizes = seq(0, 1.0, by = 0.2)
elitePopulationSizes = seq(0, 1.0, by = 0.2)
testInstances = seq(1:1)


if (isTest) {
  populationSizes = c(50)
  iterSizes = c(50)
  crossoverSizes = c(0.8)
  mutationSizes = c(0.1)
  elitePopulationSizes = c(0.05)
  testInstances = c(1)
}

invoke <- function(functionName) {
  tmpGASummary = matrix(0, 50, 6)
  for (test in testInstances) {
    print(sprintf("Test function name %s", functionName))
    #optymalizacja w dwoch wymiarach:
    testFunctionWrapper <- function(x1, x2)
    {
      goTest(par = c(x1, x2) , fnName = functionName)
    }
    
    x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
    f <- outer(x1, x2, Vectorize(testFunctionWrapper))
    #jpeg(file = sprintf("%s%s%s", path, functionName, '.jpg'))
    persp3D(
      x1,
      x2,
      f,
      theta = -50,
      phi = 20,
      color.palette = bl2gr.colors
    )
    #dev.off()
    
    filled.contour(x1, x2, f, color.palette = bl2gr.colors)
    
    for (elitsimPercentage in elitePopulationSizes) {
      for (pmutation in mutationSizes) {
        for (pcrossover in crossoverSizes) {
          for (iterationSize in iterSizes) {
            for (popSize in populationSizes) {
              elitsim = round(popSize * elitsimPercentage)
              #przepraszam Panie Robert C. Martin zmusili mnie
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
                #liczba iteracji, to to? czy run?
                elitism = elitsim,
                pcrossover = pcrossover,
                pmutation = pmutation,
                run = 100
              )
              
              
              name <- sprintf(
                "%s-p%s-i%s-c%s-m%s-e%s",
                functionName,
                popSize,
                iterationSize,
                pcrossover,
                pmutation,
                elitsimPercentage
              )
              
              jpeg(file = sprintf("%s%s.jpg", path, name)) #wychodzą kwadratowe, żeby były normalne trzeba do pdfa 
              plot(GA)
              dev.off()
              jpeg(file = sprintf("%s%s-contour.jpg", path, name))
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
              dev.off()
            }
          }
        }
      }
    }
    
    #tmpGASummary <- GA@summary + tmpGASummary
    #write.csv(GA@solution, file=sprintf("%s%s%s%s", path, "ga-",functionName, ".csv"))
  }
  
  # matplot(
  #   rownames(tmpGASummary),
  #   tmpGASummary,
  #   type = 'l',
  #   xlab = 'generations',
  #   ylab = 'fitnes',
  #   col = 1:5
  # )
  # legend(
  #   'bottomright',
  #   inset = .05,
  #   legend = colnames(tmpGASummary),
  #   pch = 1,
  #   horiz = TRUE,
  #   col = 1:5
  # )
  
}


for (fnName in fnNames) {
  invoke(fnName)
}