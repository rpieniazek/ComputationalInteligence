#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
library(GA)
library(globalOptTests)

#miejsce zapisu wykresow
path = '/Users/evelan/Desktop/ga.nosync/'

#uzyte funkcji
fnNames = c("Schubert")

# liczba przebiegow
testInstances = 30

#domyslne parametry
defaultPopSize = 50
defaultCrossover = 0.8
defaultMutation = 0.1
defaultElitePopulation = 0.05
defaultIterationSize = 100

#rysowanie wykresu temperaturowego ze znalezionym rozwiazaniem
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

#generacja kodu latex do wstawienia wykresow
getPlotName <- function(...) {
  sprintf(
    "\\clearpage\\begin{figure}[!htbp]
    \\centering
    \\mbox{
    \\subfigure{
    \\includegraphics[width=3in]{{{inc/results/%s}}}\\quad
    }
    \\subfigure{
    \\includegraphics[width=3in]{{{inc/results/%s}}}\\quad
    }
    }
    \\caption{%s %s p%s i%s c%s m%s e%s}
    \\end{figure}", ... )
}

#minimalizacja GA oraz zapis wykresow
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
    
    #rozpatrywana przestrzen
    x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
    f <- outer(x1, x2, Vectorize(testFunctionWrapper))
    
    #obliczenie liczby populacji elitarnej
    elitsim = round(popSize * elitsimPercentage)
    
    #6 wartosci (kolumn) x liczba iteracji (wiersze)
    tmpGASummary <- matrix(0, iterationSize, 6)
    
    #ilosc uruchomien testu
    for (test in 1:testInstances) {
      #minimizacja GA:
      GA <- ga(
        type = "real-valued",
        fitness =  function(x)
          - Vectorize(testFunctionWrapper(x[1], x[2])),
        # uwaga na minusa, bo szukamy glob. minimum
        min = c(-5.12, -5.12),
        #rozpatrywana przestrzen
        max = c(5.12, 5.12),
        #rozpatrywana przestrzen
        monitor = FALSE #wylaczenie logowania
      )
      #sumowanie rozwiazan
      tmpGASummary <- GA@summary + tmpGASummary
    }
    #wyznaczenie sredniej arytmetycznej rozwiazan
    tmpGASummary <- tmpGASummary / testInstances
    
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
    
    #nazwa wykresu
    filenamePlot = sprintf("generations-%s.jpg", name)
    max <- tmpGASummary[, 1]
    mean <- tmpGASummary[, 2]
    median <- tmpGASummary[, 4]
    min <- tmpGASummary[, 6]
    
    #zapis wykresu
    jpeg(file = sprintf("%s%s", path, filenamePlot))
    
    #zakres y dla rysowanego wykresu
    minPlot <- min(mean) * 0.98
    maxPlot <- max(max) * 1.02
    
    #rysowanie wykresu z zaznaczonymi wartosciami:
    #- srednia arytmetyczna rozwiazan, 
    #- mediana rozwiazan,
    #- najlepszym rozwiazaniem 
    #dla kazdej generacji 
    plot(
      mean,
      type = "o",
      col = "blue",
      pch = 20,
      ly = 2,
      ann = FALSE,
      ylim = c(minPlot, maxPlot)
    ) 
    lines(
      max,
      type = "o",
      col = "green",
      pch = 22,
      lty = 4
    )
    lines(
      median,
      type = "o",
      col = "red",
      pch = 21,
      lty = 3
    )
    title(xlab = "Generations")
    title(ylab = "Fitness value")
    grid()
    legend(
      "bottomright",
      c("mean", "best", "median"),
      cex = 0.8,
      col = c("blue", "green", "red"),
      pch = c(20, 22, 21),
      lty = c(2, 4, 3)
    )
    dev.off()
    
    #zapis wykresu temperaturowego oraz zaznaczenie znalezionego wyniku
    fileNameContour = sprintf("result-%s.jpg", name)
    jpeg(file = sprintf("%s%s", path, fileNameContour))
    showFunctionContourWithResult(x1, x2, f, GA)
    dev.off()
    
    plotTitle <- "Test optymalizacji GA"
    line = getPlotName(
      filenamePlot,
      fileNameContour,
      plotTitle,
      functionName,
      popSize,
      iterationSize,
      pcrossover,
      pmutation,
      elitsimPercentage
    )
    
    #zapis kodu latex do wygenerowanych wykresow
    write(line,
          file = sprintf("%s_latex.txt", path),
          append = TRUE)
  }

#uruchomienie testow dla roznych parametrow dla danej funkcji z argumentu
invokeTestsWithFunction <- function(functionName) {
  #zmiana wartosci populacji elitarnej

    calculateGA(
      functionName,
      defaultPopSize,
      defaultIterationSize,
      defaultElitePopulation,
      defaultCrossover,
      defaultMutation
    )
}

#START
for (fnName in fnNames) {
  print(sprintf("testing with function %s", fnName))
  invokeTestsWithFunction(fnName)
  print(sprintf("tests end for function %s", fnName))
}
