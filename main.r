#https://cran.r-project.org/web/packages/GA/vignettes/GA.html
#install.packages("GA");# do instalacji biblioteki GA
#install.packages("globalOptTests")
library(GA)
library(globalOptTests)

fnNames = c("AluffiPentini", "Bohachevsky1", "Branin")
path = '/Users/evelan/Desktop/'
file_extension = '.jpg'


testMethod <- function(functionName) {
  
}

#optymalizacja w dwoch wymiarach:
testFunctionWrapper <- function(x1, x2)
{
  goTest(par=c(x1, x2) , fnName=fnNames[1])
}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Vectorize(testFunctionWrapper))
jpeg(file = '/Users/evelan/Desktop/dupa2.jpg')
persp3D(x1, x2, f, theta = -50, phi = 20, color.palette = bl2gr.colors)
dev.off()


filled.contour(x1, x2, f, color.palette = bl2gr.colors)

#minimizacja GA:
GA <- ga(type = "real-valued", 
         fitness =  function(x) -Vectorize(testFunctionWrapper(x[1], x[2])), # uwaga na minusa, bo szukamy glob. minimum
         min = c(-5.12, -5.12), max = c(5.12, 5.12), 
         popSize = 50, maxiter = 80, run = 100)
summary(GA)
plot(GA)

#rysowanie wyniku:
filled.contour(x1, x2, f, color.palette = bl2gr.colors, 
               plot.axes = { axis(1); axis(2); 
                 points(GA@solution[,1], GA@solution[,2], 
                        pch = 3, cex = 2, col = "white", lwd = 2) })



