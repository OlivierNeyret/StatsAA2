x <- c(-0.97,-0.96,-0.93,-0.91,-0.90,-0.87,-0.85,-0.80,-0.75,-0.73,-0.66,-0.65,-0.63,-0.60,-0.58,-0.51,-0.48,-0.45,-0.44,-0.40,-0.37,-0.33,-0.30,-0.25,-0.12,-0.08,0.25,0.41,0.51,0.68)

displayFixedWidth <- function(x) {
  k=sturge(x)
  print(k)
  a0=min(x)-0.025*(max(x)-min(x))
  ak=max(x)+0.025*(max(x)-min(x))
  h=(ak-a0)/k
  
  hist = hist(x, prob=T, breaks=seq(a0,ak,h), xlim=c(a0,ak), main="Histogramme à largeur fixée")
  lines(hist$mids, hist$density, lwd=3, col="Red")
  lines(density(x), lwd=3, col="Blue")
  
  # partie specifique au partiel 2017
  t = -3*mean(x)
  x0 = seq(-t,t,0.001)
  y = (t-x0)/(2*t*t)
  lines(x0, y, lwd=3, col="Green")
  
  
  
  plot(ecdf(x), main="Fonction de répartition empirique pour largeur fixée")
}

displayFixedEffec <- function(x) {
  x = sort(x)
  k=sturge(x)
  h = length(x)/k
  i=1
  j=1
  res <- numeric(length = k+1)
  while(i<=length(x)) {
    res[j] = x[i]
    i = i+h
    j=j+1
  }
  res[k+1]=max(x)
  
  print(res)
  hist = hist(x, prob=T, breaks=res, main="Histogramme à effectif fixé")
  lines(hist$mids, hist$density, lwd=3, col="Red")
  lines(density(x), lwd=3, col="Blue")
  plot(ecdf(x), main="Fonction de répartition empirique pour effectif fixé")
}

sturge <- function(x) {
  res = (round(1 + log(length(x)) / log(2)))
  if(res < 5) {
    res = 5
  }
  return(res)
}

triangleQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e],sqrt(1-seq(1:e+1)/(e+1)), main="Q-Q Plot for triangle law") #ylim=c(-2.5,0.1),
  abline(v=0)
  abline(h=0)
}

simul_triangle <- function(nbTirage, t) {
  u <- runif(nbTirage)
  return(t-2*t*sqrt(1-u))
}

check_simul_triangle <- function(nbSimul) {
  t <- 5
  esperanceEmp <- 0
  esperanceTh <- -t/3
  varianceEmp <- 0
  varianceTh <- 2*t*t/9
  for(i in 1:nbSimul){
    vect <- simul_triangle(30, t)
    esperanceEmp <- mean(vect) + esperanceEmp
    varianceEmp <- var(vect) + varianceEmp
  }
  esperanceEmp <- esperanceEmp / nbSimul
  varianceEmp <- varianceEmp / nbSimul
  cat("\nQualité de l'espérance : ", 100*esperanceEmp/esperanceTh, "%")
  cat("\nQualité de la variance : ", 100*varianceEmp/varianceTh, "%")
}

simul_boris <- function(nbSimul) {
  tSim <- 0
  t <- 10
  scoreDispersion <- 0
  for(i in 1:nbSimul){
    vect <- simul_triangle(30, t)
    tSim <- max(vect) + tSim
    scoreDispersion <- scoreDispersion + abs(tSim-t)
  }
  tSim <- tSim/nbSimul
  scoreDispersion <- scoreDispersion
  cat(tSim)
  cat("\nbiais = ", ((tSim/t)*100), "%")
  cat("\nScore dispersion = ", scoreDispersion,"\n")
}


simul_charles <- function(nbSimul) {
  tSim <- 0
  t <- 10
  scoreDispersion <- 0
  for(i in 1:nbSimul){
    vect <- simul_triangle(30, t)
    tSim <- -min(vect) + tSim
    scoreDispersion <- scoreDispersion + abs(tSim-t)
  }
  tSim <- tSim/nbSimul
  scoreDispersion <- scoreDispersion
  cat(tSim)
  cat("\nbiais = ", ((tSim/t)*100), "%")
  cat("\nScore dispersion = ", scoreDispersion,"\n")
}

simul_david <- function(nbSimul) {
  tSim <- 0
  t <- 10
  scoreDispersion <- 0
  for(i in 1:nbSimul){
    vect <- simul_triangle(30, t)
    tSim <- (max(vect)-min(vect))/2 + tSim
    scoreDispersion <- scoreDispersion + abs(tSim-t)
  }
  tSim <- tSim/nbSimul
  scoreDispersion <- scoreDispersion
  cat(tSim)
  cat("\nbiais = ", ((tSim/t)*100), "%")
  cat("\nScore dispersion = ", scoreDispersion,"\n")
}

simul_albert <- function(nbSimul) {
  tSim <- 0
  t <- 10
  scoreDispersion <- 0
  for(i in 1:nbSimul){
    vect <- simul_triangle(30, t)
    tSim <- -3*mean(vect) + tSim
    scoreDispersion <- scoreDispersion + abs(tSim-t)
  }
  tSim <- tSim/nbSimul
  scoreDispersion <- scoreDispersion
  cat(tSim)
  cat("\nbiais = ", ((tSim/t)*100), "%")
  cat("\nScore dispersion = ", scoreDispersion,"\n")
}

triangleQQplot(x)
# displayFixedWidth(x)
# simul_boris(10000)
# simul_charles(10000)
# simul_david(10000)
# simul_albert(10000)