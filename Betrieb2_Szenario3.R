#installation der notwendigen packages
install.packages("devtools")
library(devtools)
install_github("AECP-ETHZ/ETH.OLP")
library(ETH.OLP)
install.packages("lpSolve")
library(lpSolve)
library(ETH.OLP)
library(ggplot2)
library(sensitivity)

#Optimierung Betrieb 2 Jetztzustand----

#Zielfunktion
#Variablendeklaration
#Körnermais = Kmais, Winterweizen = Weiz, Dinkel = Dink, Silo/Grünmais = SGmais, 
#Winterraps = Raps, Soja = Soja, Bohnen und Wicken = BWick,
#Obstanlagen = Obst, Naturwiese extensiv = NWex, Naturwiese mittel-intensiv = NWmi, Kunstwiese = KW, Milchkuh = Mkuh
#Kombimast Kälber = Kmast, Mastremonten = Mastr

#maximize Z= 5881Weiz + 5483Dink + 5222Bwick + 6913Raps + 5304Soja + 5509Kmais + 4631SGmais+
#30'186Obst+2153KW+2215NWex+1264NWmi+1808Heck + 4340Mkuh + 101Kmast + 496Mastr

#Flächenrestriktion
#Weiz+Dink+Bwick+Raps+Soja+Kmais+SGmais+Obst+KW+NWex+NWmi+Heck<= 106.1737 ha

#Arbeitsrestriktion
#45Weiz+44Dink+44Bwick+56Raps+74Soja+59Kmais+79SGmais+724Obst+55KW+39NWex+52NWmi+55Heck+79Mkuh+25Kmast+13.8Mastr<=22'100h

#BFF-Restriktion
#NWex+Heck >= 7.5133 ha
#0.07Obst-Nwex-Heck<=0

#Maximalkulturanteilerestriktion
#Raps <= 10.222 ha
#Bwick<=10.222ha
#Soja<=10.222ha
#Wweiz+Dink<=20.444ha
#Gmais+Kmais <=16.3552ha

#Obstanlagenrestriktion
#Obst<=2.5591


#Tiere

#Weidebeitragrestriktion
#NWmi -0.1873Kmast>= 0
#NWmi - 0.1873Mastr >= 0
#NWmi - 0.1873Mkuh >= 0

#Tierplatzrestriktion
#Mkuh <= 103
#Kmast<= 25
#Mastr<= 34


#Tierfutterrestriktionen
#Milchkuhrestriktion Gras KW+0.5926NWmi+0.2777NWex-0.5426*Mkuh>=0
#Maissilage -0.0697Mkuh+SGMais>=0
#Kmastrestriktion KW+0.5926Nwmi+0.2777NWex-0.0037Kmast>=0
#Mastr-restriktion Gras KW+0.5926NWmi+0.2777NWex-0.0037Mastr>=0
#Maissilagerestriktion SGMais-0.01645Mastr>=0


#Optimierung Betrieb 2 Jetztzustand mit Code----

#Eingabe der Parameter

#Zielfunktionskoeffizienten
c <- c(5881,5483,5222,6913,5304,5509,4631,301860,2153,2215,1264,1808,4340,101,496)

#Technische Koeffizienten
A <- matrix (c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, #Flächenrestriktion
               45,44,44,56,74,59,79,724,55,39,52,55,79,25,13.8, #Arbeitsrestriktion
               0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,  #BFFrestriktion
               0, 0, 0, 0, 0, 0, 0, 0.07, 0, -1, 0, -1, 0, 0, 0, #Spezialkulturen BFF
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -0.1873, 0, 0,  #Weidebeitragsrestriktionen
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -0.1873, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -0.1873, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,  #Tierplatzrestriktion
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
               1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #Maximalkulturenanteile Weiz Dink
               0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #Bwick
               0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #Soja
               0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, #Mais
               0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, #Obstrestriktion
               0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #Raps
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0.2777, 0.5926, 0, -0.5426, 0, 0, #Milchkuhfutterrrestriktion Gras
               0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -0.0697, 0, 0, #Maissilage
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0.2777, 0.5926, 0, 0, -0.0037, 0, #Kmastrestriktion
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0.2777, 0.5926, 0, 0, 0, -0.0037, #Mastrrestriktion Gras
               0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, -0.01645, #Mastr Maissilage
               0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 #Fixierung Wiesenflächen
), nrow=22, byrow=TRUE)

colnames(A) <- c("Winterweizen","Dinkel","Bohnen und Wicken","Winterraps","Soja","Körnermais","Silomais","Obstanlage","Kunstwiese","Naturwiese extensiv","Naturwiese mittel-intensiv","Hecke","Milchkuh","Mastkalb","Mastremonten")
rownames(A) <- c("Fläche","Arbeit","BFF","Spezialkulturen BFF","Weidebeitrag Milchkuh","Weidebeitrag Kälbermast","Weidebeitrag Mastremonten", "Tierplätze Milchkuh", "Tierplätze Kälbermast", "Tierplätze Mastremonten","Max Weizen und Dinkel","Max Ackerbohnen","Max Soja","Max Mais","Max Obstanlage","Max Raps","Milchkuh Gras","Milchkuh Silage","Kälbermast Gras","Mastremonten Gras","Mastremonten Silage", "Fixierung Wiesenflächen")


#Richtung der RHS
directions <- c("<=", "<=", ">=", "<=",">=",">=",">=","<=","<=","<=", "<=","<=","<=","<=","<=", "<=",">=",">=",">=",">=",">=", "=")
maximize = TRUE

#RHS
b <- c(106.17,22100,7.51,0, 0, 0, 0,103,34,25,20.44,10.22,10.22,16.36,2.56,10.22,0,0,0,0,0,61.3)

#Berechnung der Lösung

LP<-LP(c =c, A = A, b = b, maximize = maximize, directions = directions)
print(LP)
grueterohneackeroptimum <- LP$objective_value
kulturengrueterohnebff<-LP$solution


#Sensitivität nach Viviane Fahrni

library(lhs)
library(sensitivity)
library(ETH.OLP)



library("MatchIt")
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)

library(psych)
library(ggpubr)
library(GPArotation)
library(tidyverse)
library(writexl)
library(compiler)
library(memoise)

library(MASS)
library(lpSolve)
library(landscapeR)
library(raster)
library(landscapemetrics)
library(lctools)
library(plot3D)
library(matrixStats)
library(grDevices )
library(ggthemes)
library(extrafont)
library(scales)
library(RCurl)
library(purrr)

library(rasterVis)
library(terra)
library(tidyterra)
library(tgp)


#1. Simulationsfunktion

sim1 <- function(LHS_matrix) {
  
  # leere Liste um die Simulationswerte zu speichern
  eval.values <- rep(0, nrow(LHS_matrix))
  
  # iterieren durch alle Kombinationen von Parametern
  for (i in 1:nrow(LHS_matrix)) {
    
    # zu testende Variablen 
    c2 <- LHS_matrix[i, 1:length(c)]
    
    # Kopie von A erstellen
    A2 <- A
    
    # nur die Parameter, die variiert werden sollen, werden überschrieben
    A2[2, 1:15] <- LHS_matrix[i, 31:45]
    A2[4, 8] <- LHS_matrix[i, 68]
    A2[5, 13] <- LHS_matrix[i, 88]
    A2[6, 14] <- LHS_matrix[i, 104]
    A2[7, 15] <- LHS_matrix[i, 120]
    
    b2 <- LHS_matrix[i, (1:length(b)) + 330]
    
    # aktuelle Parameterkombination einlesen
    LP <- LP(c2, A2, b2)
    
    # Funktion
    sim_optimal_value <- objective(LP)
    
    eval.values[i] <- sim_optimal_value
  }
  
  LHS_matrix <- cbind(LHS_matrix, eval.values)
  return(LHS_matrix)
}


#LHS wird tausendmal durchgeführt. Die Zielfunktionskoeffizienten werden um 5% variiert, die Matrixeinträge um 10%.
neuematrix <- matrix(nrow=length(A)+length(c)+length(b), ncol=2)
for(m in 1:length(c)){
  neuematrix[m,1] <- c[m] - 0.05*c[m]
  neuematrix[m,2] <- c[m] + 0.05*c[m]
  
}
l<-1+length(c)

for(n in 1:nrow(A)){
  for(m in 1:ncol(A)){
    
    
    neuematrix[l,1] <- (A[n,m] - 0.1*A[n,m])
    neuematrix[l,2] <- (A[n,m] + 0.1*A[n,m])
    if(neuematrix[l,1] == 0){
      neuematrix[l,1] <- 0.00
    }
    if(neuematrix[l,2]==0){
      neuematrix[l,2] <- 0.00
    }
    l <- l+1
    p<-l
  }
}

for(k in 1:length(b)){
  neuematrix[p,1] <- (b[k]-0.1*b[k])
  neuematrix[p,2] <- (b[k] + 0.1*b[k])
  p<-p+1
  
}

param.sets1 <- lhs(n=1000, rect= neuematrix)

#colnames erstellen
# Erstellung von indexe
indexe <- character(length = ncol(param.sets1))

# Generierung von Spaltennamen basierend auf param.sets1
for(p in (length(c) + 1):ncol(param.sets1)) {
  col_index <- p - length(c)  
  for(r in 1:nrow(A)) {
    for(q in 1:ncol(A)) {
      indexe[p] <- as.character(paste("A", r, ".", q, sep = ""))
      p <- p + 1
    }
  }
}

# Entfernen von überflüssigen Werten aus indexe
indexe <- indexe[-c((length(c) + 2):(ncol(param.sets1)))]

# Anpassung der ersten Werte in indexe basierend auf colnames(A)
indexe[1:length(c)] <- colnames(A)

# Einrichten der Namen für b
namesb <- paste("b", seq_along(b))

# Vertikales Anfügen von indexe und namesb
endnamen <- c(indexe, namesb)
endnamen <- make.names(endnamen,unique=TRUE)

colnames(param.sets1) <- endnamen



# 3. Sim1 wird auf jede Parameterkombination ausgeführt
sim.results1 <- sim1(param.sets1) #apply did not do it for me, that somehow only worked with one variable

sim_results_data_frame1 <- as.data.frame(sim.results1)

indexe[length(indexe) +1] <- "OptimalValue"

colnames(sim_results_data_frame1) <- indexe


sim.results.vector1 <- sim.results1[,ncol(sim.results1)]

summary(sim_results_data_frame1)

# Plot
plot(sim.results1) # Latin Hyper Cube sieht gut verteilt aus

#Datensätze als Vektor und Data Frame
param.sets1 <- as.data.frame(param.sets1)
sim.results.vector <- as.vector(sim.results.vector1)

# 4.1 PCC ausrechnen (THIELE)
pcc.result1 <- pcc(X= param.sets1, y=sim.results.vector1, nboot = 1000, 
                   rank = TRUE)

pcc.result1


#Daten nach Höhe des Estimate sortieren

pcc_sortiert <- pcc.result1$PRCC[order(abs(pcc.result1$PRCC$original), decreasing = TRUE), ]

# Gib die sortierten Daten aus
print(pcc_sortiert)


#Nur die relevanten Einträge im Diagramm zeigen

print(pcc.result1$PRCC$original)
pcc_gefiltert <- pcc.result1$PRCC$original
print(pcc_gefiltert)
pccgefiltert <- subset(pcc_gefiltert, abs(pcc_gefiltert) > 0.05)
print(pccgefiltert)

#die richtigen Namen nehmen
index_positionen_kleiner_006 <- which(abs(pcc_gefiltert) < 0.05)
print(index_positionen_kleiner_006)

endnamengefiltert <- endnamen[setdiff(seq_along(endnamen), index_positionen_kleiner_006)]

#Plot erstellen

nummerierter_vektor <- seq_along(pccgefiltert)
names(nummerierter_vektor) <- endnamengefiltert

dataframe1 = data.frame(x=nummerierter_vektor, y= pccgefiltert)

library(ggplot2)
ggplot(dataframe1, aes(x = x, y = y)) +
  geom_point() +
  xlim(endnamengefiltert)+
  ylab("Schätzwerte")+
  ggtitle("PRCC")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "solid", color = "black", linewidth = 0.15)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



