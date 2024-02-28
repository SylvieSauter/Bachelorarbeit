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

#Optimierung Betrieb 3 mit Acker-BFF----

#Zielfunktion
#Variablendeklaration
#Winterweizen = Weiz, Winterraps = Raps, Naturwiese extensiv = NWex, Naturwiese mittel-intensiv = NWmi, Kunstwiese = KW, Hecken = Heck,
#Wintergerste = Gerst, Körnermais = Kmais, Sonnenblumen = Sblum
#Zuckerrüben = Zrüb, Nützlingsstreifen = Nützstr, Saum auf Ackerfläche = Saum, Buntbrache =Bbrach,"IWR"=in weiter Reihe, "Schon"=Ackerschonstreifen
#maximize Z= 3272Weiz+4671Raps+3488Sblum+4530Zrüb+1731NWex+495NWmi+
#1808Heck+822KW+2325Nützstr+12921Saum+3268Bbrach+2785Rbrach+3272WeizIWR+4671Schonraps+3488Schonsblum

#Flächenrestriktion
#Weiz+Raps+Sblum+Zrüb+NWex+NWmi+Heck+KW+Nützstr1+Saum+Bbrach+Rbrach+WeizIWR+Schonraps+Schonsblum<= 19.516ha

#Arbeitsrestriktion
#38Weiz+ 39Raps+37Sblum+48Zrüb+30NWex+49Nwmi+55Heck+49KW+35Nützstr+148Saum+49Bbrach+50Rbrach+38WeizIWR+39Schonraps+37Schonsblum <= 7800h

#Maximale Kulturenanteile
#Weiz+WeizIWR<=5.4222
#Raps+Schonraps<=2.7111
#Sblum+Schonsblum<=2.7111
#Rüb<=2.7111
#Bbrach + Rbrach<=2.7111

#Saumbeschränkung
#Saum<=0.4
#Nützstr<=0.2

#Ackerschonstreifenbegrenzung
#Schonraps-0.1Raps<=0
#Schonsblum-0.1Sblum<=0


#BFF-Restriktion
# NWex+ Nutzstr + Heck + Saum + Bbrach+WeizIWR+Schonraps+Schonsblum >= 1.5659 ha
#Acker-BFF Restriktion
#Saum+Bbrach+Rbrach+Nützstr+WeizIWR+Schonraps+Schonsblum>=0.3795
#max 50% IWR
#WeizIWR<=0.1896


#Optimierung Betrieb 3 mit Acker-BFF mit Code----

c <- c(3272,4671,3488,4530,1731,495,1808,822,2325,2921,3268,2785,3272,4671,3488)

#Technische Koeffizienten
A <- matrix (c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, #FLächenrestriktion
               38,39,37,48,30,49,55,49,35,48,49,50,38,39,37, #Arbeitsrestriktion
               1,0,0,0,0,0,0,0,0,0,0,0,1,0,0, #Maximale Kulturenanteile Weizen
               0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,  #Raps
               0,0,1,0,0,0,0,0,0,0,0,0,0,0,1, #Sblum
               0,0,0,1,0,0,0,0,0,0,0,0,0,0,0, #Rüben
               0,0,0,0,1,0,1,0,1,1,1,1,1,1,1, #7%BFF
               0,0,0,0,0,0,0,0,1,1,1,1,1,1,1, #Ackerbff
               0,0,0,0,0,0,0,0,0,0,1,1,0,0,0, #Bbrach
               0,0,0,0,0,0,0,0,0,1,0,0,0,0,0, #Saum
               0,0,0,0,0,0,0,0,1,0,0,0,0,0,0, #Nützstr
               0,0,0,0,0,0,0,0,0,0,0,0,1,0,0, #max 50% IWR
               0,-0.1,0,0,0,0,0,0,0,0,0,0,0,1,0, #Schonwraps
               0,0,-0.1,0,0,0,0,0,0,0,0,0,0,0,1, #Schonsblum
               0,0,0,0,1,1,0,1,0,0,0,0,0,0,0 #Fixierung Wiesenflächen
), nrow=15, byrow=TRUE)

colnames(A) <- c("Winterweizen","Winterraps","Sonnenblumen","Zuckerrüben","Naturwiese extensiv","Naturwiese mittel-intensiv","Hecke","Kunstwiese","Nützlingsstreifen","Saum","Buntbrache","Rotationsbrache","Winterweizen IWR","Schon Raps","Schon Sonnenblumen")
rownames(A) <- c("Fläche","Arbeit","Max Weizen","Max Raps","Max Sonnenblumen","Max Zuckerrüben","BFF","Acker-BFF","Max Brachen","Max Saum","Max Nützlingsstreifen","Max IWR", "Max Schonraps","Max Schon Sonnenblume","Fixierung Wiesenflächen")

#Richtung der RHS
directions <- c("<=", "<=", "<=", "<=", "<=", "<=",">=",">=","<=","<=","<=","<=","<=","<=","=")
maximize = TRUE

#RHS
b <- c(19.52,7800,5.42,2.71,2.71,2.71,1.57,0.38,2.71,0.4,0.2,0.19,0,0,8.3)

#Berechnung der Lösung

LP<-LP(c =c, A = A, b = b, maximize = maximize, directions = directions)
print(LP)
ludermitackeroptimum <- LP$objective_value
kulturenludermitbff<-LP$solution


#Vergleich mit und ohne Acker-BFF

differenzluder <- ludermitackeroptimum - luderohneackeroptimum
print(round(differenzluder,3))

differenzkulturenluder <- kulturenludermitbff[1:11] - kulturenluderohnebff
print(round(differenzkulturenluder,3))
print(kulturenludermitbff[12:15])

#Vergleich Einkommensunterschiede im Durchschnitt

differenzbetriebe <- (differenzgrueter+differenzluder+differenzanderhuboptimum)/3
print(differenzbetriebe)

#Sensitivitätsanalyse

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


#1. Sim1 auf Parameter ausführen


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
    
    b2 <- LHS_matrix[i, (1:length(b)) + 225]
    
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



# Sim1 auf alle Parameter ausführen
sim.results1 <- sim1(param.sets1) 

sim_results_data_frame1 <- as.data.frame(sim.results1)

indexe[length(indexe) +1] <- "OptimalValue"

colnames(sim_results_data_frame1) <- indexe


sim.results.vector1 <- sim.results1[,ncol(sim.results1)]

summary(sim_results_data_frame1)

# Plot
plot(sim.results1) 


param.sets1 <- as.data.frame(param.sets1)
sim.results.vector <- as.vector(sim.results.vector1)

# PCC berechnen
pcc.result1 <- pcc(X= param.sets1, y=sim.results.vector1, nboot = 1000, 
                   rank = TRUE)

pcc.result1

#Daten nach Höhe des Estimate sortieren

pcc_sortiert <- pcc.result1$PRCC[order(abs(pcc.result1$PRCC$original), decreasing = TRUE), ]

# Sortierte Daten ausgeben
print(pcc_sortiert)


#Nur die relevanten Einträge im Diagramm zeigen

print(pcc.result1$PRCC$original)
pcc_gefiltert <- pcc.result1$PRCC$original
print(pcc_gefiltert)
pccgefiltert <- subset(pcc_gefiltert, abs(pcc_gefiltert) > 0.04)
print(pccgefiltert)

#die richtigen Namen nehmen
index_positionen_kleiner_006 <- which(abs(pcc_gefiltert) < 0.04)
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





