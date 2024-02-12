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

#Optimierung Betrieb 1 Jetztzustand----

#Zielfunktion
#Variablendeklaration
#Weizen = Weiz, Raps = Raps, Naturwiese extensiv = NWex, Naturwiese mittel-intensiv = NWmi, Kunstwiese = KW, Hecken = Heck,
#Gerste = Gerst, Körnermais = Kmais,Sonnenblumen = Sblum
#Zuckerrüben = Zrüb, Nützlingsstreifen = Nützstr, Saum auf Ackerfläche = Saum, Buntbrache =Bbrach
#Pensionspferde = Ppferd
#maximize Z= 2591Weiz + 2433Gerst + 2095Kmais + 3806Raps + 1731NWex + 495NWmi + 1808Heck + 7170Ppferd

#Flächenrestriktion
#Weiz + Gerst + Kmais + Raps + NWex + NWmi + Heck <= 20.28 ha

#Arbeitsrestriktion
#40Weiz + 40Gerst + 34Kmais + 39Raps + 30NWex + 49NWmi + 55Heck + 105Ppferd <= 8980

#BFF-Restriktion
#NWex + Heck >= 1.5393 ha

##Tiere

#Raus-Restriktion
#Nwmi-0.08Ppferd >= 0

#Pferdeplatzrestriktion
#Ppferd <=2

#Pferdefutterrestriktion
#Nwmi+(30/90)*NWex -0.4689Ppferd>=0

#Maximale Kulturanteile bei 15.51 ha offene Ackerfläche
#Weiz<=7.755 50%
#Kmais<=6.204 40%
#Raps<=3.8775 25%

#min 4 Hauptkulturen
#Kmais>=1.551

#Optimierung Betrieb 1 Jetztzustand mit Code----
#Eingabe der Parameter

#Zielfunktionskoeffizienten
c <- c(2591,2433,2095,3806,1731,495,1808,7170)

#Technische Koeffizienten
A <- matrix (c(1, 1, 1, 1, 1, 1, 1, 0, #Flächenrestriktion
               40, 40, 34, 39, 30, 49, 55, 105, #Arbeitsrestriktion
               0, 0, 0, 0, 1, 0, 1, 0, #BFFrestriktion
               0, 0, 0, 0, 0, 1, 0, -0.08, #Rausrestriktionen Pferd
               0, 0, 0, 0, 0, 0, 0, 1, #Pferdeflächenrestriktion
               0, 0, 0, 0, 0.3333, 1, 0, -0.4689, #Pferdefutterrestriktionen
               1, 0, 0, 0, 0, 0, 0, 0, #Maximale Kulturenanteile Weizen
               0, 0, 1, 0, 0, 0, 0, 0, #Mais
               0, 0, 0, 1, 0, 0, 0, 0, #Raps
               0, 0, 1, 0, 0, 0, 0, 0, #min 4 Hauptkulturen
               0, 0, 0, 0, 1, 1, 0, 0  #fixierte Wiesenflächen
), nrow=11, byrow = TRUE)

colnames(A) <- c("Futterweizen", "Gerste","Körnermais","Raps","extensiveNaturwiese","mittelintensiveNaturwiese","Hecke","Pensionspferd")
rownames(A) <- c("Fläche","Arbeit","Bff","Raus Pferd","Pferdeplätze","Pferde Futter","Max Weizen","Max Mais","Max Raps","Min Mais","Fixierung Wiesen")

#Richtung der RHS
directions <- c("<=", "<=", ">=", ">=", "<=",">=","<=", "<=", "<=",">=","=")

#RHS
b <- c(20.28,8980,1.54,0,2,0,7.76,6.20,3.88,1.55,4.1)


maximize = TRUE

#Berechnung der Lösung

#Optimierung

LP<-LP(c =c, A = A, b = b, maximize = maximize, directions = directions)
print(LP)
anderhubohneackeroptimum <- LP$objective_value
kulturenanderhubohnebff<-LP$solution


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
    A2[2, 1:8] <- LHS_matrix[i, 17:24]
    A2[3, 5] <- LHS_matrix[i, 29]
    A2[3, 7] <- LHS_matrix[i, 31]
    A2[4, 8] <- LHS_matrix[i, 40]
    
    
    b2 <- LHS_matrix[i, (1:length(b)) + 88]
    
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
sim.results1 <- sim1(param.sets1) 

sim_results_data_frame1 <- as.data.frame(sim.results1)

indexe[length(indexe) +1] <- "OptimalValue"

colnames(sim_results_data_frame1) <- endnamen



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
print(pcc.result1)

# Resultate betrachten

abline(h=0, col="black") #Interpretation: je weiter weg von Null, desto grösserer Einfluss auf die Zielvariable
ggplot(pcc.result1) + coord_cartesian(ylim = c(-0.125,0.125))

#Daten nach Höhe des Estimate sortieren

pcc_sortiert <- pcc.result1$PRCC[order(abs(pcc.result1$PRCC$original), decreasing = TRUE), ]

# Gib die sortierten Daten aus
print(pcc_sortiert)


#Nur die relevanten Einträge im Diagramm zeigen

print(pcc.result1$PRCC$original)
pcc_gefiltert <- pcc.result1$PRCC$original
print(pcc_gefiltert)
pccgefiltert <- subset(pcc_gefiltert, abs(pcc_gefiltert) > 0.022)
print(pccgefiltert)

#die richtigen Namen nehmen
index_positionen_kleiner_006 <- which(abs(pcc_gefiltert) < 0.022)
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

                              





