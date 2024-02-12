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

#Optimierung Betrieb 2 mit Acker-BFF----
#Zielfunktion
#Variablendeklaration
#Körnermais = Kmais, Winterweizen = Weiz, Dinkel = Dink, Silo/Grünmais = SGmais, 
#Winterraps = Raps, Soja = Soja, Bohnen und Wicken = BWick,
#Obstanlagen = Obst, Naturwiese extensiv = NWex, Naturwiese mittel-intensiv = NWmi, Kunstwiese = KW, Milchkuh = Mkuh
#Kombimast Kälber = Kmast, Mastremonten = Mastr, "IWR"=in weiter Reihe, "schon"= Ackerschonstreifen

#maximize Z= 5881Weiz + 5483Dink + 5222Bwick + 6913Raps + 5304Soja + 5509Kmais + 4631SGmais+
#30'186Obst+2153KW+2215NWex+1264NWmi+1808Heck + 4340Mkuh + 101Kmast + 496Mastr 
#+2785Rbrach+2921Saum+2325Nützstr+5881WeizIWR+5483DinkIWR+5222Schonbwick+6913Schonraps+5304Schonsoja

#Flächenrestriktion
#Weiz+Dink+Bwick+Raps+Soja+Kmais+SGmais+Obst+KW+NWex+NWmi+Heck+Rbrach+Saum+Nützstr+WeizIWR+DinkIWR+Schonbwick+Schonraps+Schonsoja<= 106.1737 ha

#Arbeitsrestriktion
#45Weiz+44Dink+44Bwick+56Raps+74Soja+59Kmais+79SGmais+724Obst+55KW+39NWex+52NWmi+55Heck+79Mkuh+25Kmast+13.8Mastr+50Rbrach+48Saum+35Nützstr+45Weiz+44Dink
#+44Schonbwick+556Schonraps+74Schonsoja<=22100h


#Maximalkulturanteilerestriktion
#Raps +Schonraps <= 10.222 ha
#Bwick+Schonbwick<=10.222ha
#Soja+Schonsoja<=10.222ha
#Weiz+Dink+WeizIWR+DinkIWR<=20.444ha
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
#Maissilage -0.0697Mkuh +SGMais>=0
#Kmastrestriktion KW+0.5926Nwmi+0.2777NWex-0.0037Kmast>=0
#Mastrrestriktion Gras KW+0.5926NWmi+0.2777NWex-0.0037Mastr>=0
#Maissilagerestriktion SGMais -0.01645Mastr>=0

#BFF-Restriktion
#7% Spezialkulturen: 0.07Obst-Nwex-Heck-Rbrach-Saum-Nützstr1-WweizIWR-DinkIWR+Schonbwick+Schonwraps+Schonsoja <=0
#7%LN: Nwex+Heck+rbrach+Saum+Nützstr1+WweizIWR+DinkIWR+Schonbwick+Schonwraps+Schonsoja>=7.5133
#Acker-BFF: Rbrach+Saum+Nützstr1+WweizIWR+DinkIWR+Schonbwick+Schonwraps+Schonsoja>=1.431
#max 50% Getreide IWR: WweizIWR+DinkIWR<=0.7155

#Begrenzung Saum
#Saum<=0.78

#Begrenzung Ackerschonstreifen
#Schonbwick-0.1Bwick<=0
#Schonwraps-0.1Wraps<=0
#Schonsraps-0.1Sraps<=0
#Schonsoja-0.1Soja<=0

#Optimierung Betrieb 2 mit Acker-Bff mit Code----

#Eingabe der Parameter

#Zielfunktionskoeffizienten
c <- c(5881,5483,5222,6913,5304,5509,4631,301860,2153,2215,1264,1808,4340,101,496,2785,2921,2325,5881,5483,5222,6913,5304)

#Technische Koeffizienten
A <- matrix (c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1,1,1,1,1,1, #Flächenrestriktion
               45,44,44,56,74,59,79,724,55,39,52,55,79,25,13.8, 50, 48, 35, 45,44,44,56,74,#Arbeitsrestriktion
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -0.1873, 0, 0, 0, 0, 0,0,0, 0,0,0, #Weidebeitragsrestriktionen
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -0.1873, 0 , 0, 0, 0, 0,0,0,0,0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -0.1873, 0, 0, 0, 0,0, 0,0,0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,0,0,0,0,  #Tierplatzrestriktion
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,0,0,0,0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,0,0,0,0,
               1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1,1,0,0,0, #Maximalkulturenanteile Weiz Dink
               0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,1,0,0,  #Bwick
               0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,1, #Soja
               0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0, #Mais
               0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0, #Obstrestriktion
               0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,1,0,#Raps
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0.2777, 0.5926, 0, -0.5426, 0, 0, 0, 0, 0,0,0, 0,0,0,#Milchkuhfutterrrestriktion Gras
               0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -0.0697, 0, 0, 0, 0, 0, 0,0, 0,0,0,#Maissilage
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0.2777, 0.5926, 0, 0, -0.0037, 0, 0, 0, 0,0,0, 0,0,0, #Kmastrestriktion
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0.2777, 0.5926, 0, 0, 0, -0.0037, 0, 0, 0, 0,0, 0,0,0,#Mastrrestriktion Gras
               0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, -0.01645 , 0, 0, 0,0,0,0,0,0,  #Mastr Maissilage
               0, 0, 0, 0, 0, 0, 0, 0.07, 0, -1, 0, -1, 0, 0, 0, -1, -1, -1,-1,-1, -1,-1,-1, #Spezialkulturen BFF
               0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1,1,1,1,1,1, #7% BFf von LN
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,1,1, 1,1,1,#Acker-BFF
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1,1,0,0,0, #max 50% Getreide in weiter Reihe
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,0,0,0,0,0, #Begrenzung Saum
               0, 0, -0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,1,0,0, #Begrenzung Ackerschonstreifen Bwick
               0, 0, 0, -0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,1,0, #Wraps
               0, 0, 0, 0, -0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,1 #Soja
), nrow=27, byrow=TRUE)

colnames(A) <- c("Winterweizen","Dinkel","Bohnen und Wicken","Winterraps","Soja","Körnermais","Silomais","Obstanlage","Kunstwiese","Naturwiese extensiv","Naturwiese mittel-intensiv","Hecke","Milchkuh","Mastkalb","Mastremonte","Rotationsbrache","Saum","Nützlingsstreifen","Winterweizen IWR","Dinkel IWR","Schonackerbohnen","Schon Raps","Schonsoja")
rownames(A) <- c("Fläche","Arbeit","Weidebeitrag Milchkuh","Weidebeitrag Kälbermast","Weidebeitrag Mastremonten", "Tierplätze Milchkuh", "Tierplätze Kälbermast", "Tierplätze Mastremonten","Max Weizen und Dinkel","Max Ackerbohnen","Max Soja","Max Mais","Max Obstanlage","Max Raps","Milchkuh Gras","Milchkuh Silage","Kälbermast Gras","Mastremonten Gras","Mastremonten Silage","Spezialkulturen BFF","7% BFF","Acker BFF","Begrenzung Getreide IWR","Begrenzung Saum","Max Schonstreifen Ackerbohnen","Max Schon Winterraps","Max Schon Soja")

#Richtung der RHS
directions <- c("<=", "<=",">=",">=",">=","<=","<=","<=","<=","<=","<=","<=","<=", "<=",">=",">=",">=",">=",">=","<=",">=",">=",  "<=","<=","<=","<=","<=")

#RHS
b <- c(106.17,22100,0, 0, 0,103,34,25,20.44,10.22,10.22,16.36,2.56,10.22,0,0,0,0,0,0,7.51,1.43, 0.72,0.78,0,0,0)
maximize = TRUE

#Berechnung der Lösung

LP<-LP(c =c, A = A, b = b, maximize = maximize, directions = directions)
print(LP)
gruetermitackeroptimum <- LP$objective_value
kulturengruetermitbff<-LP$solution



#Vergleich Grüter

differenzgrueter <- gruetermitackeroptimum-grueterohneackeroptimum
print(differenzgrueter)

differenzkulturengrueter <- kulturengruetermitbff[1:15] - kulturengrueterohnebff
round(differenzkulturengrueter,3)
print(kulturengruetermitbff[16:23])

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
    A2[2, 1:23] <- LHS_matrix[i, 47:69]
    A2[3, 5] <- LHS_matrix[i, 29]
    A2[3, 7] <- LHS_matrix[i, 31]
    A2[4, 8] <- LHS_matrix[i, 40]
    
    
    b2 <- LHS_matrix[i, (1:length(b)) + 644]
    
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
print(endnamen)

#Manuelles benennen der Parameter, die in der Darstellung erscheinen
endnamen[657] <- "Obstanlagenbegrenzung"
endnamen[650] <- "Milchkuhbegrenzung"
endnamen[653] <- "Max Anteile Weizen & Dinkel"
endnamen[656] <- "Max Anteil Mais"
endnamen[658] <- "Max Anteil Raps"
endnamen[655] <- "Max Anteil Bohnen"
endnamen[654] <- "Max Anteil Soja"
endnamen[651] <- "Mastremontenbegrenzung"
endnamen[665] <- "BFF-Restriktion"

colnames(param.sets1) <- endnamen


# 3. Sim1 wird auf jede Parameterkombination ausgeführt
sim.results1 <- sim1(param.sets1) 

sim_results_data_frame1 <- as.data.frame(sim.results1)

indexe[length(indexe) +1] <- "OptimalValue"

colnames(sim_results_data_frame1) <- indexe

sim.results.vector1 <- sim.results1[,ncol(sim.results1)]

summary(sim_results_data_frame1)

# Plotting to see
plot(sim.results1) # Latin Hyper Cube sieht gut verteilt aus

#Datensätze als Vektor und Data Frame
param.sets1 <- as.data.frame(param.sets1)
sim.results.vector <- as.vector(sim.results.vector1)


#Param Sets um irrelevante Parameter kürzen, weil mein Rechner sonst überfordert ist
param.sets1_subset <- param.sets1[, -c(24:644)]


# 4.1 PCC ausrechnen (THIELE)
 
pcc.result1 <- pcc(X= param.sets1_subset, y=sim.results.vector1, nboot = 1000, 
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

endnamen <- names(param.sets1)[-c(24:644)]

endnamengefiltert <- endnamen[setdiff(seq_along(endnamen), index_positionen_kleiner_006)]

#Plot erstellen

nummerierter_vektor <- seq_along(pccgefiltert)
names(nummerierter_vektor) <- endnamengefiltert

# Index der sortierten Werte erhalten
sortierte_index <- order(abs(pccgefiltert), decreasing = TRUE)

# Sortierte Werte und entsprechende Namen erhalten
sortierte_werte <- pccgefiltert[sortierte_index]
sortierte_namen <- endnamengefiltert[sortierte_index]

# Nummerierung der sortierten Werte
nummerierter_vektor <- seq_along(sortierte_werte)
names(nummerierter_vektor) <- sortierte_namen

# Hinzufügen der Acker-BFF zu den sortierten Daten basierend auf Indexen
ackerbff_indices <- c(10,16,18,19,20,21,22,23) 

# Werte aus den Indexen erhalten
ackerbff_werte <- pcc.result1$PRCC$original[ackerbff_indices]
ackerbff_namen <- c("extensive Naturwiese","Rotationsbrache","Nützlingsstreifen","Weizen in weiter Reihe","Dinkel in weiter Reihe","Schonstreifen Bohnen","Schonstreifen Raps","Schonstreifen Soja")  


# Hinzufügen der niedrigeren Werte zu den sortierten Daten
sortierte_werte <- c(sortierte_werte, ackerbff_werte)
sortierte_namen <- c(sortierte_namen, ackerbff_namen)

# Sortiere die Werte nach Größe
sortierte_werte <- sort(sortierte_werte, decreasing = TRUE)
sortierte_namen <- sortierte_namen[order(sortierte_werte, decreasing = TRUE)]

# Nummerierung der sortierten Werte
nummerierter_vektor <- seq_along(sortierte_werte)
names(nummerierter_vektor) <- sortierte_namen


# Datenrahmen erstellen
dataframe1 <- data.frame(x = nummerierter_vektor, y = sortierte_werte)

library(ggplot2)
ggplot(dataframe1, aes(x = x, y = y)) +
  geom_point() +
  xlim(sortierte_namen)+
  ylab("Schätzwerte")+
  ggtitle("PRCC")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "solid", color = "black", linewidth = 0.15)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 55, hjust = 1))


