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
library(tgp)

#Optimierung Betrieb 1 mit Acker-BFF----

#Zielfunktion
#Variablendeklaration
#Winterweizen = Weiz, Winterraps = Raps, Naturwiese extensiv = NWex, Naturwiese mittel-intensiv = NWmi, Kunstwiese = KW, Hecken = Heck,
#Wintergerste = Gerst, Körnermais = Kmais, Sonnenblumen = Sblum, "IWR"= in weiter Reihe, Ackerschonstreifenraps= Schonraps
#Zuckerrüben = Zrüb, 
#Nützlingsstreifen 1-jährig= Nützstr, Saum auf Ackerfläche = Saum, Rotationsbrache = Rbrach, 
#Pensionspferde = Ppferd

#maximize Z= 2591Weiz + 2433Gerst + 2095Kmais + 3806Raps + 1731NWex + 495NWmi + 1808Heck + 7170Ppferd + 2785Rbrach + 2921Saum + 2325Nützstr + 2591WeizIWR + 2433GerstIWR + 3806Schonraps

#Flächenrestriktion
#Weiz + Gerst + Kmais + Raps + NWex + NWmi + Heck + Rbrach + Saum + Nützstr + WeizIWR + GerstIWR + Schonraps <= 20.28 ha

#Arbeitsrestriktion
#40Weiz + 40Gerst + 34Kmais + 39Raps + 30NWex + 49NWmi + 55Heck + 105Ppferd + 50Rbrach + 48Saum + 35Nützstr + 40WeizIWR + 40GerstIWR + 39Schonraps <= 8980h

##Tiere

#Raus-Restriktion
#Nwmi-0.08Pferde >= 0

#Pferdeplatzrestriktion
#Ppferd <=2

#Pferdefutterrestriktion
#Nwmi+(30/90)*NWex -0.4689Ppferd>=0

#Maximale Kulturanteile bei 15.51 ha offene Ackerfläche
#Weiz+WeizIWR<=7.755 50%
#Kmais<=6.204 40%
#Raps+Schonraps<=3.8775 25%
#Rotationsbrachen<=3.8775 20%

#Acker-BFF-Restriktion
#Rbrach+Saum+Nützstr+WeizIWR+GerstIWR+Schonraps >=0.54285
#WeizIWR+GerstIWR<=0.2714 #max 50% Getreide IWR
#Schonraps<=0.48
#BFF-Restriktion
#NWex + Heck + Rbrach+Saum+Nützstr+WeizIWR+GerstIWR+Schonraps>= 1.5393 ha
#Brachen nur im Talgebiet
#Rbrach <= 7.22

#min 4 Hauptkulturen
#Kmais>=1.551
#Gerst+GerstIWR>=1.551

#Beschränkung Saum
#Saum<=0.48

#Optimierung Betrieb 1 mit Acker-BFF mit Code----

#Eingabe der Parameter

#Zielfunktionskoeffizienten
c <- c(2591,2433,2095,3806,1731,495,1808,7170,2785,2921,2325,2591,2433,3806)

#Technische Koeffizienten
A <- matrix (c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, #Flächenrestriktion 
               40, 40, 34, 39, 30, 49, 55, 105, 50, 48, 35,40,40,39, #Arbeitsrestriktion
               0, 0, 0, 0, 0, 1, 0, -0.08, 0, 0, 0,0,0,0, #Rausrestriktionen Pferd
               0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,0,0,0, #Pferdeflächenrestriktion
               0, 0, 0, 0, 0.3333, 1, 0, -0.4689, 0, 0, 0,0,0,0, #Pferdefutterrestriktionen
               1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, #Maximale Kulturenanteile Weizen
               0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #Mais
               0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,#Raps
               0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, #Acker-BFF
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, #max 50% durch Getreide
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, #Schonrapsbegrenzung
               0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, #BFF
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, #Brachen nur im Talgebiet
               0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #min 4 Hauptkulturen Mais
               0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,#Gerste
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, #maximaler Kulturanteil Rbrachen
               0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, #Beschränkung Saum
               0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 #Fixierung Wiesen
), nrow=18, byrow = TRUE)
length(A)

colnames(A) <- c("Futterweizen","Wintergerste","Körnermais","Winterraps","extensiveNaturwiese","mittelintensiveNaturwiese","Hecke","Pensionspferd","Rotationsbrache","Saum","Nützlingsstreifen","Futterweizen IWR","Wintergerste IWR","Schonstreifen Raps")
rownames(A) <- c("Fläche","Arbeit","Raus Pferd","Plätze Pferd","Futter Pferd","Max Weizen","Max Mais","Max Raps","Acker-BFF","Max Getreide IWR","Schonstreifen Raps","BFF","Brachen nur Tal","min Mais","min Gerste","max Rotationsbrachen","max Saum", "Fixierung Wiesen")

maximize=TRUE

#Richtung der RHS
directions <- c("<=", "<=", ">=", "<=",">=","<=", "<=", "<=",">=","<=","<=",">=","<=",">=",">=","<=","<=","=")

#RHS
b <- c(20.28,8980,0,2,0,7.77,6.20,3.88,0.54, 0.27, 0.48, 1.54,7.22,1.55,1.55,3.88,0.48,4.1)

#Berechnung der Lösung


LP<-LP(c =c, A = A, b = b, maximize = maximize, directions = directions)
print(LP)
anderhubmitbffoptimum <- LP$objective_value
kulturennderhubmitbff <- LP$solution


#Vergleich zwischen ohne und mit Acker-BFF

differenzanderhuboptimum <- anderhubmitbffoptimum-anderhubohneackeroptimum
print(differenzanderhuboptimum)

differenzkulturenanderhub <- kulturennderhubmitbff[1:8] - kulturenanderhubohnebff
print(round(differenzkulturenanderhub,2))
print(kulturennderhubmitbff[9:14])


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
    A2[2, 1:14] <- LHS_matrix[i, 29:42]
    A2[3, 8] <- LHS_matrix[i, 50]
    A2[5, 5] <- LHS_matrix[i, 75]
    A2[5, 8] <- LHS_matrix[i, 78]
    
    
    b2 <- LHS_matrix[i, (1:length(b)) + 252]
    
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

colnames(sim_results_data_frame1) <- indexe


sim.results.vector1 <- sim.results1[,ncol(sim.results1)]

summary(sim_results_data_frame1)

# Plot
plot(sim.results1) # Latin Hyper Cube sieht gut verteilt aus

#Datensätze als Vektor und Data Frame
param.sets1 <- as.data.frame(param.sets1)
sim.results.vector <- as.vector(sim.results.vector1)

# 4.1 PCC ausrechnen (THIELE)
pcc.result1 <- pcc(X= param.sets1, y=sim.results.vector1, nboot = 100, 
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

