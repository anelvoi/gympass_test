#############################  
## Autora: Gabriela Pereira Anelvoi
## Data: janeiro/ 2019
#############################

if(!require("xlsx")) install.packages("xlsx")
if(!require("sqldf")) install.packages("sqldf")
if(!require("chron")) install.packages("chron")
if(!require("datetime")) install.packages("datetime")
if(!require("lubridate")) install.packages("lubridate")

#importa dados do arquivo de log
logData <- as.data.frame((read.delim2(file.choose(), header = TRUE, sep = "", dec = ",")),stringsAsFactors = FALSE)

#Define nome das colunas
names(logData) <- c("Hora", "Codigo", "","Piloto", "Volta", "Tempo", "Velocidade")
row.names(logData) <- NULL

#Exclui colunas nulas
logData <- logData[,-3]
logData <- logData[,-7]
logData <- logData[,-7]
logData <- logData[,-7]

#Ordena tabela de acordo com codigo do piloto
sortedLogData <- logData[order(logData$Codigo),]

#Armazena codigos dos pilotos
Codigos <- levels(factor(sortedLogData$Codigo))

#Armazena numero de voltas da corrida
VOltas <- levels(droplevels(factor(sortedLogData$Volta)))
op <- options(digits.secs=3)

#Trata formato dos tempos
sortedLogData$Tempo <- strftime((strptime(sortedLogData$Tempo, "%M:%OS")),
                                format="%M:%OS")


#Acha resultado final da corrida
final <- sqldf("SELECT Codigo, Piloto, 
               Volta FROM sortedLogData GROUP BY Codigo ORDER BY Hora ASC")

#Calculo do tempo total de cada piloto
lTempoTotal <- list()

for(l in c(1:length(Codigos))) {
  times <- sqldf(sprintf("SELECT Tempo FROM sortedLogData WHERE Codigo = '%s'",Codigos[[l]][[1]]))
  
  #print(times)
  totalMin <- 0
  totalSec <- 0
  total <- 0
  tempoTotal <- 0
  
  for(m in c(1:length(times[[1]]))) {  
    totalMin <- totalMin + minute(strptime(times[[1]][[m]],format="%M:%OS"))
    totalSec <- totalSec + second(strptime(times[[1]][[m]],format="%M:%OS"))
  }
  
  total <- ((totalMin*60) + totalSec)
  #print(total)
  tempoTotal <- strftime(strptime('00:00.000',format="%M:%OS")+total,format="%M:%OS3")
  lTempoTotal <- c(lTempoTotal,tempoTotal)
  #print(tempoTotal)
}
#print(lTempoTotal)

final$TempoTotal <- NA
for (i in c(1:length(Codigos))) {
  for (j in c(1:length(Codigos))) {
    if(final$Codigo[[i]] == Codigos[[j]]) {
      final$TempoTotal[[i]] <- lTempoTotal[[j]]
    }
  }
}

print("Resultado da corrida:")
print(final)

#Bonus

#Descobrir a melhor volta de cada piloto
melhorVolta <- data.frame()

for(l in c(1:length(Codigos))) {
  v <- sqldf(sprintf("SELECT Codigo, Piloto, Volta, Tempo FROM sortedLogData WHERE Codigo = '%s' ORDER BY Tempo ASC LIMIT 1",Codigos[[l]][[1]]))
  melhorVolta <- rbind(melhorVolta,v)
}

print("Melhor volta de cada piloto:")
print(melhorVolta)

#Descobrir a melhor volta da corrida

melhorVoltaCorrida<- sqldf("SELECT Codigo, Piloto, Volta,Tempo FROM sortedLogData ORDER BY Tempo ASC LIMIT 1")
print("Melhor volta da corrida:")
print(melhorVoltaCorrida)

#Calcular a velocidade média de cada piloto durante toda corrida

vel <- sqldf("SELECT Codigo, Piloto, Volta,(SUM(Velocidade)/COUNT(Codigo)) AS 'Velocidade Media' FROM sortedLogData GROUP BY Codigo ORDER BY Hora ASC")
print("Velocidade média de cada piloto durante toda corrida:")
print(vel)


#Descobrir quanto tempo cada piloto chegou após o vencedor

horaVencedor <- sqldf("SELECT Hora FROM sortedLogData GROUP BY Codigo ORDER BY Hora ASC LIMIT 1")
horaVencedor <- levels(droplevels(horaVencedor$Hora[1]))
h <- hour(strptime(horaVencedor[1],format="%H:%M:%OS"))
m <- minute(strptime(horaVencedor[1],format="%H:%M:%OS"))
s <- second(strptime(horaVencedor[1],format="%H:%M:%OS"))
totalSeconds <- ((h*60*60) + (m*60) + s)

finalHoras <- sqldf("SELECT Codigo, Piloto, Volta, Hora FROM sortedLogData GROUP BY Codigo ORDER BY Hora ASC")
finalHoras$DiffTempo <- NA

for (t in c(1:length(finalHoras$Codigo))){
  hora <- levels(droplevels(finalHoras$Hora[t]))
  tempoTotal <- strftime(strptime(hora,format="%H:%M:%OS") - totalSeconds,format="%H:%M:%OS3")
  finalHoras$DiffTempo[t] <- tempoTotal
}

print("Tempo que cada piloto chegou após o vencedor:")
print(finalHoras[-1,]) #Mostra todos os pilotos menos o vencedor