if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
source(paste0(gpth, 'R/Funcs/postHelpers.R'))
############################
library(magrittr)
load(paste0(pathResults, 'latDist.rda')) # includes object called latDist


library(countrycode)

latAngle$country1 = countrycode(latAngle$Var1, "cown", "country.name")
latAngle$country2 = countrycode(latAngle$Var2, "cown", "country.name")
latAngle$dyad = paste(latAngle$Var1, latAngle$Var2)
library(doBy)

avLatAngle = summaryBy(value~dyad,data = latAngle, FUN = mean)
avLatAngleNames = summaryBy(country1 + country2~dyad, data = latAngle, FUN = unique)
avLatAngle2 = cbind(avLatAngle, avLatAngleNames[,2:3])
avLatAngle2 = avLatAngle2[order(avLatAngle2$value.mean, decreasing = F),]
avLatAngle2 = avLatAngle2[seq(1, dim(avLatAngle2)[1], 2),]
names(avLatAngle2) = c("dyad", "AngleDifference", "country1", "country2")
dim(avLatAngle2)


avLatAngle2[1:10,]
avLatAngle2[18966:18975,]
head(avLatAngle2[which(avLatAngle2$country1 == "Israel" | avLatAngle2$country2 == "Israel" ),], 20)

load(paste0(pathDataBin, 'idPt.rda'))  # includes object called idPt
load(paste0(pathDataBin,'sScore.rda'))

sScoreData = lapply(names(sL), function(x){
  tmp=sL[[x]]; tmp$year=x
  tmp$id = paste(tmp$ccode1, tmp$ccode2, tmp$year, sep='_')
  return(tmp) }) %>% do.call('rbind', .)

latAngle = merge(latAngle, sScoreData, by.x = "dyadid", by.y = "id", all.x = T)
latAngle = merge(latAngle, idPt, by.x = "dyadid", by.y = "dyadidyr" , all.x = T)
latAngle$sOld = latAngle$sScore
latAngle$sScore = 1 - latAngle$sScore
#US China
pdf(file = paste0(pathGraphics, "DyadViz/USChina.pdf"))
plotData = latAngle[which(latAngle$country1 == "United States of America" & latAngle$country2 == "China"),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-1, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(idealpointdistance~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()
#France Germany
pdf(file = paste0(pathGraphics, "DyadViz/FranceGermany.pdf"))
plotData = latAngle[which(latAngle$country1 == "France" & latAngle$Var2 == 260),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-2, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(idealpointdistance~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()

#US Russia
pdf(file = paste0(pathGraphics, "DyadViz/USRussia.pdf"))
plotData = latAngle[which(latAngle$country1 == "United States of America" & latAngle$Var2 == 365),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-2, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(idealpointdistance~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()

#US Israel
pdf(file = paste0(pathGraphics, "DyadViz/USIsrael.pdf"))
plotData = latAngle[which(latAngle$country1 == "United States of America" & latAngle$country2 == "Israel"),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-2, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(idealpointdistance~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()
#Iran Iraq
pdf(file = paste0(pathGraphics, "DyadViz/IranIraq.pdf"))

plotData = latAngle[which(latAngle$country1 == "Iraq" & latAngle$Var2 == "630"),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-2, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(sScore~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()
#North Korea China

pdf(file = paste0(pathGraphics, "DyadViz/NoKoChina.pdf"))
plotData = latAngle[which(latAngle$country1 == "China" & latAngle$Var2 == "731"),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-2, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(idealpointdistance~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()
#North Korea South Korea
pdf(file = paste0(pathGraphics, "DyadViz/NoKoSoKo.pdf"))

plotData = latAngle[which(latAngle$Var1 == "732" & latAngle$Var2 == "731"),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-2, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(sScore~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()

#US Iraq
pdf(file = paste0(pathGraphics, "DyadViz/USIraq.pdf"))

plotData = latAngle[which(latAngle$Var1 == "2" & latAngle$Var2 == "645"),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-2, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(idealpointdistance~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()

pdf(file = paste0(pathGraphics, "DyadViz/USIraq.pdf"))

plotData = latAngle[which(latAngle$country1 == "Djibouti" & latAngle$country2 == "Australia"),]
plot(value~year.x,type = "l", data = plotData, ylim = c(-2, 5), main = paste(plotData$country1[1], plotData$country2[1], sep = "-"))
lines(idealpointdistance~year.x, data = plotData, col = "blue")
lines(sScore~year.x, data = plotData, col = "red")
dev.off()