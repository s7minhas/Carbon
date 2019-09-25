###Rev 2 Stuff

if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
  source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
  source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
source(paste0(gpth, 'R/Funcs/postHelpers.R'))

mltr = read.csv(paste0(pathResults, 'mltrDep.csv'))
load("/Users/maxgallop/Dropbox/Carbon/Data/Binaries/ally.rda")
totAlly$dyadid = paste(totAlly$ccode1, totAlly$ccode2, totAlly$year, sep = "_")
mltr$ally = totAlly$totCnt[match(mltr$dyadid, totAlly$dyadid)]
mltr$ally[is.na(mltr$ally)] = 0
load("/Users/maxgallop/Dropbox/Carbon/Data/Binaries/un.rda")
un$dyadidyr2 = paste(un$ccode2, un$ccode1, un$year, sep = "_")
mltr$agree3un[mltr$Var1 > mltr$Var2] = un$agree3un[match(mltr$dyadid[mltr$Var1 > mltr$Var2], un$dyadidyr2)] 
mltr$agree3un[mltr$Var1 < mltr$Var2] = un$agree3un[match(mltr$dyadid[mltr$Var1 < mltr$Var2], un$dyadidyr)] 
mltr$dyad = paste(mltr$Var1, mltr$Var2, sep = "_")

lagger<-function(variable, country, year, laglength){
  
  country<-as.character(country)
  laggedvar<-rep(NA,length(variable))
  
  leadingNAs<-rep(NA,laglength)
  countryshift<-c(leadingNAs, country[1:(length(country)-laglength)])
  
  variableshift<-c(leadingNAs, variable[1:(length(variable)-laglength)])
  
  replacementrefs<-country==countryshift
  replacementrefs[is.na(replacementrefs)==T]<-FALSE
  laggedvar[replacementrefs]<-variableshift[replacementrefs]
  
  laggedvar
  
} # close lagger function

mltr = mltr[order(mltr$dyad, mltr$year),]
mltr$value.l1 = lagger(mltr$value, mltr$dyad, mltr$year, 1)
mltr$ally.l1  = lagger(mltr$ally, mltr$dyad, mltr$year, 1)
mltr$agree3un.l1  = lagger(mltr$agree3un, mltr$dyad, mltr$year, 1)
mltr$allyDif = mltr$ally - mltr$ally.l1
mltr$unDif = mltr$agree3un - mltr$agree3un.l1
which(mltr$ally == 2 & mltr$ally.l1 == 0)

mltr[96813,]
mltr$country1 = countrycode(mltr$Var1, "cown", "country.name")
mltr$country2 = countrycode(mltr$Var2, "cown", "country.name")
mltr$tensDif = mltr$value - mltr$value.l1

dumb = lm(value~value.l1 + allyDif + unDif, data = mltr)
dumbchange = lm(tensDif~allyDif + unDif, data = mltr)
which(mltr$unDif < -.5)


load("/Users/maxgallop/Dropbox/Carbon/Results/tnsrDepSpace/1981_mltr.rda")
fit1981 = fit
load("/Users/maxgallop/Dropbox/Carbon/Results/mltrDep/1980_mltr.rda")
fit1980 = fit
