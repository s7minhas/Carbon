if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
############################

############################
# Load necessary files
load(paste0(pathDataBin, 'repdata.RDA')) # includes object called data
# dedupe data
id = data.frame(id=unique(data$id), stringsAsFactors=FALSE)
for(v in names(data)[-which(names(data)=='id')]){
	id$tmp = data[,v][match(id$id,data$id)] ; names(id)[ncol(id)] = v }
data = id

# pref measures
load(paste0(pathResults, 'mltrDep.rda')) # includes object called latDist
load(paste0(pathDataBin, 'idPt.rda'))  # includes object called idPt
load(paste0(pathDataBin,'sScore.rda'))
############################

############################
# make sScore long format
sScoreData = lapply(names(sL), function(x){
	tmp=sL[[x]]; tmp$year=x
	tmp$id = paste(tmp$ccode1, tmp$ccode2, tmp$year, sep='_')
	return(tmp) }) %>% do.call('rbind', .)
############################

############################
# flip idpt
idPt$idealpointdistance = rescale(
	idPt$idealpointdistance, 
	min(idPt$idealpointdistance,na.rm=TRUE),
	max(idPt$idealpointdistance,na.rm=TRUE)	
	)
############################

############################
# with igo data 1965-2005
data = data[which(data$year %in% 1965:2012),]
############################

############################
# Merge together
# Add latent space strat interest measure
data$latAngle = latAngle$value[match(data$id, latAngle$dyadid)]

# Add ideal point strat interest measures
data$idPtDist = idPt$idealpointdistance[match(data$id, idPt$dyadidyr)]
data$sScore = sScoreData$sScore[match(data$id, sScoreData$id)]

# id
data$dyadid = paste0(data$ccode1, data$ccode2)

# Drop extraneous datasets
rm(list=c('latAngle', 'idPt', 'sScoreData'))
############################

############################
# Set up models
ids = c('ccode1','ccode2','dyadid','year')
splines = c('peaceYrs','peaceYrs2','peaceYrs3')
dv = 'mid'
kivs = c( 'latAngle', "idPtDist", 'sScore' )
cntrls = c("jointdemocB", "caprat", "noncontig", "avdyadgrowth")

# Add splines to count years since dyadic conflict (Carter & Signorino 2010)
data$dyadid = num(data$dyadid)
data$dyadidYr = paste0( data$dyadid, data$year ) %>% num()
data = data[order(data$dyadidYr),]

# functions to help calculate peace years
flipBin = function(x,a=0,b=1){ z=x ; z[x==a]=b ; z[x==b]=a ; return(z) }
getPeaceCounter = function(x){
	tmp = x %>% as.numeric() %>% flipBin()
	peaceT = tmp * ave(tmp, c(0, cumsum(diff(tmp) != 0)), FUN = seq_along)
	return(peaceT) }

# Calculate
data$peaceYrs = with(data, by(mid, dyadid, function(y) getPeaceCounter(y) ) ) %>% unlist()
data$peaceYrs2 = data$peaceYrs^2
data$peaceYrs3 = data$peaceYrs^3

# Subset data
modData = data[,c(ids, splines, dv, kivs, cntrls)]

# Create lags
modData$dyadid = num( modData$dyadid )
modData$dyadidYr = paste0( modData$dyadid, modData$year ) %>% num()
modData = lagData(modData, 'dyadidYr', 'dyadid', c(kivs, cntrls, splines))

# save
save(modData, ids, splines, kivs, dv, cntrls, 
	file=paste0(pathDataBin, 'logitModData.rda'))
############################