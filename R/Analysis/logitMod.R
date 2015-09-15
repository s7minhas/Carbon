if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/setup.R")
	source("/Users/maxgallop/Documents/Carbon/R/tsDataHelpers.R")
 }


############################

############################
# Load necessary files
load(paste0(pathDataBin, 'repdata.RDA')) # includes object called data
load(paste0(pathResults, 'latDist.rda')) # includes object called latDist
load(paste0(pathDataBin, 'idPt.rda'))  # includes object called idPt
############################

############################
# Merge together
# Add latent space strat interest measures
data$unDefEntDist = latDist$unDefEntDist[match(data$id, latDist$dyadid)]
data$unAnyDist = latDist$unAnyDist[match(data$id, latDist$dyadid)]
# Add ideal point strat interest measures
data$idPtDist = idPt$idealpointdistance[match(data$id, idPt$dyadidyr)]
data$s2un = idPt$s2un[match(data$id, idPt$dyadidyr)]
data$s3un = idPt$s3un[match(data$id, idPt$dyadidyr)]

data$dyadid = paste0(data$ccode1, data$ccode2)

data = lagData(data, "year","dyadid", c("unDefEntDist", "unAnyDist", "idPtDist", "s2un", "s3un"))
# Drop extraneous datasets
rm(list=c('latDist', 'idPt'))
############################

############################
# Run logit model

# Model spec?

# Something simple and quick for now
slice = data[,c(
	'ccode1', 'ccode2', 'year', 
	'mid', 
	'lag1_unDefEntDist', 'lag1_unAnyDist',
	'lag1_idPtDist', 'lag1_s2un', 'lag1_s3un')]
slice = na.omit( slice )

# Run models, i know these models are unrealistic
mod1 = glm(mid~lag1_unDefEntDist, data=slice, family='binomial')
mod2 = glm(mid~lag1_unAnyDist, data=slice, family='binomial')
mod3 = glm(mid~lag1_idPtDist, data=slice, family='binomial')
mod4 = glm(mid~lag1_s2un, data=slice, family='binomial')
mod5 = glm(mid~lag1_s3un, data=slice, family='binomial')
############################

############################
# Right direction?
for(ii in 1:5){
	eval(parse(text=paste0('mod',ii) ) )
}
############################

############################
# Model performance
loadPkg('separationplot')
pdf(file=paste0(pathGraphics, 'quickPerfTest.pdf'))
par(mfrow=c(2,2))
for(ii in c(1,3:5)){
	separationplot(pred=eval(parse(text=paste0('mod',ii) ) )$fitted.values, actual=slice$mid, newplot=FALSE)	
}
dev.off()
############################


###models with controls

data = lagData(data, "year","dyadid", c("unDefEntDist", "unAnyDist", "idPtDist", "s2un", "s3un", "jointdemocB", "caprat", "noncontig", "avdyadgrowth"))


slice = data[,c(
	'ccode1', 'ccode2', 'year', 
	'mid', 
	'lag1_unDefEntDist', 'lag1_unAnyDist',
	'lag1_idPtDist', 'lag1_s2un', 'lag1_s3un', 'lag1_jointdemocB', 'lag1_caprat', 'lag1_noncontig', 'lag1_avdyadgrowth')]

slice = na.omit(slice)
mod1c = glm(mid~lag1_unDefEntDist + lag1_jointdemocB + lag1_caprat + lag1_noncontig + lag1_avdyadgrowth, data=slice, family='binomial')
mod2c = glm(mid~lag1_unAnyDist+ lag1_jointdemocB + lag1_caprat + lag1_noncontig + lag1_avdyadgrowth, data=slice, family='binomial')
mod3c = glm(mid~lag1_idPtDist+ lag1_jointdemocB + lag1_caprat + lag1_noncontig + lag1_avdyadgrowth, data=slice, family='binomial')
mod4c = glm(mid~lag1_s2un+ lag1_jointdemocB + lag1_caprat + lag1_noncontig + lag1_avdyadgrowth, data=slice, family='binomial')
mod5c = glm(mid~lag1_s3un+ lag1_jointdemocB + lag1_caprat + lag1_noncontig + lag1_avdyadgrowth, data=slice, family='binomial')
