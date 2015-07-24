if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
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
	'unDefEntDist', 'unAnyDist',
	'idPtDist', 's2un', 's3un')]
slice = na.omit( slice )

# Run models, i know these models are unrealistic
mod1 = glm(mid~unDefEntDist, data=slice, family='binomial')
mod2 = glm(mid~unAnyDist, data=slice, family='binomial')
mod3 = glm(mid~idPtDist, data=slice, family='binomial')
mod4 = glm(mid~s2un, data=slice, family='binomial')
mod5 = glm(mid~s3un, data=slice, family='binomial')
############################

############################
# Right direction?
for(ii in 1:5){
	print( summary( eval(parse(text=paste0('mod',ii) ) ) )$'coefficients')
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