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
# Add dyad id
data$did = paste(data$ccode1, data$ccode2, sep='_')

usisr = data[data$did=='2_666',]
plot(usisr$year, usisr$unAnyDist, type='l', col='blue')
lines(usisr$year, usisr$idPtDist, col='red')
############################