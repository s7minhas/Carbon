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
load(paste0(pathResults, 'latDist_wIGO.rda')); latDistIGO = latDist # includes object called latDist
load(paste0(pathResults, 'latDist.rda')) # includes object called latDist
load(paste0(pathDataBin, 'idPt.rda'))  # includes object called idPt
############################

############################
# Merge together
# Add latent space strat interest measures
data$unDefEntDist = latDist$unDefEntDist[match(data$id, latDist$dyadid)]
data$unAnyDist = latDist$unAnyDist[match(data$id, latDist$dyadid)]
data$unDefEntIGODist = latDistIGO$unDefEntDist[match(data$id, latDistIGO$dyadid)]
data$unAnyIGODist = latDistIGO$unAnyDist[match(data$id, latDistIGO$dyadid)]
# Add ideal point strat interest measures
data$idPtDist = idPt$idealpointdistance[match(data$id, idPt$dyadidyr)]
data$s2un = idPt$s2un[match(data$id, idPt$dyadidyr)]
data$s3un = idPt$s3un[match(data$id, idPt$dyadidyr)]

# id
data$dyadid = paste0(data$ccode1, data$ccode2)

# Drop extraneous datasets
rm(list=c('latDist', 'idPt'))
############################

############################
# Set up models
ids = c('ccode1','ccode2','dyadid','year')
dv = 'mid'
kivs = c("unDefEntDist", "unAnyDist", 
	"unDefEntIGODist", "unAnyIGODist", # Including these limits sample to 1965-2005
	"idPtDist", "s2un", "s3un")
cntrls = c("jointdemocB", "caprat", "noncontig", "avdyadgrowth")

# Subset data
modData = data[,c(ids, dv, kivs, cntrls)]

# Create lags
modData$dyadid = num( modData$dyadid )
modData$dyadidYr = paste0( modData$dyadid, modData$year ) %>% num()
modData = lagData(modData, 'dyadidYr', 'dyadid', c(kivs, cntrls))

# Finalize data for modeling
kivs = paste0('lag1_', kivs)
cntrls = paste0('lag1_', cntrls)
modData = na.omit( modData[,c(ids, dv, kivs, cntrls)] )

# Divide into train and test
train = modData[modData$year<2001,]
test = modData[modData$year>=2001,]
############################

############################
# Create model specifications and run
modForms = lapply(kivs, function(x){
	formula( paste0(dv,' ~ ' ,paste(c(x, cntrls), collapse=' + '))) })
mods = lapply(modForms, function(x){ glm(x, data=train, family='binomial') })
names(mods) = gsub('lag1_','',kivs)
############################

############################
# Check direction/sig of coefficient
lapply(mods, function(x){ summary(x)$'coefficients'[2,,drop=FALSE] })
############################s

############################
# Compare out of sample performance
# Roc Plot
rocData = lapply(1:length(mods), function(ii){
	tProb = predict(object=mods[[ii]], newdata=test, type='response')
	tAct = test$mid %>% as.numeric()
	r = roc(tProb, tAct)
	p = cbind(r, model=names(mods)[ii])
	return(p) })
rocData = do.call('rbind', rocData)
rocPlot(rocData)

# Get AUCs
lapply(mods, function(x){ 
	tProb = predict(object=x, newdata=test, type='response')
	tAct = test$mid %>% as.numeric()	
	getAUC(tProb, tAct)
	})

# Separation plots
# loadPkg('separationplot')
# pdf(file=paste0(pathGraphics, 'quickPerfTest.pdf'))
# par(mfrow=c(2,2))
# for(ii in c(1,3:5)){
# 	separationplot(pred=eval(parse(text=paste0('mod',ii) ) )$fitted.values, actual=slice$mid, newplot=FALSE)	
# }
# dev.off()
############################