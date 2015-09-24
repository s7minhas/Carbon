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

# ############################
# # Run basic logit model

# # lag
# data = lagData(data, "year","dyadid", c("unDefEntDist", "unAnyDist", "idPtDist", "s2un", "s3un"))

# # Something simple and quick for now
# slice = data[,c(
# 	'ccode1', 'ccode2', 'year', 
# 	'mid', 
# 	'lag1_unDefEntDist', 'lag1_unAnyDist',
# 	'lag1_idPtDist', 'lag1_s2un', 'lag1_s3un')]
# slice = na.omit( slice )

# # Run models, i know these models are unrealistic
# mod1 = glm(mid~lag1_unDefEntDist, data=slice, family='binomial')
# mod2 = glm(mid~lag1_unAnyDist, data=slice, family='binomial')
# mod3 = glm(mid~lag1_idPtDist, data=slice, family='binomial')
# mod4 = glm(mid~lag1_s2un, data=slice, family='binomial')
# mod5 = glm(mid~lag1_s3un, data=slice, family='binomial')
# ############################

# ############################
# # Right direction?
# for(ii in 1:5){
# 	eval(parse(text=paste0('mod',ii) ) )
# }
# ############################

# ############################
# # Model performance
# loadPkg('separationplot')
# pdf(file=paste0(pathGraphics, 'quickPerfTest.pdf'))
# par(mfrow=c(2,2))
# for(ii in c(1,3:5)){
# 	separationplot(pred=eval(parse(text=paste0('mod',ii) ) )$fitted.values, actual=slice$mid, newplot=FALSE)	
# }
# dev.off()
# ############################

###models with controls
ids = c('ccode1','ccode2','dyadid','year')
dv = 'mid'
kivs = c("unDefEntDist", "unAnyDist", 
	"unDefEntIGODist", "unAnyIGODist", # Including these limits sample to 1965-2005
	"idPtDist", "s2un", "s3un")
cntrls = c("jointdemocB", "caprat", "noncontig", "avdyadgrowth")

# Subset data
modData = data[,c(ids, dv, kivs, cntrls)]

# Create lags
modData = lagData(modData, 'year', 'dyadid', c(kivs, cntrls))

# Finalize data for modeling
kivs = paste0('lag1_', kivs)
cntrls = paste0('lag1_', cntrls)
modData = na.omit( modData[,c(ids, dv, kivs, cntrls)] )

# Create model specifications and run
modForms = lapply(kivs, function(x){
	formula( paste0(dv,' ~ ' ,paste(c(x, cntrls), collapse=' + '))) })
mods = lapply(modForms, function(x){ glm(x, data=modData, family='binomial') })
names(mods) = gsub('lag1_','',kivs)

# Compare performance
# Roc Plot
rocData = lapply(1:length(mods), function(ii){
	r = roc(mods[[ii]]$fitted.values,mods[[ii]]$y)
	p = cbind(r, model=names(mods)[ii])
	return(p) })
rocData = do.call('rbind', rocData)
rocPlot(rocData)

# Get AUCs
lapply(mods, function(x){ getAUC(x$fitted.values, x$y) })