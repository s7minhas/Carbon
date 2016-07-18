if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R")
	source("/Users/maxgallop/Documents/Carbon/R/tsDataHelpers.R")
 }

############################

############################
# Load necessary files
load(paste0(pathDataBin, 'repdata.RDA')) # includes object called data
# load(paste0(pathResults, 'latDist_wIGO.rda')) 
# latDistIGO = latDist # includes object called latDist
load(paste0(pathResults, 'latDist.rda')) # includes object called latDist
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
# with igo data 1965-2005
data = data[which(data$year %in% 1965:2012),]
############################

############################
# Merge together
# Add latent space strat interest measures
data$latDist = latDist$dist[match(data$id, latDist$dyadid)]
data$latDist = data$latDist + abs(min(data$latDist,na.rm=TRUE)) + 1

# Add ideal point strat interest measures
data$idPtDist = idPt$idealpointdistance[match(data$id, idPt$dyadidyr)]
data$sScore = sScoreData$sScore[match(data$id, sScoreData$id)]

# id
data$dyadid = paste0(data$ccode1, data$ccode2)

# Drop extraneous datasets
rm(list=c('latDist', 'idPt', 'sScoreData'))

summary(data[,c('latDist','idPtDist','sScore')])
############################

############################
# Set up models
ids = c('ccode1','ccode2','dyadid','year')
splines = c('peaceYrs','peaceYrs2','peaceYrs3')
dv = 'mid'
kivs = c(
	"latDist", 
	"idPtDist", 'sScore'
	)
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

# Finalize data for modeling
kivs = paste0('lag1_', kivs)
cntrls = paste0('lag1_', cntrls)
splines = paste0("lag1_", splines)

# remove missing data
modData = na.omit( modData[,c(ids, splines, dv, kivs, cntrls)] )

# impute missing data
# modData = modData[which(modData$year %in% 2001:2012),]

# Divide into train and test
cutYear=2004
train = modData[modData$year<cutYear,]
test = modData[modData$year>=cutYear,]
############################

############################
# Create model specifications and run
modForms = lapply(kivs, function(x){
	formula( paste0(dv,' ~ ' , 
		paste(c(x, cntrls, splines), collapse=' + '))) 
	})
mods = lapply(modForms, function(x){
	glm(x, data=train, family='binomial' ) })
names(mods) = gsub('lag1_','',kivs)
############################

############################
# Check direction/sig of coefficient
lapply(mods, function(x){ summary(x)$'coefficients'[2,,drop=FALSE] }) %>% do.call('rbind',.)
############################s

############################
# Compare out of sample performance
# Get AUCs
lapply(mods, function(x){ 
	tProb = predict(object=x, newdata=test, type='response')
	tAct = test$mid %>% as.numeric()	
	getAUC(tProb, tAct)
	}) %>% unlist() %>% sort(.,decreasing=TRUE) %>% print()

# # Roc Plot
# rocData = lapply(1:length(mods), function(ii){
# 	tProb = predict(object=mods[[ii]], newdata=test, type='response')
# 	tAct = test$mid %>% as.numeric()
# 	r = roc(tProb, tAct)
# 	p = cbind(r, model=names(mods)[ii])
# 	return(p) })
# rocData = do.call('rbind', rocData)
# rocPlot(rocData)

# area under precision-recall curves (Beger 2016 [arxiv])
lapply(mods, function(x){
	tProb = predict(object=x, newdata=test, type='response')
	tAct = test$mid %>% as.numeric()
	auc_pr(tAct, tProb)
	}) %>% unlist() %>% sort(., decreasing=TRUE) %>% print()

x = mods[[1]]
lapply(mods, function(x){
tProb = predict(object=x, newdata=test, type='response')
tAct = test$mid %>% as.numeric()
mean( tProb[tAct==1] )
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