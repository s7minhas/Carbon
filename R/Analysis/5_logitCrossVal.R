if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
############################

############################
load( paste0(pathDataBin, 'logitModData.rda') )

# Finalize data for modeling
kivs = paste0('lag1_', kivs)
cntrls = paste0('lag1_', cntrls)
splines = paste0("lag1_", splines)	

# remove missing data
modData = na.omit( modData[,c(ids, splines, dv, kivs, cntrls)] )

# Create model specifications and run
modForms = lapply(kivs, function(x){
	formula( paste0(dv,' ~ ' , 
		paste(c(x, cntrls, splines), collapse=' + '))) })

# create folds
folds=100
set.seed(6886) ; modData$fold = sample(1:folds, size=nrow(modData), replace=TRUE)
############################

############################
# run model by fold
modResults = do.call('rbind', lapply(modForms, function(x){
	tmp = do.call('rbind', lapply(1:folds, function(k){
		trainData = modData[modData$fold!=k,]
		testData = modData[modData$fold==k,]
		mod = glm(x, data=trainData, family='binomial' )
		outProb = predict(object=mod, newdata=testData, type='response')
		outAUC = getAUC(outProb, testData$mid)
		return(cbind(kivCoef=coef(mod)[2], auc=outAUC))
	}) )
	df = data.frame(tmp) ; df$model = rownames(tmp)[1] ; return(df)
}) )

tapply(modResults$auc, modResults$model, mean)
############################