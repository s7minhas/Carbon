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
# add mod with idPtDist + sScore
kivs = c(kivs, 'lag1_idPtDist_sScore')
modData$lag1_idPtDist_sScore = modData$lag1_idPtDist 
modForms[[4]] = formula(paste0(dv, ' ~ ', paste(c(kivs[c(4,3)],cntrls,splines), collapse = ' + ')))
modForms[[length(modForms)+1]] = formula( paste0(dv, '~', paste(c(cntrls,splines), collapse = ' + ') ) )

# create folds
folds=100
set.seed(6886) ; modData$fold = sample(1:folds, size=nrow(modData), replace=TRUE)
############################

############################
# run model by fold
outPerf = lapply(modForms, function(x){
	outByForm=lapply(1:folds, function(k){
		trainData = modData[modData$fold!=k,]
		testData = modData[modData$fold!=k,]
		mod = glm(x, data=trainData, family='binomial' )
		outProb = predict(object=mod, newdata=testData, type='response')
		rocAUC = getAUC(outProb, testData$mid)
		prAUC = auc_pr(testData$mid, outProb)
		summ = cbind(
			kivCoef=coef(mod)[2], kivStdError=sqrt(diag(vcov(mod)))[2], 
			rocAUC=rocAUC, prAUC=prAUC )
		rocPrRes = cbind(prob=outProb, actual=testData$mid, fold=k)
		return(list(summ=summ, rocPrRes=rocPrRes))
	})
	tmp = do.call('rbind', lapply(outByForm, function(x){x$summ}))
	df = data.frame(tmp) ; df$model = rownames(tmp)[1]
	tmp2 = do.call('rbind', lapply(outByForm, function(x){x$rocPrRes}))
	rocPR = data.frame(tmp2) ; rocPR$model = rownames(tmp)[1]
	list(df=df, rocPR=rocPR)
})

# org
modSumm = do.call('rbind', lapply(outPerf, function(x){ x$df } ) )
rocPrData = do.call('rbind', lapply(outPerf, function(x){ x$rocPR } ) )

# save
save(modSumm, rocPrData, file=paste0(pathResults, 'crossValResults.rda'))
############################