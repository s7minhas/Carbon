if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') 
}

#### 
# Load ALM function
source(paste0(rFuncs, "functions_als.r"))
source(paste0(rFuncs, "functions_bayes.r"))
source( paste0(rFuncs, 'tfunctions.r') )
#### 

#### 
# data
load(paste0(inPath, "train.rda"))
load(paste0(inPath, "test.rda"))
####

####
# First try standardizing all variables within country-pairs
stdzCtryPair = function(objArr){
	for(var in 1:dim(objArr)[3]){
		slice=objArr[,,var,]
		muVar = apply(slice, c(1,2), mean)
		sdVar = apply(slice, c(1,2), sd)
		sdVar[sdVar==0]=1
		for(t in 1:dim(objArr)[4]){
			objArr[,,var,t] = (objArr[,,var,t]-muVar)/sdVar
		}
	}
	return(objArr)
}

stdzTime = function(objArr){
	for(var in 1:dim(objArr)[3]){
		for(t in 1:dim(objArr)[4]){
			slice = objArr[,,var,t]
			muVarT = mean(as.vector(slice))
			sdVarT = sd(as.vector(slice))
			objArr[,,var,t] = (objArr[,,var,t]-muVarT)/sdVarT
		}
	}
	return(objArr)
}

yIn = stdzTime(yIn)
xIn = stdzTime(xIn)
yOut = stdzTime(yOut)
xOut = stdzTime(xOut)
####

####
# Variables
mods = list(
	dimnames(xIn)[[3]][1:2], # autoreg only	
	dimnames(xIn)[[3]][1:6], # net only
	dimnames(xIn)[[3]][1:10], # net + dyad
	dimnames(xIn)[[3]][c(1:10, 11, 15)], # net + dyad	+ polity
	dimnames(xIn)[[3]][c(1:10, 11, 15, 12, 16)], # net + dyad	+ polity + gdp
	dimnames(xIn)[[3]] # all	
	)
####

#### Maxim Likelihood
# Parameters for parallelization
toLoad = c('foreach', 'doParallel')
loadPkg(toLoad)
cl = makeCluster( length(mods) )
registerDoParallel(cl)

B = foreach(ii = 1:length(mods)) %dopar% { 
	return( alm.ALS(yIn, xIn[,,mods[[ii]],] ) )
}
stopCluster(cl)

# save(B, file='~/Dropbox/other/mleModelSelecBetaStdz2.rda')
save(B, file='~/Dropbox/other/mleModelSelecBetaStdz2_Add.rda')
####

#### Model selection
# Helpful functions
diagFix = function(obj){
	for(var in 1:dim(obj)[3] ){
		for(yr in 1:dim(obj)[4] ){
			diag(obj[,,var,yr]) = 0 } }
	return(obj)
}
getAggPerf = function(pred, actual){
	rsq = function(YP, Y){ 1-apply((YP-Y)^2,3,mean)/apply(Y^2,3,mean) }
	rmse = function(YP, Y){ apply((YP-Y)^2, 3, function(x){ sqrt(mean(x)) }) }	
	rmsePerf = rmse(pred, actual )
	rsqPerf = rsq(pred, actual)
	aggPerf = cbind(rmsePerf, rsqPerf)
	return(aggPerf)
}

# Calculate predicted values
toLoad = c('foreach', 'doParallel')
loadPkg(toLoad)
cl = makeCluster( length(mods) )
registerDoParallel(cl)
modPerf = foreach(ii = 1:length(mods), .packages='magrittr') %dopar% { 
	# predIn = tprod(xIn[,,mods[[ii]],], B[[ii]]) %>% diagFix(.) 
	# predOut = tprod(xOut[,,mods[[ii]],], B[[ii]]) %>% diagFix(.) 
	predIn = tsum(xIn[,,mods[[ii]],], B[[ii]]) %>% diagFix(.) 
	predOut = tsum(xOut[,,mods[[ii]],], B[[ii]]) %>% diagFix(.) 	
	aggIn = getAggPerf(predIn, yIn); colnames(aggIn) = paste0('In-',colnames(aggIn) )
	aggOut = getAggPerf(predOut, yOut); colnames(aggOut) = paste0('Out-',colnames(aggOut) )
	aggPerf = cbind(aggIn, aggOut)
	return(aggPerf)
}
stopCluster(cl)

# save(modPerf, file='~/Dropbox/other/mleModelSelecPerfStdz2.rda')
save(modPerf, file='~/Dropbox/other/mleModelSelecPerfStdz2_Add.rda')

print(modPerf)
####