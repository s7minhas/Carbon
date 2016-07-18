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
Y = yIn
X = xIn
####

#### Maxim Likelihood
X = array(rnorm(length(Y)), dim=dim(Y))
B = mlm.ALS(Y[,,,1], X[,,,1])
save(B, file=paste0('mlikTradeConf_v2.rda'))
####

#### MCMC 
# mcmc function parameters
NS = 2500 ; NB = 500 ; sdens = 100 ; plot = FALSE
seed = 6886 ; rstart = FALSE
# output name
fname="tensorTradeConf_v2" 
setwd(outPath)
# run
source(paste0(rFuncs, "mcmc.r") )