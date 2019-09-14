if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

#### 
# Load ALM function
loadPkg('abind')
source(paste0(mltrFuncs, "tfunctions.r"))	
source(paste0(mltrFuncs, "functions_als.r"))
source(paste0(mltrFuncs, "functions_bayes.r"))
#### 

#### 
# data
load( paste0(pathDataBin,'amenData_all_stdz.rda') )
inclVars = c('agree3un','totAllyCnt')

# pull out relevant variables
amData = lapply(amData, function(x){
	x = x[,,inclVars]
	return(x)
	})

# create one big array
cntries = lapply(amData, rownames) %>%
	Reduce('intersect',.)

# subset to countries in sample for full pd
amData = lapply(amData, function(x){
	return(x[cntries,cntries,]) })
Y = array(unlist(amData), 
	dim=c(
		length(cntries),length(cntries),
		length(inclVars),
		length(amData)
		)
	)
dimnames(Y) = list(
	cntries,cntries,
	inclVars, 
	names(amData)
	)

loadPkg('gdata')
for(t in 1:dim(Y)[4]){
	y = Y[,,2,t]
	lowerTriangle(y) = upperTriangle(y, byrow=TRUE)
	Y[,,2,t] = y
}

# random c val
X = array(
	rnorm(length(c(Y))), 
	dim=dim(Y)
	)
####

#### MCMC 
# mcmc function parameters
# Create directory to save latent space results
outPath=paste0(pathResults, 'mltrDep/')
dir.create(outPath, showWarnings=FALSE)

NS = 7500 ; NB = 500 ; sdens = 100 ; plot = FALSE
seed = 6886 ; rstart = FALSE
# output name
fname="tensorStatePref" 
setwd(outPath)
# run
source(paste0(mltrFuncs, "mcmc.r") )