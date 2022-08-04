if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

if(Sys.info()["user"] %in% c('Owner','herme','S7M')){
	u = Sys.info()["user"]
	source(paste0('C:/Users/', u, '/Research/Carbon/R/setup.R')) }

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

# break up into year chunks
yrs = (min(num(names(amData)))+2):max(num(names(amData)))
yrBuck = lapply(yrs, function(t){
	return( (t-2):t ) })
names(yrBuck) = yrs

# create smaller versions of mltr
amDataBuck = lapply(yrBuck, function(t){
	amX = amData[char(t)]
	cntries = lapply(amData, rownames) %>%
		Reduce('intersect',.)
	amX = lapply(amX, function(x){
		return(x[cntries,cntries,]) })
	return( amX ) })
names(amDataBuck)

# random c val
X = array(
	rnorm(length(c(Y))),
	dim=dim(Y)
	)
####

#### MCMC
# parallelize
loadPkg(c('foreach','doParallel'))
cores = length(amDataBuck)
cl = makeCluster(cl)
registerDoParalle(cl)
foreach(t in names(amDataBuck)) %dopar% {
	# mcmc function parameters
	# Create directory to save latent space results
	outPath=paste0(pathResults, 'mltrDep_threeYr/')
	dir.create(outPath, showWarnings=FALSE)

	NS = 7500 ; NB = 500 ; sdens = 100 ; plot = FALSE
	seed = 6886 ; rstart = FALSE
	# output name
	fname=paste0("tensorStatePref_", t)
	setwd(outPath)
	# run
	source(paste0(mltrFuncs, "mcmc.r") )
}
