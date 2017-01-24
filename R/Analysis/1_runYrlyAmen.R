if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

############################
# Load amen data
load( paste0(pathDataBin,'amenData_all_rescaled.rda') )
############################

############################
# Create directory to save latent space results
dir.create(paste0(pathResults, 'ameLatSpace/'), showWarnings=FALSE)

# Subset to relev vars
inclVars = c('agree3un','totAllyCnt')
print(inclVars)
amData = lapply(amData, function(x){
	x = x[,,inclVars]
	return(x)
	})

# Parallelize run for every year
cl = makeCluster(6)
registerDoParallel(cl)
yrs = names(amData)
foreach(yr = yrs, .packages=c("amen")) %dopar% {
	imp = 100000
	toBurn = 50001
	# Run Amen model
	fit = ame_rep(amData[[yr]], 
		symmetric=TRUE, R=2, model='nrm', intercept=FALSE,
		burn=toBurn, nscan=imp, odens=1, 
		seed=6886, print=FALSE, plot=FALSE )
	# Save lat space
	save(fit, file=paste0(pathResults, 'ameLatSpace/',yr,'_rescale.rda'))
}

# Free my clusters
stopCluster(cl)
############################