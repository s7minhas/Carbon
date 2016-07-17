if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

############################
# Load amen data
load( paste0(pathDataBin,'amenData.rda') )
############################

############################
# Run yearly amen models in parallel
for(script in list.files( paste0(gpth, 'R/Analysis/ameNull') )){ 
	paste0(gpth, 'R/Analysis/ameNull/') %>% paste0(.,script) %>% source(.) 
}

# Create directory to save latent space results
dir.create(paste0(pathResults, 'ameLatSpace/'), showWarnings=FALSE)

# Parallelize run for every year
cl = makeCluster(4)
registerDoParallel(cl)
yrs = dimnames(amenData)[[4]]
foreach(yr = yrs, .packages=c("amen")) %dopar% {
	imp = 1000
	toBurn = 501
	# Run Amen model
	fit = ame_rep(amenData[,,,yr], 
		symmetric=FALSE, R=2, model='nrm', intercept=FALSE,
		burn=toBurn, nscan=imp, odens=1, 
		seed=6886, print=FALSE, plot=FALSE )
	# Save lat space
	save(fit, file=paste0(pathResults, 'ameLatSpace/',yr,'_icews.rda'))
}

# Free my clusters
stopCluster(cl)
############################