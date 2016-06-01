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
	fit = ameRepNull(
		Y=amenData[,,,yr], 
		Xdyad = NULL, Xrow = NULL, Xcol = NULL,
		model='nrm', symmetric=FALSE, R=2,
		seed=6886, nscan=imp, burn=toBurn, odens=1,
		plot=FALSE, print = FALSE, intercept=FALSE )
	# Save lat space
	save(fit, file=paste0(pathResults, 'ameLatSpace/',yr,'_icews.rda'))
}

# Free my clusters
stopCluster(cl)
############################