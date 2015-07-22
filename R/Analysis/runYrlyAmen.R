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
cl = makeCluster(8)
registerDoParallel(cl)
yrs = names(amData)
foreach(yr = yrs, .packages=c("amen")) %dopar% {
	imp = 3000
	toBurn = 2501
	# Run Amen model
	fit = ameRepNull(
		Y=amData[[yr]][,,1:2], # 2, specifies defensive-entente alliance
		Xdyad = NULL, Xrow = NULL, Xcol = NULL,
		model='nrm', symmetric=TRUE, R=2,
		seed=6886, nscan=imp, burn=toBurn, odens=1,
		plot=FALSE, print = FALSE )
	# Save lat space
	latSpace = fit$ulAll[toBurn:imp]
	save(latSpace, file=paste0(pathResults, 'ameLatSpace/',yr,'_agree3un_defEntAlly.rda'))
}

# Free my clusters
stopCluster(cl)

# Parallelize run for every year
cl = makeCluster(8)
registerDoParallel(cl)
yrs = names(amData)
foreach(yr = yrs, .packages=c("amen")) %dopar% {
	imp = 3000
	toBurn = 2501
	# Run Amen model
	fit = ameRepNull(
		Y=amData[[yr]][,,c(1,3)], # 3, specifies any alliance
		Xdyad = NULL, Xrow = NULL, Xcol = NULL,
		model='nrm', symmetric=TRUE, R=2,
		seed=6886, nscan=imp, burn=toBurn, odens=1,
		plot=FALSE, print = FALSE )
	# Save lat space
	latSpace = fit$ulAll[toBurn:imp]
	save(latSpace, file=paste0(pathResults, 'ameLatSpace/',yr,'_agree3un_anyAlly.rda'))
}

# Free my clusters
stopCluster(cl)
############################