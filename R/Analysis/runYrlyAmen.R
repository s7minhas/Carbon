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
	fit = ameRepNull(
		Y=amData[[yr]][,,1:2], # 2, specifies defensive-entente alliance
		Xdyad = NULL, Xrow = NULL, Xcol = NULL,
		model='nrm', symmetric=TRUE, R=2,
		seed=6886, nscan=3000, burn=2500, odens=1,
		plot=FALSE, print = FALSE
		)
	save(fit$ulAll, file=paste0(pathResults, 'ameLatSpace/',yr,'_agree3un_defEntAlly.rda'))
}

# Free my clusters
stopCluster(cl)

# Parallelize run for every year
cl = makeCluster(8)
registerDoParallel(cl)
yrs = names(amData)
foreach(yr = yrs, .packages=c("amen")) %dopar% {
	fit = ameRepNull(
		Y=amData[[yr]][,,c(1,3)], # 3, specifies any alliance
		Xdyad = NULL, Xrow = NULL, Xcol = NULL,
		model='nrm', symmetric=TRUE, R=2,
		seed=6886, nscan=3000, burn=2500, odens=1,
		plot=FALSE, print = FALSE
		)
	save(fit$ulAll, file=paste0(pathResults, 'ameLatSpace/',yr,'_agree3un_anyAlly.rda'))
}

# Free my clusters
stopCluster(cl)
############################