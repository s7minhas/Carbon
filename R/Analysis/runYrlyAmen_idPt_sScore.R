############################
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
for(script in list.files( paste0(gpth, 'R/Analysis/ameNull') )){ 
	paste0(gpth, 'R/Analysis/ameNull/') %>% paste0(.,script) %>% source(.) }
############################

###########################
# Load amen data
load( paste0(pathDataBin,'amenData_idPt_sScore_wMeanRepl.rda') ); outName = '_idPt_sScore_MeanRepl.rda'
# Create directory to save latent space results
dir.create(paste0(pathResults, 'ameLatSpace/'), showWarnings=FALSE)
###########################

############################
# Run yearly amen models in parallel
# Parallelize run for every year
# cl = makeCluster(6)
# registerDoParallel(cl)
yrs = names(amData)
# foreach(yr = yrs, .packages=c("amen")) %dopar% {
	yr=yrs[10]
	imp = 10
	toBurn = 2
	# Run Amen model
	fit = ameRepNull(
		Y=amData[[yr]], 
		Xdyad = NULL, Xrow = NULL, Xcol = NULL,
		model='nrm', symmetric=TRUE, R=2,
		seed=6886, nscan=imp, burn=toBurn, odens=1,
		plot=FALSE, print = FALSE )
	# Save lat space
	latSpace = fit$ulAll[toBurn:imp]
	# save(latSpace, file=paste0(pathResults, 'ameLatSpace/',yr,outName))
# }

fromFN = fit$'U'
fullSet = lapply(latSpace, function(x) x$'U')
tmp = array(unlist(fullSet), dim=c(65,2,9))
head(fullSet[[6]])
tmp[1:6,,6]
apply(tmp,c(1,2),mean) %>% head()
head(fit$'U')

# Free my clusters
# stopCluster(cl)
############################