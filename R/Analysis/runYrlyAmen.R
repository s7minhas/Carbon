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

test = ameRepNull(
	Y=amData[[1]][,,1:2], 
	Xdyad = NULL, Xrow = NULL, Xcol = NULL,
	model='nrm', symmetric=TRUE, R=2,
	seed=6886, nscan=10, burn=1, odens=1
	)
############################