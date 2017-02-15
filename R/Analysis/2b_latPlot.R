if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){ source('~/Research/Carbon/R/setup.R') }
source(paste0(gpth, 'R/Funcs/ggCirc.R'))
############################

############################
# Get dyadic dist from each yrly latent space
yrs=c(1980, 1990, 2000, 2010)
############################

############################
# pull out data
latRdas = paste0(pathResults, 'ameLatSpace') %>%
	list.files() %>%
	.[grepl('_rescale_noABeffects.rda', .)] %>%
	paste0(pathResults, 'ameLatSpace/', .) %>%
	.[grep(paste(yrs,collapse='|'), .)]

load(latRdas[[1]])
# loadPkg('ggrepel')
# ggCirc(Y=fit$YPM, U=fit$U, showActLinks=FALSE, geomLabel=FALSE)
circplot(fit$YPM, fit$U, lty=0) # need to modify
############################

############################
# ij descriptives 

############################