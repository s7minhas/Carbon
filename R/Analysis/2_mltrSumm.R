if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
############################

############################
# Get dyadic dist from each yrly latent space
yrs=1965:2012
############################

############################
# Pull out dyadic distance measure
# Files to pull
# retaining latAngle name for coding simplicity
latAngle = lapply(yrs, function(yr){
	load(paste0(pathResults, 'tnsrDepSpace/',yr,'_mltr.rda'))
	return(b1) }) %>%
	do.call('rbind',.)
############################

############################
# Add id variables to both datasets
latAngle$dyadid = with(latAngle, paste(Var1, Var2, year, sep='_'))

# Remove i=j rows
latAngle = with( latAngle, latAngle[which(Var1 != Var2),] )
############################

############################
# Save
save(latAngle, file=paste0(pathResults, 'mltrDep.rda'))
############################