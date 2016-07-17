if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
############################

############################
# Get dyadic dist from each yrly latent space
yrs=1965:2005
############################

############################
# Pull out dyadic distance measure
# Source script with helpful functions
source( paste0(gpth, 'R/Analysis/ameNull/latDistHelpers.R') )

# Files to pull
latRdas = paste0(pathResults, 'ameLatSpace') %>% list.files() %>% .[grepl('_rescale.rda', .)] %>% paste0(pathResults, 'ameLatSpace/', .)

latDist = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, '_rescale.rda')
	getLatDist(file, label=yr, labelName='year') 
	}) %>% do.call('rbind', .)
############################

############################
# Merge
# Add id variables to both datasets
latDist$dyadid = paste(latDist$ccode1, latDist$ccode2, latDist$year, sep='_')

# Remove i=j rows
latDist = latDist[which(latDist$ccode1 != latDist$ccode2),]
############################

############################
# Save
save(latDist, file=paste0(pathResults, 'latDist.rda'))
############################