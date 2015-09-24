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
latRdas = paste0(pathResults, 'ameLatSpace') %>% list.files() %>% .[grepl('_agree3un_', .)] %>% paste0(pathResults, 'ameLatSpace/', .)
unDefEntFiles = paste0(pathResults, 'ameLatSpace/', yrs, '_agree3un_defEntAlly_IGO.rda')
unAnyFiles = paste0(pathResults, 'ameLatSpace/', yrs, '_agree3un_anyAlly_IGO.rda')
setdiff( c(unDefEntFiles, unAnyFiles), latRdas ) # Check to make sure all files exist

unDefEntDist = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, '_agree3un_defEntAlly_IGO.rda')
	getLatDist(file, label=yr, labelName='year') }) %>% do.call('rbind', .)

unAnyDist = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, '_agree3un_anyAlly_IGO.rda')
	getLatDist(file, label=yr, labelName='year') }) %>% do.call('rbind', .)
############################

############################
# Merge
# Add id variables to both datasets
unDefEntDist$dyadid = paste(unDefEntDist$ccode1, unDefEntDist$ccode2, unDefEntDist$year, sep='_')
unAnyDist$dyadid = paste(unAnyDist$ccode1, unAnyDist$ccode2, unAnyDist$year, sep='_')

# Merge together
latDist = unDefEntDist
names(latDist)[3] = 'unDefEntDist'
latDist$unAnyDist = unAnyDist$dist[match(latDist$dyadid, unAnyDist$dyadid)]

# Remove i=j rows
latDist = latDist[which(latDist$ccode1 != latDist$ccode2),]
############################

############################
# Save
save(latDist, file=paste0(pathResults, 'latDist_wIGO.rda'))
############################