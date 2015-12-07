if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
############################

############################
# Get dyadic dist from each yrly latent space
yrs=1946:2012
############################

############################
# Pull out dyadic distance measure
# Source script with helpful functions
source( paste0(gpth, 'R/Analysis/ameNull/latDistHelpers.R') )

# Files to pull
outNameMiss = '_idPt_sScore_Miss.rda'
outNameMeanRepl = '_idPt_sScore_MeanRepl.rda'
latRdas = paste0(pathResults, 'ameLatSpace') %>% list.files() %>% .[grepl('_idPt_sScore_', .)] %>% paste0(pathResults, 'ameLatSpace/', .)
unDefEntFiles = paste0(pathResults, 'ameLatSpace/', yrs, outNameMiss)
unAnyFiles = paste0(pathResults, 'ameLatSpace/', yrs, outNameMeanRepl)
setdiff( c(unDefEntFiles, unAnyFiles), latRdas ) # Check to make sure all files exist

idPtSScoreMissDist = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, outNameMiss)
	getLatDist(file, label=yr, labelName='year') }) %>% do.call('rbind', .)

idPtSScoreMeanReplDist = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, outNameMeanRepl)
	getLatDist(file, label=yr, labelName='year') }) %>% do.call('rbind', .)
############################

############################
# Merge
# Add id variables to both datasets
idPtSScoreMissDist$dyadid = paste(idPtSScoreMissDist$ccode1, idPtSScoreMissDist$ccode2, idPtSScoreMissDist$year, sep='_')
idPtSScoreMeanReplDist$dyadid = paste(idPtSScoreMeanReplDist$ccode1, idPtSScoreMeanReplDist$ccode2, idPtSScoreMeanReplDist$year, sep='_')

# Merge together
latDist_idPtSScore = idPtSScoreMissDist
names(latDist)[3] = 'idPtSScoreMissDist'
latDist$idPtSScoreMeanReplDist = idPtSScoreMeanReplDist$dist[match(latDist$dyadid, idPtSScoreMeanReplDist$dyadid)]

# Remove i=j rows
latDist = latDist[which(latDist$ccode1 != latDist$ccode2),]
############################

############################
# Save
save(latDist, file=paste0(pathResults, 'latDist_idPt_sScore.rda'))
############################