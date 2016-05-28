if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
############################

############################
# Get dyadic dist from each yrly latent space
yrs=1946:2011
############################

############################
# Pull out dyadic distance measure
# Source script with helpful functions
source( paste0(gpth, 'R/Analysis/ameNull/latDistHelpers.R') )

# Files to pull
outNameMeanRepl = '_idPt_sScore_MeanRepl.rda'
latRdas = paste0(pathResults, 'ameLatSpace') %>% list.files() %>% .[grepl('_idPt_sScore_', .)] %>% paste0(pathResults, 'ameLatSpace/', .)
tmp = paste0(pathResults, 'ameLatSpace/', yrs, outNameMeanRepl)
setdiff( tmp, latRdas ) # Check to make sure all files exist

idPtSScoreMeanReplDist = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, outNameMeanRepl)
	load(file) # Loads object called latSpace
	pzMu=getPosInSpace(out[['ULUPM']])
	u=getLatDist(file, label=yr, labelName='year') 
	}) %>% do.call('rbind', .)
############################

############################
# Merge
# Add id variable
idPtSScoreMeanReplDist$dyadid = paste(idPtSScoreMeanReplDist$ccode1, idPtSScoreMeanReplDist$ccode2, idPtSScoreMeanReplDist$year, sep='_')
idPtSScoreMeanReplDist$id1Yr = paste(idPtSScoreMeanReplDist$ccode1, idPtSScoreMeanReplDist$year, sep='_')
idPtSScoreMeanReplDist$id2Yr = paste(idPtSScoreMeanReplDist$ccode2, idPtSScoreMeanReplDist$year, sep='_')

# rename
latDist = idPtSScoreMeanReplDist
names(latDist)[3] = 'idPtSScoreMeanReplDist'

# Remove i=j rows
latDist = latDist[which(latDist$ccode1 != latDist$ccode2),]
############################

############################
# Add in random effects
nodalEffect = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, outNameMeanRepl)
	load(file) # Loads object called latSpace
	nEff = data.frame ( apm = out$'APM', row.names=NULL ) 
	nEff$ccode = names( out$'APM' )
	nEff$year = yr
	nEff$id = paste(nEff$ccode, nEff$year, sep='_')
	return(nEff)
	}) %>% do.call('rbind', .)

# Merge
latDist$apm1 = nodalEffect$apm[match(latDist$id1Yr, nodalEffect$id)]
latDist$apm2 = nodalEffect$apm[match(latDist$id2Yr, nodalEffect$id)]
############################

############################
# Save
save(latDist, file=paste0(pathResults, 'latDist_idPt_sScore.rda'))
############################