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

latSpaces = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, outNameMeanRepl)
	label=yr
	labelName='year'
	symmetric=TRUE
	load(file) # Loads object called latSpace
	pzMu=getPosInSpace(out[['ULUPM']])
	return(pzMu)
	})

latSpaceSumm = lapply(1:length(latSpaces), function(ii){ 
	x = latSpaces[[ii]]
	summ = apply(x, 2, function(x){ c(min(x), mean(x), max(x)) })
	colnames(summ) = c('d1', 'd2')
	summD = data.frame(summ)
	summD$var = c('min', 'mean', 'max')
	summD = melt(summD, id='var')
	summD$year = yrs[ii]
	return(summD)
	}) %>% do.call('rbind', .)

lsDims = ggplot(latSpaceSumm, aes(x=year, y=value, color=variable)) + geom_point() + geom_line() + facet_wrap(variable~var, ncol=3, scales='free') 
lsDims

rownames(latSpaces[[1]])

latSpaceData = lapply(1:length(latSpaces), function(ii){
	x = latSpaces[[ii]]
	colnames(x) = c('d1', 'd2')
	xD = data.frame(x, row.names=NULL)
	xD$cntry = rownames(x)
	# xD = melt(xD, id='cntry')
	xD$year = yrs[ii]
	return(xD)
	}) %>% do.call('rbind', .)

lsPlot = ggplot(latSpaceData, aes(x=d1, y=d2, label=cntry, color=factor(cntry))) + geom_text() + facet_wrap(~year, ncol=3, scales='free') + theme(legend.position='none')
ggsave(lsPlot, file='~/Desktop/lsPlot.pdf', height=100, width=100, limitsize=FALSE)