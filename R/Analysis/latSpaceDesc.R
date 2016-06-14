if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

if(Sys.info()["user"]=="maxgallop"){
	source('~/Documents/Carbon/R/setup.R') }

############################

############################
# Get dyadic dist from each yrly latent space
yrs=2001:2011
############################

############################
# Pull out dyadic distance measure
# Source script with helpful functions
source( paste0(gpth, 'R/Analysis/ameNull/latDistHelpers.R') )

# Files to pull
outNameMeanRepl = '_icews.rda'
latRdas = paste0(pathResults, 'ameLatSpace') %>% list.files() %>% .[grepl('icews', .)] %>% paste0(pathResults, 'ameLatSpace/', .)
tmp = paste0(pathResults, 'ameLatSpace/', yrs, outNameMeanRepl)
setdiff( tmp, latRdas ) # Check to make sure all files exist

latSpaces = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, outNameMeanRepl)
	label=yr
	labelName='year'
	symmetric=TRUE
	load(file) # Loads object called latSpace
	pzMu=getPosInSpace2(fit[['UVPM']])
	return(pzMu)
	})

latSpaceSumm1 = lapply(1:length(latSpaces), function(ii){ 
	x = latSpaces[[ii]][[1]]
	summ = apply(x, 2, function(x){ c(min(x), mean(x), max(x)) })
	colnames(summ) = c('d1', 'd2')
	summD = data.frame(summ)
	summD$var = c('min', 'mean', 'max')
	summD = melt(summD, id='var')
	summD$year = yrs[ii]
	return(summD)
	}) %>% do.call('rbind', .)

latSpaceSumm2 = lapply(1:length(latSpaces), function(ii){ 
	x = latSpaces[[ii]][[2]]
	summ = apply(x, 2, function(x){ c(min(x), mean(x), max(x)) })
	colnames(summ) = c('d1', 'd2')
	summD = data.frame(summ)
	summD$var = c('min', 'mean', 'max')
	summD = melt(summD, id='var')
	summD$year = yrs[ii]
	return(summD)
	}) %>% do.call('rbind', .)

lsDims = ggplot(latSpaceSumm1, aes(x=year, y=value, color=variable)) + geom_point() + geom_line() + facet_wrap(variable~var, ncol=3, scales='free') 
lsDims

rownames(latSpaces[[1]][[1]])

latSpaceData = lapply(1:length(latSpaces), function(ii){
	x = cbind(latSpaces[[ii]][[1]], latSpaces[[ii]][[2]])
	colnames(x) = c('d1U', 'd2U', 'd1V', 'd2V')
	xD = data.frame(x, row.names=NULL)
	xD$cntry = rownames(x)
	# xD = melt(xD, id='cntry')
	xD$year = yrs[ii]
	return(xD)
	}) %>% do.call('rbind', .)

lsPlot = ggplot(latSpaceData, aes(x=d1U, y=d2U, label=cntry, color=factor(cntry))) + geom_text() + facet_wrap(~year, ncol=3, scales='free') + theme(legend.position='none')
ggsave(lsPlot, file='~/Desktop/lsPlot.pdf', height=100, width=100, limitsize=FALSE)