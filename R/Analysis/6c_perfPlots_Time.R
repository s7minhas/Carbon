if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

	if(Sys.info()["user"]=="Owner"){
		source('C:/Users/Owner/Research/Carbon/R/setup.R') }

if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
############################

############################
# load results
load(paste0(pathResults, 'crossValResults_time.rda')) # adds modSumm, rocPrData

# rename
key = data.frame(dirty=unique(rocData$model),stringsAsFactors=FALSE)
key$clean = c('Ideal Point\nSimilarity', 'Ideal Point &\nS-Score', 'Base\nModel', 'Tensor\nDependence', 'S-Score')
key$clean = factor(key$clean,levels=key$clean[c(4,2,1,5,3)])

# cleanup names
names(predDfs) = key$clean[match(names(predDfs), key$dirty)]
rownames(aucSumm) = key$clean[match(rownames(aucSumm), key$dirty)]
rocData$model = key$clean[match(rocData$model, key$dirty)]
prData$model = key$clean[match(prData$model, key$dirty)]
############################

############################
# plotting

# model col/lty
ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')
ggLty = c('dashed', 'dotdash', 'dotted', 'twodash', 'solid')

tmp = rocPlot(rocData, linetypes=ggLty, legPos=c(.7, .23), legText=7, legSpace=1.75) +
	annotate('text', hjust=0, x=.88, y=.43,
		label='AUC (ROC)', family='Source Sans Pro Black', size=2.85) +
	annotate('text', hjust=0, x=.9, y=seq(.01, .4, .09),
		label=rev(aucSumm[,1]),
		family='Source Sans Pro Light', size=2.7)
ggsave(tmp, file=paste0(pathGraphics, 'roc_outSample_time.pdf'), width=5, height=5, device=cairo_pdf)

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
	r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocPrData = do.call('rbind', rocPrData)
rocPrData$model = factor(rocPrData$model, levels=levels(rocData$model))

tmp=rocPlot(rocPrData, type='pr', linetypes=ggLty, legPos=c(.7,.7), legText=7, legSpace=1.75) +
	annotate('text', hjust=0, x=.88, y=.95,
		label='AUC (PR)', family='Source Sans Pro Black', size=2.85) +
	annotate('text', hjust=0, x=.9, y=seq(.54, .93, .09),
		label=rev(aucSumm[,2]), family='Source Sans Pro Light', size=2.7)
ggsave(tmp, file=paste0(pathGraphics, 'rocPr_outSample_time.pdf'), width=5, height=5, device=cairo_pdf)
################################################
