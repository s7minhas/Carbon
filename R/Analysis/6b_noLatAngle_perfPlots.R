if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
############################

############################
# load results
load(paste0(pathResults, 'crossValResults.rda')) # adds modSumm, rocPrData

# rename
key = data.frame(dirty=unique(rocData$model),stringsAsFactors=FALSE)
key$clean = c('Ideal Point\nDistance', 'Ideal Point &\nS-Score', 'Base\nModel', 'Latent Angle\nDistance', 'S-Score')
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
ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')[-1]
ggLty = c('dotdash', 'dotted', 'twodash', 'solid')

# subset
rocData$model = char(rocData$model)
rocData = rocData[which(rocData$model!='Latent Angle\nDistance'),]
rocData$model = factor(rocData$model, levels=unique(rocData$model)[c(2,1,4,3)])
aucSumm = aucSumm[-match('Latent Angle\nDistance', rownames(aucSumm)),]

#
tmp = rocPlot(rocData, 
	linetypes=ggLty, colorManual=ggCols,
	legPos=c(.7, .23), legText=7, legSpace=1.75
	) + 
	annotate('text', hjust=0, x=.88, y=.38, 
		label='AUC (ROC)', size=2.85) + 
	annotate('text', hjust=0, x=.9, 
		y=seq(.05, .34, length.out=nrow(aucSumm)), 
		label=rev(aucSumm[,1]), size=2.7)
ggsave(tmp, file=paste0(pathGraphics, 'roc_outSample_noLatAngle.pdf'), width=5, height=5)

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
	r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocPrData = do.call('rbind', rocPrData)
rocPrData$model = factor(rocPrData$model, levels=levels(rocData$model))

# subset
rocPrData$model = char(rocPrData$model)
rocPrData = rocPrData[which(rocPrData$model!='Latent Angle\nDistance'),]
rocPrData$model = factor(rocPrData$model, levels=levels(rocData$model))

tmp=rocPlot(rocPrData, type='pr', 
	linetypes=ggLty, colorManual=ggCols,
	legPos=c(.7,.75), legText=7, legSpace=1.75) +
	annotate('text', hjust=0, x=.88, y=.95, 
		label='AUC (PR)', size=2.85) + 
	annotate('text', hjust=0, x=.9, y=seq(.63, .93, .09), 
		label=rev(aucSumm[,2]), size=2.7)
ggsave(tmp, file=paste0(pathGraphics, 'rocPr_outSample_noLatAngle.pdf'), width=5, height=5)
################################################