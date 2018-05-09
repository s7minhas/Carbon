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
ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')
ggLty = c('dashed', 'dotdash', 'dotted', 'twodash', 'solid')

rocData$model = char(rocData$model)
sliceRoc = rocData[rocData$model != "Latent Angle\nDistance",]
sliceRoc$model = factor(sliceRoc$model)

sliceCols = brewer.pal(length(unique(rocPrData$model)), 'Set1')[-1]
names(sliceCols) = unique(sliceRoc$model)
tmp = rocPlot(sliceRoc, linetypes=ggLty, legPos=c(.7, .23), legText=7, legSpace=1.75, colorManual=sliceCols) + 
	annotate('text', hjust=0, x=.88, y=.43, 
		label='AUC (ROC)', family='Source Sans Pro Black', size=2.85) + 
	annotate('text', hjust=0, x=.9, y=seq(.1, .4, .09), 
		label=rev(aucSumm[-1,1]),
		family='Source Sans Pro Light', size=2.7)	
ggsave(tmp, file=paste0(pathGraphics, 'roc_outSample_notUs.pdf'), width=5, height=5, device=cairo_pdf)

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
	r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocPrData = do.call('rbind', rocPrData)
rocPrData$model = factor(rocPrData$model, levels=levels(rocData$model))

rocPrData$model = char(rocPrData$model)
slice = rocPrData[rocPrData$model != "Latent Angle\nDistance",]
slice$model = factor(slice$model)

sliceCols = brewer.pal(length(unique(rocPrData$model)), 'Set1')[-1]
tmp=rocPlot(
  slice, type='pr', linetypes=ggLty[-1], 
  legPos=c(.7,.7), legText=7, legSpace=1.75, colorManual = sliceCols) +
	annotate('text', hjust=0, x=.88, y=.95, 
		label='AUC (PR)', family='Source Sans Pro Black', size=2.85) + 
	annotate('text', hjust=0, x=.9, y=seq(.63, .93, .09), 
		label=rev(aucSumm[-1,2]), family='Source Sans Pro Light', size=2.7)
ggsave(tmp, file=paste0(pathGraphics, 'rocPr_outSampleNotUs.pdf'), width=5, height=5, device=cairo_pdf)
################################################