if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
############################

############################
# org results
load(paste0(pathResults, 'crossValResults.rda')) # adds modSumm, rocPrData
predDfs = split(rocPrData,rocPrData$model)

# tabular data
aucSumm=do.call('rbind', lapply(predDfs,function(x){
	aucROC=getAUC(x$prob,x$actual) ; aucPR=auc_pr(x$actual,x$prob)
	return( c('AUC'=aucROC,'AUC (PR)'=aucPR) ) }) )
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))

# roc data
rocData=do.call('rbind', 
	lapply(predDfs, function(x){
		y=roc(x$prob,x$actual);y$model=unique(x$model);return(y) }))

# precision recall curve data
prData=do.call('rbind', 
	lapply(predDfs, function(x){
		y=rocdf(x$prob,x$actual,type='pr');y$model=unique(x$model);return(y) }))

# rename
key = data.frame(dirty=unique(rocData$model),stringsAsFactors=FALSE)
key$clean = c('Ideal Point', 'NULL', 'Latent Angle', 'S-Score')
key$clean = factor(key$clean,levels=c('NULL', 'S-Score', 'Ideal Point', 'Latent Angle'))

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
ggLty = c('dashed', 'dotted', 'twodash', 'solid')

# Separation plots
loadPkg('png')
sepPngList = lapply(1:length(predDfs), function(ii){
	fSepPath = paste0(pathGraphics,'sep_',names(predDfs)[ii],'_outSample.png')
	# save as pngs for potential use outside of roc
	tmp = data.frame(act=predDfs[[ii]]$actual, proba=predDfs[[ii]]$'prob')
	tmp = unique(round(tmp,4))
	ggSep(actual=tmp$act, proba=tmp$proba, 
		color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
	sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
	return(sepG)
})

tmp = rocPlot(rocData, linetypes=ggLty)+guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
for(ii in 1:length(sepPngList)){
	tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
	yLo = yLo + .1 ; yHi = yHi + .1 }
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.35,.1), label=names(predDfs), family="Source Sans Pro Light")
ggsave(tmp, file=paste0(pathGraphics, 'roc_outSample.pdf'), width=5, height=5, device=cairo_pdf)

# area under precision-recall curves
rocPrData = lapply(1:length(predDfs), function(ii){
	r = rocdf(predDfs[[ii]]$'prob', predDfs[[ii]]$'actual', type='pr')
	p = cbind(r, model=names(predDfs)[ii])
	return(p) })
rocPrData = do.call('rbind', rocPrData)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty) +
	guides(linetype=FALSE, color=FALSE) + 
	# geom_rect(xmin=.05, ymin=.01, xmax=.58, ymax=.55, color='white', fill='white', size=.5) + 
	annotate('text', hjust=0, x=c(.01, .29, .47), y=.45, 
		label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), family='Source Sans Pro Black', size=4) + 
	annotate('text', hjust=0, x=.01, y=seq(.05, .35, .1), 
		label=rev(apply(cbind(rownames(aucSumm), aucSumm), 1, function(x){paste(x, collapse='     ')})),
		family='Source Sans Pro Light')
ggsave(tmp, file=paste0(pathGraphics, 'rocPr_outSample.pdf'), width=5, height=5, device=cairo_pdf)
################################################