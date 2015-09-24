# Roc curve, depends ROCR
roc = function(prediction, actual){
      pred = prediction(prediction, actual)
      perf = performance(pred,"tpr","fpr")
      rocData = data.frame(attributes(perf)$x.values[[1]], attributes(perf)$y.values[[1]])
      names(rocData) = c('FPR', 'TPR')
      return(rocData)
}

# Auc, depends ROCR
getAUC = function(prediction, actual){
	pred = prediction(prediction, actual)	
	attributes(performance(pred,"auc"))$y.values[[1]]
}

# Plot roc curves, depends RColorBrewer
rocPlot = function(rocData, colorPal = 'Set1'){
	tmp=ggplot(rocData, aes(x=FPR, y=TPR, color=model)) + geom_line()
	tmp=tmp + geom_abline(intercept=0, slope=1, color='darkgrey')
	tmp=tmp + ylab('True Positive Rate (Sensitivity)') + xlab('False Positive Rate (1-Specificity)')
	# tmp=tmp + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
	# tmp=tmp + scale_colour_manual( values=c( rgb(222, 45, 38, maxColorValue=255), rgb(54, 144, 192, maxColorValue=255) ) )
	tmp=tmp + scale_color_brewer(palette=colorPal)
	tmp=tmp + theme(
		legend.position='top', legend.title=element_blank(),
		panel.grid=element_blank(),
		axis.ticks=element_blank()
		)
	return(tmp)
}