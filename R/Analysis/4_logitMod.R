if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
source(paste0(gpth, 'R/Funcs/postHelpers.R'))
############################

############################
load( paste0(pathDataBin, 'logitModData.rda') )

# Finalize data for modeling
kivs = paste0('lag1_', kivs)
cntrls = paste0('lag1_', cntrls)
splines = paste0("lag1_", splines)

# remove missing data
modData = na.omit( modData[,c(ids, splines, dv, kivs, cntrls)] )

# Create model specifications and run
modForms = lapply(kivs, function(x){
	formula( paste0(dv,' ~ ' , 
		paste(c(x, cntrls, splines), collapse=' + '))) })

# add mod with idPtDist + sScore
kivs = c(kivs, 'lag1_idPtDist_sScore')
modData$lag1_idPtDist_sScore = modData$lag1_idPtDist 
modForms[[4]] = formula(paste0(dv, ' ~ ', paste(c(kivs[c(4,3)],cntrls,splines), collapse = ' + ')))

# Run model
sdz = function(x){ (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)  }
mods = lapply(modForms, function(x){
	# modData[,c(splines,kivs,cntrls)] = apply(modData[,c(splines,kivs,cntrls)],2,sdz)
	glm(x, data=modData, family='binomial' ) })
names(mods) = gsub('lag1_','',kivs)

# save results
coefData = lapply(mods, function(x){
	x = summary(x)$'coefficients' %>% data.frame(.)
	x$mod = rownames(x)[2] ; x$var = rownames(x)
	rownames(x) = NULL ; names(x)[1:2] = c('mean','sd')
	x = getCIVecs(x) %>% getSigVec(.)
	return(x)
}) %>% do.call('rbind',.)

# clean up vars
modKey = data.frame(dirty=unique(coefData$mod))
modKey$clean = c('Latent Angle\nDistance', 'Ideal Point\nDistance', 'S-Score', 'Ideal Point &\nS-Score')
coefData$modClean = modKey$clean[match(coefData$mod,modKey$dirty)]
coefData$modClean = factor(coefData$modClean, levels=modKey$clean)
varKey = data.frame(dirty=unique(coefData$var))
varKey$clean = c(
	'(Intercept)', 
	'Latent Angle\nDistance$_{ij,t-1}$',
	'Joint Democracy$_{ij,t-1}$',
	'Capability Ratio$_{ij,t-1}$',
	'Geographically\nContiguous$_{ij,t-1}$',
	'Avg Dyad\nGDP Growth$_{ij,t-1}$',
	'Peace Years$_{ij,t-1}$',
	'Peace Years$^{2}_{ij,t-1}$',
	'Peace Years$^{3}_{ij,t-1}$',
	'Ideal Point\nDistance$_{ij,t-1}$',
	'S-Score$_{ij,t-1}$',
	'Ideal Point\nDistance$_{ij,t-1}$'
	)
varKey = varKey[c(2,10,12,11,3:6,7:9,1),]
coefData$varClean = varKey$clean[match(coefData$var,varKey$dirty)]
coefData$varClean = factor(coefData$varClean, levels=rev(varKey$clean[-2]))
coefData = coefData[which(!coefData$varClean %in% varKey$clean[8:12]),]

# plot
posDodge = .5
ggCoef=ggplot(coefData, aes(x=varClean, y=mean, color=sig, group=modClean)) + 
	geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
	geom_point(aes(shape=modClean), size=4, position=position_dodge(width = posDodge)) + 
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1, position=position_dodge(width = posDodge)) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5, position=position_dodge(width = posDodge)) +	
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	ylab('') + scale_x_discrete('', labels=TeX(rev(unique(varKey$clean[-2])))) +	
	# ylab(TeX('$\\beta_{p} \\times \\frac{\\sigma_{x_{p}}}{\\sigma_{y}}$')) +
	coord_flip() + 
	theme(
		legend.position='top', legend.title=element_blank(),
		legend.text=element_text(family="Source Sans Pro Light"),
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.x=element_text(family="Source Sans Pro Light"),
		axis.text.y=element_text(family="Source Sans Pro Light", hjust=0)
	)
ggsave(ggCoef, file=paste0(pathGraphics,'betaEst.pdf'), width=7, height=7, device=cairo_pdf)
############################