if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
source(paste0(gpth, 'R/Funcs/postHelpers.R'))

loadPkg(c('magrittr','countrycode','doBy'))
############################

############################
load(paste0(pathResults, 'mltrDep.rda')) # includes object called latAngle
load(paste0(pathDataBin, 'idPt.rda'))  # includes object called idPt
load(paste0(pathDataBin,'sScore.rda'))
############################

############################
# pick cntries
cntries <- c(2,710,732,731)
cntryKey <- data.frame(cbind(
	ccode=cntries,
	cname=panel$cname[match(cntries,panel$ccode)],
	abb=c('USA','CHN','ROK','DPRK')
	))

# focus on relevant dyads and time period
idPt$idealpointdistance = rescale(
	idPt$idealpointdistance, 
	min(idPt$idealpointdistance),
	max(idPt$idealpointdistance)	
	)
idPt = idPt[
	idPt$year==2012 & 
	idPt$ccode1 %in% cntries & 
	idPt$ccode2 %in% cntries
	,]
idPt$abb1 = cntryKey$abb[match(idPt$ccode1,cntryKey$ccode)]
idPt$abb2 = cntryKey$abb[match(idPt$ccode2,cntryKey$ccode)]
idPt$id = with(idPt, paste(abb1,abb2,sep='/'))

idPt = idPt[match(
	c(
		'USA/ROK','DPRK/ROK',
		'USA/DPRK','CHN/ROK',
		'USA/CHN','CHN/DPRK'),
	idPt$id),c('id','idealpointdistance')]
# idPt$idealpointdistance = 1-idPt$idealpointdistance
# idPt$idealpointdistance = rescale(idPt$idealpointdistance, 1, 0)

sScore = sL$'2012'
sScore = sScore[
	sScore$ccode1 %in% cntries & 
	sScore$ccode2 %in% cntries
	,]
sScore$abb1 = cntryKey$abb[match(sScore$ccode1,cntryKey$ccode)]
sScore$abb2 = cntryKey$abb[match(sScore$ccode2,cntryKey$ccode)]
sScore$id = with(sScore, paste(abb1,abb2,sep='/'))
sScore = sScore[match(
	c(
		'USA/ROK','DPRK/ROK',
		'USA/DPRK','CHN/ROK',
		'USA/CHN','CHN/DPRK'),
	sScore$id),c('id','sScore')]
# sScore$sScore = 1-sScore$sScore 
# sScore$sScore = rescale(sScore$sScore, 1, 0)


# latDist
latAngle = latAngle[match(
	c(
		'2_732_2012', '731_732_2012',
		'2_731_2012', '710_732_2012',
		'2_710_2012', '710_731_2012'
		),
	latAngle$dyadid),c('dyadid','value')]
# latAngle$value = rescale(latAngle$value, 1, 0)
############################	

############################	
# viz
ggData = cbind(idPt, sScore=sScore[,'sScore'])
ggData = melt(ggData, id='id')
ggData$id = factor(ggData$id, 
	levels=sScore$id[order(sScore$sScore)]
	)

# clean up facet labels
ggData$variable = char(ggData$variable)
ggData$variable[
	ggData$variable=='idealpointdistance'
	] = 'Ideal Point Similarity'
ggData$variable[
	ggData$variable=='sScore'
	] = 'S-Score'	
ggData$variable = factor(ggData$variable,
	levels=c('S-Score','Ideal Point Similarity'))
ccols = brewer.pal(3, 'Set1')

g=ggplot(ggData, aes(x=id, y=value,color=variable)) + 
	geom_linerange(aes(ymin=0,ymax=value)) +
	geom_point(aes(shape=variable), size=1.5) +
	facet_wrap(~variable, scales='free_y', nrow=2) +
	labs(color='', shape='') + ylab('') + xlab('') +
	# scale_color_brewer(palette='Set1') + 
	scale_color_manual(values=ccols[2:3]) + 
	scale_shape_manual(values=c('triangle','square')) + 
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		legend.text=element_text(family="Source Sans Pro Light"),
		legend.position='top',
		axis.text.x=element_text(
			family="Source Sans Pro Light", angle=45, hjust=1),
		axis.text.y=element_text(
			family="Source Sans Pro Light", size=6),
		strip.text.x = element_text(color='white',
			family="Source Sans Pro Semibold"),		
		strip.text.y = element_text(color='white',
			family="Source Sans Pro Semibold"),			
		strip.background = element_rect(fill = "#525252", color='#525252')				
		)
ggsave(g, height=3.5, width=7,
	file=paste0(pathGraphics, 'idPtScoreViz.pdf'),
	device=cairo_pdf
	)
############################	

############################	
# viz
ggData = cbind(idPt, 
	sScore=sScore[,'sScore'], latAngle=latAngle[,'value'])
ggData = melt(ggData, id='id')
ggData$id = factor(ggData$id, 
	levels=sScore$id[order(sScore$sScore)]
	)

# clean up facet labels
ggData$variable = char(ggData$variable)
ggData$variable[
	ggData$variable=='idealpointdistance'
	] = 'Ideal Point Similarity'
ggData$variable[
	ggData$variable=='sScore'
	] = 'S-Score'	
ggData$variable[
	ggData$variable=='latAngle'
	] = 'Tensor Dependence'		
ggData$variable = factor(ggData$variable,
	levels=c('Tensor Dependence','S-Score','Ideal Point Similarity'))

g=ggplot(ggData, aes(x=id, y=value,color=variable)) + 
	geom_linerange(aes(ymin=0,ymax=value)) +
	geom_hline(aes(yintercept=0),linetype='dashed',color='grey') +
	geom_point(aes(shape=variable), size=1.5) +
	facet_wrap(~variable, scales='free_y', nrow=3) +
	labs(color='', shape='') + ylab('') + xlab('') +
	scale_color_brewer(palette='Set1') + 
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		legend.text=element_text(family="Source Sans Pro Light"),
		legend.position='top',
		axis.text.x=element_text(
			family="Source Sans Pro Light", angle=45, hjust=1),
		axis.text.y=element_text(
			family="Source Sans Pro Light", size=6),
		strip.text.x = element_text(color='white',
			family="Source Sans Pro Semibold"),
		strip.text.y = element_text(color='white',
			family="Source Sans Pro Semibold"),			
		strip.background = element_rect(fill = "#525252", color='#525252')				
		)
ggsave(g, height=5.5, width=7,
	file=paste0(pathGraphics, 'idPtScoreLatAngleViz.pdf'),
	device=cairo_pdf
	)
############################	