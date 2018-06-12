if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
source(paste0(gpth, 'R/Funcs/postHelpers.R'))

loadPkg(c('magrittr','countrycode','doBy'))
############################

############################
load(paste0(pathResults, 'latDist.rda')) # includes object called latAngle
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

# latDist
latAngle = latAngle[match(
	c(
		'2_732_2012', '731_732_2012',
		'2_731_2012', '710_732_2012',
		'2_710_2012', '710_731_2012'
		),
	latAngle$dyadid),c('dyadid','value')]
latAngle$value = rescale(latAngle$value,1,-1)
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
	] = 'Ideal Point Distance'
ggData$variable[
	ggData$variable=='sScore'
	] = 'S-Score'	
ggData$variable = factor(ggData$variable,
	levels=c('S-Score','Ideal Point Distance'))

g=ggplot(ggData, aes(x=id, y=value)) + 
	geom_linerange(aes(ymin=0,ymax=value)) +
	geom_point() +
	facet_wrap(~variable, scales='free_y', nrow=2) +
	labs(y='',x='') +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
ggsave(g, height=3, width=7,
	file=paste0(pathGraphics, 'idPtScoreViz.pdf')
	)
############################	

############################	
# viz
ggData = cbind(idPt, 
	sScore=sScore[,'sScore'], latAngle=latAngle[,'value'])
ggData = melt(ggData, id='id')
ggData$id = factor(ggData$id, 
	levels=sScore$id[order(latAngle$value)]
	)

# clean up facet labels
ggData$variable = char(ggData$variable)
ggData$variable[
	ggData$variable=='idealpointdistance'
	] = 'Ideal Point Distance'
ggData$variable[
	ggData$variable=='sScore'
	] = 'S-Score'	
ggData$variable[
	ggData$variable=='latAngle'
	] = 'Latent Angle Distance'		
ggData$variable = factor(ggData$variable,
	levels=c('Latent Angle Distance','S-Score','Ideal Point Distance'))

g=ggplot(ggData, aes(x=id, y=value)) + 
	geom_linerange(aes(ymin=0,ymax=value)) +
	geom_hline(aes(yintercept=0),linetype='dashed',color='grey') +
	geom_point() +
	facet_wrap(~variable, scales='free_y', nrow=3) +
	labs(y='',x='') +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
ggsave(g, height=5, width=7,
	file=paste0(pathGraphics, 'idPtScoreLatAngleViz.pdf')
	)
############################	