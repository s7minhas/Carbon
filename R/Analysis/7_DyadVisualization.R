if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
source(paste0(gpth, 'R/Funcs/postHelpers.R'))

loadPkg(c('magrittr','countrycode','doBy'))
############################

############################
load(paste0(pathResults, 'mltrDep.rda')) # includes object called latDist
load(paste0(pathDataBin, 'idPt.rda'))  # includes object called idPt
load(paste0(pathDataBin,'sScore.rda'))
############################

############################
latAngle$country1 = panel$cname[match(latAngle$Var1,panel$ccode)]
latAngle$country2 = panel$cname[match(latAngle$Var2,panel$ccode)]
latAngle$dyad = paste(latAngle$Var1, latAngle$Var2, sep='_')

avLatAngle = summaryBy(value~dyad,data = latAngle, FUN = mean)
avLatAngleNames = summaryBy(country1 + country2~dyad, data = latAngle, FUN = unique)
avLatAngle2 = cbind(avLatAngle, avLatAngleNames[,2:3])
avLatAngle2 = avLatAngle2[order(avLatAngle2$value.mean, decreasing = F),]
avLatAngle2 = avLatAngle2[seq(1, dim(avLatAngle2)[1], 2),]
names(avLatAngle2) = c("dyad", "AngleDifference", "country1", "country2")

sScoreData = lapply(names(sL), function(x){
  tmp=sL[[x]]; tmp$year=x
  tmp$id = paste(tmp$ccode1, tmp$ccode2, tmp$year, sep='_')
  return(tmp) }) %>% do.call('rbind', .)

latAngle = merge(latAngle, sScoreData, by.x = "dyadid", by.y = "id", all.x = T)
latAngle = merge(latAngle, idPt, by.x = "dyadid", by.y = "dyadidyr" , all.x = T)
latAngle$sOld = latAngle$sScore
# latAngle$sScore = 1 - latAngle$sScore

# # rescale each to between 0 and 1
# latAngle$value = rescale(latAngle$value, 1, 0)
# latAngle$sScore = rescale(latAngle$sScore, 1, 0)
# latAngle$idealpointdistance = rescale(latAngle$idealpointdistance, 1, 0)
latAngle$idealpointdistance = rescale(
	latAngle$idealpointdistance, 
	min(latAngle$idealpointdistance,na.rm=TRUE),
	max(latAngle$idealpointdistance,na.rm=TRUE)	
	)
latAngle$value = stdz(latAngle$value)
latAngle$sScore = stdz(latAngle$sScore)
latAngle$idealpointdistance = stdz(latAngle$idealpointdistance)
############################

############################
plausPlot = function(dyadIds, dyadLabs, pW=8, pH=5, fName, fck=FALSE){
	fig1Plaus = latAngle[
		which( latAngle$dyad %in% dyadIds ),
		c('country1','country2','dyad','year.x','value','sScore','idealpointdistance')] 
	names(fig1Plaus)[5:7] = c('Tensor\nDependence', 'S-Score','Ideal Point\nSimilarity')
	fig1Plaus$dyadAbb = NA
	for(i in 1:length(dyadIds)){ fig1Plaus$dyadAbb[fig1Plaus$dyad==dyadIds[i]]=dyadLabs[i] }
	ggFig1 = reshape2::melt(fig1Plaus[,4:8], id=c('dyadAbb','year.x'))
	ggFig1$dyadAbb = factor(ggFig1$dyadAbb, levels=dyadLabs)
	ggFig1$variable = factor(ggFig1$variable, levels=names(fig1Plaus)[5:7])

	if(fck){ggFig1 = ggFig1[ggFig1$variable!='Ideal Point\nSimilarity',]}

	ggPlaus = ggplot(ggFig1, aes(x=year.x, y=value, group=variable, color=variable)) +
		geom_hline(yintercept=0, color='grey', linetype='dashed', size=1) +	
		geom_line(size=.8) + geom_point(aes(shape=variable), size=1.5) +
		scale_color_brewer(palette='Set1') + 
		facet_grid(variable~dyadAbb, scales='free') +
		labs(color='', shape='') + ylab('') + xlab('') +
		theme(
			axis.ticks=element_blank(),
			# panel.border=element_blank(),
			legend.text=element_text(family="Source Sans Pro Light"),
			legend.position='top',
			axis.text.x=element_text(family="Source Sans Pro Light", angle=45, hjust=1),
			axis.text.y=element_text(family="Source Sans Pro Light"),
			strip.text.x = element_text(color='white',
				family="Source Sans Pro Semibold"),
			strip.text.y = element_text(color='white',
				family="Source Sans Pro Semibold"),			
			strip.background = element_rect(fill = "#525252", color='#525252')				
		)
	ggsave(ggPlaus, file=fName, width=pW, height=pH, device=cairo_pdf)
}

# France-Germany | US-Israel | China-NoKo 
plausPlot(
	dyadIds=c('220_260', '2_666', '710_731'),
	dyadLabs=c('France-Germany', 'USA-Israel', 'China-North Korea'),
	fName=paste0(pathGraphics, 'plausPlot_1_border.pdf')
	)

# SoKo-NoKo | Iraq-Iran
plausPlot(
	dyadIds=c('2_365', '2_645', '2_710'),
	dyadLabs=c('USA-Russia', 'USA-Iraq', 'USA-China'),
	fName=paste0(pathGraphics, 'plausPlot_2_border.pdf')
	)

# SoKo-NoKo | Iraq-Iran
toReplace = latAngle$idealpointdistance[
	latAngle$dyadid %in% paste0('731_732_',1965:2012)]
latAngle$idealpointdistance[
	latAngle$dyadid %in% paste0('732_731_',1965:2012)] = toReplace
toReplace = latAngle$idealpointdistance[
	latAngle$dyadid %in% paste0('630_645_',1965:2012)]
latAngle$idealpointdistance[
	latAngle$dyadid %in% paste0('645_630_',1965:2012)] = toReplace

plausPlot(
	dyadIds=c('732_731', '645_630'),
	dyadLabs=c('South Korea-North Korea', 'Iraq-Iran'),
	pW=8, pH=5, #fck=TRUE,
	fName=paste0(pathGraphics, 'plausPlot_3_border.pdf')
	)
############################s