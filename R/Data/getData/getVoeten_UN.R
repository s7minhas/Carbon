if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

###############################################################
# Downloaded data manually from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
## on July 7, 2015: 5pm
load(paste0(pathDataRaw,'Voeten/session_affinity_scores_un_67_02132013-cow.RData')); un=x ; rm(list='x')
###############################################################

###############################################################
# Match UN names to panel
cntries = c(un$stateaname, un$statebname) %>% char() %>% unique() %>% data.frame(cntry=.)
cntries$cname = cname(cntries$cntry)

# Fix few cnames issue so it matches with panel
cntries$cname[cntries$cntry=="Yemen People's Republic"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yemen PDR (South)"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yugoslavia"] = 'SERBIA'
cntries$cname[cntries$cntry=="Czechoslovakia"] = 'CZECH REPUBLIC'
cntries$cname[cntries$cntry=="Germany, East"] = 'German Democratic Republic'

# Add ccode
cntries$ccode = panel$ccode[match(cntries$cname,panel$cname)]

# Merge updated cname and ccode to un
un$cname1 = cntries$cname[match(un$stateaname, cntries$cntry)]
un$cname2 = cntries$cname[match(un$statebname, cntries$cntry)]
un$ccode1 = cntries$ccode[match(un$stateaname, cntries$cntry)]
un$ccode2 = cntries$ccode[match(un$statebname, cntries$cntry)]

# Check for duplicates
un$dyadidyr = paste(un$ccode1, un$ccode2, un$year, sep='_')
stopifnot( length( table(un$dyadidyr)[table(un$dyadidyr)>1] ) == 0 )
###############################################################

###############################################################
# use UN data to set nodes for dyadic dataset
yrs = un$year %>% unique() %>% sort()
unFrame = lapply(yrs, function(yr){
	slice = un[un$year==yr,c('ccode1','ccode2')]
	cntries = c(slice$ccode1,slice$ccode2) %>% unique() %>% num()
	frYr = expand.grid(i=cntries, j=cntries)
	frYr = frYr[frYr$i != frYr$j,]
	frYr$ij = paste(frYr$i, frYr$j, sep='_')
	frYr$ji = paste(frYr$j, frYr$i, sep='_')	
	frYr = cbind(frYr, t=yr)
	return(frYr)
	})
names(unFrame) = yrs
###############################################################

###############################################################
# Create count of un agreements by dyad-yr
un$agreeCnt = round( un$agree3un*un$jointvotes3 )

# Convert into a list format for object per year
yrs = un$year %>% unique() %>% sort()
aun3L = convToList(un, yrs, 'year', c('ccode1','ccode2'), 'agree3un', standardize=FALSE)
aun2L = convToList(un, yrs, 'year', c('ccode1','ccode2'), 'agree2un', standardize=FALSE)

# Turn undirected un data into complete frame
aun3Lfull = lapply(aun3L, function(unSl){
	unDat=data.frame( rbind(
		cbind( ij=paste(unSl$ccode1, unSl$ccode2, sep='_'), unSl ),
		cbind( ij=paste(unSl$ccode2, unSl$ccode1, sep='_'), unSl ) ) )
	unDat[,4] = num( unDat[,4] )
	return(unDat)
})

aun2Lfull = lapply(aun2L, function(unSl){
	unDat=data.frame( rbind(
		cbind( ij=paste(unSl$ccode1, unSl$ccode2, sep='_'), unSl ),
		cbind( ij=paste(unSl$ccode2, unSl$ccode1, sep='_'), unSl ) ) )
	unDat[,4] = num( unDat[,4] )
	return(unDat)
})
###############################################################

###############################################################
# Save
save(un, aun3Lfull, aun2Lfull, unFrame, file=paste0(pathDataBin, 'un.rda'))
###############################################################