if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

############################
# Load un and alliance data
load( paste0(pathDataBin, 'ally.rda') )
load( paste0(pathDataBin, 'un.rda') )
############################

############################
# Form list of n x n x p arrays for every t
# n = # cntries, p = # vars, t = # pds
amData = lapply(yrs, function(yr){
	
	# Turn undirected un data into directed format
	unSl = aun3L[[char(yr)]]
	unDat=data.frame( rbind(
		cbind( paste(unSl$ccode1, unSl$ccode2, sep='_'), unSl[,'agree3un'] ),
		cbind( paste(unSl$ccode2, unSl$ccode1, sep='_'), unSl[,'agree3un'] ) ) )
	names(unDat) = c('ij', 'agree3un')
	unDat$agree3un = num( unDat$agree3un )

	# Add id vectors into alliance dataset
	allySl = defEntAllyL[[char(yr)]]
	allySl$ij = paste(allySl$ccode1, allySl$ccode2, sep='_')
	
	# Merge covariate data into frame slice
	fSl = frame[[char(yr)]]	
	fSl$agree3un = unDat$agree3un[match(fSl$ij, unSl$ij)]
	fSl$defEnt = allySl$defEnt[match(fSl$ij, allySl$ij)]
	fSl$defEnt[is.na(fSl$defEnt)] = 0

	# Create empty array
	cntries = c( fSl$i, fSl$j ) %>% unique() %>% num() %>% sort()
	eArr = array(0, 
		dim=c( length(cntries), length(cntries), 2 ), 
		dimnames=list( cntries, cntries, c('un', 'ally') ) )
	
	# Add values from relev data

	})
############################s