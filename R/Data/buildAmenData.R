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
yrs = un$year %>% unique() %>% sort()
amData = lapply(yrs, function(yr){
	
	# Pull yearly slice from un data
	unSl = aun3Lfull[[char(yr)]]

	# Add id vectors into alliance dataset
	defEntSl = defEntAllyL[[char(yr)]]
	defEntSl$ij = paste(defEntSl$ccode1, defEntSl$ccode2, sep='_')
	anyAllySl = anyAllyL[[char(yr)]]
	anyAllySl$ij = paste(anyAllySl$ccode1, anyAllySl$ccode2, sep='_')

	# Merge covariate data into frame slice
	fSl = unFrame[[char(yr)]]	
	fSl$agree3un = unSl$agree3un[match(fSl$ij, unSl$ij)]
	fSl$agree3un[is.na(fSl$agree3un)] = mean(unSl$agree3un)
	
	fSl$defEnt = defEntSl$defEnt[match(fSl$ij, defEntSl$ij)]
	fSl$defEnt[is.na(fSl$defEnt)] = 0

	fSl$any = anyAllySl$any[match(fSl$ij, anyAllySl$ij)]
	fSl$any[is.na(fSl$any)] = 0

	# Create empty array
	cntries = c( fSl$i, fSl$j ) %>% unique() %>% char() %>% sort() 
	eArr = array(0, 
		dim=c( length(cntries), length(cntries), 3 ), 
		dimnames=list( cntries, cntries, c('agree3un', 'defEntAlly', 'anyAlly') ) )
	
	# Add values from relev data
	diagZero = function(x){ diag(x)=0 ; return(x) }
	eArr[,,'agree3un'] = acast(i ~ j, data=fSl, value.var='agree3un') %>% .[cntries,cntries] %>% diagZero()
	eArr[,,'defEntAlly'] = acast(i ~ j, data=fSl, value.var='defEnt') %>% .[cntries,cntries] %>% diagZero()
	eArr[,,'anyAlly'] = acast(i ~ j, data=fSl, value.var='any') %>% .[cntries,cntries] %>% diagZero()
	return(eArr)
	})
############################s

############################s
# Save
save(amData, file='amenData.rda')
############################s