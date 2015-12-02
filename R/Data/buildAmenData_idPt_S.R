if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

############################
# Load un and alliance data
load( paste0(pathDataBin, 'idPt.rda') )
############################

############################
# Form list of n x n x p arrays for every t
# n = # cntries, p = # vars, t = # pds

# Years to loop through
yrs = un$year %>% unique() %>% sort()

# Process data into list of arrays
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
	fSl$agreeCnt = unSl$agreeCnt[match(fSl$ij, unSl$ij)]
	fSl$agreeCnt[is.na(fSl$agreeCnt)] = mean(unSl$agreeCnt)
	
	fSl$defEnt = defEntSl$defEnt[match(fSl$ij, defEntSl$ij)]
	fSl$defEnt[is.na(fSl$defEnt)] = 0

	fSl$any = anyAllySl$any[match(fSl$ij, anyAllySl$ij)]
	fSl$any[is.na(fSl$any)] = 0

	# Create empty array
	cntries = c( fSl$i, fSl$j ) %>% unique() %>% char() %>% sort() 
	eArr = array(0, 
		dim=c( length(cntries), length(cntries), 3 ), 
		dimnames=list( cntries, cntries, c('agreeCnt', 'defEntAlly', 'anyAlly') ) )
	
	# Add values from relev data
	diagVal = function(x,val=NA){ diag(x)=val ; return(x) }
	eArr[,,'agreeCnt'] = acast(i ~ j, data=fSl, value.var='agreeCnt') %>% .[cntries,cntries] %>% diagVal()
	eArr[,,'defEntAlly'] = acast(i ~ j, data=fSl, value.var='defEnt') %>% .[cntries,cntries] %>% diagVal()
	eArr[,,'anyAlly'] = acast(i ~ j, data=fSl, value.var='any') %>% .[cntries,cntries] %>% diagVal()
	return(eArr)
	})
names(amData) = yrs
############################s

############################s
# Save
save(amData, file=paste0(pathDataBin,'amenData.rda'))
############################s