if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

############################
# Load un and alliance data
load( paste0(pathDataBin, 'ally.rda') )
load( paste0(pathDataBin, 'un.rda') )
# load( paste0(pathDataBin, 'igo.rda') )
############################

############################
# Form list of n x n x p arrays for every t
# n = # cntries, p = # vars, t = # pds

# Years to loop through
yrs = 1965:2012

# Process data into list of arrays
amData = lapply(yrs, function(yr){
	
	# Pull yearly slice from un data
	aun3Sl = aun3Lfull[[char(yr)]]
	aun2Sl = aun2Lfull[[char(yr)]]

	# Pull yearly slice from alliance dataset
	anyAllySl = anyAllyL[[char(yr)]]		
	totAllySl = totAllyL[[char(yr)]]
	defEntSl = defEntAllyL[[char(yr)]]
	defEntSumAllySl = defEntSumAllyL[[char(yr)]]
	defAllySl = defAllyL[[char(yr)]]

	# # Pull yearly slice from igo
	# igoSl = igoL[[char(yr)]]

	# Merge covariate data into frame slice
	addVar = function(
		fromVar, fromID, toID=fSl$ij, 
		naZero=TRUE, rescale=TRUE, stdzVar=FALSE){
		tmp = fromVar[match(toID, fromID)]
		tmp = num(tmp)
		if(naZero){ tmp[is.na(tmp)] = 0  }
		if(rescale){ tmp = rescale(tmp, 10, 1) }
		if(stdzVar){ tmp = stdz(tmp) }
		return(tmp)
	}

	fSl = unFrame[[char(yr)]]	
	fSl$agree3un = addVar(aun3Sl$agree3un, aun3Sl$ij,
		# naZero=FALSE) # _rescaled output
		naZero=FALSE, rescale=FALSE, stdzVar=TRUE) # _stdz output
	# fSl$agree2un = addVar(aun2Sl$agree2un, aun2Sl$ij, naZero=FALSE)
	# fSl$anyAlly = addVar(anyAllySl$any, anyAllySl$ij)
	fSl$totAllyCnt = addVar(totAllySl$totCnt, totAllySl$ij
		# ) # _rescaled output
		, rescale=FALSE, stdzVar=TRUE) # _stdz output
	# fSl$defEnt = addVar(defEntSl$defEnt, defEntSl$ij)
	# fSl$defEntSum = addVar(defEntSumAllySl$defEntSum, defEntSumAllySl$ij)
	# fSl$defense = addVar(defAllySl$defense, defAllySl$ij)
	# fSl$igo = addVar(igoSl$igo, igoSl$ij)

	# Create empty array
	cntries = c( fSl$i, fSl$j ) %>% unique() %>% char() %>% sort() 
	vars = names(fSl)[6:ncol(fSl)]
	eArr = array(0, 
		dim=c( length(cntries), length(cntries), length(vars) ), 
		dimnames=list( cntries, cntries, vars ) )
	
	# Add values from relev data
	diagVal = function(x,val=NA){ diag(x)=val ; return(x) }
	for(var in vars){
		eArr[,,var] = acast(i ~ j, data=fSl, value.var=var) %>% .[cntries,cntries] %>% diagVal()
	}
	return(eArr)
	})
names(amData) = yrs
############################s

############################
# Save
# save(amData, file=paste0(pathDataBin,'amenData_all.rda'))
# save(amData, file=paste0(pathDataBin,'amenData_all_rescaled.rda'))
save(amData, file=paste0(pathDataBin,'amenData_all_stdz.rda'))
############################