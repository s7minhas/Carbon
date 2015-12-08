if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

############################
# Load un and alliance data
load( paste0(pathDataBin, 'un.rda') )
load( paste0(pathDataBin, 'idPt.rda') )
load( paste0(pathDataBin, 'SScore.rda') )
############################

############################
# Form list of n x n x p arrays for every t
# n = # cntries, p = # vars, t = # pds

# Years to loop through
yrs = un$year %>% unique() %>% sort()

# Process data into list of arrays
amData = lapply(yrs, function(yr){

	# add id vectors to idPt and sL
	idSl = idPtL[[char(yr)]]
	idSl$ij = paste(idSl$ccode1, idSl$ccode2, sep='_')
	sSl = sL[[char(yr)]]
	sSl$ij = paste(sSl$ccode1, sSl$ccode2, sep='_')

	# Merge covariate data into frame slice
	fSl = unFrame[[char(yr)]]	
	fSl$idPt = idSl$idealpointdistance[match(fSl$ij, idSl$ij)]
	fSl$sScore = sSl$sScore[match(fSl$ij, sSl$ij)]

	# Mean replacement of missing values
	fSl$idPt[is.na(fSl$idPt)] = mean(fSl$idPt, na.rm=TRUE)
	fSl$sScore[is.na(fSl$sScore)] = mean(fSl$sScore, na.rm=TRUE)

	# Create empty array
	cntries = c( fSl$i, fSl$j ) %>% unique() %>% char() %>% sort() 
	eArr = array(0, 
		dim=c( length(cntries), length(cntries), 2 ), 
		dimnames=list( cntries, cntries, c('idPt', 'sScore') ) )
	
	# Add values from relev data
	diagVal = function(x,val=NA){ diag(x)=val ; return(x) }
	eArr[,,'idPt'] = acast(i ~ j, data=fSl, value.var='idPt') %>% .[cntries,cntries] %>% diagVal()
	eArr[,,'sScore'] = acast(i ~ j, data=fSl, value.var='sScore') %>% .[cntries,cntries] %>% diagVal()
	return(eArr)
	})
names(amData) = yrs
############################s

############################s
# Save
# save(amData, file=paste0(pathDataBin,'amenData_idPt_sScore_wMissing.rda'))
save(amData, file=paste0(pathDataBin,'amenData_idPt_sScore_wMeanRepl.rda'))
############################s