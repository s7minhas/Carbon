if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Ruthenium/R/setup.R') }

###############################################################
# Load in datasets
dvData='kaopen.rda'
monData=c('icrg', 'worldBank', 'polity', 'constraints', 'dpi', 'gwto', 'gwf') %>% paste0('.rda')
dyData=c('colony','distMats','desta', 'imfTrade') %>% paste0('.rda')
###############################################################

###############################################################
# Merge monadic variables from icrg, worldBank, polity, constraints into kaopen
for(pkg in pathDataBin %>% paste0(c(dvData, monData)) ) { load( pkg ) }; rm(list='pkg')

# Merge kaopen and icrg
icrg = icrg[,c('cyear', names(icrg)[5:16])]
aData = merge(kaopen, icrg, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge kaopen and worldbank
aData = merge(aData, worldBank, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge kaopen and polity
polity = polity[,c('cyear', names(polity)[8:21])]
aData = merge(aData, polity, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge kaopen and constraints
constraints = constraints[,c('cyear', names(constraints)[8:10])]
aData = merge(aData, constraints, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge DPI
aData = merge(aData, dpi, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge gwto
aData = merge(aData, gwto, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge gwf
aData = merge(aData, gwf, by='cyear', all.x=TRUE, all.y=FALSE)

# Remove leftover datasets
rm(list=c(substr(dvData, 1, nchar(dvData)-4),
	substr(monData, 1, nchar(monData)-4)) )
###############################################################

###############################################################
# Create lags

# Select vars to lag
noLag = c('cyear','CNTRY_NAME', 'COWCODE', 'country_name',
	'GWCODE', 'year', 'cname', 'ccode', 
	'kaopen', 'ka_open', 'gwf_nonautocracy')
toLag = setdiff(names(aData), noLag)

# Adjustments to id variables
aData$cyear = num(aData$cyear)
aData$ccode = num(aData$ccode)

# Make sure all variables to be lagged are numeric
sum(apply(aData[,toLag],2,class)=='numeric')/length(toLag)

# Lag selected variables 1 year
aData = lagData(aData, 'cyear', 'ccode', toLag, lag=1)
###############################################################

###############################################################
# Save
save(aData, file=paste0(pathDataBin,'analysisData.rda'))
###############################################################