####
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/Carbon/R/setup.R') }
####	

####
# Load data from ICEWS
load( paste0(pathDataBin, 'coop_icews.rda') ) # loads coopList
####

####
# Clean
coopList = lapply(coopList, function(icewsData){
	icewsData = icewsData[icewsData$source_country != icewsData$target_country,]

	# add time variables
	icewsData$time = char(icewsData$year)

	# Add id var
	icewsData$source = countrycode(icewsData$source_country, 'iso3c', 'country.name')
	icewsData$source[icewsData$source_country=='KOS'] = 'KOSOVO'
	icewsData$target = countrycode(icewsData$target_country, 'iso3c', 'country.name')
	icewsData$target[icewsData$target_country=='KOS'] = 'KOSOVO'

	# Add in ccodes from panel
	icewsData$sCode = panel$ccode[match(icewsData$source, panel$cname)]
	icewsData$tCode = panel$ccode[match(icewsData$target, panel$cname)]

	# Drop all the mini countries
	# print('Dropping following countries from sender list:')
	# print( unique(icewsData[is.na(icewsData$sCode),c('source_country','source')]) )
	# print('Dropping following countries from target list:')
	# print( unique(icewsData[is.na(icewsData$tCode),c('target_country','target')]) )
	icewsData = na.omit(icewsData)

	# aggregate
	icewsData$id = paste(icewsData$sCode, icewsData$tCode, icewsData$time, sep='_')
	byForm = formula( paste0(names(icewsData)[5], ' ~ id') )
	icewsDataDyad = summaryBy(byForm, data=icewsData, FUN=sum, keep.names=TRUE,
		id=names(icewsData)[-which( names(icewsData) %in% c(names(icewsData)[5],'month','id') )] )	
	return(icewsDataDyad)
	})
names(coopList) = unlist( lapply(coopList, function(x){ names(x)[2]  }) )
####

####
# Turn into array for analysis
# Limits units included to be those that exist during 2012
vars = names(coopList)
units = sort(unique(panel$ccode[panel$year==2012]))
pds = char(2001:2014)
coopArr = array(0, dim=c(length(units),length(units),length(vars),length(pds)), dimnames=list(units,units,vars,pds) )
for(v in vars){
	coopDyad = coopList[[v]]
	for(t in pds){
		slice = coopDyad[coopDyad$time==t,c('sCode','tCode', names(coopDyad)[2])]
		for(cntry in units){
			sliceC = slice[slice$sCode==cntry,]
			val = sliceC[match(units, sliceC$tCode), names(coopDyad)[2] ]
			val[is.na(val)] = 0
			coopArr[char(cntry),,v,t] = val
		}
	}
}
####

####
# Subset actors
load(paste0(pathDataBin, 'repdata.RDA')) # includes object called data
cntries = unique(c(data$ccode1, data$ccode2))
# leads to us dropping:
# [1] "MONACO"                          "LIECHTENSTEIN"                  
# [3] "ANDORRA"                         "SAN MARINO"                     
# [5] "MARSHALL ISLANDS"                "MICRONESIA, FEDERATED STATES OF"
amenData = coopArr[char(cntries),char(cntries),,]
####

####
save(amenData, file=paste0(pathDataBin, 'amenData.rda'))
####