if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

############################
# Download file from ICOW site
midURL = 'http://www.correlatesofwar.org/data-sets/MIDs/mid-level/at_download/file'
midName = paste0(pathDataRaw, 'mid.zip')
if(!file.exists(midName)) { download.file(midURL, midName) }

mid = unzip(midName, 
	'MIDB_4.01.csv') %>% read.csv()
############################

##############################################################
# Clean MIDs data
# clean up names
names(mid) = tolower(names(mid))
dispnums = unique(mid$dispnum3)

midDyad = do.call(rbind, lapply(dispnums, function(x){
                 slice = mid[mid$dispnum3 == x,]
                 sideA = slice[which(slice$sidea ==1),]
                 sideB = slice[which(slice$sidea ==0),]
                 rawDyad = data.frame(cbind(expand.grid(sideA$ccode, sideB$ccode), unique(slice$dispnum3)))
                 names(rawDyad) = c("ccode_1", "ccode_2", "dispnum3")
                 rawDyadF = merge( rawDyad, slice[, c('dispnum3', 'ccode', 'hostlev')],  by.x = c('dispnum3', 'ccode_2'), by.y = c('dispnum3', 'ccode'), all.x = T)
                 return(rawDyadF)
                 }) )

 


# note there are multiple endyears per dispute so can't just add directly in the lapply function above, must merge as in below
midDyadYr = merge(midDyad, mid[, c('dispnum3', 'styear', 'endyear')], by = c("dispnum3"), all = T)
midDyadYr = unique(midDyadYr)
# create unique dyadIDs
countries = unique(mid$ccode)
dyadAll = expand.grid(countries, countries)  
dyadAll$Var1 = as.character(dyadAll$Var1); dyadAll$Var2 = as.character(dyadAll$Var2)
dyadAll= dyadAll[-which(dyadAll[,1] == dyadAll[,2]),]
dyadAll$dname = paste(dyadAll$Var1, dyadAll$Var2, sep = "_")

dyadAll$dyadID = 0
for ( i in 1:dim(dyadAll)[1] ) {
if(dyadAll$dyadID[i] ==0){   
dyadAll$dyadID[which(dyadAll$Var1 %in% c(dyadAll[i,]) & dyadAll$Var2 %in% c(dyadAll[i,]))] = i
}
}


# # Add unique dyad ID to mid dyadic dataset
midDyadYr$dname = paste(midDyadYr$ccode_1, midDyadYr$ccode_2, sep ="_")
midDyadYr$dyadID = dyadAll$dyadID[match(midDyadYr$dname, dyadAll$dname)]


 
## Expand the dataset to account for conflicts over all years
midDyadAllYr  = panelyear(midDyadYr, midDyadYr$styear, midDyadYr$endyear)
midDyadAllYr$mid = 1


 

# Aggregate mids per dyad-year
midDyadAggYr = select(midDyadAllYr, -(c(styear, endyear))) %>% group_by (dyadID, year) %>% summarize(hostlev = mean(hostlev), mid = sum(mid))

 
# # Add back ccode and country names 
midDyadAggYr$cow_1 = num(dyadAll$Var1[match(midDyadAggYr$dyadID, dyadAll$dyadID)])
midDyadAggYr$cow_2 = num(dyadAll$Var2[match(midDyadAggYr$dyadID, dyadAll$dyadID)])


midDyadAggYr$ccode_1 = panel$ccode[match(midDyadAggYr$cow_1, panel$COWCODE)]
midDyadAggYr$ccode_2 = panel$ccode[match(midDyadAggYr$cow_2, panel$COWCODE)]

midDyadAggYr$cname_1 = panel$cname[match(midDyadAggYr$ccode_1, panel$ccode)]
midDyadAggYr$cname_2 = panel$cname[match(midDyadAggYr$ccode_2, panel$ccode)]


midDyadAggYr = midDyadAggYr[-which(is.na(midDyadAggYr$ccode_1)|is.na(midDyadAggYr$ccode_2)),]

# select variables/years you want
mid = data.frame(midDyadAggYr)

summary(midFINAL)
save(mid, file=paste0(pathDataBin, 'mid.rda'))
###############################################################