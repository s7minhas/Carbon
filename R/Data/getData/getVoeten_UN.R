if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

###############################################################
# Clean UN data, from project with Cindy

# vote – Vote choice 
# 1 – Yes 
# 2 – Abstain 
# 3 – No 
# 8 – Absent 
# 9 – Not a member 

#### Make new measures from the raw data
 
# load data
# Downloaded data manually from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
## on July 7, 2015: 5pm
load(paste0(pathDataRaw,'Voeten/undata-213.RData')); unData=x ; rm(list='x')
unData$uniquename = char(unData$uniquename)

# Create year - roll call variable
unData$yrRcid = paste(substring(unData$date, 1, 4), unData$rcid, sep ="_")

# Subset the data to only abstain votes, only yes votes, only no votes
unAbstain = unData[which(unData$vote ==2), c('yrRcid', 'vote', 'ccode', 'uniquename')]
unYes = unData[which(unData$vote ==1), c('yrRcid', 'vote', 'ccode', 'uniquename')]
unNo = unData[which(unData$vote ==3), c('yrRcid', 'vote', 'ccode', 'uniquename')]
unJoint = unData[-which(unData$vote ==8 | unData$vote ==9), c('yrRcid', 'vote', 'ccode', 'uniquename')]

# Create dyad pairs for countries that both agree on a resolution
AbstainDyad = makeDyad(unAbstain, 'yrRcid')
YesDyad = makeDyad(unYes, 'yrRcid')
NoDyad = makeDyad(unNo, 'yrRcid')
JointDyad = makeDyad(unJoint, 'yrRcid')
          
# make unique dyad ID
countries = unique(unData$uniquename)
dyadAll = expand.grid(countries, countries)  
dyadAll$Var1 = char(dyadAll$Var1); dyadAll$Var2 = char(dyadAll$Var2)
dyadAll= dyadAll[-which(dyadAll[,1] == dyadAll[,2]),]
dyadAll$dname = paste(dyadAll$Var1, dyadAll$Var2, sep = "_")

dyadAll$dyadID = 0
for ( i in 1:dim(dyadAll)[1] ) {
if(dyadAll$dyadID[i] ==0){   
dyadAll$dyadID[which(dyadAll$Var1 %in% c(dyadAll[i,]) & dyadAll$Var2 %in% c(dyadAll[i,]))] = i
}
}

# Add unique dyad ID to Agree dyadic dataset
AbstainDyad$dyadID = dyadAll$dyadID[match(AbstainDyad$dname, dyadAll$dname)]
YesDyad$dyadID = dyadAll$dyadID[match(YesDyad$dname, dyadAll$dname)]
NoDyad$dyadID = dyadAll$dyadID[match(NoDyad$dname, dyadAll$dname)]
JointDyad$dyadID = dyadAll$dyadID[match(JointDyad$dname, dyadAll$dname)]

# aggregate data by year

AbstainDyadYear = aggDyad(AbstainDyad, 'year', 'abstain') 
YesDyadYear = aggDyad(YesDyad, 'year', 'yes') 
NoDyadYear = aggDyad(NoDyad, 'year', 'no')
JointDyadYear = aggDyad(JointDyad, 'year', 'no') 


## Merge datasets
AllDyadYr0 = merge(AbstainDyadYear, YesDyadYear, by = c("dyadID", "year"), all = T)
AllDyadYr1 = merge(NoDyadYear, JointDyadYear, by = c("dyadID", "year"), all = T)
AllDyadYr = merge(AllDyadYr0, AllDyadYr1, by = c("dyadID", "year"), all = T)

# add back country names
AllDyadYr$state_1 = dyadAll$Var1[match(AllDyadYrFINAL$dyadID, dyadAll$dyadID)]
AllDyadYr$state_2 = dyadAll$Var2[match(AllDyadYrFINAL$dyadID, dyadAll$dyadID)]


# Make variable that calculates the proportion of times two countries agree
AllDyadYr = AllDyadYr[, c(1, 2, 4, 5, 8, 6, 7, 3)]
AllDyadYr$jointAgreeNew = rowSums(dplyr::select(AllDyadYr, contains("UN")), na.rm = T )
AllDyadYr$agree3unNew = AllDyadYr$jointAgreeNew/AllDyadYr$all

# Add ccode 
states = union(AllDyadYr$state_1, AllDyadYr$state_2)
temp = data.frame(cbind(states, cname =  toupper(countrycode(states, 'country.name', 'country.name')) ))
temp$cname = char(temp$cname)
temp$ccode = panel$ccode[match(temp$cname, panel$cname)]
 
AllDyadYr$cname_1 = temp$cname[match(AllDyadYr$state_1,temp$states)]
AllDyadYr$cname_2 = temp$cname[match(AllDyadYr$state_2,temp$states)]

AllDyadYr$ccode_1 = temp$ccode[match(AllDyadYr$state_1,temp$states)]
AllDyadYr$ccode_2 = temp$ccode[match(AllDyadYr$state_2,temp$states)]

AllDyadYr$cname_1Year = paste(AllDyadYr$cname_1, AllDyadYr$year, sep = "")

un = AllDyadYr

save(un, file=paste0(pathDataBin, 'un.rda'))