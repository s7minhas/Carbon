if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

############################
# Download file from ICOW site
allyURL = 'http://www.correlatesofwar.org/data-sets/formal-alliances/alliances-data-dta-zip/at_download/file'
allyName = paste0(pathDataRaw, 'ally.zip')
if(!file.exists(allyName)) { download.file(allyURL, allyName) }

ally = unzip(allyName, 
	'version4.1_dta/alliance_v4.1_by_directed_yearly.dta') %>% read.dta()
############################

############################
# Match ally names to panel
ally$ccode1=num(ally$ccode1)
ally$ccode2=num(ally$ccode2)

ctyNameA=countrycode(ally$ccode1, "cown", "country.name")
ctyNameB=countrycode(ally$ccode2, "cown", "country.name")

sancIDs=data.frame(unique(cbind(ally$ccode1, ally$ccode2, ctyNameA, ctyNameB)))

sancIDs$V1= num(sancIDs$V1)
sancIDs$V2 = num(sancIDs$V2)
sancIDs$ctyNameA =char(sancIDs$ctyNameA)
sancIDs$ctyNameB =char(sancIDs$ctyNameB)

sancIDs2 = unique(
	data.frame(cbind(
			rbind(cowcode=t(t(sancIDs[,c(1)])), cowcode=t(t(sancIDs[,c(2)]))),
			rbind(country=t(t(sancIDs[,c(3)])), country=t(t(sancIDs[,c(4)]))) ) ) )
names(sancIDs2) = c('cowcode', 'country')

sancIDs2$cowcode = num(sancIDs2$cowcode)
sancIDs2$country = char(sancIDs2$country)

#fix time
sancIDs2[sancIDs2$cowcode==245,'country'] = 'BAVARIA'
sancIDs2[sancIDs2$cowcode==267,'country'] = 'BADEN'
sancIDs2[sancIDs2$cowcode==300,'country'] = 'AUSTRIA-HUNGARY'
sancIDs2[sancIDs2$cowcode==730,'country'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs2[sancIDs2$cowcode==731,'country'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs2[sancIDs2$cowcode==678,'country'] = 'YEMEN'
sancIDs2[sancIDs2$cowcode==680,'country'] = 'S. YEMEN' 
sancIDs2[sancIDs2$cowcode==817,'country'] = 'S. VIETNAM'
sancIDs2[sancIDs2$cowcode==260,'country'] = 'GERMANY'
sancIDs2[sancIDs2$cowcode==345,'country'] = 'SERBIA'
sancIDs2[sancIDs2$cowcode==315,'country'] = 'CZECH REPUBLIC'

# Add in the data from the panel
sancIDs2$ccode = panel$ccode[match(sancIDs2$country, panel$cname)]
sancIDs2$cname = panel$cname[match(sancIDs2$country, panel$cname)]

sancIDs2[is.na(sancIDs2$ccode),]	# Checks for NAs
sancIDs2[is.na(sancIDs2$cname),] 

# Add back to ally
ally2 = ally[,c('ccode1', 'ccode2', 'state_name1', 'state_name2','year')]
colnames(ally2)[1:2] = c('cowcode1', 'cowcode2')

ally2$ccode_1 = sancIDs2$ccode[match(ally2$cowcode1, sancIDs2$cowcode)]
ally2$ccode_2 = sancIDs2$ccode[match(ally2$cowcode2, sancIDs2$cowcode)]

ally2$cname_1 = sancIDs2$cname[match(ally2$cowcode1, sancIDs2$cowcode)]
ally2$cname_2 = sancIDs2$cname[match(ally2$cowcode2, sancIDs2$cowcode)]

allyFINAL = na.omit(ally2)
allyFINAL$ally = 1
ally = allyFINAL
############################

############################
# Build alliance matrices
allyMats = DyadBuild(variable='ally', dyadData=ally, 
	time=1960:2012, panel=panel, directed=FALSE)
############################

############################
# Save
save(ally, allyMats,
	file=paste0(pathDataBin, 'ally.rda'))
############################