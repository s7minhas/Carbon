if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

###############################################################
# Download file from ICOW site
allyURL = 'http://www.correlatesofwar.org/data-sets/formal-alliances/alliances-data-dta-zip/at_download/file'
allyName = paste0(pathDataRaw, 'ally.zip')
if(!file.exists(allyName)) { download.file(allyURL, allyName) }

ally = unzip(allyName, 
	'version4.1_dta/alliance_v4.1_by_directed_yearly.dta') %>% read.dta()

# Include only post 1946 data
ally = ally[ally$year>=1946,]

# Subset to relevant vars
ally = ally[,c(
	'ccode1','ccode2','state_name1','state_name2','year',
	'defense', 'neutrality', 'nonaggression', 'entente'
	)]
###############################################################

###############################################################
# Match ally names to panel
cntries = c(ally$state_name1, ally$state_name2) %>% char() %>% unique() %>% data.frame(cntry=.)
cntries$cname = cname(cntries$cntry)

# Fix few cnames issue so it matches with panel
cntries$cname[cntries$cntry=="Yemen People's Republic"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yugoslavia"] = 'SERBIA'
cntries$cname[cntries$cntry=="Czechoslovakia"] = 'CZECH REPUBLIC'

# Add ccode
cntries$ccode = panel$ccode[match(cntries$cname,panel$cname)]

# Merge updated cname and ccode to un
ally$cname1 = cntries$cname[match(ally$state_name1, cntries$cntry)]
ally$cname2 = cntries$cname[match(ally$state_name2, cntries$cntry)]
ally$ccode1 = cntries$ccode[match(ally$state_name1, cntries$cntry)]
ally$ccode2 = cntries$ccode[match(ally$state_name2, cntries$cntry)]

# Create separate dfs for defense and any
ally$any = apply(ally[,c('defense', 'neutrality', 'nonaggression', 'entente')], 1, sum) %>% ifelse(., 1, 0)
ally$defEnt = apply(ally[,c('defense', 'entente')], 1, sum) %>% ifelse(., 1, 0)

# Aggregate to count variable for any alliance, need to split DF for this
loadPkg('doBy')
anyAlly = ally[ally$any==1,c('ccode1','ccode2','cname1','cname2','year','any')]
anyAlly = summaryBy(any ~ ccode1 + ccode2 + year, data=anyAlly, keep.names=TRUE, FUN=sum)

# Aggregate to count variable for def/ent, need to split DF for this
defEntAlly = ally[ally$defEnt==1,c('ccode1','ccode2','cname1','cname2','year','defEnt')]
defEntAlly = summaryBy(defEnt ~ ccode1 + ccode2 + year, data=defEntAlly, keep.names=TRUE, FUN=sum)
###############################################################

###############################################################
# Merge with frame from UN data

###############################################################

###############################################################
# Convert to list
yrs = ally$year %>% unique() %>% sort()
anyAllyL = convToList(anyAlly, yrs, 'year', c('ccode1','ccode2'), 'any')
defEntAllyL = convToList(defEntAlly, yrs, 'year', c('ccode1','ccode2'), 'defEnt')
###############################################################

###############################################################
# Save
save(anyAlly, anyAllyL, defEntAlly, defEntAllyL, file=paste0(pathDataBin, 'ally.rda'))
###############################################################