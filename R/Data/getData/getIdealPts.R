if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

###############################################################
# Downloaded data manually from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
## on July 7, 2015: 5pm
load(paste0(pathDataRaw,'Voeten/Idealpointsdyadicdistance_1.RData')); idPt=x ; rm(list='x')

# Fix Nauru error
idPt$countryname1[idPt$ccode1==946] = 'Kiribati'
idPt$countryname2[idPt$ccode2==946] = 'Kiribati'
###############################################################

###############################################################
# Match idPt names to panel
cntries = c(idPt$countryname1, idPt$countryname2) %>% char() %>% unique() %>% data.frame(cntry=.)
cntries$cname = cname(cntries$cntry)

# Fix few cnames issue so it matches with panel
cntries$cname[cntries$cntry=="Yemen People's Republic"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yemen PDR (South)"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yugoslavia"] = 'SERBIA'
cntries$cname[cntries$cntry=="Czechoslovakia"] = 'CZECH REPUBLIC'
cntries$cname[cntries$cntry=="Germany, East"] = 'German Democratic Republic'

# Add ccode
cntries$ccode = panel$ccode[match(cntries$cname,panel$cname)]

# Merge updated cname and ccode to idPt
idPt$cname1 = cntries$cname[match(idPt$countryname1, cntries$cntry)]
idPt$cname2 = cntries$cname[match(idPt$countryname2, cntries$cntry)]
idPt$ccode1 = cntries$ccode[match(idPt$countryname1, cntries$cntry)]
idPt$ccode2 = cntries$ccode[match(idPt$countryname2, cntries$cntry)]

# Check for duplicates
idPt$dyadidyr = paste(idPt$ccode1, idPt$ccode2, idPt$year, sep='_')
stopifnot( length( table(idPt$dyadidyr)[table(idPt$dyadidyr)>1] ) == 0 )
###############################################################

###############################################################
# Convert into list format for object per year
yrs = idPt$year %>% unique() %>% sort()
idPtL = convToList(idPt, yrs, 'year', c('ccode1','ccode2'), 'idealpointdistance')
###############################################################

###############################################################
# Save
save(idPt, idPtL, file=paste0(pathDataBin, 'idPt.rda'))
###############################################################