if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop" ){
  source('~/Documents/Carbon/R/setup.R') }

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
	'dyad_end_year', 'dyad_st_year',
	'defense', 'neutrality', 'nonaggression', 'entente'
	)]

# Check to make sure if i-j exist then j-i exists
ally$c1c2yr = paste(ally$ccode1, ally$ccode2, ally$year, sep='xyz')
ally$c2c1yr = paste(ally$ccode2, ally$ccode1, ally$year, sep='xyz')
setdiff(ally$c1c2yr, ally$c2c1yr) %>% length()
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
cntries$ccode = panel$ccode[match(toupper(cntries$cname),panel$cname)]
cntries$ccode[is.na(cntries$ccode)] = countrycode(cntries[is.na(cntries$ccode),"cname"], "country.name", "cown")
# Merge updated cname and ccode to un
ally$cname1 = cntries$cname[match(ally$state_name1, cntries$cntry)]
ally$cname2 = cntries$cname[match(ally$state_name2, cntries$cntry)]
ally$ccode1 = cntries$ccode[match(ally$state_name1, cntries$cntry)]
ally$ccode2 = cntries$ccode[match(ally$state_name2, cntries$cntry)]

# Subset
ally = ally[!is.na(ally$dyad_end_year),]
###############################################################

###############################################################
# Create parameters for calcing s scores
ally$anyally = ally$defense | ally$neutrality | ally$entente
ally$allyweighted = 3*ally$defense + 2*ally$neutrality*(1 - ally$defense) + ally$entente*(1 - ally$defense)*(1 - ally$neutrality)
yrs = unique(ally$year) %>% sort()
###############################################################

###############################################################
# Functions to calculate s score
twoS = function(mat, i, j, wvec){
	r1 = mat[i,]
	r2 = mat[j,]
	dmax = sum(wvec)
	deltmax = max(mat) - min(mat)
	d = abs(r1 - r2) %*% wvec/dmax/deltmax
	return(1 - 2*d)
}

rowS = function(i, mat, wvec){
	j = as.matrix(1:dim(mat)[1])
	out = apply(j, 1, twoS, mat = mat, wvec = wvec, i = i)
	return(out)
}

matrixS = function(mat){
	mat = as.matrix(mat)
	wvec = rep(1, nrow(mat))
	smat = apply(as.matrix(1:dim(mat)[1]),1, rowS, mat = mat, wvec = wvec)
	return(smat)	
	}

ally = ally[!is.na(ally$ccode1) & !is.na(ally$ccode2),]
# Create list of matrices with allyweighted in cross-sections
allyList = lapply(yrs, function(yr){
	slice = ally[ally$year==yr,c('ccode1','ccode2','allyweighted')] %>% cbind(., c12=paste0(.[,1], '_',.[,2]))
	ccs = c(slice$ccode1, slice$ccode2) %>% unique() %>% sort()
	full = expand.grid(ccode1=ccs, ccode2=ccs) %>% cbind(., c12=paste0(.[,1], '_',.[,2]))
	full$allyweighted = 0
	full$allyweighted[match(slice$c12, full$c12)] = slice$allyweighted
	mat = acast(full, ccode1~ccode2, value.var='allyweighted')
	diag(mat) = 3
	return( mat )
	})
names(allyList) = yrs
	
# Calculate s score	and reformat into set of long dfs
sL = lapply(allyList, function(mat){
	tmp = 1-  matrixS(mat); rownames(tmp)=colnames(tmp)=rownames(mat)
	diag(tmp) = NA
	tmp = ( tmp - mean(tmp, na.rm=TRUE) ) / sd(tmp, na.rm=TRUE)
	diag(tmp) = 0
	tmpL=melt(tmp); colnames(tmpL)=c('ccode1','ccode2','sScore')
	tmpL = tmpL[tmpL$ccode1 != tmpL$ccode2,]
	return(tmpL)
	})

save(sL, file = paste0(pathDataBin,'sScore.rda'))
###############################################################