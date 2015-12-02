if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

###############################################################
# Download file from ICOW site
allyURL = 'http://www.correlatesofwar.org/data-sets/formal-alliances/alliances-data-dta-zip/at_download/file'
allyName = paste0(pathDataRaw, 'ally.zip')
if(!file.exists(allyName)) { download.file(allyURL, allyName) }

ally = unzip(allyName, 
	'version4.1_dta/alliance_v4.1_by_directed_yearly.dta') %>% read.dta()

# Include only post 1960 data
ally = ally[ally$year>=1960,]

# Subset to relevant vars
ally = ally[,c(
	'ccode1','ccode2','state_name1','state_name2','year',
	'dyad_end_year', 'dyad_st_year',
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
###############################################################

###############################################################
# Subset to alliance events
ally$anyally = ally$defense | ally$neutrality | ally$entente
###############################################################

###############################################################
# Calc S scores
ally$allyweighted = 3*ally$defense + 2*ally$neutrality*(1 - ally$defense) + ally$entente*(1 - ally$defense)*(1 - ally$neutrality)
ccs = c(ally$ccode1, ally$ccode2) %>% unique() %>% sort()
yrs = 1960:2012
ally = ally[!is.na(ally$dyad_end_year),]

allyArray = array(0, 
	dim = c(length(ccs), length(ccs), length(yrs)),
	dimnames = list(ccs, ccs, yrs)
	)

for(i in 1:dim(ally)[1]){
	allyArray[char(ally$ccode1[i]), char(ally$ccode2[i]), char(ally$year[i])] = ally$allyweighted[i]
	allyArray[char(ally$ccode2[i]), char(ally$ccode1[i]), char(ally$year[i])] = ally$allyweighted[i]
}

twoS = function(mat, i, j, wvec){
	r1 = mat[i,]
	r2 = mat[j,]
	dmax = sum(wvec)
	deltmax = max(mat) - min(mat)
	d = abs(r1 - r2) %*% wvec/dmax
	return(1 - 2*d/dmax)
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
	
sArray = allyArray*0

for(i in 1:dim(sArray)[3]){
	tmp = 1-matrixS(allyArray[,,i])
	# stdz
	sArray[,,i] = ( tmp - mean(tmp) ) / sd(tmp)
}

save(sArray, file = paste0(pathDataBin,'sArray.rda'))
###############################################################