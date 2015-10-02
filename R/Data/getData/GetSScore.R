
library(foreign)

ally.dyad = read.dta("~/Dropbox/carbon/Data/Components/version4.1_dta/alliance_v4.1_by_dyad_yearly.dta")

ally.dyad$anyally = ally.dyad$defense | ally.dyad$neutrality | ally.dyad$entente

ally.dyad = ally.dyad[ally.dyad$year != 0,]


ally.dyad$allyweighted = 3*ally.dyad$defense + 2*ally.dyad$neutrality*(1 - ally.dyad$defense) + ally.dyad$entente*(1 - ally.dyad$defense)*(1 - ally.dyad$neutrality)
ccs = unique(c(ally.dyad$ccode1, ally.dyad$ccode2))


ally.array = array(0, dim = c(length(ccs), length(ccs), max(ally.dyad$dyad_end_year) - min(ally.dyad$dyad_st_year) + 1))


for(i in 1:dim(ally.dyad)[1]){
	r = which(ccs == ally.dyad$ccode1[i])
	c = which(ccs == ally.dyad$ccode2[i])
	sl = ally.dyad$year[i] - min(ally.dyad$dyad_st_year) + 1
	ally.array[r,c,sl] = ally.array[c,r,sl] = ally.dyad$allyweighted[i]
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


matrixS = function(mat, wvec){
	mat = as.matrix(mat)
	smat = apply(as.matrix(1:dim(mat)[1]),1, rowS, mat = mat, wvec = wvec)
	return(smat)	
	}
	
S.array = ally.array*0

for(i in 1:dim(S.array)[3]){
	S.array[,,i] = matrixS(ally.array[,,i], wvec)
}


save(S.array, file = "~/Dropbox/carbon/Data/Components/sarray.zip")
	
