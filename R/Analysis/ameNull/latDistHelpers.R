# Wrapper for all the functions in script
getLatDist = function(file, label, labelName){
	load(file) # Loads object called latSpace
	pzMu=getPosInSpace(latSpace)
	latDist=getDyadDist(pzMu, ids=rownames(pzMu))
	latDist=stdize(latDist, divMean=FALSE)
	res = melt(latDist)
	res$value[which(res$Var1 == res$Var2)] = NA
	names(res) = c('ccode1', 'ccode2', 'dist')
	res = cbind(res, label)
	names(res)[ncol(res)] = labelName
	return(res)
}

# Procrustes transformation: rotation and reflection
proc.rr = function(Y,X){
	k<-dim(X)[2]
	A<-t(Y)%*%(  X%*%t(X)  )%*%Y
	eA<-eigen(A,symmetric=T)
	Ahalf<-eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
	t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) }

# Get latent space positions
getPosInSpace = function(latObject){
	# Convert to array format
	## didn't do this earlier because arrays take up more space than lists
	U = lapply(latObject, function(x) x$'U')
	nss = length(U)
	n = dim(U[[1]])[1]
	k = dim(U[[1]])[2]
	ids = rownames(U[[1]])
	PZ = U %>% unlist() %>% array(., dim=c(n,k,nss))

	#find posterior mean of Z %*% t(Z)
	ZTZ=matrix(0,n,n)
	for(i in 1:dim(PZ)[3] ) { ZTZ=ZTZ+PZ[,,i]%*%t(PZ[,,i]) }
	ZTZ=ZTZ/dim(PZ)[3] 

	#a configuration that approximates posterior mean of ZTZ
	tmp=eigen(ZTZ)
	Z.pm=tmp$vec[,1:k]%*%sqrt(diag(tmp$val[1:k]))

	#now transform each sample Z to a common orientation
	for(i in 1:dim(PZ)[3] ) { PZ[,,i]=proc.rr(PZ[,,i],Z.pm) }

	# Find posterior mean of country positions
	pzMu=apply(PZ, c(1,2), mean); rownames(pzMu)=ids
	return(pzMu)
}

# Euclidean distance between two points
getDyadDist=function(posMatrix, ids){
	n=nrow(posMatrix)
	distMatrix = matrix(NA, nrow=n, ncol=n, dimnames=list(ids, ids))
	for(ii in 1:length(ids)){
	  for(jj in 1:length(ids)){
	    distMatrix[ii,jj] = sqrt( (posMatrix[ids[ii],1] - posMatrix[ids[jj],1])^2 
	      + (posMatrix[ids[ii],2] - posMatrix[ids[jj],2])^2 )
	  }
	}
	return( distMatrix )
}

# Standardize
stdize=function(x, divMean=TRUE){
	mu=mean(x)
	if(divMean){return(x/mu)}
	if(!divMean){sig=sd(x); return((x-mu)/sig)}
}