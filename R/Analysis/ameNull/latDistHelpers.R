# Wrapper for all the functions in script
getLatDist = function(file, label, labelName, symmetric=TRUE){
	if(symmetric){
	load(file) # Loads object called latSpace
	out = fit 
	pzMu=getPosInSpace(out[['ULUPM']])
	latDist=getDyadDist(pzMu, ids=rownames(pzMu))
	latDist=stdize(latDist, divMean=FALSE)
	res = melt(latDist)
	res$value[which(res$Var1 == res$Var2)] = NA
	names(res) = c('ccode1', 'ccode2', 'dist')
	res = cbind(res, label)
	names(res)[ncol(res)] = labelName
	return(res)}
	if(!symmetric){
		load(file) # Loads object called latSpace
	pzMu=getPosInSpace(fit[['UVPM']], symmetric = FALSE)
	latDist.U=getDyadDist(pzMu$U, ids=rownames(pzMu$U))
	latDist.V=getDyadDist(pzMu$V, ids=rownames(pzMu$U))

	latDist.U=stdize(latDist.U, divMean=FALSE)
	latDist.V=stdize(latDist.V, divMean=FALSE)

	res.U = melt(latDist.U)
	res.V = melt(latDist.V)
	res.U$value[which(res.U$Var1 == res.U$Var2)] = res.V$value[which(res.V$Var1 == res.V$Var2)]  = NA
	res = cbind(res.U, res.V$value)
	names(res) = c('ccode1', 'ccode2', 'dist.U', 'dist.V')
	res = cbind(res, label)
	names(res)[ncol(res)] = labelName
	return(res)	
	}
}

# Procrustes transformation: rotation and reflection
proc.rr = function(Y,X){
	k<-dim(X)[2]
	A<-t(Y)%*%(  X%*%t(X)  )%*%Y
	eA<-eigen(A,symmetric=T)
	Ahalf<-eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
	t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) }

# Get latent space positions
getPosInSpace = function(latObject, symmetric = TRUE){
	if(symmetric){
		ULUPM = latObject
		eULU = eigen(ULUPM)
		eR<- which( rank(-abs(eULU$val),ties.method="first") <= 2 )
		U<-eULU$vec[,seq(1,2,length=2),drop=FALSE] %*% sqrt(diag(eULU$val[1:2]))
		L<-eULU$val[eR]   
		rownames(U)<-dimnames(ULUPM)[[1]]
		return(U)
	}
	if(!symmetric){
		UVPM = latObject
		R = 2
	    UDV<-svd(UVPM)
	    U<-UDV$u[,seq(1,R,length=R)]%*%diag(sqrt(UDV$d)[seq(1,R,length=R)],nrow=R)
	    V<-UDV$v[,seq(1,R,length=R)]%*%diag(sqrt(UDV$d)[seq(1,R,length=R)],nrow=R)
	    rownames(U)<-rownames(V)<-dimnames(UVPM)[[1]]
	    return(list(U = U, V = V))
	}
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