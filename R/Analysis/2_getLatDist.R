if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){ source('~/Research/Carbon/R/setup.R') }
############################

############################
# Get dyadic dist from each yrly latent space
yrs=1965:2012
############################

############################
# Pull out dyadic distance measure
# Source script with helpful functions
source( paste0(gpth, 'R/Analysis/ameNull/latDistHelpers.R') )

# Files to pull
latRdas = paste0(pathResults, 'ameLatSpace') %>% list.files() %>% .[grepl('_rescale_noABeffects.rda', .)] %>% paste0(pathResults, 'ameLatSpace/', .)

getLatAngle = function(x, lab){
	load(x) ; U=getPosInSpace(fit$ULUPM)
	V<-U ; vscale<-1
	mu<-sqrt( apply(U^2,1,sum) ) ; mv<-sqrt( apply(V^2,1,sum) )
	u<-diag(1/mu)%*%U ; v<-diag(1/mv)%*%V*vscale
	rownames(u) = names(U)
	angles = matrix(acos(u[,1]), nrow(u), 1)
	rownames(angles) = rownames(U)

	dists = expand.grid(rownames(angles), rownames(angles))
	dists = dists[dists$Var1 != dists$Var2,]
	dists$ang1 = angles[dists$Var1] ; dists$ang2 = angles[dists$Var2]
	dists$value = abs( dists$ang1 - dists$ang2 )
	dists$year = lab
	return(dists[,c('Var1','Var2','value','year')])
}

latAngle = lapply(yrs, function(yr){
	file = paste0(pathResults, 'ameLatSpace/', yr, '_rescale.rda')
	return( getLatAngle(file, lab=yr) )
	}) %>% do.call('rbind', .)
############################

############################
# Add id variables to both datasets
latAngle$dyadid = with(latAngle, paste(Var1, Var2, year, sep='_'))

# Remove i=j rows
latAngle = with( latAngle, latAngle[which(Var1 != Var2),] )
############################

############################
# Save
save(latAngle, file=paste0(pathResults, 'latDist.rda'))
############################