if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }
if(Sys.info()["user"]=="maxgallop"){
	source("/Users/maxgallop/Documents/Carbon/R/setup.R") }
############################

############################
load( paste0(pathDataBin, 'logitModData.rda') )

# Finalize data for modeling
kivs = paste0('lag1_', kivs)
cntrls = paste0('lag1_', cntrls)
splines = paste0("lag1_", splines)

# remove missing data
modData = na.omit( modData[,c(ids, splines, dv, kivs, cntrls)] )

# Create model specifications and run
modForms = lapply(kivs, function(x){
	formula( paste0(dv,' ~ ' , 
		paste(c(x, cntrls, splines), collapse=' + '))) })

# Run model
mods = lapply(modForms, function(x){
	glm(x, data=modData, family='binomial' ) })
names(mods) = gsub('lag1_','',kivs)

lapply(mods, function(x){ summary(x)$'coefficients'[2,,drop=FALSE] }) %>% do.call('rbind',.)

# save results

############################