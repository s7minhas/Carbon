# Clear workspace
rm(list=ls())

####################################
# Set up paths
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	dpth='~/Dropbox/Research/Carbon/'
	gpth='~/Research/Carbon/'
	pathDataRaw=paste0(dpth, 'data/components/')
	pathDataBin=paste0(dpth, 'data/binaries/')
	pathGraphics=paste0(dpth, 'graphics/')
	pathResults=paste0(dpth, 'results/') }

 if(Sys.info()["user"]=="maxgallop" ){
	 dpth='~/Dropbox/Carbon/'
	 gpth='/Users/maxgallop/Documents/Carbon/'
	 pathDataRaw=paste0(dpth, 'data/components/')
	 pathDataBin=paste0(dpth, 'data/binaries/')
	 pathGraphics=paste0(dpth, 'graphics/')
	 pathResults=paste0(dpth, 'results/') }

####################################

####################################
# Load helpful libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	} }

toLoad=c(
	'foreign', 'RMySQL', # load data
	'cshapes', # R pkgs to get data
	'countrycode', # Matching countries
	'reshape2', 'dplyr', 'doBy', # Data manip
	'ggplot2', 'grid', 'xtable', 'tikzDevice',  # plotting/output
	'amen', 'MASS', # stat pkgs
	'magrittr', # other
	'foreach', 'doParallel', # Parallelization	
	'ROCR' # Performance
	)

# Run function over lib vector
loadPkg(toLoad)

## gg theme
theme_set(theme_bw())

## Please note version of each package in sessInfo.tex, especially countrycode
# sessFile = file(paste0(gpth, 'R/sessInfo.tex'))
# sessionInfo() %>% toLatex(., locale=FALSE) %>% writeLines(., con=sessFile)
# close(sessFile)
####################################

####################################
# Helpful functions
char = function(x) { as.character(x) }
num = function(x) { as.numeric(char(x)) }
cname = function(x) { countrycode(x, 'country.name', 'country.name') }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
substrRight = function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }
stdz = function(x, na=TRUE){ (x - mean(x, na.rm=na))/sd(x, na.rm=na) }
mysqlSetup = function(user=NULL, pw=NULL, db=NULL, host=NULL) {
	tryCatch(conn <<- dbConnect(MySQL(), user=user, password=pw, 
		dbname=db, host=host), 
	error=function(e) warning("MySQL connection does not work") )
}


# Relational Data Helper Functions
source(paste0(gpth, 'R/relDataHelpers.R'))

# Time Series Data Helper Functions
source(paste0(gpth, 'R/tsDataHelpers.R'))

# Binary performance Helper Functions
source(paste0(gpth, 'R/binPerfHelpers.R'))
####################################

####################################
# Load panel dataset
load(paste0(pathDataBin, 'panel.rda'))

# Add 2013 to panel
p13 = panel[panel$year==2012,]
p13$year = 2013
p13$ccodeYear = paste0(p13$ccode, p13$year)
p13$cnameYear = paste0(p13$cname, p13$year)
panel = rbind(panel, p13)
rm(list='p13')

# Helper to add more years later
panelyear<-function(dataset, styear, endyear){
fulldata<-list()
for ( i in 1:length(dataset[,1])){
	fulldata[[i]] <- cbind(dataset[i,], year=styear[i]:endyear[i], row.names = NULL)
}
fulldata1 <- do.call(rbind, fulldata)
return(fulldata1)
}
####################################

####################################
# vector of objects from setup file
setupObjects = c(ls(), 'setupObjects')
####################################