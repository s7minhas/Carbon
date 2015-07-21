if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Carbon/R/setup.R') }

###############################################################
# Downloaded data manually from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
## on July 7, 2015: 5pm
load(paste0(pathDataRaw,'Voeten/undata-213.RData')); un=x ; rm(list='x')
###############################################################

###############################################################
un$year = unlist(lapply(strsplit(char(un$date), '-'), function(x) x[1]))

tmp = un[un$year==1955,]

tmp[which(tmp$uniquename %in% c('South Africa', 'Spain')),c('unres','ccode','vote')]
###############################################################