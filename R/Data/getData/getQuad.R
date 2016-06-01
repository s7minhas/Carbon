####
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/Carbon/R/setup.R')
	load('~/Dropbox/Research/icewsServerInfo.rda') }
####	

####
# connect to sql db
mysqlSetup(user=icewsSN, pw=icewsPwd, host=icewsHost, db='event_data')
on.exit(dbDisconnect(conn))
####

####
# Find descriptions for event types
rs = dbSendQuery(conn, 'select * from eventtypes')
eTypes = fetch(rs, n=-1)
dbClearResult(rs)

ugh = c(41, 42, 47, 48, 58, 65, 66, 78, 79, 80, 82, 85, 43, 44, 45, 46, 49)
slice = eTypes[which(eTypes$eventtype_ID %in% ugh),]
slice[,c(2,7,3)]
slice$code
####

####
# Create tables
genQuery = function(codes, varName, tableName){
	paste0('CREATE TABLE ',tableName,' AS
	SELECT YEAR(e.event_date) AS year
	  , MONTH(e.event_date) AS month
	  , cSource.ISOA3Code AS source_country
	  , cTarget.ISOA3Code AS target_country
	  , COUNT(*) AS ',varName,'
	FROM simple_events e
	  JOIN eventtypes t ON e.eventtype_id = t.eventtype_ID
	  JOIN countries cSource ON e.source_country_id = cSource.id
	  JOIN countries cTarget ON e.target_country_id = cTarget.id
	WHERE t.code IN (',codes,')	
	GROUP BY YEAR(e.event_date), MONTH(e.event_date), e.source_country_id, e.target_country_id')
}

# Send queries
codes = c(
	"'030', '031', '0311', '0312', '0313', '0314', '032', '033', '0331', '035', '036', '037', '050', '051', '052', '054', '057'",
	"'060', '061', '062', '063', '064', '070', '071', '072', '073', '074'"
	)
varNames=c( 'verbCoop', 'matlCoop' )
tabNames = paste0('my_tables.',varNames,'_carbon')

# get matl coop data
coopList = lapply(1:length(codes), function(ii){
	dbSendQuery(conn, paste0('DROP TABLE ', tabNames[ii]))
	dbSendQuery(conn, genQuery(codes[ii], varNames[ii], tabNames[ii]))
	mat = dbGetQuery(conn, paste0("SELECT * FROM ", tabNames[ii],";") )	
	return(mat)
	} )
####

####
# Save
save(coopList, file=paste0(pathDataBin, 'coop_icews.rda'))
####