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
# WHERE SUBSTRING(t.code, 1, 2) IN (',codes,')

# Send queries
codes = c(
	"'01', '02', '03', '04', '05'", # verbCoop
	"'06', '07', '08'", # matlCoop
	"'09', '10', '11', '12', '13'", # verbConf
	"'14', '15', '16', '17', '18', '19', '20'", # matlConf	
	"'060', '061', '062', '063', '064', '070', '071', '072', '073', '074'"
	)
varNames=c( 'verbCoop', 'matlCoop', 'verbConf', 'matlConf', 'matlCoop2' )
tabNames = paste0('my_tables.',varNames,'_netEvolve')

# get matl coop data
dbSendQuery(conn, paste0('DROP TABLE ', tabNames[5]))
dbSendQuery(conn, genQuery(codes[5], varNames[5], tabNames[5]))
matlCoop = dbGetQuery(conn, paste0("SELECT * FROM ", tabNames[5],";") )	
####

####
# Save
save(matlCoop, file=paste0(inPath, 'matlCoop_icews.rda'))
####