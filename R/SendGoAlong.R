#' send Partner Pairs To DataBase
#'
#' Method that sends found partners to a PostGIS database
#'
#' @import RPostgreSQL
#'
#' @param dataframe Dataframe list
#'
#' @param dataSourceInfo A object class dataSourceInfo
#'
#' @param tablename Name of table 
#'
#' @return send the partners list for a database
#'

setGeneric(
  name = "sendPartnerPairsToDB",
  def = function(dataframe,dataSourceInfo,tablename)
  {
    
    standardGeneric("sendPartnerPairsToDB")
  }
)
#' Method that sends found partners to a PostGIS database with object DataSourceInfo
#'
#'@rdname sendPartnerPairsToDB
setMethod(
  f = "sendPartnerPairsToDB",
  signature = c("list","DataSourceInfo","character"),
  definition = function(dataframe,dataSourceInfo,tablename)
  {
    


    partnerframe <- data.frame(dataframe)

    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = dataSourceInfo@db,
                     host = dataSourceInfo@host, port = dataSourceInfo@port,
                     user = dataSourceInfo@user, password = dataSourceInfo@password)
    on.exit(dbDisconnect(con))

    if(!dbExistsTable(con, tablename)){
      sql_command <- paste("CREATE TABLE", tablename,"
                           (
                           begintime timestamp with time zone, endtime timestamp with time zone,id1 text, id2 text
                           )
                           WITH (
                           OIDS=FALSE
                           );
                           ")
# sends the command and creates the table
dbGetQuery(con, sql_command)

}

dbWriteTable(con, tablename,
value = partnerframe, append = TRUE, row.names = FALSE)

##dbDisconnect(con)
  }
)
#' Method that sends found partners to a PostGIS database using object PosgreSQLConnection
#'
#'@rdname sendPartnerPairsToDB

setMethod(
  f = "sendPartnerPairsToDB",
  signature = c("list","PostgreSQLConnection","character"),
  definition = function(dataframe,dataSourceInfo,tablename)
  {
    


    partnerframe <- data.frame(dataframe)


    con <- dataSourceInfo

    if(!dbExistsTable(con, tablename)){
      sql_command <- paste("CREATE TABLE", tablename,"
                           (
                           begintime timestamp with time zone, endtime timestamp with time zone,id1 text, id2 text
                           )
                           WITH (
                           OIDS=FALSE
                           );
                           ")
# sends the command and creates the table
dbGetQuery(con, sql_command)

}

dbWriteTable(con, tablename,
value = partnerframe, append = TRUE, row.names = FALSE)


  }
)
