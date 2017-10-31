#' Data Source Info
#' 
#' Class to connect in a database
#' 
#'@import methods
#' 
#'@slot user user of database
#'
#'@slot title Title of database
#'
#'@slot accessDriver the database access driver
#' 
#'@slot host host of the database (e.g. local host)
#'
#'@slot port port the database (e.g. 5432)
#'
#'@slot timeout timeout time of connection
#' 
#'@slot password password of database
#' 
#'@slot db database name
#'
#'@slot encoding encoding of database (e.g.CP1252 )
#'
#'@slot dbtype the type of the database (e.g. POSTGIS)
#'
#'@slot path path of the database
#'
DataSourceInfo <- setClass(
  # Set the name for the class
  "DataSourceInfo",

  # Define the slots
  slots = c(
    title = "character",
    accessDriver="character",
    path="character",
    host="character",
    port="character",
    user="character",
    password="character",
    db="character",
    timeout="character",
    encoding="character",
    dbtype="character"
  ),

  prototype=list(
    title = "",
    accessDriver="",
    path="",
    host="localhost",
    port="5432",
    user="",
    password="",
    db="",
    timeout="4",
    encoding="CP1252",
    dbtype="POSTGIS"

  )

)
