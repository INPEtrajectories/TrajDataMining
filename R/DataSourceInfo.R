#' Data Source Info
#' 
#' Class to connect in a database
#' 
#'@import methods
#' 
#'@slot user User of database
#'
#'@slot title Title of database
#'
#'@slot accessDriver The database access driver
#' 
#'@slot host Host of the database (e.g. localhost)
#'
#'@slot port Port the database (e.g. 5432)
#'
#'@slot timeout Timeout time of connection
#' 
#'@slot password Password of database
#' 
#'@slot db Database name
#'
#'@slot encoding Encoding of database (e.g.CP1252 )
#'
#'@slot dbtype The type of the database (e.g. POSTGIS)
#'
#'@slot path Path of the database
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
