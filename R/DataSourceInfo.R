#' Data Source Info
#' 
#' Class to connect in a database
#' 
#'@slot user user of database
#' 
#'@slot password password of database
#' 
#'@slot db database name
#' 
#'@export
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
