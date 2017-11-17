#' Speed filter
#'
#' A speed filter that filters out trajectory observations whose speeds are above a user-defined maximum velocity
#'
#' @param A1 Represents a single trajectory followed by a person, animal or object
#'
#' @param speed Is the maximum speed parameter
#'
#'@author Diego Monteiro
#'
#'@examples
#'library(ggplot2)
#'
#'speed <- min(A1@connections$speed)
#'
#'sf <- speedFilter(A1,speed)
#'
#'df <- data.frame(x=sf@sp@coords[,1],y=sf@sp@coords[,2])
#'
#'ggplot(df,aes(x=df$x,y=df$y))+geom_path(aes(group = 1), arrow = arrow(),color='blue')
#'
#' @export
setGeneric(
  name = "speedFilter",
  def = function(A1,speed)
  {
    standardGeneric("speedFilter")
  }
)
#'@rdname speedFilter
setMethod(
  f = "speedFilter",
  signature = c("Track", "numeric"),
  definition = function(A1, speed)
  {

    if (is.null(A1)|| length(A1@sp) < 3){
      return (A1)}


    firstPoint = 1
    lastPoint = length(A1@sp)
    pointIndexsToKeep <- list()
    pointIndexsToKeep[1]
    pointIndexsToKeep[1] = firstPoint
    pointIndexsToKeep[2] = lastPoint
    size <- (length(A1@sp)-1)
    for (i in 1:size){
            sp<- A1@connections$speed[i]
            if(sp<speed){
              pointIndexsToKeep <- c(pointIndexsToKeep,i)
              }
      }


    return(IndexToTrack(A1,pointIndexsToKeep))

  }
)
