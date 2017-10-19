#' Speed filter
#'
#' Method to return where speed was higher than the defined parameter
#'
#' @param A1 Track object
#'
#'@param speed is the maximum speed parameter
#'
#' @export
#'
setGeneric(
  name = "speedFilter",
  def = function(A1,speed)
  {
    standardGeneric("speedFilter")
  }
)

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
