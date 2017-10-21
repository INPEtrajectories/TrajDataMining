#' Ow Meratinia By
#'
#' Method that reduces trajectories spatiotemporally
#'
#'@import geosphere
#'
#'@param A1 track object
#'
#'@param dist distance of object
#'
#'@param speed speed of object
#'
#'@return reduces trajectories spatiotemporally
#'
#'@rdname owMeratniaBy
#'
#'@export

setGeneric(
  name = "owMeratniaBy",
  def = function(A1, dist, speed)
  {
   
    standardGeneric("owMeratniaBy")
  }
)
#'@rdname owMeratniaBy
setMethod(
  f = "owMeratniaBy",
  signature = c("Track", "numeric", "numeric"),
  definition = function(A1, dist, speed)
  {

    if (is.null(A1)|| length(A1@sp) < 3){
      return (A1)}

    stop_point = FALSE
    anchor = 2
    firstPoint = 1
    lastPoint = length(A1@sp)
    pointIndexsToKeep <- list()
    pointIndexsToKeep[1]
    pointIndexsToKeep[1] = firstPoint
    pointIndexsToKeep[2] = lastPoint
size <- (length(A1@sp)-1)
    for (e in anchor:size){
      if (stop_point==FALSE){
        for (i in anchor:e){
          if (stop_point==FALSE && i < size){
            dte<- as.numeric(A1@endTime[e]-A1@endTime[anchor])
            dti<- as.numeric(A1@endTime[i]-A1@endTime[anchor])
            xyline <- (A1@sp[e,]@coords-A1@sp[anchor,]@coords)*(dti/dte)+A1@sp[anchor,]@coords
            sp1<- A1@connections$speed[i-1]
            sp2<- A1@connections$speed[i]
            dsp <- sp1-sp2
            if((dsp < 0) ==TRUE){
              dsp <-dsp*-1
            }
            dcos <- distCosine(xyline, A1@sp[i,]@coords, r=6378137)
            if(is.nan(dcos)){
              dcos=0;
            }
            if(dcos>dist||dsp>speed){
              stop_point=TRUE
            }
          }
        }
      }
             if(stop_point==TRUE){
               pointIndexsToKeep <- c(pointIndexsToKeep,i)
               anchor = i
               stop_point=FALSE
             }
    }

return(IndexToTrack(A1,pointIndexsToKeep))

  }
)
