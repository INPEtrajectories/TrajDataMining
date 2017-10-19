#' Right size verifier
#' 
#' @param diffTracks difftrack
#' 
#' @param begintime begin time 
#' 
#' @param endtime end time
#' 
#' @param sizeMultiplier number will multiplier the diff
#' 
#' @return boolean
#'
#'
#'
setGeneric(
  name = "RightSize",
  def = function(diffTracks,begintime,endtime,sizeMultiplier)
  {
  
    standardGeneric("RightSize")
  }
)

setMethod(
  f = "RightSize",
  signature = c("difftrack","character","character","numeric"),
  definition = function(diffTracks,begintime,endtime,sizeMultiplier)
  {
    size1<-length(which(diffTracks@conns1@data[1]>begintime & diffTracks@conns1@data[1]<endtime))
    size2<-length(which(diffTracks@conns2@data[1]>begintime & diffTracks@conns2@data[1]<endtime))

    if(size1==size2){
      return(TRUE)
    }
    else if(size1>size2){
      if(size1>(size2*sizeMultiplier)){
        return(FALSE)
      }
      else{
        return(TRUE)
      }
    }
    else{
      if(size2>(size1*sizeMultiplier)){
        return(FALSE)
      }
      else{
        return(TRUE)
      }
    }
  }
)
