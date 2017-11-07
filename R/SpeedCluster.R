#' Speed Cluster
#'
#'Method for check the regions where speed was lower than the defined parameter
#'
#' Order the speed so it will start with the slowest speed cluster
#'
#'@import rgdal
#'
#'@import trajectories
#'
#'@import sp
#'
#'@import spacetime
#'
#'@param track represents a single trajectory followed by a person, animal or object
#'
#'@param  avg is the average speed of track
#'
#'@param minT is the minimun period at the speed of track
#'
#'@param sl is the speed limit of track
#'
#'@return returns regions where speed was lower than the defined parameter
#'
#'@author Diego Monteiro
#'
#'@examples
#'avgSpeed <- mean(A1@connections$speed)
#'
#'minSpeed <- min(A1@connections$speed)
#'
#'speed <- speedCluster(A1,avgSpeed,minSpeed,586)
#'@export
#'

setGeneric(
  name = "speedCluster",
  def = function(track, avg, minT , sl)
  {
    
    standardGeneric("speedCluster")
  }
)

#'@rdname speedCluster
#'
# Creates the speeed clusters Track is a given track, avg is the average speed,
# minT is the minimum period at the speed. And sl is the speed limit
setMethod(
  f = "speedCluster",
  signature = c("Track","numeric", "numeric", "numeric"),
  definition = function(track, avg, minT , sl)
  {

    if (is.null(track)|| length(track) < 2){
      return (0)}
    cl<-list()
    clusterId = 1;
    # Order the speed so it will start with the slowest speed cluster
    speedOrder <- order(track@connections$speed)
    for(n in 1:length(speedOrder)){
    x<-LimitedNeighborhood(track,speedOrder[n],minT,clusterId , cl, avg , sl)
      if(is.list(x)){
        clusterId = clusterId + 1
        cl <- x
      }
    }
   return (cl)

  }
)
