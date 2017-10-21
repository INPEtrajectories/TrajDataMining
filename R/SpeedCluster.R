#' Speed Cluster
#'
#' Method for check the regions where speed was lower than the defined parameter
#'
#' Order the speed so it will start with the slowest speed cluster
#'
#'@param track Track object
#'
#'@param  avg is the average speed
#'
#'@param minT is the minimun period at the speed
#'
#'@param sl is the speed limit
#'
#'@import rgdal
#'
#'@import trajectories
#'
#'@import sp
#'
#'@import spacetime
#'
#'@return returns regions where speed was lower than the defined parameter
#'
#'@export
#'
setGeneric(
  name = "speedCluster",
  def = function(track, avg, minT , sl)
  {
    
    standardGeneric("speedCluster")
  }
)
#' Speed Cluster
#'
#' Method for check the regions where speed was lower than the defined parameter
#'
#' Order the speed so it will start with the slowest speed cluster
#'
#'@param track Track object
#'
#'@param  avg is the average speed
#'
#'@param minT is the minimun period at the speed
#'
#'@param sl is the speed limit
#'
#'@import rgdal
#'
#'@import trajectories
#'
#'@import sp
#'
#'@import spacetime
#'
#'@return returns regions where speed was lower than the defined parameter
#'
#'@export
#'
#'@rdname speedFilter
#'
# Creeates the speeed clusters Track is a given track, avg is the average speed,
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
