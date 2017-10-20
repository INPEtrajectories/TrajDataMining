#' Ow Meratnial By Collection
#'
#' Method that reduces a set of trajectories spatiotemporally
#'
#' @param A1 TracksCollection object
#'
#' @param dist Distance of object
#'
#' @param speed Speed of object
#'
#' @return Trajectory spatiotemporally reduced
#'
#' @export
setGeneric(
  name = "owMeratniaByCollection",
  def = function(A1, dist, speed)
  {
    
    standardGeneric("owMeratniaByCollection")
  }
)

setMethod(
  f = "owMeratniaByCollection",
  signature = c("TracksCollection", "numeric", "numeric"),
  definition = function(A1, dist, speed)
  {
    PartnerList <- list()
    compressed <- list()
    i = 1
    for (n in 1:length(A1@tracksCollection)){
      print("Tracks")
      print(n)
      for (m in 1:length(A1@tracksCollection[[n]]@tracks)){
        compressed[m] <- owMeratniaBy(A1@tracksCollection[[n]]@tracks[[m]], dist, speed)
     }
      PartnerList <- c(PartnerList,Tracks(compressed))
      compressed <- NULL
      compressed <- list()
    }
      return (TracksCollection(PartnerList))}

  )
