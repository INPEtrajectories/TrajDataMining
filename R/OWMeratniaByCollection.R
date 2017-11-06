#' Ow Meratnial By Collection
#'
#' Method that reduces a set of trajectories spatiotemporally
#'
#' @param A1 represents a collection of trajectories followed by different persons, animals or objects
#'
#' @param dist Distance of object
#'
#' @param speed Speed of object
#'
#' @return Trajectory spatiotemporally reduced
#' 
#'@author Diego Monteiro
#' 
#' @examples 
#' 
#'library(magrittr)
#'
#'library(ggplot2)
#'ow <-owMeratniaByCollection(tracksCollection,13804.84 ,0.03182201) %>% coordinates()
#'
#'df <- data.frame(x=ow[,1],y=ow[,2])
#'
#'ggplot(df,aes(x=x,y=y))+geom_path(aes(group = 1), arrow = arrow(),color='blue')
#'@rdname owMeratniaByCollection
#'
#' @export
setGeneric(
  name = "owMeratniaByCollection",
  def = function(A1, dist, speed)
  {
    
    standardGeneric("owMeratniaByCollection")
  }
)
#'@rdname owMeratniaByCollection
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
