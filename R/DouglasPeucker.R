#' Douglas Peucker
#'
#' Method that reduces a trajectory spatially
#' 
#'@import geosphere
#' 
#'@param A1 represents a single trajectory followed by a person, animal or object.
#'
#'@param dist distance 
#'
#'@return track reduced trajectory spatially
#' 
#'@rdname douglasPeucker
#'
#'@examples 
#'library(ggplot2)
#'
#'dist <- max(A1@connections$distance)
#'
#'douglasp <- douglasPeucker(A1,dist)
#' 
#'df <- data.frame(x=douglasp@sp@coords[,1],y=douglasp@sp@coords[,2])
#'ggplot(df,aes(x=df$x,y=df$y))+geom_path(aes(group = 1))
#'
#'@author Diego Monteiro
#'
#'@export
setGeneric(
  name = "douglasPeucker",
  def = function(A1, dist)
  {
   
    standardGeneric("douglasPeucker")
  }
)
#' Douglas Peucker RP
#'
#' Method that reduces a trajectory spatially with first point and last point
#'
#' @param A1 track object
#' 
#' @param dist distance
#'
#' @param firstp given first point
#'
#' @param lastp given last point
#'
#' @export
setGeneric(
  name = "douglasPeuckerRP",
  def = function(A1,firstp,lastp, dist)
  {
    
    standardGeneric("douglasPeuckerRP")
  }
)
#' @rdname douglasPeucker
setMethod(
  f = "douglasPeucker",
  signature = c("Track", "numeric"),
  definition = function(A1, dist)
  {
    if (is.null(A1)|| length(A1@sp) < 3){
      return (A1)}

    firstPoint = 1
    lastPoint = length(A1@sp)
    pointIndexsToKeep <- list()
    pointIndexsToKeep[1]
    timelist<-list()
    ylist<-list()
    xlist<-list()


    pointIndexsToKeep[1] = firstPoint
    pointIndexsToKeep[2] = lastPoint
    pointIndexsToKeep <- c(pointIndexsToKeep,douglasPeuckerRP(A1,firstPoint,lastPoint,dist))
    pointIndexsToKeep<- unlist(pointIndexsToKeep, recursive=FALSE)
    pointIndexsToKeep <- sort(pointIndexsToKeep,method="quick")

    saveddf<-as.data.frame(matrix(0,ncol = length(A1@data),nrow=length(pointIndexsToKeep)))
    colnames(saveddf)<-colnames(A1@data)

    for (n in 1:length(pointIndexsToKeep)){
      i <- pointIndexsToKeep[n]
      timelist<-c(timelist,as.character(as.POSIXct(A1@endTime[i])))
      ylist<-c(ylist,A1@sp[i,]@coords[2])
      xlist<-c(xlist,A1@sp[i,]@coords[1])
      saveddf[n,]<-A1@data[i,]
    }

    xlist=unlist(xlist,recursive = FALSE)
    ylist=unlist(ylist,recursive = FALSE)
    dat <- data.frame(x=xlist,y=ylist)
    xy <- coordinates(dat)

    timelist<- unlist(timelist)
    timelist<-as.POSIXct(timelist ,format="%Y-%m-%d %H:%M:%S")
    sti<- STIDF(SpatialPoints(xy, A1@sp@proj4string),timelist,saveddf,timelist)

    AR = Track(sti)

    return(AR)
    # return(pointIndexsToKeep)
  }
)
#' @rdname douglasPeuckerRP
setMethod(
  f = "douglasPeuckerRP",
  signature = c("Track","numeric","numeric","numeric"),
  definition = function(A1,firstp,lastp, dist)
  {
    maxDistance = 0;
    indexFarthest = 1;
    pointIndexsToKeep <- list()
    for (n in  firstp:lastp)
    {
      distance <- dist2gc(A1@sp[firstp],A1@sp[lastp],A1@sp[n])
      if (distance > maxDistance)
      {
        maxDistance = distance;
        indexFarthest = n;
      }
    }

    if (maxDistance > dist && indexFarthest != 1)
    {
      # Add the largest point that exceeds the tolerance
      pointIndexsToKeep <- c(pointIndexsToKeep,indexFarthest)
      pointIndexsToKeep <- c(pointIndexsToKeep,douglasPeuckerRP(A1,firstp,indexFarthest,dist))
      pointIndexsToKeep <- c(pointIndexsToKeep,douglasPeuckerRP(A1,indexFarthest,lastp,dist))
    }
    return (pointIndexsToKeep)
  }
)

