#'Conversão de index pra track
#'
#'@param Track Track object
#' 
#'@param list an index list
#'
#'@return track object
setGeneric(
  name = "IndexToTrack",
  def = function(A1, index)
  {

    standardGeneric("IndexToTrack")
  }
)

setMethod(
  f = "IndexToTrack",
  signature = c("Track", "list"),
  definition = function(A1, index)
  {
    timelist<-list()
    ylist<-list()
    xlist<-list()

    pointIndexsToKeep<- unlist(index, recursive=FALSE)
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
    for(n in 1:length(timelist)){
      print(timelist[n])
      if(is.na(timelist[n])){
        timelist[n]=(timelist[n-1]+timelist[n+1])/2 
      }
    }
    timelist<-as.POSIXct(timelist ,format="%Y-%m-%d %H:%M:%S")

    sti<- STIDF(SpatialPoints(xy, A1@sp@proj4string),timelist,saveddf,timelist)

    AR = Track(sti)

    return(AR)


  }
)
