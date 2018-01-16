#' Create Spatial Cluster
#'
#' Method for create a spatial cluster
#'
#' @param A3 Track object
#'
#' @param clusterlist Numeric list of cluster positions create spatial objects
#' 
#' @author Deigo Monteiro
#
#' @return cluster of polygons
#'
#' @rdname  createSpatialCluster
#' 
#' @examples 
#' spcluster<-createSpatialCluster(A1,speedCluster(A1,mean(A1@connections$speed),12,min(A1@connections$speed)+4))
#'
#' @export
# given a Track and a list of cluster positions create spatial objects
setGeneric(
  name = "createSpatialCluster",
  def = function(A3, clusterlist)
  {
    
    standardGeneric("createSpatialCluster")
  }
)
#' @rdname  createSpatialCluster
setMethod(
  f = "createSpatialCluster",
  signature = c("Track","list"),
  definition = function(A3, clusterlist)
  {
    firstinteraction = TRUE;
    currentcluster = -1;
    polygons = list();
    for(i in 1:length(clusterlist)){
      if(!is.null(clusterlist[[i]])){

        if((currentcluster!=clusterlist[[i]])&&(firstinteraction==FALSE)){
          if(minx==maxx){
          minx=minx-0.0000001
          maxx=maxx+0.0000001
          }
          if(miny==maxy){
            miny=miny-0.0000001
            maxy=maxy+0.0000001
          }
          x<-c(minx,minx,maxx,maxx,minx)
          y<-c(maxy,miny,miny,maxy,maxy)
          xy<-cbind(x,y)
          polygons <- c(polygons,Polygon(xy))

          firstinteraction=TRUE
          currentcluster=clusterlist[[i]]
        }


        if(firstinteraction==TRUE){
          minx<-as.numeric(A3@sp@coords[i,1])
          miny<-as.numeric(A3@sp@coords[i,2])
          maxx<-as.numeric(A3@sp@coords[i,1])
          maxy<-as.numeric(A3@sp@coords[i,2])
          firstinteraction=FALSE
        }else{


          newminx<-as.numeric(A3@sp@coords[i,1])
          newminy<-as.numeric(A3@sp@coords[i,2])
          newmaxx<-as.numeric(A3@sp@coords[i,1])
          newmaxy<-as.numeric(A3@sp@coords[i,2])

          if(newminx<minx){
            minx=newminx
          }
          if(newminy<miny){
            miny=newminy
          }
          if(newmaxx>maxx){
            maxx=newmaxx
          }
          if(newmaxy>maxy){
            maxy=newmaxy
          }
        }
      }
    }
    if(length(polygons)==0){
      return("No polygons were found.")
    }
    polygons <- Polygons(polygons,"x")
    polygons <- SpatialPolygons(list(polygons))
    attr = data.frame(a=1:1, b=1:1, row.names=c("x"))
    polygons <- SpatialPolygonsDataFrame(polygons, attr)
    return(polygons)
  }
)
