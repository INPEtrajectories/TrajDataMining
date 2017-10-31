#' Direction Cluster
#'
#'that given a Track and maximum change parameter, returns regions where direction changed more than the defined parameter
#'
#' @param track Track object
#'
#' @param minT is the minimun period at the speed
#'
#' @param minD is the minimun direction change
#'
#' @param tolerance is the maximum change parameter
#'
#' @return returns regions where direction changed more than the defined parameter
#' 
#' @rdname directionCluster
#'
#' @export
setGeneric(
  name = "directionCluster",
  def = function(track, minD, minT , tolerance)
  {
   
    standardGeneric("directionCluster")
  }
)
#' @rdname directionCluster
setMethod(
  f = "directionCluster",
  signature = c("Track","numeric", "numeric", "numeric"),
  definition = function(track, minD, minT , tolerance)
  {

    if (is.null(track)|| length(track) < 2){
      return (0)}
    cl<-list()
    clusterId = 1
    clusterOpen = FALSE
    tracksize <- length(track@connections$direction)
    tol = tolerance
    clusterini = 0
    lastindex=1

    for(n in 1:(tracksize-1)){
      dirc = track@connections$direction[n]-track@connections$direction[n+1]

      if(dirc<0){
        dirc = dirc*(-1)
      }
      if(dirc >= minD){
        cl[n]<-clusterId
        if(!clusterOpen){
          clusterini = n
        }
        clusterOpen = TRUE
      }
      else{
        if(clusterOpen){
          i=1
          tol=tolerance
          while(tol >0 && (n+i)<(tracksize+1)){
            dirc = track@connections$direction[n+i]-track@connections$direction[n+i+1]
            if(dirc<0){
              dirc = dirc*(-1)
            }
            if(dirc>=minD){
              i = i+1
              lastindex = n+i
              break

            }
            else{
              tol = tol-1
              if(tol==0){
                lastindex = n+i
              }
            }
          }
        }

        if(lastindex<(n+tolerance)){
          for(j in n:lastindex){

            cl[j]<-clusterId
          }
          n=lastindex
        }
        else{
          ctime=0
          for (j in clusterini:n){
            ctime = ctime + track@connections$duration[j]
          }
          if(ctime>minT){

            n=lastindex
            for (j in clusterini:n){
              cl[j]<-clusterId
            }
            clusterId=clusterId+1
          }
          else{
            for (j in clusterini:n){
              # print("j e clusterId")
              print(j)
              print(clusterId)
              cl[j]<--1
            }
          }
          clusterOpen = FALSE
        }
      }
    }
    return (cl)

  }
)
