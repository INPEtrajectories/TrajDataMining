#' Limited Neighborhood
#'
#' Check the limite of Neighborhood
#'
#' @param track Track obeject
#'
#' @param ini numeric parameter
#'
#' @param minT is the minimun period at the speed
#'
#'@param cln numeric value
#'
#'@param cl numeric list
#'
#'@param avg average value of speed
#'
#'@param sl is the speed limit
#'
setGeneric(
    name = "LimitedNeighborhood",
  def = function(track, ini,minT,cln , cl, avg , sl)
  {
   
    standardGeneric("LimitedNeighborhood")
  }
)

setMethod(
  f = "LimitedNeighborhood",
  signature = c("Track","numeric", "numeric","numeric", "list", "numeric", "numeric"),
  definition = function(track, ini, minT ,cln , cl, avg, sl)
  {
    sln <- SlowestNeighborhood(track,ini,minT,cl)
    sln <- sort(unlist(sln))
    print(sln)
    avgspeed = 0
    time = 0
    for(i in 1:length(sln)){
      # receives the indexes calculated in the slowestneighborhood
      indexsln <- sln[[i]]
      # avgspeed <- receives the values to calculate the weightned average.
      avgspeed = avgspeed + track@connections$speed[indexsln]*track@connections$duration[indexsln]
     time = time + track@connections$duration[indexsln]
     }
    if (is.null(sln) || time < minT || avgspeed/time > avg){
      return (FALSE)
    }
    for(i in 1:length(sln)){
      indexsln <- sln[[i]]
      cl[indexsln]<- cln
      cl[ini]<-cln
    }
    while(TRUE){
      newp = 0
      ln <- sln[[1]]-1
      rn <- sln[[length(sln)]]+1
      if(ln<1 && rn > length(track@connections$speed)){
        break
      }
      else if(ln <1){
        ln <- rn
      }
      else if(rn > length(track@connections$speed)){
        rn <- ln
      }
      if(track@connections$speed[ln]<track@connections$speed[rn]){
        newp <- ln
      }
      if(track@connections$speed[ln]>=track@connections$speed[rn]){
        newp <- rn
      }

      newavg <- avgspeed + track@connections$speed[newp]*track@connections$duration[newp]
      newtime = time + track@connections$duration[newp]

      if(newavg/newtime < avg && track@connections$speed[newp]<sl){
          if(is.null(cl[newp]) || is.na(cl[newp])){
            cl[newp]<- cln
            sln <- c(snl,newp)
            sln<- sort(sln)

          }
        else{
          break}
      }
      else{
        break}
      avgspeed <- newavg
      time <- newtime
      
    }
    return(cl)
  }
)
