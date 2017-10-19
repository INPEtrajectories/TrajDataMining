#' Slowest Neighborhood
#'
#' Method for check slowest neighborhood
#'
#' @param track Track obeject
#'
#' @param ini numeric parameter
#'
#' @param minT is the minimun period at the speed
#'
#' @param cl numeric list
#'
setGeneric(
  name = "SlowestNeighborhood",
  def = function(track, ini,minT, cl)
  {
    
    standardGeneric("SlowestNeighborhood")
  }
)

setMethod(
  f = "SlowestNeighborhood",
  signature = c("Track","numeric", "numeric", "list"),
  definition = function(track, ini, minT, cl)
  {

    if (is.null(track)|| length(track) < 2){
      return (0)}
    count = 0
    duration = 0
    incl = 1
    incr = 1
    lista <- list()
    while(duration < minT && count<length(track@connections$speed) ){
      li <- ini-incl
      ri <- ini+incr

      if(li<=0){
        li=1
      }
      lp <- track@connections$speed[li]

      if(ri>length(track@connections$speed)){
        ri=length(track@connections$speed)

      }
      rp <- track@connections$speed[ri]
      if(lp<=rp && (is.null(cl[li][[1]])|| is.na(cl[li]))){
        lista <- c(lista,li)
        duration = duration + track@connections$duration[li]
        incl = incl + 1
        }
      else if(lp>rp && (is.null(cl[ri][[1]])|| is.na(cl[ri]))){
        lista <- c(lista,ri)

        duration = duration + track@connections$duration[ri]
        incr = incr + 1
      }
      else{
        print("break")
        break
      }
count = count + 1
    }

return(lista)
  }
)
