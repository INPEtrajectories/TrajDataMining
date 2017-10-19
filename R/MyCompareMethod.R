#' Sigled diff track
#'
#' My compare method get distances between 2 Track objects for each point in time where they overlap and create a corresponding line
#'
#' @param tr1 Track object
#'
#' @param tr2 Track object
#'
#' @return a difftrack object
#' 
#' @import xts
#'
#' @export
setClass("singledifftrack",
         slots=c(track1 ="Track", track2 = "Track",
                 conns1 = "SpatialLinesDataFrame")
)
## compare tracks
setGeneric(
  name = "mycompare",
  def = function(tr1, tr2) standardGeneric("mycompare")
)

## get distances between 2 Track objects for each point in time where they overlap
## extend each track with these points
## create corresponding lines
## returns a difftrack object
mycompare.track <- function(tr1, tr2) {
  
  if (!(xts::first(tr1@endTime) < xts::last(tr2@endTime) && xts::first(tr2@endTime) < xts::last(tr1@endTime)))
    stop("Time itervals don't overlap!")
  if (!identicalCRS(tr1, tr2))
    stop("CRS are not identical!")
  crs <- CRS(proj4string(tr1))
 
  track1.df <- cbind(as.data.frame(tr1)[c(coordnames(tr1), "time")])
  track2.df <- cbind(as.data.frame(tr2)[c(coordnames(tr2), "time")])
  # intervals timestamps fall in
  

  ivs1 <- findInterval(track1.df$time, track2.df$time)
  ivs2 <- findInterval(track2.df$time, track1.df$time)
  # find points and create new extended data frames
  

  newTrack1.df <- findPoints(track2.df, track1.df, ivs2)
  newTrack2.df <- findPoints(track1.df, track2.df, ivs1)
  # points on the original
  

  conns12 <- merge(newTrack2.df, track1.df, "time")
  conns21 <- merge(track2.df, newTrack1.df, "time")

  if(length(conns12$time)>0){
  conns12 <- lineConnections(conns12, crs)
  }
  if(length(conns21$time)>0){
  conns21 <- lineConnections(conns21, crs)
  }
  # extended tracks
  newTrack1 <- STIDF(SpatialPoints(cbind(newTrack1.df$x, newTrack1.df$y), crs), newTrack1.df$time, data.frame(1:nrow(newTrack1.df)))
  newTrack2 <- STIDF(SpatialPoints(cbind(newTrack2.df$x, newTrack2.df$y), crs), newTrack2.df$time, data.frame(1:nrow(newTrack2.df)))
  newTrack1 <- Track(newTrack1)
  newTrack2 <- Track(newTrack2)

  if(class(conns12)[1]=="SpatialLinesDataFrame"&&class(conns21)[1]=="SpatialLinesDataFrame"){
    new("difftrack", track1 = newTrack1, track2 = newTrack2, conns1 = conns12, conns2 = conns21)
  }
  else if(!class(conns12)[1]=="SpatialLinesDataFrame"){
    new("singledifftrack", track1 = newTrack1, track2 = newTrack2, conns1 = conns21)
  }
  else if(!class(conns21)[1]=="SpatialLinesDataFrame"){
    new("singledifftrack", track1 = newTrack1, track2 = newTrack2, conns1 = conns12)

  }

}

setMethod("mycompare", signature("Track"), mycompare.track)

## finds corresponding points for track1 on track2
findPoints <- function(tr1, tr2, ivs) {
  x <- tr2[,1]
  y <- tr2[,2]
  time <- tr2[,3]
  for (i in 1:nrow(tr1)) {
    if (!ivs[i] == 0 && !ivs[i] == nrow(tr2)) {
      iv <- ivs[i]
      tdiff1 <- tr1$time[i] - tr2$time[iv] # diff between timestamp and start of the interval it falls in
      tdiff2 <- tr2$time[iv+1] - tr2$time[iv] # diff between timestamps (calculated here because it often varies)
      ratio <- as.numeric(tdiff1)/as.numeric(tdiff2)
      x1 <- tr2[iv,1] # segment coordinates
      y1 <- tr2[iv,2]
      x2 <- tr2[iv+1,1]
      y2 <- tr2[iv+1,2]
      x <- c(x, x1 + ratio * (x2 - x1)) #find point
      y <- c(y, y1 + ratio * (y2 - y1))
      time <- c(time, tr1$time[i])
    }
  }
  newTrack <- data.frame(x, y, time)
  newTrack <- newTrack[!duplicated(newTrack),] # remove duplicates
  newTrack <- newTrack[order(newTrack$time),] # sort by timestamp
  newTrack
}

## creates SpatialLines
lineConnections <- function(conns, crs) {
  Lines <- list()
  coords1 <- cbind(conns[,2], conns[,3])
  coords2 <- cbind(conns[,4], conns[,5])
  for (i in 1:nrow(conns)) {
    Lines <- c(Lines, list(Lines(Line(rbind(coords1[i,], coords2[i,])), ID = i)))
  }
  sl <- SpatialLines(Lines, crs)
  dists <- SpatialLinesLengths(sl)
  sl <- SpatialLinesDataFrame(sl, data.frame(time = conns$time, dists), FALSE)
  sl
}

