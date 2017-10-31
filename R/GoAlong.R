#' Partner
#'
#' Method to identify if two trajectories are partners
#'
#' @param A1 Track object
#'
#' @param A2 Track object
#'
#' @param dist Distance of object
#'
#' @param maxtime Is the maximum time
#'
#' @param mintime Is the minimum time
#'
#' @param datasource Is object class DataSourceInfo
#'
#' @param tablename The table name
#' 
#' @rdname partner
#' 
#' @examples 
#' 
#' partner(A1,A2,110792,2277,0,FALSE)
#'
#' @return list with begin time and end time of two  objects partner
#' 
#'@export
setGeneric(
  name = "partner",
  def = function(A1, A2, dist, maxtime,mintime,datasource,tablename)
  {
    
    standardGeneric("partner")
  }
)
#' Partner
#' 
#' DataSourceInfo connection
#'
#'@rdname partner
setMethod(
  f = "partner",
  signature = c("Track","Track","numeric","numeric","numeric","DataSourceInfo","character"),
  definition = function(A1, A2, dist, maxtime,mintime,datasource,tablename)
  {
    tempo=maxtime
    
    initialTime = -1
    finalTime = -1
    inCounter = FALSE
    minSize = FALSE
    firstContant = 1
    iniFinList <- list()
    allPartnerList <-list()
    allPartner <- data.frame(begintime=as.POSIXct(character()),
                             endtime=as.POSIXct(character()),
                             id1=character(),
                             id2=character(),
                             stringsAsFactors=FALSE)
    id1="1"
    id2="2"
    # Try to store trajectory unique Id if it does not work try obj id, if it doesn't work give up
    
    if("traj" %in% colnames(A1@data)){
      id1=as.character(levels(A1@data["traj"][[1]]))
    }
    else if("name" %in% colnames(A1@data)){
      id1=as.character(levels(A1@data["name"][[1]]))
    }
    
    # Try to store trajectory unique Id if it does not work try obj id, if it doesn't work give up
    if("traj" %in% colnames(A2@data)){
      id2=as.character(levels(A2@data["traj"][[1]]))
    }
    else if("name" %in% colnames(A2@data)){
      id2=as.character(levels(A2@data["name"][[1]]))
    }
    count = 0
    time  = 0
    if (id1==id2){
      break;
    }
    
    
    if (!(xts::first(A1@endTime) < xts::last(A2@endTime) && xts::first(A2@endTime) < xts::last(A1@endTime)))
      return("Time itervals don't overlap!")
    if (!identicalCRS(A1, A2))
      return("CRS are not identical!")
    
    timeSeries <- mycompare(A2,A1)
    if(class(timeSeries)!="singledifftrack"&&class(timeSeries)!="difftrack"){
      return("Tracks don't match!")
    }
    # Contador para as conexoes
    i = 1;
    j = 1;
    
    if(length(timeSeries@conns1@data$dists)>2){
      for (n in 2:(length(timeSeries@conns1@data$dists)-1)) {
        if (timeSeries@conns1@data$dists[n] <= dist && inCounter==FALSE) {
          initialTime = timeSeries@conns1@data$time[n]
          firstContant=n
          inCounter=TRUE
          
          
        }
        if (timeSeries@conns1@data$dists[n] > dist) {
          time = time + difftime(timeSeries@conns1@data$time[n+1],timeSeries@conns1@data$time[n],units="secs")
          if (time > tempo && inCounter==TRUE){
            finalTime = timeSeries@conns1@data$time[n]
            iniFinList <- c(Begin=as.character(initialTime),End=as.character(finalTime),Id1=id1,Id2=id2)
            timefromstart <- difftime(timeSeries@conns1@data$time[n],timeSeries@conns1@data$time[firstContant],units="secs")
            if(timefromstart > mintime){
              allPartner[nrow(allPartner)+1,]<-c(iniFinList)
            }
            # allPartnerList <- append(allPartnerList,iniFinList)
            iniFinList <- NULL
            inCounter = FALSE
            time=0
            
          }
        }
        i = i + 1
        
        
        if(n==(length(timeSeries@conns1)-1) && inCounter==TRUE){
          finalTime = timeSeries@conns1@data$time[n]
          # iniFinList <- c(Begin=initialTime,End=finalTime,Id1=id1,Id2=id2)
          if(as.numeric(id1)>as.numeric(id2)){
            tempvar <- id1
            id1<-id2
            id2<-id1
          }
          iniFinList <- c(Begin=as.character(initialTime),End=as.character(finalTime),Id1=id1,Id2=id2)
          timefromstart <- difftime(timeSeries@conns1@data$time[n],timeSeries@conns1@data$time[firstContant],units="secs")
          if(timefromstart > mintime){
            allPartner[nrow(allPartner)+1,]<-c(iniFinList)
          }
          # allPartnerList <- append(allPartnerList,iniFinList)
          iniFinList <- NULL
          inCounter = FALSE
        }
      }
    }
    sendPartnerPairsToDB(allPartner,datasource,tablename)
    return (allPartner)
  }
 
)

#' Partner
#' 
#' Use Postgresql connection
#' 
#' @rdname partner
#' 
#'@export

setMethod(
  f = "partner",
  signature = c("Track","Track","numeric","numeric","numeric","PostgreSQLConnection","character"),
  definition = function(A1, A2, dist, maxtime,mintime,datasource,tablename)
  {
    tempo=maxtime
    
    initialTime = -1
    finalTime = -1
    inCounter = FALSE
    minSize = FALSE
    firstContant = 1
    iniFinList <- list()
    allPartnerList <-list()
    allPartner <- data.frame(begintime=as.POSIXct(character()),
                             endtime=as.POSIXct(character()),
                             id1=character(),
                             id2=character(),
                             stringsAsFactors=FALSE)
    id1="1"
    id2="2"
    # Try to store trajectory unique Id if it does not work try obj id, if it doesn't work give up
    
    if("traj" %in% colnames(A1@data)){
      id1=as.character(levels(A1@data["traj"][[1]]))
    }
    else if("name" %in% colnames(A1@data)){
      id1=as.character(levels(A1@data["name"][[1]]))
    }
    
    # Try to store trajectory unique Id if it does not work try obj id, if it doesn't work give up
    if("traj" %in% colnames(A2@data)){
      id2=as.character(levels(A2@data["traj"][[1]]))
    }
    else if("name" %in% colnames(A2@data)){
      id2=as.character(levels(A2@data["name"][[1]]))
    }
    count = 0
    time  = 0
    
    if(id1==id2){
      return(allPartner)
    }
    
    
    if (!(xts::first(A1@endTime) < xts::last(A2@endTime) && xts::first(A2@endTime) < xts::last(A1@endTime)))
      return("Time itervals don't overlap!")
    if (!identicalCRS(A1, A2))
      return("CRS are not identical!")
    
    timeSeries <- mycompare(A2,A1)
    if(class(timeSeries)!="singledifftrack"&&class(timeSeries)!="difftrack"){
      return("Tracks don't match!")
    }
    if(class(timeSeries)=="singledifftrack"){
      return(allPartner)
    }
    # Contador para as conexoes
    i = 1;
    j = 1;
    
    if(length(timeSeries@conns1@data$dists)>2){
      for (n in 2:(length(timeSeries@conns1@data$dists)-1)) {
        if (timeSeries@conns1@data$dists[n] <= dist && inCounter==FALSE) {
          initialTime = timeSeries@conns1@data$time[n]
          firstContant=n
          inCounter=TRUE
          
          
        }
        if (timeSeries@conns1@data$dists[n] > dist) {
          time = time + difftime(timeSeries@conns1@data$time[n+1],timeSeries@conns1@data$time[n],units="secs")
          if (time > tempo && inCounter==TRUE){
            finalTime = timeSeries@conns1@data$time[n]
            # iniFinList <- c(Begin=initialTime,End=finalTime,Id1=id1,Id2=id2)
            iniFinList <- c(Begin=as.character(initialTime),End=as.character(finalTime),Id1=id1,Id2=id2)
            timefromstart <- difftime(timeSeries@conns1@data$time[n],timeSeries@conns1@data$time[firstContant],units="secs")
            if(timefromstart > mintime){
              if(RightSize(timeSeries,as.character(initialTime),as.character(finalTime),3)){
                allPartner[nrow(allPartner)+1,]<-c(iniFinList)
              }
            }
            allPartnerList <- append(allPartnerList,iniFinList)
            iniFinList <- NULL
            inCounter = FALSE
            time=0
            
          }
        }
        i = i + 1
        
        
        if(n==(length(timeSeries@conns1)-1) && inCounter==TRUE){
          finalTime = timeSeries@conns1@data$time[n]
          #iniFinList <- c(Begin=initialTime,End=finalTime,Id1=id1,Id2=id2)
          if(as.numeric(id1)>as.numeric(id2)){
            tempvar <- id1
            id1<-id2
            id2<-id1
          }
          iniFinList <- c(Begin=as.character(initialTime),End=as.character(finalTime),Id1=id1,Id2=id2)
          timefromstart <- difftime(timeSeries@conns1@data$time[n],timeSeries@conns1@data$time[firstContant],units="secs")
          if(timefromstart > mintime){
            if(RightSize(timeSeries,as.character(initialTime),as.character(finalTime),3)){
              allPartner[nrow(allPartner)+1,]<-c(iniFinList)
            }
          }
          # allPartnerList <- append(allPartnerList,iniFinList)
          iniFinList <- NULL
          inCounter = FALSE
        }
      }
    }
    
    sendPartnerPairsToDB(allPartner,datasource,tablename)
    
    # if(id2=="49101" && id1 =="48651"){
    #  assign("truck1", A1, envir = .GlobalEnv)
    #  assign("truck2", A2, envir = .GlobalEnv)
    #  assign("problemgoalong", allPartner, envir = .GlobalEnv)
    #}
    
    return (allPartner)
  }
 )
#' Partner
#' 
#' Use Logical param in database connection
#' 
#' @rdname partner
#' 
#'@export
setMethod(
  f = "partner",
  signature = c("Track","Track","numeric","numeric","numeric","logical","missing"),
  definition = function(A1, A2, dist, maxtime,mintime,datasource,tablename)
  {
    tempo=maxtime
    
    initialTime = -1
    finalTime = -1
    inCounter = FALSE
    minSize = FALSE
    firstContant = 1
    iniFinList <- list()
    allPartnerList <-list()
    allPartner <- data.frame(begintime=as.POSIXct(character()),
                             endtime=as.POSIXct(character()),
                             id1=character(),
                             id2=character(),
                             stringsAsFactors=FALSE)
    id1="1"
    id2="2"
    # Try to store trajectory unique Id if it does not work try obj id, if it doesn't work give up
    
    if("traj" %in% colnames(A1@data)){
      id1=as.character(levels(A1@data["traj"][[1]]))
    }
    else if("name" %in% colnames(A1@data)){
      id1=as.character(levels(A1@data["name"][[1]]))
    }
    
    # Try to store trajectory unique Id if it does not work try obj id, if it doesn't work give up
    if("traj" %in% colnames(A2@data)){
      id2=as.character(levels(A2@data["traj"][[1]]))
    }
    else if("name" %in% colnames(A2@data)){
      id2=as.character(levels(A2@data["name"][[1]]))
    }
    count = 0
    time  = 0
    
    
    
    if (!(xts::first(A1@endTime) < xts::last(A2@endTime) && xts::first(A2@endTime) < xts::last(A1@endTime)))
      return("Time itervals don't overlap!")
    if (!identicalCRS(A1, A2))
      return("CRS are not identical!")
    
    timeSeries <- mycompare(A2,A1)
    if(class(timeSeries)!="singledifftrack"&&class(timeSeries)!="difftrack"){
      return("Tracks don't match!")
    }
    # Contador para as conexoes
    i = 1;
    j = 1;
    
    if(length(timeSeries@conns1@data$dists)>2){
      for (n in 2:(length(timeSeries@conns1@data$dists)-1)) {
        if (timeSeries@conns1@data$dists[n] <= dist && inCounter==FALSE) {
          initialTime = timeSeries@conns1@data$time[n]
          firstContant=n
          inCounter=TRUE
          
          
          
        }
        if (timeSeries@conns1@data$dists[n] > dist) {
          time = time + difftime(timeSeries@conns1@data$time[n+1],timeSeries@conns1@data$time[n],units="secs")
          if (time > tempo && inCounter==TRUE){
            finalTime = timeSeries@conns1@data$time[n]
            # iniFinList <- c(Begin=initialTime,End=finalTime,Id1=id1,Id2=id2)
            iniFinList <- c(Begin=as.character(initialTime),End=as.character(finalTime),Id1=id1,Id2=id2)
            timefromstart <- difftime(timeSeries@conns1@data$time[n],timeSeries@conns1@data$time[firstContant],units="secs")
            if(timefromstart > mintime){
              allPartner[nrow(allPartner)+1,]<-c(iniFinList)
            }
            allPartnerList <- append(allPartnerList,iniFinList)
            iniFinList <- NULL
            inCounter = FALSE
            time=0
            
          }
        }
        i = i + 1
        
        
        if(n==(length(timeSeries@conns1)-1) && inCounter==TRUE){
          finalTime = timeSeries@conns1@data$time[n]
          if(as.numeric(id1)>as.numeric(id2)){
            tempvar <- id1
            id1<-id2
            id2<-id1
          }
          iniFinList <- c(Begin=as.character(initialTime),End=as.character(finalTime),Id1=id1,Id2=id2)
          timefromstart <- difftime(timeSeries@conns1@data$time[n],timeSeries@conns1@data$time[firstContant],units="secs")
          if(timefromstart > mintime){
            allPartner[nrow(allPartner)+1,]<-c(iniFinList)
          }
          allPartnerList <- append(allPartnerList,iniFinList)
          iniFinList <- NULL
          inCounter = FALSE
        }
      }
    }
    # sendPartnerPairsToDB(allPartner,datasource,"trucks_partners50m_15min_p2")
    return (allPartner)
  }
 
)
#' Partner
#' 
#' Use Missing database connection
#' 
#' @rdname partner
#' 
#'@export

setMethod(
  f = "partner",
  signature = c("TracksCollection","missing","numeric","numeric","numeric","missing","missing"),
  definition = function(A1, A2, dist, maxtime,mintime,datasource,tablename)
  {
    tempo=maxtime
    
    
    allPartner <- data.frame(begintime=as.POSIXct(character()),
                             endtime=as.POSIXct(character()),
                             id1=character(),
                             id2=character(),
                             stringsAsFactors=FALSE)
    
    
    
    for (n in 1:length(A1@tracksCollection)) {
     for (m in 1:length(A1@tracksCollection[[n]]@tracks)) {
        
        for (l in n:length(A1@tracksCollection)) {
          for (k in 1:length(A1@tracksCollection[[l]]@tracks)) {
            if(length(A1@tracksCollection[[n]]@tracks[[m]])>5 & length(A1@tracksCollection[[l]]@tracks[[k]])>5 ){
              ga<- partner(A1@tracksCollection[[l]]@tracks[[k]],A1@tracksCollection[[n]]@tracks[[m]],dist,tempo,mintime,FALSE)
            }
            if(class(ga)=="data.frame"){
              if (nrow(ga)>0){
                for(j in 1:nrow(ga)){
                  if(ga[[j,3]]!=ga[[j,4]]){
                    allPartner[nrow(allPartner)+1,]<-ga[j,]
                  }
                  
                }
              }
            }
          }
        }
      }
    }
    
    
    return (allPartner)
    
  }
  
  
  
)
#' Partner
#' 
#' Use Missing database connection and TrackCollection objects
#' 
#' @rdname partner
#' 
#'@export

setMethod(
  f = "partner",
  signature = c("TracksCollection","TracksCollection","numeric","numeric","numeric","missing","missing"),
  definition = function(A1, A2, dist, maxtime,mintime,datasource,tablename)
  {
    tempo=maxtime
    
    
    allPartner <- data.frame(begintime=as.POSIXct(character()),
                             endtime=as.POSIXct(character()),
                             id1=character(),
                             id2=character(),
                             stringsAsFactors=FALSE)
    
    
    
    for (n in 1:length(A1@tracksCollection)) {
      for (m in 1:length(A1@tracksCollection[[n]]@tracks)) {
        
        for (l in 1:length(A1@tracksCollection)) {
          for (k in 1:length(A1@tracksCollection[[l]]@tracks)) {
            if(length(A1@tracksCollection[[n]]@tracks[[m]])>5 & length(A2@tracksCollection[[l]]@tracks[[k]])>5 ){
              ga<- partner(A2@tracksCollection[[l]]@tracks[[k]],A1@tracksCollection[[n]]@tracks[[m]],dist,tempo,mintime,FALSE)
            }
            if(class(ga)=="data.frame"){
              if (nrow(ga)>0){
                for(j in 1:nrow(ga)){
                  if(ga[[j,3]]!=ga[[j,4]]){
                    allPartner[nrow(allPartner)+1,]<-ga[j,]
                  }
                  
                }
              }
            }
          }
        }
      }
    }
    
    
    return (allPartner)
    
  }
  
  
  
)
#' Partner
#' 
#' Use Missing database connection 
#' 
#' @rdname partner
#'
#'@export

setMethod(
  f = "partner",
  signature = c("TracksCollection","Track","numeric","numeric","numeric","missing","missing"),
  definition = function(A1, A2, dist, maxtime,mintime,datasource,tablename)
  {
    tempo=maxtime
    
    
    allPartner <- data.frame(begintime=as.POSIXct(character()),
                             endtime=as.POSIXct(character()),
                             id1=character(),
                             id2=character(),
                             stringsAsFactors=FALSE)
    
    
    
    for (n in 1:length(A1@tracksCollection)) {
   
      for (m in 1:length(A1@tracksCollection[[n]]@tracks)) {
        
        
        if(length(A1@tracksCollection[[n]]@tracks[[m]])>5 ){
          ga<- partner(A2,A1@tracksCollection[[n]]@tracks[[m]],dist,tempo,mintime,FALSE)
        }
        if(class(ga)=="data.frame"){
          if (nrow(ga)>0){
            for(j in 1:nrow(ga)){
              if(ga[[j,3]]!=ga[[j,4]]){
                allPartner[nrow(allPartner)+1,]<-ga[j,]
              }
              
            }
          }
          
        }
      }
    }
    
    
    return (allPartner)
    
  }
  
  
  
)