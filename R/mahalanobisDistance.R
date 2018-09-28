#' Mahalanobis distance
#'
#' Generates a score for the Mahalanobis distance of 
#' any response, a measure of outlier analysis.
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' person1<-c(1,1,1,1,1,1,1,1,1)
#' person2<-c(1,2,3,3,2,1,2,2,2)
#' values<-rbind(person1,person2)
#' malahanobisDist(values)


##function xx - Mahalanobis distance

##creating copy dataset for use in running all functions at once

mahalanobisDist<-function(Data1,scoreReport="squared",scales=1,missingImpute="median",scaleAgg="mean",...){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  rm(x)
  items<-ncol(Data2)
  if((items%%scales)!=0){
    print("You have uneven scales - this might not work right.")
  }
  
  if(missingImpute=="median"){
    ###median scale replacement for M2 calculations
    f<-(scales-1)
    g<-(items/scales)
    for(i in 1:nrow(Data2)){
      for(w in 0:f){
        a<-((w*g)+1)
        b<-((w*g)+g)
        for(j in a:b){
          if((is.na(Data2[i,j]))==TRUE){
            Data2[i,j]<-median(as.numeric(Data2[i,a:b]),na.rm=TRUE)
          } 
        }
      }
      rm(a,b,i,j,w)     
    }
    rm(f,g)
  }
  mahalanobisDx<-(matrix(nrow=nrow(Data2),ncol=scales))
  
  if(scales==1){
    mahalanobisDx[,1]<-mahalanobis(Data2,center=FALSE,cov=(var(Data2)))
  }
  else{
    f<-(scales-1)
    g<-(items/scales)
    for(w in 0:f){
      a<-((w*g)+1)
      b<-((w*g)+g)
      mahalanobisDx[,(w+1)]<-mahalanobis(Data2[,a:b],center=FALSE,cov=(var(Data2[,a:b])))
      rm(a,b)     
    }
    rm(f,g)
  }
  mahalanobisD<-matrix(nrow=(nrow(Data2)),ncol=1)
  
  if(scales>1){
    if(scaleAgg=="mean"){
      for(i in 1:nrow(Data2)){
        mahalanobisD[i,]<-mean(mahalanobisDx[i,1:scales])
      }
    }
    if(scaleAgg=="sum"){
      for(i in 1:nrow(Data2)){
        mahalanobisD[i,]<-sum(mahalanobisDx[i,1:scales])
      }
    }
  }
  
  mahalanobisD<-as.data.frame(mahalanobisD)
  mahalanobisD5<-sqrt(mahalanobisD)
  
  
  
  ##some cleaning and storing from earlier versions
  
  #Data2$mahalanobisD<-mahalanobisD[,1]
  #Data2$longLength[x]<-longLength
  #Data2$longResponse[x]<-longResponse
  #Data2$longLength[x]<<-longLength
  #Data2$longResponse[x]<<-longResponse
  #rm(longLength,longResponse)
  
  
  # display data file with appended data
  #Data2
  # generate basic stats of mahalanobisD
  summary(mahalanobisD)
  
  ##generate histogram of calculated values
  hist(mahalanobisD[,1], breaks=10, col="blue")
  
  
  ##generate scores to return, user can store as values
  #rm(a,b)
  
  
  if(scoreReport=="squared"){
    return(mahalanobisD)
  }
  if(scoreReport=="sqroot"){
    return(mahalanobisD5)
  }
  
  
  ##cleaning remaining variables
  
  
  rm(mahalanobisD,mahalanobisD5,mahalanobisDx,i,items,scaleAgg,scales,w)
}