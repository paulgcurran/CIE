#' Resampled internal reliability
#'
#' This function generates scores for resampled internal reliability, a measure of within-person consistency.  NOTE: This function may take a minute or more to run. Be patient!
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' resampIntRel()


# function 7 - resampled internal reliability

resampIntRel<-function(Data1,scales){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<<-Data1
  }
  
  
  rm(x)
  ##set number of items from the dimensions of the input file
  dimen<-dim(Data1)
  nItems<-dimen[2]
  subscales<-nItems/scales
  subscalesH<-subscales/2
  
  if (nItems%%scales!=0) {
    print ("You have uneven scales.")
  }
  
  if (subscalesH%%1!=0) {
    print ("You have a odd number of items in subscales.")
  }
  
  ##variable 'scales' will assume equal length sequential
  
  
  
  
  ##building constraints for while loop
  #dim<-dim(Data1)
  persons<-dimen[1]
  x<-0
  
  
  
  ##creating empty variable to be filled by while loop
  resampIntRel<-NULL
  
  for (x in 1:persons) {
    r<-100
    values<-matrix(nrow=r)
    
    for (z in 1:r){
      corMatrix<-NULL
      corMatrix<-matrix(nrow=scales,ncol=2)
      j<-NULL
      k<-NULL
      t<-NULL
      
      for (s in 1:scales) {
        j<-((s-1)*subscales)+1
        k<-j+1
        t<-((s*subscales))
        odd<-NULL
        even<-NULL
        set<-sample(j:t,subscales,replace=FALSE)
        firstItems<-set[1:subscalesH]
        secondItems<-set[(subscalesH+1):subscales]
        first<-data.frame(Data1[x,firstItems])
        second<-data.frame(Data1[x,secondItems])
        corMatrix[s,1]<-mean(unlist(first),na.rm = TRUE)
        corMatrix[s,2]<-mean(unlist(second),na.rm = TRUE)
        rm(j,k,t,first,second,firstItems,secondItems)
      }
      
      vfirst<-corMatrix[,1]
      vsecond<-corMatrix[,2]
      
      values[z]<-cor(vfirst,vsecond,use="pairwise.complete.obs");
      rm(vfirst,vsecond)
       
    }
    
    resampIntRel[x]<-mean(values,na.rm=TRUE)
    
  }
  
  
  
return(resampIntRel)
  
# display data file with appended data
#Data2
# generate basic stats of person total correlations
summary(resampIntRel)
# generate histogram of calculated values
hist(resampIntRel, breaks=50, col="blue")


rm(r,nItems,persons)


}