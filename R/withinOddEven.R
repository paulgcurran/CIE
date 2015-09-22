#' Odd-even within person correlations
#'
#' Generates scores for odd-even within person correlations, 
#' a measure of within-person consistency.
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' person1<-c(1,1,1,1,1,1,1,1,1)
#' person2<-c(1,2,3,3,2,1,2,2,2)
#' values<-rbind(person1,person2)
#' withinOddEven(values)


##function 2 - odd-even within person correlations

##creating file for later use of higher level commands

withinOddEven<-function(Data1,scales){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
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
  coefOddEven<-NULL
  
  for (x in 1:persons) {
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
      oddItems<-seq(j,t,2)
      evenItems<-seq(k,t,2)
      odd<-data.frame(Data1[x,oddItems])
      even<-data.frame(Data1[x,evenItems])
      corMatrix[s,1]<-mean(unlist(odd),na.rm = TRUE)
      corMatrix[s,2]<-mean(unlist(even),na.rm = TRUE)
      rm(j,k,t,odd,even,oddItems,evenItems)
    }
    vOdd<-corMatrix[,1]
    vEven<-corMatrix[,2]
    
    coefOddEven[x]<-cor(vOdd,vEven,use="pairwise.complete.obs");
    rm(vOdd,vEven)
  }
  
  
  
  ##older code for displaying variables
  # display data file with appended data
  #Data2
  # generate basic stats of person total correlations
  summary(coefOddEven)
  # generate histogram of calculated values
  hist(coefOddEven, breaks=50, col="blue")
  
  #coefOddEven<-Data2$coefOddEven
  ##returns values for user to store
  return(coefOddEven)
  
  ##cleaning remaining variables
  rm(dim,evenItems,nItems,oddItems,x,persons)
  
  
}
