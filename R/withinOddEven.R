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


########### function 2 - odd-even within person correlations

withinOddEven<-function(Data1){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  rm(x)
  # set number of items from the dimensions of the input file
  dim<-dim(Data1)
  nItems<-dim[2]
  #nItems<-50
  
  # if odd number of items, disregard the final one
  if (nItems%%2==1) {
    nItems<-nItems-1
  }
  #building sequences of numbers
  oddItems<-seq(1,nItems,2)
  evenItems<-seq(2,nItems,2)
  #building constraints for while loop
  #dim<-dim(Data1)
  persons<-dim[1]
  x<-0
  #creating empty variable to be filled by while loop
  Data2$coefOddEven<-NULL
  
  #iteratively computing calculation within each person, populating new variable
  
  while (x<persons) {
    x<-(x+1);
    odd<-c(Data1[x,oddItems]);
    even<-c(Data1[x,evenItems]);
    vOdd<-unlist(odd);
    vEven<-unlist(even);
    #Data2$coefOddEven[x]<<-cor(vOdd,vEven,use="pairwise.complete.obs");
    Data2$coefOddEven[x]<-cor(vOdd,vEven,use="pairwise.complete.obs");
    rm(odd,even,vOdd,vEven)
  }
  
  
  # display data file with appended data
  #Data2
  # generate basic stats of person total correlations
  summary(Data2$coefOddEven)
  # generate histogram of calculated values
  hist(Data2$coefOddEven, breaks=50, col="blue")
  
  coefOddEven<-Data2$coefOddEven
  return(coefOddEven)
  
  rm(dim,evenItems,nItems,oddItems,x,persons)
  
  
}
