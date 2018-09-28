#' Person Total Correlations
#'
#' Generates scores for person total correlations, a measure 
#' of within-person consistency.
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' person1<-c(1,1,1,1,1,1,1,1,1)
#' person2<-c(1,2,3,3,2,1,2,2,2)
#' values<-rbind(person1,person2)
#' personTotalCor(values)


##function 1- person total correlations

##creating data file if one doesn't exist for full version

personTotalCor<-function(Data1){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  #if (reverse==TRUE){
  #  DataR<-Data1
  #}
  rm(x)
  ##old code to run this through psychometric package
  ##new code runs independent of other packages but needs testing
  #install.packages("psych")
  #library(psych)
  #install.packages("psychometric")
  #library(psychometric)
  
  ##finding number of items and number of people
  dimen<-dim(Data1)
  nItems<-dimen[2]
  nPeople<-dimen[1]
  x<-0
  sumData1<-NULL
  personTotal<-NULL
  
  ##creating values of sum scores for each item
  while (x<nItems) {
    x<-x+1;
    sumData1[x]<-sum(Data1[,x],na.rm = TRUE);
  }
  
  rm(x)
  
  ##running correlation for each person
  ##this is corrected person-total through subtraction of own score
  for (i in 1:nPeople) {
    person<-t(Data1[i,]);
    others<-(sumData1-person);
    personTotal[i]<-cor(person,others,use="pairwise.complete.obs");
    rm(person,others);
  }
  
  ##general cleaning
  rm(nItems,nPeople,sumData1)
  # transpose original item by person matrix
  #tData1<-t(Data1)
  # run model to generate item total correlations on transposed matrix
  #modelITCor<-psychometric::item.exam(tData1)
  # pull out item total correlations from model estimates
  #personTotal<-modelITCor$Item.Tot.woi
  #append person totals to original data
  Data2$personTotal<-personTotal
  #rm(modelITCor,personTotal,tData1)
  
  ##returns values that can be stored by user
  return(personTotal)
  
  ##generate basic stats of person total correlations
  summary(Data2$personTotal)
  ##generate histogram of calculated values
  hist(Data2$personTotal, breaks=50, col="blue")
  #print(Data2)
  
}