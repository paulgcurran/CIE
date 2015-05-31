#' Psychometric Antonymns
#'
#' TGenerates scores for psychometric antonyms, a measure of 
#' within-person consistency.
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' person1<-c(1,1,1,1,1,1,1,1,1)
#' person2<-c(1,2,3,3,2,1,2,2,2)
#' values<-rbind(person1,person2)
#' psychometricAnt(values)


##function 3 - psychometric antonyms 

##creating file for compilation of metrics

psychometricAnt<-function(Data1){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  rm(x)
  
##set number of items from the dimensions of the input file
dim<-dim(Data1)
nItems<-dim[2]
#nItems<-50

##need to pick how many pairs of antonyms used (default to top 5)
####need to modify this to allow for user inputs
nPairs<-5

##need to set minimum value for selected correlations (default to .5)
###need to modify this to allow for user inputs
minCor<-(-.5)

##finding correlations
cors<-cor(Data1,use="pairwise.complete.obs")
#hist(cors)
cors[cors==1]<-NA
#hist(cors)
corsOrder<-order(cors,decreasing=FALSE)
topPairs<-(corsOrder[seq(1,20,2)])
print(corsOrder[1:10])
topCors<-cors[topPairs]
print(topCors)
##displays an error message if any correlations below min
if ((topCors[10])>minCor) {print("WARNING:the correlation of at least one pair of items is below the minimum threshold.")
}  else {print("All correlations are above the minimum threshold.")
} 

#rm(rowPlaceVect,columnPlaceVect)
rowPlaceVect<-NULL
columnPlaceVect<-NULL

##building constraints for while loops
dim<-dim(Data1)
persons<-dim[1]
x<-0

##creating empty variable to be filled by while loop
Data2$psychAntonyms<-NULL

##creating vectors of top pairs
while (x<10) {
  x<-x+1;
  rowPlace<-ceiling(topPairs[x]/nItems);
  columnPlace<-(topPairs[x]%%nItems);
  if (columnPlace==0) {
    columnPlace<-nItems;
  }
  rowPlaceVect[x]<-rowPlace;
  columnPlaceVect[x]<-columnPlace;
  rm(rowPlace,columnPlace);
}

j<-0

##iterating over people

while (j<persons) {
  j<-j+1
  first<-c(Data1[j,rowPlaceVect]);
  second<-c(Data1[j,columnPlaceVect]);
  vFirst<-unlist(first);
  vSecond<-unlist(second);
  #Data2$psychAntonyms[j]<<-cor(vFirst,vSecond,use="pairwise.complete.obs");
  Data2$psychAntonyms[j]<-cor(vFirst,vSecond,use="pairwise.complete.obs");
  #rm(first,second,vFirst,vSecond);
}

##older code for displaying values
# display data file with appended data
#Data2
# generate basic stats of person total correlations
summary(Data2$psychAntonyms)
# generate histogram of calculated values
hist(Data2$psychAntonyms, breaks=50, col="blue")

psychAntonyms<-Data2$psychAntonyms
##returns values for user to store
return(psychAntonyms)

##cleaning remaining variables
rm(cors,columnPlaceVect,corsOrder,dim,first,j,minCor,nItems,nPairs,rowPlaceVect,second,topCors,topPairs,vFirst,vSecond,x,persons)

}
