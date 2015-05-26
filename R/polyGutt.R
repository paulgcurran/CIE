#' Polytomous Guttman Errors
#'
#' Generates a score for the number of polytomous Guttman 
#' errors, a measure of within-person consistency.
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' person1<-c(1,1,1,1,1,1,1,1,1)
#' person2<-c(1,2,3,3,2,1,2,2,2)
#' values<-rbind(person1,person2)
#' polyGutt(values)


# function 9 - polytomous Guttman errors

polyGutt<-function(Data1){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  rm(x)

# set number of items from the dimensions of the input file
dim<-dim(Data1)
nItems<-dim[2]
#nItems<-50

meanData1<-NULL


#need to go through and change to k in i and x in j
dim<-dim(Data1)
y<-dim[2]
x<-0

while (x<y) {
  x<-x+1;
  meanData1[x]<-mean(Data1[,x],na.rm = TRUE);
}

meanOrder<-order(meanData1,decreasing=FALSE)

j<-dim[1]
y<-dim[2]
x<-0
z<-0
gOrder<-data.frame(matrix(NA,nrow=j,ncol=y))
while (x<j) {
  x<-x+1
  personOrder<-NULL
  z<-0
  while (z<y) {
    z<-z+1
    personOrder[z]<-Data1[x,meanOrder[z]] 
  }
  
  gOrder[x,]<-personOrder
  rm(personOrder)
} 

x<-0
while (x<j) {
  x<-x+1
  i<-0
  gErrors<-0
  while (i<y-1) {
    i<-i+1
    if ((is.na(gOrder[x,i]))==TRUE){
      i<-i+1
      if (i>y-1){
        break
      }
    }
    
    k<-i+1
    if ((is.na(gOrder[x,k]))==TRUE){
      k<-k+1
      if (k>y){
        break
      }
    }
   
    if ((is.na(gOrder[x,i]))==FALSE){
      if((is.na(gOrder[x,k]))==FALSE){
        if (gOrder[x,i]>gOrder[x,k]) {
          gErrors<-gErrors+1
        }
      }
    }
    
  }
  Data2$gErrors[x]<-gErrors
  #Data2$gErrors[x]<<-gErrors
  rm(gErrors)
}


# display data file with appended data
#Data2
# generate basic stats of person total correlations
summary(Data2$gErrors)
# generate histogram of calculated values
hist(Data2$gErrors, breaks=50, col="blue")

#generate proportional Guttman errors (corrects for missingness)
dim<-dim(Data1)
nPeople<-dim[1]
nItems<-dim[2]
val<-nItems+1
notMissing<-rowSums(!is.na(Data1))
b<-0
for (b in 1:nPeople) {
  Data2$propGuttErrors[b]<-(Data2$gErrors[b])/(notMissing[b])
  #Data2$propGuttErrors[b]<<-(Data2$gErrors[b])/(notMissing[b])
}

gErrors<-Data2$gErrors
propGuttErrors<-Data2$propGuttErrors
vals<-list(gErrors,propGuttErrors)
return(vals)

rm(nPeople)
rm(gOrder,dim,i,j,k,meanData1,meanOrder,x,y,z,nItems,b)
}