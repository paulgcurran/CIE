#' Long string, general
#'
#' Generates a score for the longest continuous string of each
#'  response, a measure of within-person consistency.
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' person1<-c(1,1,1,1,1,1,1,1,1)
#' person2<-c(1,2,3,3,2,1,2,2,2)
#' values<-rbind(person1,person2)
#' longStringFull(values) 




# function 12 - long strings, longest string of each response

longStringFull<-function(Data1){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  rm(x)

# set number of items from the dimensions of the input file
dim<-dim(Data1)
nItems<-dim[2]
persons<-dim[1]
minR<-min(Data1,na.rm = TRUE)
maxR<-max(Data1,na.rm = TRUE)
kPractical<-maxR-minR+1
#rm(longString)
longString<-data.frame(matrix(nrow=persons))
#namesV<-c("longStringA","longStringB","longStringC","longStringD","longStringE","longStringF","longStringG","longStringH","longStringI")


#building the necessary vectors based on number of response options

if (kPractical==1){
  print("There is only one response in the data, this isn't going to end well.")
}
if (kPractical>1){
  longString$longStringA<-0
  longString$longStringB<-0
  if(kPractical>2){
    longString$longStringC<-0
    if (kPractical>3){
      longString$longStringD<-0
      if (kPractical>4){
        longString$longStringE<-0
        if (kPractical>5){
          longString$longStringF<-0
          if(kPractical>6){
            longString$longStringG<-0
            if (kPractical>7){
              longString$longStringH<-0
              if (kPractical>8){  
                longString$longStringI<-0
                if (kPractical>9){
                  print("You have more than 9 response categories?  This isn't going to work very well.")
                }
              }
            }
          }
        }
      }
    }
  }
}

longStringVal<-longString[-1]
rm(longString)
Data3<-Data1
Data3[is.na(Data3)==TRUE]<-(-99)
#Data3<<-Data3

# now have a matrix that just needs to be populated by series of for loops

nItemsR<-nItems-1
x<-0
countV<-0
responseList<-NULL

for (r in minR:maxR){
  countV<-countV+1
  responseList[countV]<-r
  
  for (x in 1:persons){
    
    longLength<-1
    length<-1
    
    for (k in 1:nItemsR){
      
      if (Data3[x,k]==r){
        
        if (Data3[x,k]==Data3[x,k+1]){
          length<-length+1
          #does this take into account a long string that ends on the last item?
        } 
        else {
          if (length>longLength) {
            longLength<-length            
          } 
          if (k==nItemsR){
            if (length>longLength) {
              longLength<-length
              
            } 
          }
          length<-1
        }
        
      }
      
      longStringVal[x,countV]<-longLength
      
    }
  }
}

Data2<-cbind(Data2,longStringVal)


return(longStringVal)


# if the matrix of long string counts is interesting, comment this out to keep it
# rm(longStringVal)


# display data file with appended data
#Data2
# generate basic stats of person total correlations

# generate histogram of calculated values


rm(dim,k,length,nItems,nItemsR,persons,x,countV,kPractical,longLength,maxR,minR,r,responseList,Data3)

}