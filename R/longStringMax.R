#' Long string, specific
#'
#' Generates a score for the longest continuous string of 
#' any response, a measure of within-person consistency.
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' person1<-c(1,1,1,1,1,1,1,1,1)
#' person2<-c(1,2,3,3,2,1,2,2,2)
#' values<-rbind(person1,person2)
#' longStringMax(values)


##function 11 - long strings, longest string and response

##creating copy dataset for use in running all functions at once

longStringMax<-function(Data1,scoreReport="counts"){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  rm(x)
  
  ##set number of items from the dimensions of the input file
  dim<-dim(Data1)
  nItems<-dim[2]
  persons<-dim[1]
  ##creating variables to be populated
  longResponse<-NULL
  Data2$longLength<-NULL
  Data2$longResponse<-NULL
  nItemsR<-nItems-1
  x<-0
  
  ##creating file to work with to account for NA
  Data3<-Data1
  Data3[is.na(Data3)==TRUE]<-(-99)
  
  ##outer loop to iterate over people
  ##inner loop to generate values
  
  for (x in 1:persons){
    
    ##minimum value this can ever be is 1  
    longLength<-1
    length<-1
    
    for (k in 1:nItemsR){
      
      ##checking if adjacent values are the same, iterating    
      if (Data3[x,k]==Data3[x,k+1]){
        response<-Data3[x,k]
        length<-length+1
        #storing value if current longest
        if (length>longLength) {
          longLength<-length
          longResponse<-response
        } 
      } 
      #if values are different, resetting initial counts
      else {
        length<-1
        response<-0
      }
      
    } 
    
    
    ##some cleaning and storing from earlier versions
    Data2$longLength[x]<-longLength
    Data2$longResponse[x]<-longResponse
    #Data2$longLength[x]<<-longLength
    #Data2$longResponse[x]<<-longResponse
    #rm(longLength,longResponse)
    
  }
  
  # display data file with appended data
  #Data2
  # generate basic stats of person total correlations
  summary(Data2$longLength)
  summary(Data2$longResponse)
  ##generate histogram of calculated values
  hist(Data2$longLength, breaks=10, col="blue")
  hist(Data2$longResponse, breaks=10, col="blue")
  
  ##generate scores to return, user can store as values
  rm(longLength,longResponse)
  longLength<-NULL
  longResponse<-NULL
  longLength<-Data2$longLength
  longResponse<-Data2$longResponse
  vals<-list(longLength,longResponse)
  if(scoreReport=="counts"){
    return(longLength)
  }
  if(scoreReport=="response"){
    return(longResponse)
  }
  if(scoreReport=="both"){
    return(vals)  
  }    
  
  
  ##cleaning remaining variables
  rm(dim,k,length,nItems,nItemsR,persons,response,x)
  
  
}