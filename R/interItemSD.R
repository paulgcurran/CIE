#' Inter-item standard deviation (ISD)
#'
#' Generates scores for inter-item standard deviations, 
#' a measure of within-person consistency.
#' @param Data1 This is a matrix of person by item responses to polytomous items.
#' @keywords data cleaning
#' @export
#' @examples
#' person1<-c(1,1,1,1,1,1,1,1,1)
#' person2<-c(1,2,3,3,2,1,2,2,2)
#' values<-rbind(person1,person2)
#' interItemSD(values)


##function x - inter-item standard deviations (ISD)

##creating file for later use of higher level commands

interItemSD<-function(Data1,scales=1,scaleAgg="mean"){
  x<-exists("Data2")
  if(x==FALSE){
    Data2<-Data1
  }
  rm(x)
  ##set number of items from the dimensions of the input file
  dim<-dim(Data1)
  nItems<-dim[2]
  ##set number of people from dimensions of input file
  nPeople<-dim[1]
  
  ##creating variables to populate
  interItemSD<-NULL
  personMean<-NULL
  x<-NULL
  num<-NULL
  den<-NULL
  
  ##iterates over individuals
  if(scales==1){
    for (i in 1:nPeople){
      x<-t(Data1[i,])
      personMean<-mean(x)
      den<-(nItems-1)
      num<-0
      
      ##iterates over items
      for (j in 1:nItems){
        val<-(Data1[i,j]-personMean)
        val2<-val*val
        num<-num+val2
        val<-NULL
        val2<-NULL
      }
      
      vari<-(num/den)
      interItemSD[i]<-(sqrt(vari))
      
      x<-NULL
      personMean<-NULL
      den<-NULL
      num<-NULL
      vari<-NULL
    }
  }
  
  
  
  if(scales>1){
    interItemSD2<-matrix(ncol=scales,nrow=nPeople)
    
    f<-(scales-1)
    g<-(nItems/scales)
    
    #looping over scales
    for(w in 0:f){
      a<-((w*g)+1)
      b<-((w*g)+g)
      
      for (i in 1:nPeople){
        x<-t(Data1[i,a:b])
        personMean<-mean(x)
        den<-(g-1)
        num<-0
        
        ##iterates over items
        for (j in 1:g){
          val<-(Data1[i,j]-personMean)
          val2<-val*val
          num<-num+val2
          val<-NULL
          val2<-NULL
        }
        
        vari<-(num/den)
        interItemSD2[i,w+1]<-(sqrt(vari))
        
        x<-NULL
        personMean<-NULL
        den<-NULL
        num<-NULL
        vari<-NULL
      }
    }
    
    #collapses the scale scores
    for(i in 1:nPeople){
      interItemSD[i]<-mean(interItemSD2[i,1:scales])
    }
    
    return(interItemSD)
    rm(den,num,personMean,val,val2,vari,x)
    
  }
  
}