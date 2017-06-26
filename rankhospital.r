rankhospital<-function(state, outcome,num="best") {
        
        
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        
        #Check State Validity
        
        if (!state  %in% data$State)
        {
                stop("invalid state")
        }
       
        
        ##Check outcome validity
        ##check outcome validity
        if(outcome != "heart attack" && outcome !="heart failure" && outcome!="pneumonia")
        {
                stop("invalid outcome")
        }
        
      if (outcome=="heart attack")
        {
                data<-data[data$State==state, c(2,11)]
                
        }
        
        if (outcome=="heart failure")
        {
                
                data<-data[data$State==state, c(2,17)]
        }
        
        if (outcome=="pneumonia")
        {
                
                data<-data[data$State==state, c(2,23)]
        }
     
        #
        data = as.data.frame(data)
        colnames(data)<-c("HospitalName", "Outcome")
        data[,2]=suppressWarnings(as.numeric(data[,2]))
      
        data<-data[!is.na(data$Outcome),]
        
        data<-data[order(data$Outcome,data$HospitalName),]
        
   
       
        if(num=="best")
        {
                hospName<-data[1,1]      
        }
        else if(num=="worst")
        {
                hospName<-data[nrow(data),1]
        }
        else if(num>nrow(data))
                hospName<-NA
        else
                hospName<-data[num,1]
        
        return (hospName)
        
        
}