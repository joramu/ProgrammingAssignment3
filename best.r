best<-function(state, outcome) {
        
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##Check State Validity
        if(!any(state==data$State)) {
                stop("invalid state")
        }
        
        
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
        
        
        data = as.data.frame(data)
        colnames(data)<-c("HospitalName", "Outcome")
        data[,2]=suppressWarnings(as.numeric(data[,2]))
        
        data<-data[!is.na(data$Outcome),]
        
        data<-data[order(data$Outcome,data$HospitalName),]
        
        hospital<-data[1,1]
        
        return (hospital)
}
        