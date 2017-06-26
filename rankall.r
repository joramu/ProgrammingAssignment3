rankall<-function(outcome,num="best") {
        
        
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        
        #Check State Validity
        
      
        
        ##Check outcome validity
        ##check outcome validity
        if(outcome != "heart attack" && outcome !="heart failure" && outcome!="pneumonia")
        {
                stop("invalid outcome")
        }
        
        if (outcome=="heart attack")
        {
                data<-data[, c(2,7,11)]
                
        }
        
        if (outcome=="heart failure")
        {
                
                data<-data[, c(2,7,17)]
        }
        
        if (outcome=="pneumonia")
        {
                
                data<-data[, c(2,7,23)]
        }
        
        #
        data = as.data.frame(data)
        colnames(data)<-c("HospitalName","State", "Outcome")
        data[,3]=suppressWarnings(as.numeric(data[,3]))
        
        data<-data[!is.na(data$Outcome),]
        
        data<-data[order(data$Outcome,data$HospitalName),]
        
        
        data<-split(data, data$State)
         
        #No Need To Order Data, Already Done before
        
        RankedHospital<-lapply(data, function(x) {
                
                
                if(num=="best")
                {
                        return (c(x$HospitalName[1], x$State[1]))
                }
                else if(num=="worst")
                {
                        
                        return (c(x$HospitalName[nrow(x)], x$State[nrow(x)]))
                }
                else if(num>nrow(x))
                        
                        return (c(NA,x$State[1]))
                else
                        
                        return (c(x$HospitalName[num], x$State[num]))
                
        }
        
        )
       
        RankedHospital<-do.call(rbind, RankedHospital)

        RankedHospital<-data.frame(RankedHospital)
        
        colnames(RankedHospital)<-c("Hospital","State")
        
        return(RankedHospital)
      
        
        
}