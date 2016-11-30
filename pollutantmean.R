
pollutantmean<- function(dire="specdata",pollutant,id=1:332)
{
  pathe<-"R:/R_files"
  pathf<-paste(pathe,dire,sep='/')
  files <- list.files(path = pathf, pattern="*.csv", full.names=T, recursive=FALSE)
  filesid<-files[id]
  r <- numeric()
  for (i in (1:length(filesid)))
    { 
      p <- read.csv(filesid[i])
      if (pollutant=="sulfate")
         {  r <- c(r,p$sulfate)  }
      else if (pollutant=="nitrate")
         {  r <- c(r,p$nitrate)  }
    }
  return(mean(r,na.rm=TRUE))   
}
