complete<- function(dire="specdata",id=1:332)
{
  pathe<-"R:/R_files"
  pathf<-paste(pathe,dire,sep='/')
  files <- list.files(path = pathf, pattern="*.csv", full.names=T, recursive=FALSE)
  com <- data.frame(id=numeric(),nobs=numeric())
  filesid<-files[id]
  for (i in (1:length(filesid)))
  { 
    p <- read.csv(filesid[i])
    b <- sum(complete.cases(p))
    a <- c(id[i],b)
    com <- rbind(com,a)
  }
  colnames(com)<-c("id","nobs")
  return(com)
}  

