corr <- function(directory,threshold=0)
{
  pathe<-"R:/R_files"
  pathf<-paste(pathe,directory,sep='/')
  files <- list.files(path = pathf, pattern="*.csv", full.names=T, recursive=FALSE)
  vect <- c()
  for (i in (1:length(files)))
   { 
      p <- read.csv(files[i])
      b <- sum(complete.cases(p))
      a <- complete.cases(p)
      d <- p[a,]
      if (b > threshold)
        {
          k <- cor(d$sulfate,d$nitrate)
          vect <- c(vect,k)
        }
    }
   return(vect)
}