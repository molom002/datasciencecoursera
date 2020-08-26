rm(list=ls())
setwd("~/Downloads/specdata")
list.files()
pollutantmean<-function(directory, pollutant, id=1:332) {
  filelist<-list.files(path=directory, pattern=".csv", full.names=TRUE)
  values<-numeric()
  for(i in id){
    data<-read.csv(fileslist[i])
    values<-c(values, data[[pollutant]])
  }
  mean(values, na/rm=TRUE)
}
pollutantmean("/Users/mandkhai/Downloads/specdata","sulfate", 1:10)
pollutantmean("/Users/mandkhai/Downloads/specdata","nitrate", 70:72) 
pollutantmean("/Users/mandkhai/Downloads/specdata","sulfate", 34)
pollutantmean("/Users/mandkhai/Downloads/specdata","nitrate")



complete<-function(directory, id=1:332){
  filelist<-list.files(path=directory, pattern=".csv",full.names=TRUE)
  nobs<-numeric()
  for (i in id) {
    data<-read.csv(filelist[i])
    nobs<-c(nobs, sum(complete.cases(data)))
  }
  data.frame(id, nobs)
  }
complete("/Users/mandkhai/Downloads/specdata", 1)
cc <- complete("/Users/mandkhai/Downloads/specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("/Users/mandkhai/Downloads/specdata", 54)
print(cc$nobs)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("/Users/mandkhai/Downloads/specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


corr<-function(directory, threshold=0){
  filelist<-list.files(path=directory, pattern=".csv", full.names=TRUE)
  df<-complete(directory)
  ids<-df[df["nobs"]>threshold,]$id
  corrr<-numeric()
  for (i in ids){
    data<-read.csv(filelist[i])
    dff<-data[complete.cases(data),]
    corrr<-c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
}
cr<-corr("/Users/mandkhai/Downloads/specdata/", 150)
head(cr)         
cr <- corr("/Users/mandkhai/Downloads/specdata/")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("/Users/mandkhai/Downloads/specdata/", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("/Users/mandkhai/Downloads/specdata/", 2000)                
n <- length(cr)                
cr <- corr("/Users/mandkhai/Downloads/specdata/", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
