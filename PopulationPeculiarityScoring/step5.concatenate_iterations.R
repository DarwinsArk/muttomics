#!/usr/bin/env Rscript
## Step 5 of permutation testing. Concantenates results for each iteration of permutation testing.  
args = commandArgs(trailingOnly=TRUE)
library(tidyverse)

settype <- as.character(args[1])
indir = paste("runs/",settype,"/",sep="")
out <- paste("runs.",settype,".counts.csv",sep="")
files <- list.files(path=indir, pattern=paste("stat.qftype.pval2.csv",sep=""), full.names=TRUE, recursive=FALSE)
alld <- tibble(type = character(), set=character(),qf=character(),n=integer(), nreal=integer(),value = numeric(),ntotal=integer(),nhigh=integer(),n2=integer(),low2=integer(),high2=integer())

for (file in files){
      d <- read.csv(file,header=T)
  d <- d %>% filter(!is.na(value)&value!="NA"&value!="0")
  d <- d %>% filter(!str_detect(qf,"T"))
  d <- d %>% group_by(type,set,qf,n) %>% summarize(nreal=n(),value=sum(value),ntotal=sum(ntotal),nhigh=sum(nhigh),n2=sum(n2),low2=sum(low2),high2=sum(high2))
  d <- d %>% select(colnames(alld))
  alld <- alld %>% bind_rows(d) %>% group_by(type,set,qf,n) %>% summarize(nreal=sum(nreal),value=sum(value),ntotal=sum(ntotal),nhigh=sum(nhigh),n2=sum(n2),low2=sum(low2),high2=sum(high2))
}
alld <- alld %>% mutate(value=value/nreal) %>% rename(avgValue=value)
write.csv(alld,out,row.names=F)
print(out)



