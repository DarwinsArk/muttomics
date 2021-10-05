#!/usr/bin/env Rscript
## Step 5 of permutation testing. Concantenates results for each iteration of permutation testing.  
args = commandArgs(trailingOnly=TRUE)
library(tidyverse)

settype <- as.character(args[1])
##settype <- "cand"
indir = paste("runs/",settype,".counts",sep="")
out <- paste("runs.",settype,".counts.csv",sep="")
files <- list.files(path=indir, pattern=paste("stat.qftype.pval2.csv.gz",sep=""), full.names=TRUE, recursive=FALSE)
alld <- tibble(type = character(), set=character(),qf=character(),ndogs=integer(), value = numeric(),ntotal=integer(),nhigh=integer(),low2=integer(),high2=integer())

for (file in files){
  print(file)
  d <- as_tibble(read.csv(file,header=T))
      #d <- d %>% filter(!is.na(value)&value!="NA"&value!="0")
  d <- d %>% group_by(type,set,qf,ndogs) %>% summarize(value=sum(value),ntotal=sum(ntotal),nhigh=sum(nhigh),low2=sum(low2),high2=sum(high2))
  d <- d %>% select(colnames(alld))
  alld <- alld %>% bind_rows(d) %>% group_by(type,set,qf,ndogs) %>% summarize(value=sum(value),ntotal=sum(ntotal),nhigh=sum(nhigh),low2=sum(low2),high2=sum(high2))
}

alld <- alld %>% mutate(value=value/ntotal) %>% rename(avgValue=value)
write.csv(alld,out,row.names=F)
print(out)



