#!/usr/bin/env Rscript
## Permutation testing. Parses permutations for each real sampling.
## Also gets limits for calculating max(T) p values in next step. 

args = commandArgs(trailingOnly=TRUE)
infile=args[1] 

### for test runs 
#infile="runs/cand/cand.it61.stat.csv"

library(tidyverse)

outroot <- sub(".csv","",infile)
outroot <- sub(".gz","",outroot)
out1 <- paste(outroot,".pval.csv",sep="")
out2 <- paste(outroot,".limits.csv",sep="")

if (! file.exists(out1)){

alld <- as_tibble(read.csv(infile,header=T))
alld <- alld %>% gather(stat,value,-type,-set,-qf,-i,-n) %>% mutate(qftype=if_else(str_detect(qf,"F"),"Factor","Question")) %>% rename(ndogs=n)
alld <- alld %>% select(-stat)

allqf <- unique(alld$qf)
#allqf <- allqf[1:10]
real <- alld %>% filter(type!="random") %>% distinct() %>% select(-i) 
rand <- alld %>% filter(type=="random") %>% distinct() %>% select(-set,-type)

highn <- tibble( high = integer(), value = numeric(),nperm=integer(),qf=character())

for (qfname in allqf){
  print(qfname)
    real2 <- real %>% filter(qf==qfname) 
    rand2 <- rand %>% filter(qf==qfname) %>% select(-i)
    nperm=length(rand2$value)
    values<-unique(real2$value)
    for (rvalue in values) {
      tmp <- rand2 %>% filter(value>rvalue) %>% summarise(high=n()) %>% mutate(value=rvalue,nperm=nperm,qf=qfname)
      highn <- highn %>% bind_rows(tmp)
    }
}

outd <- real %>% inner_join(highn)  %>% rename(nhigh=high,ntotal=nperm) 
outd <- outd %>% select("type","set","qf","ndogs","value","ntotal","nhigh") %>% mutate(value=round(value,6))
write.csv(outd,out1,row.names=F)

mins <- rand %>% group_by(i,qftype) %>% summarize(min=min(value),max=max(value),ntest=length(value))  %>% select(qftype,i,min,max,ntest)
write.csv(mins,out2,row.names=F)

}
