#!/usr/bin/env Rscript
## Permutation testing. Parses permutations for each real sampling.
## Also gets limits for calculating max(T) p values in next step. 
library(tidyverse)

args = commandArgs(trailingOnly=TRUE)
infile=args[1] 

### for test runs 
##infile="runs/cand/cand.it2002.stat.csv"
indir <- str_remove(infile,"/[a-z]+.it[0-9]+.stat.csv")
outdir <- paste(indir,".counts",sep="")
if (!dir.exists(outdir)){
  dir.create(outdir,recursive=TRUE)
}

outroot <- sub(".csv","",infile)
outroot <- sub(".gz","",outroot)
outroot <- str_replace(outroot,indir,outdir)
##out1 <- paste(outroot,".pval.csv",sep="")
##out2 <- paste(outroot,".limits.csv",sep="")
out <- paste(outroot,".qftype.pval2.csv.gz",sep="")

  
if (!file.exists(out)){
  print(paste("Making ",out,sep=""))
alld <- as_tibble(read.csv(infile,header=T))
alld <- alld %>% rename(ndogs=n,value=mean) 
  
real <- alld %>% filter(type!="random") %>% distinct() 
rand <- alld %>% filter(type=="random") %>% distinct() %>% select(-type,-set) %>% rename(rvalue=value)
highn <- real %>% inner_join(rand)
highn <- highn %>% mutate(high=if_else(value>rvalue,1,0),nperm=1) %>% select(high,value,nperm,qf,i,set)
  
outd <- real %>% inner_join(highn)  %>% rename(nhigh=high,ntotal=nperm) 
outd <- outd %>% select("type","set","qf","i","ndogs","value","ntotal","nhigh") %>% mutate(value=round(value,6))
#write.csv(outd,out1,row.names=F)

limits <- rand %>% mutate(qftype=if_else(str_detect(qf,"F"),"Factor","Question")) %>% group_by(i,qftype) %>% summarize(min=min(rvalue),max=max(rvalue),ntest=length(rvalue))  %>% select(qftype,i,min,max,ntest)
#write.csv(limits,out2,row.names=F)

outd <- outd %>% mutate(qftype=if_else(str_detect(qf,"F"),"Factor","Question"))
dcnts2 <- outd %>% full_join(limits)
#n2s <- dcnts2 %>% group_by(qftype,i,set,qf) %>% count() %>% rename(n2=n)
low2s <- dcnts2 %>% group_by(qftype,i,set,qf) %>% filter(value<min) %>% count() %>% rename(low2=n)
high2s <- dcnts2 %>% group_by(qftype,i,set,qf) %>% filter(value>max) %>% count() %>% rename(high2=n)

dcnts2 <- dcnts2 %>% left_join(low2s) %>% left_join(high2s) %>% replace_na(list(low2=0,high2=0))
dcnts2 <- dcnts2 %>% select(type,set,qftype,qf,i,ndogs,value,ntotal,nhigh,low2,high2)
write.csv(dcnts2,gzfile(out),row.names=F)
}

