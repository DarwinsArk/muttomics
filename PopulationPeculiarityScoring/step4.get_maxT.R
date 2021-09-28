#!/usr/bin/env Rscript
## Get factor/question-wide counts for calculating corrected p values

library(tidyverse)
args = commandArgs(trailingOnly=TRUE)
infile=args[1] 

###infile <- "runs/cand/cand.it60.stat.pval.csv"
limitfile <- str_replace(infile,".pval.csv",".limits.csv")
outfile <- str_replace(infile,".pval.csv",".qftype.pval2.csv")

if (!file.exists(outfile)){ 
ndogs <- tibble(type=c("age","conf","cand"),n=c(100,50,25))

d <- as_tibble(read.csv(infile,header=T))
d <- d %>% mutate(qftype=if_else(str_detect(qf,"F"),"Factor","Question"))
d$it2 <- c(1:nrow(d))

lim <- as_tibble(read.csv(limitfile,header=T))

d2 <- d %>% full_join(lim)
n2s <- d2 %>% group_by(qftype,it2) %>% count() %>% rename(n2=n)
low2s <- d2 %>% group_by(qftype,it2) %>% filter(value<min) %>% count() %>% rename(low2=n)
high2s <- d2 %>% group_by(qftype,it2) %>% filter(value>max) %>% count() %>% rename(high2=n)

d <- d %>% left_join(n2s) %>% left_join(low2s) %>% left_join(high2s) %>% replace_na(list(low2=0,high2=0))
d <- d %>% left_join(ndogs)
d <- d %>% select(type,set,qftype,qf,n,value,ntotal,nhigh,n2,low2,high2)
write.csv(d,outfile,row.names=F)

}
