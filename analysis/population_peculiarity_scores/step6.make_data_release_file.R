#!/usr/bin/env Rscript
## Step 6 of permutation testing. Concatenates results for all ndogs in one file 
args = commandArgs(trailingOnly=TRUE)

out <- "DarwinsArk_20191115_survey_permutations.csv"

sets <- as_tibble(read.csv("input.all.sets.csv",header=T))
nsets <- sets %>% group_by(set,type,qf) %>% count() %>% rename(ndogs_available=n)

library(tidyverse)
files <- list.files(pattern = "\\.mperm.csv$")
alld <- tibble(id=integer(),idtype = character(), type=character(),set=character(),ndogs=integer(), avg_real_value=numeric(),p=numeric(),z=numeric(),pcorr=numeric(),nperm=numeric())
alldcomp <- tibble(id=integer(),idtype = character(), type=character(),set=character(),ndogs=integer(), avg_real_value=numeric(),p=numeric(),z=numeric(),pcorr=numeric())

for (file in files){
  print(file)
  d <- as_tibble(read.csv(file,header=T))
  d <- d %>% left_join(nsets)
  dim(d)
  d <- d %>% mutate(idtype=if_else(str_detect(qf,"Q"),"question",if_else(str_detect(qf,"F"),"factor","other")))
  d <- d %>% filter(!str_detect(qf,"T")) %>% mutate(id=as.numeric(str_remove(str_remove(qf,"Q"),"F")))
  d <- d %>% select(c(colnames(alld),ndogs_available))
  d <- d %>% mutate(type=if_else(type=="age","by_year",if_else(type=="conf","confirmed_breed",if_else(type=="cand","candidate_breed","error"))))
  alld <- alld %>% bind_rows(d)
  
}  

## remove sets with too little data
max_qf <- alld %>% select(idtype,id) %>% distinct() %>% group_by(idtype) %>% count() %>% rename(ntot=n)
counts <- alld %>% group_by(idtype,type,set) %>% count() %>% arrange(n) %>% left_join(max_qf) %>% mutate(frac=n/ntot)

## remove any by_year sets with less than complete data
bad_age <- counts %>% filter(type=="by_year"&frac<1) %>% select(type,set) %>%  distinct()
alld <- alld %>% anti_join(bad_age)

## remove any by_year sets with fewer than 500 available dogs
bad_age <- alld %>% select(type,set,ndogs_available) %>% distinct() %>% filter(type=="by_year") %>% group_by(type,set) %>% summarize(min=min(ndogs_available)) %>% filter(min<500) %>% select(type,set)
alld <- alld %>% anti_join(bad_age)

## remove any candidate_breed sets with scores for <25% of questions
bad_breed <- counts %>% filter(idtype=="question"&str_detect(type,"breed")&frac<0.25) %>% select(type,set) %>%  distinct()
alld <- alld %>% anti_join(bad_breed)

alld <- alld %>% mutate(p_2tail=if_else(p>0.5,1-p,p))
alld <- alld %>% arrange(idtype,id,type,set,ndogs)
write.csv(alld,out,row.names=F)


