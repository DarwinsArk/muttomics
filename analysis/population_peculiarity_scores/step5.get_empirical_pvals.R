#!/usr/bin/env Rscript
## Step 6 of permutation testing. Calculates pvalues from concatenated results of all iterations of permutation testing.  
args = commandArgs(trailingOnly=TRUE)
library(tidyverse)

files <- list.files(pattern = "\\.counts.csv$")


for (infile in files){
  outPerm <- str_replace(infile,".csv",".mperm.csv")
  all <- as_tibble(read.csv(infile,header=T) ) 
  all <- all %>% rename(avg_real_value=avgValue,nperm=ntotal)
  all <- all %>% mutate(p=(nhigh+1)/(nperm+1)) %>% mutate(p=if_else(p==1,1-(1/(nperm+1)),p))
  all <- all %>% mutate(n2=nperm) %>% mutate(pcorr=1-((high2+low2+1)/(nperm+1)))  %>% mutate(pcorr=if_else(pcorr==0,1/(nperm+1),pcorr))
  all <- all %>% mutate(z=round(qnorm(p),5))
  all <- all %>% select(qf,type,set,ndogs,nperm,avg_real_value,p,z,pcorr)
  write.csv(all,outPerm,row.names=F)
  print(outPerm)
}




