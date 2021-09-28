#!/usr/bin/env Rscript
## Step 2 of permutation testing. Compares a sampling of N dogs (ndogs) from each set to random sampling of N dogs (nperm).
## it is the iteration number

args = commandArgs(trailingOnly=TRUE)

library(tidyverse)

prefix <- as.character(args[1])
it <- as.numeric(args[2])

nperm <- 100
ndogs <- 25

### parameters for test runs
##prefix <- "conf"
##it <- 1 

if (prefix=="conf"){
  ndogs <- 50
}
if (prefix=="age"){
  ndogs <- 100
}



indir <- "/seq/vgb/elinor/mutt_paper.perms.final/"
odir <- paste(indir,"runs/",prefix,"/",sep="")
if (!dir.exists(odir)){
  dir.create(odir)
}
dir.create(odir,showWarnings=F)
 print(odir)
 
scoreF <- paste(indir,"input.all.scores.csv",sep="")
setF <- paste(indir,"runs/",prefix,".sets.csv",sep="")
ofile <- paste(odir,prefix,".it",it,".stat.csv",sep="")
print (ofile)
print (setF)

sets <- as_tibble(read.csv(setF,header=T))
scores <- as_tibble(read.csv(scoreF,header=T))  
sets <- sets %>% inner_join(scores) %>% distinct()
scores <- scores %>% distinct() %>% filter(qf %in% sets$qf)

stats <- sets %>% group_by(type,set,qf) %>% sample_n(ndogs) %>% summarise(mean=mean(value),n=length(value),i=0)   %>% select(type,set,qf,mean,n,i)
write.table(stats,ofile,row.names=F,col.names=T,append=F,sep=",")

for (i in 1:nperm){
  rstats <- scores %>% group_by(qf) %>% sample_n(ndogs) %>% summarise(mean=mean(value),n=length(value),i=i)
  rstats <- rstats %>% mutate(type="random",set="NA")  %>% select(type,set,qf,mean,n,i)
  write.table(rstats,ofile,row.names=F,col.names=F,append=T,sep=",")
}




