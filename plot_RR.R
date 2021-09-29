#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)
infile = args[1]

library(tidyverse)
#library(forcats)
#library(ggrepel)
#library(wesanderson)
#library(rstatix)

# define input files

## point to location of folder with paper data release files.
indir <- "../../../data_release/"
rrIn <- paste(indir,"DarwinsArk_20191115_relative_risk.csv",sep="")

facIn <- paste(indir,"DarwinsArk_20191115_factors.csv",sep="")
fac_scoreIn <- paste(indir,"DarwinsArk_20191115_factor_scores.csv",sep="")
breedsIn <- paste(indir,"ReferenceData_breeds.csv",sep="")
dogsIn <- paste(indir,"DarwinsArk_20191115_dogs.csv",sep="")

# get relative risk data
raw <- as_tibble(read.csv(rrIn,header=T,na.strings=c("","<NA>","NA","n/a","#N/A")))

## remove breeds that aren't in reference breed list
breedlist <- as_tibble(read.csv(breedsIn,header=T),na.strings=c("NA","#N/A",""))
raw <- raw %>% filter(breed %in% breedlist$breed_name)

facInfo <- as_tibble(read.csv(facIn,header=T)) %>% filter(factor <= 8) %>% select(factor,name,negative,positive) %>% distinct() 
facInfo <- facInfo %>% rename(string=name) %>% pivot_longer(c(-factor,-string)) %>% rename(tail=name,pheno=value)

## get factor scores (to get breed counts, eventually) 
allF <-  as_tibble(read.csv(fac_scoreIn,header=T,na.strings=c("NA","#N/A",""," ","N/A"))) %>% filter(!is.na(score))

## get dog info (to get breed counts, eventually) 
dogs <- as_tibble(read.csv(dogsIn,header=T),na.strings=c("NA","#N/A",""," ","  ","N/A")) %>% rename(dog=id)
dogs <- dogs %>% filter(!is.na(owner_breed)|!is.na(regseq_breed)) %>% mutate(conf_breed=regseq_breed,cand_breed=if_else(!is.na(regseq_breed),regseq_breed,owner_breed))
dogs <- dogs %>% select(dog,cand_breed,conf_breed) %>% pivot_longer(-dog) %>% filter(!is.na(value)) %>% rename(breedtype=name,breed=value) 
dogs <- dogs %>% filter(breed %in% breedlist$breed_name)
dogs <- allF %>% filter(!is.na(score.norm)) %>% select(dog,factor) %>% distinct() %>% inner_join(dogs) #%>% mutate(breed=str_trim(breed)) %>% filter(str_length(breed)>0)

dogcounts <- dogs %>% ungroup() %>% group_by(breedtype,breed,factor) %>% count()
dogcounts <- dogcounts %>% pivot_wider(names_from=breedtype,values_from=n)
dogcounts <- dogcounts %>% group_by(breed) %>% summarize(cand_breed=min(cand_breed),conf_breed=min(conf_breed)) %>% replace_na(list(conf_breed=0))

d <- dogcounts %>% inner_join(raw) 
d <- facInfo %>% right_join(d) %>% select(-tail)
d <- d %>% mutate(diff=if_else(upper<1,"1under",if_else(lower>1,"3over","2exp")))

breeds <- d %>% ungroup() %>% select(breed,cand_breed) %>%  distinct() %>% arrange(desc(cand_breed)) %>% pull(breed)
pd <- d 
pd <- pd %>% mutate(pheno2=paste(string,"\n",pheno,sep="")) 
pd <- pd %>% mutate(breedname=paste(breed," (N=",cand_breed,")",sep=""))

pd50 <- pd %>% filter(cand_breed>=50) 
pd50 <- pd50 %>% mutate(pheno2=str_replace_all(pheno2,": ","\n"))

p <- ggplot(pd50,aes(x=log(mid),y=breedname,yend=breedname))
p <- p + geom_vline(xintercept=log2(1),color="grey30",size=0.3)
p <- p + geom_point(aes(color=diff),shape=16,size=0.75) + geom_segment(aes(color=diff,x=log(lower),xend=log(upper)),size=0.5)
p <- p + scale_color_manual(values=c("#4575b4","#878787","#b2182b"))
p <- p + facet_grid(.~pheno2,scales="free_x",space="free_x")
p <- p + ggtitle("Relative risk that a dog in a particular breed will be in top quartile of all dogs")
p <- p + scale_y_discrete("breed",limits=rev)
p <- p + scale_x_continuous("relative risk",breaks=log(c(1/8,0.25,0.5,1,2,3)),labels=c("1/8","1/4","1/2","1","2","3"))
p <- p + theme_minimal()
p <- p + theme(legend.position="none",
               strip.text.y=element_text(size=5,angle=0,hjust=0), 
               strip.text.x=element_text(size=5,vjust=0,hjust=0.5),
               plot.title=element_text(size=6,face="bold"),
               panel.grid.minor=element_blank(), 
               panel.grid.major.x=element_line(size=0.1,color="grey70"),
               panel.grid.major.y=element_line(size=0.1,color="grey70"),
               plot.subtitle=element_text(size=4),
               axis.text.y = element_text(hjust=1,size=4),
               axis.title.y = element_blank(), 
               axis.text.x = element_text(hjust=0.5,size=4),
               axis.title.x=element_text(hjust=0.5,size=5),
               panel.border = element_rect(colour = "grey20", fill=NA, size=0.3))

ggsave(p,filename="Fig_RR_breed_risk.pdf",width=6.5,height=3)

p <- ggplot(pd50,aes(x=mid*0.25,y=breedname,yend=breedname))
p <- p + geom_vline(xintercept=0.25,color="grey30",size=0.3)
p <- p + geom_point(aes(color=diff),shape=16,size=0.85)
p <- p + geom_segment(aes(color=diff,x=lower*0.25,xend=upper*0.25),size=0.4)
p <- p + scale_color_manual(values=c("#4575b4","#878787","#b2182b"))
p <- p + facet_grid(.~pheno2,scales="free_x",space="free_x")
p <- p + ggtitle("If you predict a dog will be in top 25% of all dogs, how often can you expect to be right?")
p <- p + scale_y_discrete("breed",limits=rev)
p <- p + scale_x_continuous("prediction accuracy",breaks=c(0:4)/4,labels=paste((c(0:4)/4)*100,"%",sep=""))#limits=c(0,1))
p <- p + theme_minimal()
p <- p + theme(legend.position="none",
               strip.text.y=element_text(size=5,angle=0,hjust=0), 
               strip.text.x=element_text(size=5,vjust=0,hjust=0.5),
               plot.title=element_text(size=6,face="bold"),
               panel.grid.minor=element_blank(), 
               panel.grid.major.x=element_line(size=0.1,color="grey70"),
               panel.grid.major.y=element_line(size=0.1,color="grey70"),
               plot.subtitle=element_text(size=4),
               axis.text.y = element_text(hjust=1,size=4),
               axis.title.y = element_blank(), 
               axis.text.x = element_text(hjust=0.5,size=4),
               axis.title.x=element_text(hjust=0.5,size=5),
               panel.border = element_rect(colour = "grey20", fill=NA, size=0.3))
ggsave(p,filename="Fig_RR_pred_accuracy.pdf",width=6.5,height=3)



