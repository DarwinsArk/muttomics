args = commandArgs(trailingOnly=TRUE)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(wesanderson)
library(ggrepel)

indir <- "../../data_release/"
palette <- c("#00A3E0","#D9027D","#F8E600","#CD0000","#00AB84","#0085ca","purple","#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#bc80bd","#ccebc5")

### get information about factors
facInfo <- as_tibble(read.csv(paste(indir,"/DarwinsArk_20191115_factors.csv",sep=""))) %>% mutate(idtype="factor",questype="factor")
facInfo <- facInfo %>% select(idtype,factor,name,negative,positive,questype) %>% filter(factor<=8) %>% distinct() %>% mutate(id=factor,string=name)
facInfo <- facInfo %>% mutate(qf=paste("F",id,sep=""),plotname=paste("F",id," ",string,sep="")) %>% filter(id<=8)
facInfoEdges <- facInfo %>% select(id,qf,negative,positive) 
facInfo <- facInfo %>% select(id,qf,plotname) %>% mutate(plotname=str_remove(plotname,"F[0-9] "))

## get survey responses to factors 
factorsIn <- paste(indir,"DarwinsArk_20191115_factor_scores.csv",sep="")
allF <-  as_tibble(read.csv(factorsIn,header=T,na.strings=c("NA","#N/A","","N/A")))
allF <- allF %>% filter(!is.na(score)) %>% select(-score,-discovery) %>% rename(score=score.norm)
allF <- allF %>% mutate(qf=paste("F",factor,sep="")) %>% select(-factor)

pd <- allF %>% filter(!is.na(score))  %>% filter(!is.na(age))
pd <- pd %>% group_by(qf) %>% count() %>% mutate(xlabel=paste(qf,"\nN=",n,sep="")) %>% full_join(pd)
p <- ggplot(pd,aes(x=xlabel,y=score))
p <- p + geom_violin(fill="#0C3D73",alpha=0.25,color="#0C3D73",size=0.25,draw_quantiles = c(0.25,0.5,0.75))
p <- p + theme_minimal()
p <- p + theme(legend.position="none",
               panel.grid.minor=element_blank(), 
               panel.grid.major.x=element_blank(),
               panel.grid.major.y=element_line(size=0.2,color="grey70"),
               axis.text.y = element_text(hjust=1,size=7),
               axis.title.y = element_text(hjust=0.5,size=7,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=7),
               axis.title.x=element_blank())
ggsave(plot=p,filename="Fig_VAR.factor_score_violins.pdf",width=6.5,height=4)

## get standardized breed names
breednames <- as_tibble(read.csv(paste(indir,"/ReferenceData_breeds.csv",sep=""))) %>% rename(breed=breed_name)
breednames <- breednames %>% mutate(breed_name_short=if_else(is.na(breed_name_short),breed,breed_name_short))

## get dog information  
dogsIn <- paste(indir,"DarwinsArk_20191115_dogs.csv",sep="")
dogs <- as_tibble(read.csv(dogsIn,header=T),na.strings=c("NA","#N/A",""," ","  ","N/A"))
dogs <- dogs %>% select(id,consensus_breed,mutt,cand,conf) %>% distinct() %>% rename(dog=id)

## make set of all dogs of any ancestry
dogsets <- dogs %>% mutate(ds="all_dogs",breed="any") %>% select(dog,ds,breed)

## add sets for each breed (candidate and confirmed)
dogsets <- dogs %>% filter(cand) %>% rename(breed=consensus_breed) %>% filter(breed %in% breednames$breed) %>% mutate(ds="cand_breed") %>% select(dog,ds,breed) %>% bind_rows(dogsets)
dogsets <- dogs %>% filter(conf) %>% rename(breed=consensus_breed) %>% filter(breed %in% breednames$breed) %>% mutate(ds="conf_breed") %>% select(dog,ds,breed) %>% bind_rows(dogsets)

## make tibble with merged data for dog sets and factors.
dogsets <- dogsets %>% inner_join(allF)
dogsets <- dogsets %>% select(-age)  %>% distinct()

## make dataset only with breeds > 50 dogs
dogsets <- dogsets %>% group_by(ds,breed,qf) %>% count() %>% rename(nqf=n) %>% ungroup() %>% group_by(ds,breed) %>% summarize(min_ndogs=min(nqf)) %>% inner_join(dogsets)
dogsets <- dogsets %>% filter(min_ndogs>=50) %>% mutate(breedname=paste(breed," (N=",min_ndogs,")",sep="")) %>% select(-min_ndogs)

## check if breed distribution differs from all dogs distribution using Kolmogorov-Smirnov Test

allF <- allF  %>% filter(dog %in% dogsets$dog)

pdksAll <- tibble()
for (infact in c(1:8)){
  print(infact)
  cdfF <- ecdf(allF %>% filter(qf==paste("F",infact,sep="")) %>% pull(score))
  pd <- dogsets %>% filter(ds != "all_dogs") %>% filter(qf==paste("F",infact,sep=""))
  pdks <- pd %>% group_by(ds,qf,breed) %>% summarize(p=ks.test(score,cdfF)$p,stat=ks.test(score,cdfF)$statistic)
    if (nrow(pdksAll) > 0L){ #!empty(pdksAll)){
    pdksAll <- pdksAll %>% bind_rows(pdks)
  }
  if (nrow(pdksAll) == 0) { #empty(pdksAll)){
    pdksAll <- pdks 
  }
}
pdksAll <- pdksAll   %>% adjust_pvalue(method="BH")

# Make stats file about factors
breed_factor_stats <- dogsets %>% select(-breedname) %>% group_by(ds,qf,breed)  %>% summarize(n=n(),mean=mean(score),sd=sd(score),median=median(score),q25=quantile(score,0.25),q75=quantile(score,0.75),IQR=IQR(score))
#all_stats <- allF %>% group_by(qf) %>% summarize(n=n(),mean=mean(score),sd=sd(score),median=median(score),q25=quantile(score,0.25),q75=quantile(score,0.75),IQR=IQR(score)) %>% mutate(ds="all")
#breed_factor_stats <- breed_factor_stats %>% bind_rows(all_stats) %>% arrange(qf,ds,breed)
breed_factor_stats <- breed_factor_stats %>% full_join(pdksAll) %>% rename(ks.stat=stat,ks.p=p,ks.p.adj=p.adj)
write.csv(breed_factor_stats,"Data_VAR_FAC_KS.csv",row.names=F)

color <-  breed_factor_stats %>% filter(!is.na(ks.p.adj)) %>% mutate(sig=if_else(ks.p.adj<=0.05,TRUE,FALSE)) %>% select(ds,breed,qf,sig) %>% filter(!is.na(sig))
# plot all distributions for all breeds with > 50 dogs
pd <- dogsets %>% ungroup() %>% inner_join(color) #%>% left_join(facInfo)
pd <- pd %>% mutate(dsname=if_else(ds == "cand_breed","candidate purebred dogs",if_else(ds == "conf_breed","confirmed purebred dogs","error")))
breed_count_string <- pd %>% select(dsname,breed) %>% distinct()  %>% group_by(dsname) %>% count() %>% mutate(dsname=str_replace(dsname,"purebred dogs","breeds")) %>% mutate(string=paste(n,dsname)) %>% ungroup() %>% summarize(string=paste(string,collapse=" and ")) %>% pull(string)


counts <- pd %>% select(dsname,breed,qf,sig) %>% distinct()
countsSig <- counts %>% filter(sig) %>% group_by(dsname,qf) %>% count() %>% rename(sig=n)
counts <- counts %>% group_by(dsname,qf) %>% count() %>% full_join(countsSig) %>% replace_na(list(sig=0)) %>% mutate(frac=sig/n)
counts <- counts %>% mutate(label=paste("Sig: ",sig,"/",n,sep=""))


allpd <- pd %>% select(dsname,qf) %>% distinct() %>% full_join(allF) # allF %>% mutate(ds="all dogs")
p <- ggplot(pd,aes(x=score))
p <- p + geom_density(aes(group=breed,color=sig),fill=NA,alpha=0.5,size=0.1,adjust = 2)
p <- p + geom_density(fill="grey50",data=allpd,color=NA,size=0.3,alpha=0.25,adjust = 2)
p <- p + geom_vline(xintercept=0,color="#302E2C",size=0.1)
p <- p + geom_text(aes(label=label),color="red",x=-4,y=0.6,data=counts,hjust=0,vjust=1,size=2)
p <- p + facet_grid(dsname~qf) #,scales="free",ncol=4)
p <- p + scale_color_manual(values=c("grey10","red"))
p <- p + scale_fill_manual(values=palette)
p <- p + ggtitle("Factor distributions for all breeds with N > 50 dogs",subtitle=paste("For each factor, between 26% (factor 2) and 78% (factor 4) of breed distributions\ndiffer from the overall population, with 66.3% (median) in the overrepresented tail\ntwo-tailed Kolmogorov-Smirnov test; pBH<0.05 (red)\ngrey = all dogs\n",breed_count_string,sep="")) 
p <- p + theme_minimal()
p <- p + theme(legend.position="bottom",legend.title = element_text(hjust=0.5,size=5),
               legend.text=element_text(hjust=0.5,size=5),
               legend.key.size = unit(0.1, 'in'),
               strip.text.y=element_text(angle=0,size=6,hjust=0.5,vjust=0.5), 
               strip.text.x=element_text(size=5,face="bold"),
               plot.title=element_text(size=6,face="bold"),
               panel.grid.minor=element_blank(), 
               panel.grid.major=element_blank(),
               #panel.grid.major.y=element_line(size=0.2,color="grey70"),
               plot.subtitle=element_text(size=5),
               axis.text.y = element_text(hjust=1,size=5),
               axis.title.y = element_text(hjust=0.5,size=6,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"))
ggsave(plot=p,filename="Fig_VAR_FAC_ALL.pdf",width=12,height=4)

 