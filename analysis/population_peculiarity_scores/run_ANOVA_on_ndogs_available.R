args = commandArgs(trailingOnly=TRUE)
library(tidyverse)

### This script analyzes whether the number of dogs available for sampling has an effect on the results. 

indir <- "../../data_release/"

permIn<- paste(indir,"DarwinsArk_20191115_survey_permutations.csv",sep="")
ppsd <- as_tibble(read.csv(permIn,header=T) ) #%>% filter(idtype=="factor")
ppsd <- ppsd %>% mutate(qf=paste(idtype,id,sep=""))
ppsd <- ppsd %>% filter(type=="candidate_breed"&ndogs_available>=50) %>% mutate(type="candidate_breed50") %>% bind_rows(ppsd)
aov1 <- ppsd %>% group_by(type) %>% anova_test(abs(z) ~ ndogs_available+qf)

aov <- as_tibble(aov1)  %>%  filter(Effect=="ndogs_available")
table <- ppsd %>% select(type,qf,set) %>% distinct() %>% group_by(type,qf) %>% count()  %>% group_by(type) %>% summarize(median_sets=median(n),min_sets=min(n),max_sets=max(n))
table <- table %>% mutate(sample_size=if_else(type=="by_year",100,if_else(type=="confirmed_breed",50,25)))
table <- ppsd %>% group_by(type) %>% summarize(min_available=min(ndogs_available),max_available=max(ndogs_available)) %>% full_join(table)
table <- ppsd %>% group_by(type) %>% count() %>% rename(ntests=n) %>% full_join(table)
#summarize(min_available=min(ndogs_available),max_available=max(ndogs_available)) %>% full_join(table)

table <- table %>% full_join(aov) %>% select(-Effect,-DFn,-`p<.05`)
prtable <- table %>% mutate(median_nsets=paste(median_sets,if_else(min_sets==max_sets,"",paste(" (",min_sets," - ",max_sets,")",sep="")),sep="")) 
prtable <- prtable %>% mutate(ndogs_available=paste(min_available,"-",max_available)) 
prtable <- prtable %>% select(type,ntests,median_nsets,sample_size,ndogs_available,ges,DFd,F,p)

write.csv(prtable,"DarwinsArk_20191115_survey_permutations.anova.csv",row.names=F)


ttest <- ppsd %>% mutate(sizegrp=if_else(ndogs_available<50,"lt50","gt50"),qfgrp="all")
ttest %>% mutate(absz=abs(z)) %>% t_test(absz~sizegrp,conf.level=0.95,detailed=T) 


sigcnts <- ppsd %>% filter(type=="candidate_breed") %>% mutate(sig=if_else(pcorr<=0.05,TRUE,FALSE))
sigcntsN <- sigcnts %>% group_by(set) %>% summarize(mindogs=min(ndogs_available)) 
sigcnts <- sigcntsN %>% mutate(small=if_else(mindogs<50,TRUE,FALSE)) %>% full_join(sigcnts)
sigcnts <- sigcnts %>% select(set,id,idtype,sig,small)
ntots <- sigcnts %>% group_by(set,small) %>% count() %>% rename(ntot=n)
sigcnts <- sigcnts %>% filter(sig) %>% group_by(set,small) %>% count() %>% rename(nsig=n) %>% full_join(ntots)
sigcnts <- sigcnts %>% replace_na(list(nsig=0)) %>% mutate(frac=nsig/ntot) %>% filter(ntot>100)
sigcntsSum <- sigcnts %>% group_by(small) %>% summarize(nbreeds=n(),mean=mean(frac),sd=sd(frac))
sigcnts %>% ungroup() %>% t_test(frac~small,detailed=T)

