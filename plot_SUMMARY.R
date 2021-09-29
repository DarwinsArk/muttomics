#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)
library(RColorBrewer)
library(tidyverse)
library(rstatix)

## point to location of folder with paper data release files.
indir <- "../../../data_release/"
facIn <- paste(indir,"DarwinsArk_20191115_factors.csv",sep="")
quesIn <- paste(indir,"DarwinsArk_20191115_questions.csv",sep="")

ordering <- tibble(stat=c("h2SNP.sum","ges.breed","pps.sd_z","lmer.sd_t","gwas.min_p","overlap_frac","magma.min_p"),
                   order=c(1:7))


# get titles etc for labelling plots
qfinfo <- as_tibble(read.csv(facIn)) %>% filter(factor <= 8) 
qfinfo <- qfinfo %>% select(factor,name,negative,positive) %>% distinct() %>% mutate(idtype="behavior factor") %>% rename(id=factor)
qfinfo <- qfinfo %>% mutate(name=str_remove(name,"Factor [0-9]: "))
quesInfo <- as_tibble(read.csv(quesIn)) 
quesInfo <- quesInfo %>% select(id,short,negative,positive) %>% distinct() %>% mutate(idtype=if_else(id>=120,"physical trait","behavior question")) %>% rename(name=short)
quesInfo <- quesInfo %>% filter(id==121)
qfinfo <- qfinfo %>% bind_rows(quesInfo)

allqf <- qfinfo %>% select(id,idtype) %>% distinct()



### get normalized heritability numbers 
herIn <- paste(indir,"DarwinsArk_20191115_heritability_LD-stratified.csv",sep="")
her <- as_tibble(read.csv(herIn,header=T,na.strings=c("NA","#N/A",""," ","N/A"))) %>% rename(id=index,idtype=class) 
her <- allqf %>% inner_join(her)
her <- her %>% filter(id!=121|trait=="Size")
her <- her %>% filter(set=="all dogs"&type=="Variance") %>% select(idtype,id,h2SNP.sum) %>% filter(!is.na(h2SNP.sum)) %>% rename(value=h2SNP.sum)  %>% mutate(stat="h2SNP.sum")
max <- max(her$value)
min <- min(her$value)
her <- her %>% mutate(norm=(value-min)/(max-min)) 

her <- her %>% mutate(data_input="surveys+genetics")
her <- her %>% mutate(method="h2SNP (LD corrected)") %>% mutate(n=1)
pd <- her %>% select(idtype,id,value,norm,n,stat,method,data_input) 

### get normalized ANOVA
anovaIn <- paste(indir,"DarwinsArk_20191115_anova.csv",sep="")
anova <- as_tibble(read.csv(anovaIn,header=T)) %>% mutate(id=as.integer(str_remove(qf,"[FQ]")))
anova <- anova %>% mutate(idtype=if_else(idtype=="factor","behavior factor",if_else(id>=120,"physical trait","behavior question")))
anova <- allqf %>% inner_join(anova)
anova <- anova %>% filter(model_type=="w_breed"&ds=="conf_breed") %>% filter(Effect=="breed") %>% rename(value=ges) %>% mutate(stat="ges.breed")
min <- min(anova$value)
max <- max(anova$value)
anova <- anova %>% mutate(norm=(value-min)/(max-min)) 
anova <- anova %>% mutate(data_input="surveys")
anova <- anova  %>% mutate(method="ges (effect of breed) for all confirmed purebred dogs ")  %>% mutate(n=1)
anova <- anova  %>% select(colnames(pd))
pd <- pd %>% bind_rows(anova)

### get PPS
ppsIn <- paste(indir,"DarwinsArk_20191115_survey_permutations.csv",sep="")
pps <- as_tibble(read.csv(ppsIn,header=T))
pps <- pps %>% mutate(idtype=if_else(idtype=="factor","behavior factor",if_else(id>=120,"physical trait","behavior question")))
pps <- allqf %>% inner_join(pps)
pps <- pps %>% filter(type=="candidate_breed") 
pps <- pps %>% group_by(idtype,id) %>% summarize(n=n(),value=sd(z)) %>% mutate(stat="pps.sd_z")
min <- min(pps$value)
max <- max(pps$value)
pps <- pps %>% mutate(norm=(value-min)/(max-min)) 
pps <- pps %>% mutate(data_input="surveys")
pps <- pps %>% mutate(method=paste("Standard deviation of z scores (PPS test) for ",n," breeds",sep=""))
pps <- pps %>% ungroup() %>% select(colnames(pd))
pd <- pd %>% bind_rows(pps)

### get LMER

LMERIn <- paste(indir,"DarwinsArk_20191115_LMER_models.csv",sep="")
lmer <- as_tibble(read.csv(LMERIn,header=T))
lmer <- lmer %>% rename(idtype=type) %>% mutate(idtype=str_replace(idtype,"behavioral","behavior"))
lmer <- allqf %>% inner_join(lmer)

lmer <- lmer %>% group_by(idtype,id) %>% summarize(n=n(),value=sd(REML.t.val)) %>% mutate(stat="lmer.sd_t")
max <- max(lmer$value)
min <- min(lmer$value)
lmer <- lmer %>% mutate(norm=(value-min)/(max-min))  #%>% mutate(score=ceiling(norm*4)) %>% mutate(score=if_else(score==0,1,score))

lmer <- lmer %>% mutate(data_input="surveys+genetics")
lmer <- lmer %>% mutate(method=paste("Standard deviation of REML.t.val scores for ",n," breeds",sep=""))
lmer <- lmer %>% select(colnames(pd))
pd <- pd %>% bind_rows(lmer)

# get GWAS
gwasIn <- paste(indir,"DarwinsArk_20191115_GWASloci.csv",sep="")
gwas <- as_tibble(read.csv(gwasIn,header=T)) 
gwas <- gwas %>% rename(idtype=class,id=index) %>% filter(set=="all dogs")
gwas <- allqf %>% inner_join(gwas)
gwas <- gwas %>% filter(p<=1e-6) %>% group_by(idtype,id) %>% summarize(value=-log10(min(p)),n=n()) %>% mutate(stat="gwas.min_p") 
min <- -log10(1e-6)
max <- max(gwas$value)
gwas <- gwas %>% ungroup() %>% mutate(norm=(value-min)/(max-min))# %>% mutate(score=ceiling(norm*4)) %>% mutate(score=if_else(score==0,1,score))
#gwas <- gwas %>% ungroup() %>% mutate(norm = ntile(value-min,100)/100)  #%>% mutate(score=ceiling(norm*4)) %>% mutate(score=if_else(score==0,1,score))
gwas <- gwas %>% mutate(data_input="surveys+genetics")
gwas <- gwas %>% mutate(method=paste("log10(p) of top GWAS peak of ",n," peaks < 1e-6, binned into 100 ordered window partitions",sep=""))
gwas <- gwas %>% select(colnames(pd))
pd <- pd %>% bind_rows(gwas)

#get MAGMA
magIn <-  paste(indir,"DarwinsArk_20191115_magma.csv",sep="")
magma <- as_tibble(read.csv(magIn,header=T,na.strings=c("NA","#N/A",""))) #%>% rename(p=P)
magma <- magma %>% mutate(idtype=if_else(idtype=="factor","behavior factor",if_else(id>=120,"physical trait","behavior question")))
magma <- allqf %>% inner_join(magma)
magma <- magma %>% filter(source=="GTex.Brain") %>% filter(idtype=="behavior factor"|id==121)
magma <- magma %>% group_by(idtype,id) %>% summarize(n=n(),value=-log10(min(p)),stat="magma.min_p")
min <- min(magma$value)
max <- max(magma$value)
magma <- magma %>% mutate(norm=(value-min)/(max-min))# %>% mutate(score=ceiling(norm*4)) %>% mutate(score=if_else(score==0,1,score))
magma <- magma %>% mutate(data_input="surveys+genetics")
magma <- magma %>% mutate(method=paste("min log10(p) of ",n," GTEX brain-expressed gene sets",sep=""))
magma <- magma %>% select(colnames(pd))
pd <- pd %>% bind_rows(magma)

# get PBS overlap

pbsIn <- paste(indir,"DarwinsArk_20191115_GWASloci_breedPBS_permutations.csv",sep="")
pbs <- as_tibble(read.csv(pbsIn,header=T))
pbs <- pbs %>% mutate(idtype=if_else(type=="factor","behavior factor",if_else(id>=120,"physical trait","behavior question")))
pbs <- allqf %>% inner_join(pbs)
sig <- pbs %>% filter(pbs.max.perm.percentile.adj.fdr<=0.05) %>% group_by(idtype,id) %>% count() %>% rename(sig=n)
pbs <- pbs %>% group_by(idtype,id) %>% count() %>% full_join(sig) %>% replace_na(list(sig=0)) %>% mutate(value=sig/n,stat="overlap_frac")
min <- min(pbs$value)
max <- max(pbs$value)
pbs <- pbs %>% mutate(norm=(value-min)/(max-min)) #%>% mutate(score=ceiling(norm*4)) %>% mutate(score=if_else(score==0,1,score))
pbs <- pbs %>% mutate(data_input="surveys+genetics")
pbs <- pbs %>% mutate(method=paste("fraction of PBS peaks significantly overlapping gwas peak out of ",n," overlaps",sep=""))
pbs <- pbs %>% select(colnames(pd))
pd <- pd %>% bind_rows(pbs)

pd <- pd %>% select(idtype,id,stat) %>% distinct() %>% group_by(idtype,id) %>% count() %>% filter(n>=7) %>% select(-n) %>% inner_join(pd)

pd <- pd %>% left_join(ordering) %>% mutate(xlabel=paste(order,stat,sep="\n"))
pd <- qfinfo %>% right_join(pd)
write.csv(pd,"Fig_SUMMARY.input.csv",row.names=F)

pd <- pd %>% filter(idtype=="behavior factor"|id==121)
pd <- pd %>% mutate(xlabel=str_replace_all(xlabel,"\\.","\n"))  %>% mutate(xlabel=str_replace_all(xlabel,"\\_","\n"))

#pd <- pd %>% mutate(score=if_else(score>0,score-1,score))
#pd <- pd %>% mutate(scoresize=if_else(score==3,6,score+0.1))

yaxis <- pd %>% select(idtype,id,name) %>% distinct() %>% arrange(id)
yaxis$ypos <- c(1:nrow(yaxis))
yaxis <- yaxis %>% mutate(ylabel=if_else(idtype=="behavior factor",paste("F",id,sep=""),paste("Q",id,sep="")))
yaxis <- yaxis %>% mutate(ylabel=paste(ylabel,name))

p <- ggplot(pd,aes(x=xlabel,y=factor(id)))
p <- p + geom_hline(yintercept=c(-2,-4,-6,-8),color="#525252",size=0.25)
p <- p + geom_hline(yintercept=c(-1,-3,-5,-7),color="#bdbdbd",size=0.25)
p <- p + geom_point(aes(size=norm,color=data_input,alpha=norm),shape=16) #,data=(pd %>% filter(score>0)))
p <- p + scale_size(range=c(1,10))
p <- p + scale_color_manual(values=c("#627e74","#1f1817"))
p <- p + scale_alpha_continuous(range=c(0.25,1))
p <- p + scale_y_discrete("question/factor",breaks=yaxis$id,labels=yaxis$ylabel,limit=rev)
#p <- p + scale_y_discrete("",limits=rev)
p <- p + theme_minimal()
p <- p + theme(legend.position="bottom",strip.text=element_text(size=6,face="bold"), 
               plot.title=element_text(size=6,face="bold"),panel.grid.minor=element_blank(), 
               panel.grid.major.x=element_blank(), 
               ##panel.grid.major.y=element_line(color="grey30",size=0.25),
               plot.subtitle=element_text(size=5),
               axis.title.y = element_blank(),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.text.y = element_text(hjust=1,size=5),
               axis.title.x=element_blank(),
               strip.text.x=element_text(hjust=0.5,size=6))
ggsave(plot=p,filename="Fig_SUMMARY.grid.pdf",width=6,height=6)

