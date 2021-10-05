#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)
indir <- "../../data_release/"

library(tidyverse)

herIn <- paste(indir,"DarwinsArk_20191115_heritability_LD-stratified.csv",sep="")
allHer <- as_tibble(read.csv(herIn,header=T,na.strings=c("NA","#N/A",""," ","N/A"))) %>% rename(id=index,idtype=class) 
questypes <- c("physical trait","physical trait related","motor pattern","factor","other behavior")
my_comparisons <- list(c(questypes[1],questypes[5] ),c(questypes[2],questypes[5] ),c(questypes[3],questypes[5] ),c(questypes[4],questypes[5] ))
colors <- c("#4a3466","#7e55a3","#128b82","#1d75bc","#99c868")
quesInfo <- as_tibble(read.csv(paste(indir,"/DarwinsArk_20191115_questions.csv",sep=""),header=T)) %>% mutate(idtype="question")  %>% rename(questype=question.type)
info <- tibble(id=c(1:8))
info <- info %>% mutate(idtype="behavior factor",questype="factor")
info <- quesInfo %>% select(idtype,id,questype) %>% mutate(idtype=if_else(id>=120,"physical trait","behavior question")) %>% bind_rows(info)
her <- info %>% inner_join(allHer) %>% filter(type=="Variance"&set=="all dogs")
pd <- her %>% select(idtype,id,questype,h2SNP.sum)
pd$questype <- factor(pd$questype,levels=questypes)
xlabels <- pd %>% select(questype) %>% distinct() %>% mutate(xlabel=str_replace_all(questype," ","\n"))
p <- ggplot(pd,aes(x=questype,y=h2SNP.sum))
p <- p + geom_violin(aes(color=questype,fill=questype),alpha=0.5)
p <- p + geom_point(aes(color=questype),shape=21)
p <- p + stat_compare_means(comparisons = my_comparisons,method="t.test",p.adjust.method = "bh",size=3,label = "p.format")
p <- p + scale_color_manual(values=colors)
p <- p + scale_fill_manual(values=colors)
p <- p + scale_x_discrete("question type",breaks=xlabels$questype,labels=xlabels$xlabel)
p <- p + theme_minimal()
p <- p + theme(legend.position = "none")
ggsave(plot=p,filename="Fig_HER.violin.pdf",width=4,height=4)





### check heritability of new questions vs existing questions 

allnew <- c(17,18,19,20,28,29,30,35,38,40,45,50,59,60,62,63,64,65,66,67,68,69,70,76,86,90,92,96,98,99,100)
quesIn <- paste(indir,"DarwinsArk_20191115_questions.csv",sep="")
quesInfo <- as_tibble(read.csv(quesIn)) 
quesInfo <- quesInfo %>% select(id,short,source,negative,positive) %>% distinct() %>% filter(id<120)
herIn <- paste(indir,"DarwinsArk_20191115_heritability_all-SNPs.csv",sep="")
allHer <- as_tibble(read.csv(herIn,header=T,na.strings=c("NA","#N/A",""," ","N/A"))) %>% rename(id=index,idtype=class) 

her <- allHer
her <- her %>% filter(trait != "Retrieves objects (binary)")
her <- her %>% filter(set=="all dogs"&type=="Variance") %>% select(idtype,id,h2SNP) %>% filter(!is.na(h2SNP)) 
her <- her %>% filter(idtype!="behavior factor") %>% filter(id<120)
pd <- her %>% mutate(source=if_else((id %in% allnew),"new","other"))

top_q25_other <- pd %>% filter(source=="other") %>% summarize(q=quantile(h2SNP,0.75)) %>% pull(q)
nhigh_new <- pd %>% ungroup() %>% filter(source=="new"&h2SNP>=top_q25_other) %>% count() %>% pull(n)
n_new <- pd %>% ungroup() %>% filter(source=="new") %>% count() %>% pull(n)
prop.test(nhigh_new, n_new, p=.25, alt="greater", correct=FALSE)

