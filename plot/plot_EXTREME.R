#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)
infile <- "../../data_release/"

library(tidyverse)
min_ndogs <- 50

quesInfo <- as_tibble(read.csv("../../data_release/DarwinsArk_20191115_questions.csv",header=T)) %>% select(id,short)


raw <- as_tibble(read.csv("../../data_release/DarwinsArk_20191115_answers.csv",header=T))
ans <- raw
dogs <- as_tibble(read.csv("../../data_release/DarwinsArk_20191115_dogs.csv",header=T))  %>% rename(dog=id)
dogs <- dogs  %>% select(dog,consensus_breed,conf,cand) %>% rename(cand_breed=cand,conf_breed=conf,breed=consensus_breed) %>% pivot_longer(c(-dog,-breed))
dogs <- dogs  %>% filter(value) %>% select(-value) %>% pivot_wider(names_from=name,values_from=breed)

ans <- ans %>% filter(!is.na(answer)) %>% rename(id=question) %>% select(dog,id,option,answer)  %>% filter(id<120) 
options <- ans %>% select(id,answer,option) %>% distinct()
ans <- dogs %>% inner_join(ans) 

breed_sets <- dogs %>% pivot_longer(-dog) %>% filter(!is.na(value)) %>% mutate(type=if_else(name=="cand_breed","candidate_breed",if_else(name=="conf_breed","confirmed_breed","error"))) %>% rename(set=value)
breed_sets <- breed_sets %>% select(dog,type,set) %>% distinct() 

ans_sets <- ans %>% inner_join(breed_sets)
all_answers <- crossing(breed_sets %>% select(-dog) %>% distinct(),options)

#totals <- crossing(quesInfo,breed_sets) %>% select(-dog) %>% distinct()
totals <- ans_sets %>% select(id,dog,type,set) %>% distinct() %>% group_by(id,type,set) %>% count() %>% rename(ntot=n) %>% filter(ntot>=min_ndogs)
responses <- ans_sets %>% distinct() %>% group_by(id,type,set,answer) %>% count() %>% full_join(all_answers) %>% replace_na(list(n=0))
responses <- responses %>% inner_join(totals) %>% mutate(frac=n/ntot)
responses <- responses
responses <- responses %>% left_join(quesInfo) %>% select(id,short,type,set,ntot,answer,option,n,frac)

pd <- responses %>% filter(answer==0|answer==4)
pd <- pd %>% ungroup() %>% select(answer,option) %>% distinct() %>% arrange(option) %>% group_by(answer) %>% summarize(optionstr=paste(option,collapse=" or ")) %>% full_join(pd)
pd <- pd %>% mutate(ylabel=paste("Q",id," ",short,sep=""))
pd <- pd %>% mutate(anstype=if_else(option=="Always"|option=="Never","Never - Always","Strongly Agree - Strongly Disagree"))

breed_count_string <- pd %>% select(type,set) %>% distinct() %>% group_by(type) %>% count() %>% ungroup() %>% mutate(string=paste(n," ",str_replace_all(type,"\\_"," "),"s",sep="")) %>% summarize(string=paste(string,collapse=" and ")) %>% pull(string)
p <- ggplot(pd,aes(y=fct_reorder(ylabel, id),x=frac)) + geom_point(aes(color=type,shape=type),size=1,alpha=0.5)
p <- p + ggtitle("No behaviors are exclusive to a subset of breeds",subtitle=paste("For breeds >",min_ndogs,"dogs, the fraction of owners choosing an exteme answer\n(e.g. Never, Always, Strongly Agree, or Strongly Disagree) never reaches 100%\n",breed_count_string,"\nclosed circles: confirmed breeds; open circles: candidate breeds"))
p <- p + facet_grid(anstype~factor(optionstr),scales="free",space="free")
p <- p + scale_y_discrete("question",limits=rev)
p <- p + scale_color_manual(values=c("#4292c6","#08306b"))
p <- p + scale_shape_manual(values=c(21,16))
p <- p + scale_x_continuous("fraction of dogs in breed",limits=c(0,1),breaks=c(0,0.5,1),labels=c(0,0.5,1))
p <- p + theme_minimal() 
p <- p + theme(legend.position = "none",
               panel.grid.minor=element_blank(), #panel.grid.major.y=element_blank(),
               axis.ticks.y=element_blank(),
               axis.ticks.x = element_line(size=0.25),
               axis.text.y=element_text(size=5),
               axis.text.x=element_text(size=6),
               axis.title.y=element_blank(),
               axis.title.x=element_text(size=7),
               plot.title = element_text(size=8, face="bold"),
               plot.subtitle = element_text(size=6),
               strip.text = element_text(size=7,face="bold",lineheight=0.75),
               legend.title=element_text(size=6,face="bold"),legend.text = element_text(size=6))
ggsave(plot=p,filename="Fig_EXTREME_FRAC.pdf",width=6.5,height=8)

