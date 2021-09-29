args = commandArgs(trailingOnly=TRUE)
library(tidyverse)
library(rstatix)
library(wesanderson)

### This script runs and plots the results of the ANOVA (Analysis of variance) analysis. 

## point to location of folder with paper data release files.
indir <- "../../../data_release/"
min_ndogs_breed <- 5
min_dog_age_yr <- 1


palette <- wes_palette("Zissou1",n=5)
palette <- palette[c(5,1,3,4)]

breednames <- as_tibble(read.csv(paste(indir,"/ReferenceData_breeds.csv",sep=""))) %>% rename(breed=breed_name)
breednames <- breednames %>% mutate(breed_name_short=if_else(is.na(breed_name_short),breed,breed_name_short))

dogsIn <- paste(indir,"DarwinsArk_20191115_dogs.csv",sep="")
dogs <- as_tibble(read.csv(dogsIn,header=T),na.strings=c("NA","#N/A",""," ","  ","N/A"))
doginfo <- dogs %>% select(id,sex,sterilized,size,consensus_breed,cand,conf) %>% distinct() %>% rename(dog=id)

# keep only dogs with sex, size, spay/neuter and breed information
doginfo <- doginfo %>% filter(!is.na(sex)&!is.na(size)&!is.na(sterilized)) %>% filter(cand|conf)

# combine sex and sterilization status
doginfo <- doginfo %>% mutate(sex=if_else(sterilized=="no",paste(sex,"intact",sep="."),sex)) %>% select(-sterilized)

## make candidate and confirmed breed sets 
dogsets <- doginfo %>% filter(cand) %>% rename(breed=consensus_breed) %>% mutate(ds="cand_breed") %>% select(dog,sex,size,ds,breed)
dogsets <- doginfo %>% filter(conf) %>% rename(breed=consensus_breed) %>% mutate(ds="conf_breed") %>% select(dog,sex,size,ds,breed) %>% bind_rows(dogsets) %>% distinct()

# read in heritability values
herIn <- paste(indir,"DarwinsArk_20191115_heritability_LD-stratified.csv",sep="")
her <- as_tibble(read.csv(herIn,header=T,na.strings=c("NA","#N/A",""," ","N/A"))) 
her <- her %>% mutate(idtype=if_else(str_detect(class,"factor"),"factor","question")) %>% rename(id=index)
her <- her %>% filter(type=="Variance"&set=="all dogs")
her <- her %>% select(id,idtype,trait,h2SNP.sum) %>% rename(h2SNP=h2SNP.sum)

# remove values for alternative phenotype definitions
her <- her %>% filter(id!=121|trait=="Size")
her <- her %>% filter(id!=122)
her <- her %>% filter(id!=54|trait=="Retrieves objects")

## remove breeds that aren't in reference breed list
dogsets <- dogsets %>% filter(breed %in% breednames$breed)

## get information about questions & factors 
quesInfo <- as_tibble(read.csv(paste(indir,"/DarwinsArk_20191115_questions.csv",sep=""),header=T)) %>% mutate(idtype="question")  %>% rename(questype=question.type)
quesSources <- quesInfo %>% select(id,source) %>% mutate(idtype="question")
quesInfo <- quesInfo %>% select(idtype,id,string,abbr_text,negative,positive,questype) %>% distinct() 
quesInfo <- quesInfo %>% mutate(qf=paste("Q",id,sep=""),plotname=paste("Q",id," ",abbr_text,sep=""))
quesInfo <- quesInfo %>% select(qf,idtype,id,plotname,questype)
quesInfo <- quesInfo %>% mutate(questype=if_else(questype=="aging related surveys","other behavior",questype))

facInfo <- as_tibble(read.csv(paste(indir,"/DarwinsArk_20191115_factors.csv",sep=""))) %>% mutate(idtype="factor",questype="factor") %>% rename(id=factor,string=name)
facQuestions <- facInfo %>% select(id,question) %>% distinct() %>% filter(id<=8)
facInfo <- facInfo %>% select(idtype,id,string,negative,positive,questype) %>% distinct() 
facInfo <- facInfo %>% mutate(qf=paste("F",id,sep=""),plotname=paste("F",id," ",string,sep=""))
facInfo <- facInfo %>% select(colnames(quesInfo))

plotInfo <- quesInfo %>% bind_rows(facInfo)

## get survey responses to factors and questions 
quesIn <- paste(indir,"DarwinsArk_20191115_answers.csv",sep="") # input_files/input.all.scores.csv"
allQ <- as_tibble(read.csv(quesIn,header=T,na.strings=c("NA","#N/A","","N/A")))

factorsIn <- paste(indir,"DarwinsArk_20191115_factor_scores.csv",sep="")
allF <-  as_tibble(read.csv(factorsIn,header=T,na.strings=c("NA","#N/A","","N/A")))

## normalize question responses, remove responses without age, and standardize column headers
ques <- allQ %>% mutate(idtype="question",qf=paste("Q",question,sep="")) %>% select(-time,-option) %>% rename(id=question,age=age_years) 
ques <- ques %>% rename(value=ans.norm) %>% select(idtype,qf,id,dog,value,age)   %>% filter(!is.na(value))
ques <- ques %>% distinct() %>% filter(!is.na(age))

## remove factor responses without age and standardize column headers
fac <- allF %>% mutate(qf=paste("F",factor,sep=""),idtype="factor") %>% rename(id=factor,value=score.norm) %>% select(qf,idtype,id,dog,value,age)
fac <- fac %>% filter(!is.na(value))  %>% filter(!is.na(age))

pd <- fac %>% group_by(id) %>% count() %>% mutate(xlabel=paste("factor ",id,"\nN=",n,sep="")) %>% full_join(fac)
p <- ggplot(pd,aes(x=xlabel,y=value))
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
ggsave(plot=p,filename="Fig_ANOVA.factor_score_dist.pdf",width=6.5,height=4)

# get stats for text 

facStats <- fac %>% group_by(id) %>% summarize(meanAll=mean(value),sdAll=sd(value),n=n()) %>% rename(factor=id)
brFacStats <- dogs %>% rename(dog=id) %>% select(dog,cand,conf,consensus_breed) %>% pivot_longer(c(-dog,-consensus_breed)) %>% filter(value) %>% select(-value) %>% inner_join(allF)
brFacStats <- brFacStats %>% rename(breed=consensus_breed) %>% filter(!is.na(score.norm)) %>% inner_join(facStats)
brFacStats <- brFacStats %>% mutate(sd1=if_else(score.norm>=(meanAll-sdAll)&score.norm<=(meanAll+sdAll),TRUE,FALSE),sd2=if_else(score.norm>=(meanAll-(2*sdAll))&score.norm<=(meanAll+(2*sdAll)),TRUE,FALSE))

counts <- brFacStats %>% filter(sd1) %>% group_by(breed,name) %>% count() %>% mutate(stat="within_1sd")
counts <- brFacStats %>% filter(sd2)  %>% group_by(breed,name) %>% count() %>% mutate(stat="within_2sd") %>% bind_rows(counts)
counts <- brFacStats %>% group_by(breed,name) %>% count() %>% mutate(stat="tot") %>% bind_rows(counts)
counts <- counts %>% pivot_wider(names_from=stat,values_from=n) %>% filter(tot>=25) %>% mutate(per1=within_1sd/tot,per2=within_2sd/tot)
counts1 <- counts %>% select(breed,name,per1) %>% rename(percent=per1) %>% mutate(stat="within_1sd") %>% filter(!is.na(percent))
counts1 <- counts %>% select(breed,name,per2) %>% rename(percent=per2) %>% mutate(stat="within_2sd")%>% filter(!is.na(percent)) %>% bind_rows(counts1) 
counts <- counts1 %>% group_by(name,stat) %>% summarize(mean=mean(percent),sd=sd(percent),n=n())

brFacStats <- brFacStats %>% select(breed,name,sd1,dog) %>% group_by(breed,name,sd1) %>% count() 
brFacStats <- brFacStats %>% pivot_wider(names_from=sd1,values_from=n) %>% replace_na(list(`TRUE`=0,`FALSE`=0)) %>% mutate(tot=`TRUE`+`FALSE`)
brFacStats <- brFacStats %>% filter(tot>=50) %>% mutate(frac=`TRUE`/tot) %>% arrange(frac)
last <- brFacStats %>% group_by(name) %>% count() %>% rename(ntot=n)
last <- brFacStats %>% filter(frac<0.5) %>% group_by(name) %>% count() %>% rename(nlow=n) %>% full_join(last) %>% replace_na(list(nlow=0))
last <- last %>% mutate(frac=nlow/ntot)



## make dataset of all factors and questions with dog information 
allscores <- fac %>% bind_rows(ques) %>% inner_join(dogsets) %>% filter(!is.na(value))

## remove responses from dogs under min_dog_age_yr years of age. 
allscores <- allscores %>% filter(age>=min_dog_age_yr)

## remove breeds with fewer than min_ndogs_breed dogs 
breedcounts <- allscores %>% select(ds,qf,breed,dog) %>% distinct() %>% group_by(ds,qf,breed) %>% count() %>% ungroup() %>% group_by(ds,breed) %>% summarize(ndogs=min(n))
breedcounts <- breedcounts %>% filter(ndogs>=min_ndogs_breed) 
allscores <- breedcounts %>% select(-ndogs) %>% inner_join(allscores)

### remove size question bc it breaks the Anova
### allscores <- allscores %>% filter(qf!="Q121")

## count dogs in each dataset (for use in plot titles later on)
dogcounts <- allscores %>% select(idtype,ds,dog) %>% distinct() %>% group_by(idtype,ds) %>% count() %>% rename(ndogs=n)
dogcounts <- allscores %>% select(idtype,ds,breed) %>% distinct() %>% group_by(idtype,ds) %>% count() %>% rename(nbreeds=n) %>% inner_join(dogcounts)
dogcounts <- dogcounts %>% mutate(ds_long=if_else(ds=="cand_breed","candidate purebred dogs",if_else(ds=="conf_breed","confirmed purebred dogs","error")))
dogcounts <- dogcounts %>% mutate(subtitle=paste(ds_long,": ",ndogs," dogs in ",nbreeds," breeds (>=",min_ndogs_breed," dogs/breed)",sep=""))

anov.breed <- as_tibble(allscores %>% filter(qf!="Q121") %>% group_by(idtype,ds,qf) %>% anova_test(value ~ breed+sex+age+size)) %>% mutate(anova="breed+sex+age+size",model_type="w_breed")
anov.no_size <- as_tibble(allscores %>% filter(qf=="Q121") %>% group_by(idtype,ds,qf) %>% anova_test(value ~ breed+sex+age)) %>% mutate(anova="breed+sex+age",model_type="w_breed")
anov.breed <- anov.breed %>% bind_rows(anov.no_size) %>% adjust_pvalue(method="BH")

dall <- anov.breed %>% select(-`p<.05`)
dall <- plotInfo %>% right_join(dall)
write.csv(dall,"../../data_release/DarwinsArk_20191115_anova.csv",row.names=F)

dall <- dall %>% mutate(psig=if_else(p.adj>0.05,"","*"))
dall <- dall %>% mutate(sig=if_else(p.adj<=0.05,"sig","ns"))

# make supplemental table
supp <- dall %>% rename(Index=id,Class=idtype,Type=questype,Trait=plotname,Model=anova)
supp <- supp %>% mutate(Set=if_else(ds=="cand_breed","candidate purebreds",if_else(ds=="conf_breed","confirmed purebreds",ds)))
supp <- supp %>% select("Index","Class","Type","Trait","Model","Set","Effect","DFn","DFd","F","p","ges","p.adj")
supp <- supp %>% rename(`p.adj (BH)`=p.adj) %>% mutate(Class=if_else(Type=="physical trait",Type,paste("behavioral",Class)))
supp <- supp %>% mutate(Trait=str_replace(Trait,"F[0-9] Factor","Factor"))
supp <- supp %>% arrange(Class,Index,Model,Set,Effect)

write.csv(supp,"Table_ANOVA.supp.csv",row.names=F)

## get magnitude of Effect size difference between candidate and confirmed purebred dogs for text
compare <- supp %>% filter(Effect=="breed") %>% mutate(Set=str_remove(Set," purebred")) %>%  select(Index,Class,Type,Trait,Model,Set,ges) %>% pivot_wider(names_from=Set,values_from=ges)
pvalue <- cor.test(compare$candidates,compare$confirmeds,exact=TRUE)$p.value
cor <- cor.test(compare$candidates,compare$confirmeds,exact=TRUE)$estimate
compare %>% mutate(ratio=candidates/confirmeds) %>% summarize(mean=mean(ratio),sd=sd(ratio),min=min(ratio),max=max(ratio),n=n())

qf_counts <- allscores %>% ungroup() %>% select(idtype,ds,qf,dog) %>% distinct() %>% group_by(idtype,ds,qf) %>% count() %>% ungroup() %>% group_by(idtype,ds) %>% summarize(min_ndogs=min(n),max_ndogs=max(n)) %>% mutate(nstr=paste(min_ndogs,"-",max_ndogs," dogs/",idtype,sep=""))
dall <- dall %>% left_join(qf_counts)
limits <- dall %>% filter(p.adj>0) %>% group_by(idtype,ds,anova) %>% summarize(lowestp=min(p.adj))

dall <- dall %>% inner_join(limits) %>% mutate(p.adj.label=if_else(p.adj==0,"0",format(p.adj,digits=2))) 
dall <- dall %>% mutate(facet=if_else(anova=="sex+age+size","without breed in model",if_else(anova=="breed+sex+age+size"|anova=="breed+sex+age","with breed in model","error")))
dall <- dall %>% mutate(facet=if_else(ds=="conf_breed",paste("confirmed purebred dogs",facet,sep="\n"),if_else(ds=="cand_breed",paste("candidate purebred dogs",facet,sep="\n"),if_else(ds=="all_dogs",paste("all dogs",facet,sep="\n"),paste(ds,facet,sep="\n")))))
dall <- dall %>% mutate(facet=paste(facet,nstr,sep="\n"))

#### plot factors 
pd <- dall %>% filter(idtype=="factor") 
subtitle <- paste("Anova 1-tailed analysis of factors (dogs with age, sex, size and breed (>",min_dog_age_yr," years old)\nanova_test in rstatix 0.7.0\n",sep="")
subtitle <- paste(subtitle,dogcounts %>% filter(ds=="cand_breed" & idtype=="factor") %>% pull(subtitle),"\n",sep="")
subtitle <- paste(subtitle,dogcounts %>% filter(ds=="conf_breed" & idtype=="factor") %>% pull(subtitle),"\n",sep="")
subtitle <- paste(subtitle,"y is Generalized Eta-Squared measure of effect size. p-values are BH-adjusted",sep="\n")

pd$Effect <- factor(pd$Effect, levels = c("breed","age","sex","size"))

p <- ggplot(pd,aes(x=id,y=ges))
p <- p + geom_bar(aes(fill=model_type),alpha=0.8,stat="identity",width=0.75)
p <- p + geom_hline(yintercept=log2(1),color="grey20")
p <- p + scale_fill_manual(values=c("#B40F20","grey30"))
p <- p + scale_color_manual(values=c("#B40F20","grey30"))
p <- p + geom_text(aes(label=psig),vjust=0, hjust=0.5,position = position_dodge(0.75), size=1.5,color="grey30")
p <- p + scale_y_continuous("Effect size (ges)",limits=c(0,0.25),breaks=c(0,0.1,0.2))
p <- p + scale_x_continuous("behavior factor",breaks=c(1:8)) #,breaks=c(0:5)*0.2) #,limits=c(0,1)) #breaks=yaxisF$yat)
p <- p + ggtitle("Effect of age, sex, size and breed on factor scores",subtitle=subtitle)
p <- p + facet_grid(facet~Effect) #,scales="free_y")
p <- p + theme_minimal()
p <- p + theme(legend.position="none",
               strip.text.y=element_text(size=5,angle=0,hjust=0), 
               strip.text.x=element_text(size=5,face="bold"),
               plot.title=element_text(size=6,face="bold"),
               panel.grid.minor=element_blank(), 
               panel.grid.major.x=element_blank(),
               panel.grid.major.y=element_line(size=0.2,color="grey70"),
               plot.subtitle=element_text(size=4),
               axis.text.y = element_text(hjust=1,size=5),
               axis.title.y = element_text(hjust=0.5,size=6,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"))
ggsave(plot=p,filename="Fig_ANOVA_BAR_ALL.pdf",width=3.5,height=3)

pd <- pd %>% filter(ds=="conf_breed"&model_type=="w_breed")
subtitle <- paste("Anova 1-tailed analysis of factors\nDogs with age, sex, size and breed (>",min_dog_age_yr," years old)\nanova_test in rstatix 0.7.0\n",sep="")
subtitle <- paste(subtitle,dogcounts %>% filter(ds=="conf_breed" & idtype=="factor") %>% pull(subtitle),"\n",sep="")
subtitle <- paste(subtitle,"y is Generalized Eta-Squared measure of effect size\np-values are BH-adjusted",sep="\n")
subtitle <- paste(subtitle,pd %>% select(id,DFd) %>% distinct() %>% mutate(string=paste("Factor ",id,": ",DFd," dogs",sep="")) %>% summarize(string=paste(string,collapse="\n")) %>% pull(string),sep="\n")
p <- ggplot(pd,aes(x=id,y=ges))
p <- p + geom_bar(alpha=0.8,stat="identity",width=0.75)
p <- p + geom_hline(yintercept=log2(1),color="grey20")
p <- p + geom_text(aes(label=psig),vjust=0, hjust=0.5,position = position_dodge(0.75), size=1.5,color="grey30")
p <- p + scale_y_continuous("Effect size (ges)",limits=c(0,0.25),breaks=c(0,0.1,0.2))
p <- p + scale_x_continuous("behavior factor",breaks=c(1:8)) #,breaks=c(0:5)*0.2) #,limits=c(0,1)) #breaks=yaxisF$yat)
p <- p + ggtitle("Effect of age, sex, size and breed on factor scores",subtitle=subtitle)
p <- p + facet_grid(Effect~.) #,scales="free_y")
p <- p + theme_minimal()
p <- p + theme(legend.position="none",
               strip.text.y=element_text(size=5,angle=0,hjust=0), 
               strip.text.x=element_text(size=5,face="bold"),
               plot.title=element_text(size=4,face="bold"),
               panel.grid.minor=element_blank(), 
               panel.grid.major.x=element_blank(),
               panel.grid.major.y=element_line(size=0.2,color="grey70"),
               plot.subtitle=element_text(size=2),
               axis.text.y = element_text(hjust=1,size=5),
               axis.title.y = element_text(hjust=0.5,size=6,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"))
ggsave(plot=p,filename="Fig_ANOVA_BAR_CONF.pdf",width=1.25,height=3)

#### plot questions 
pd <- dall %>% filter(idtype=="question"&model_type=="w_breed"&ds=="conf_breed") 
subtitle <- paste("Anova 1-tailed analysis of questions for dogs with age, sex, size, and breed (>",min_dog_age_yr," years old)\nanova_test in rstatix 0.7.0\n",sep="")
subtitle <- paste(subtitle,dogcounts %>% filter(ds=="conf_breed" & idtype=="question") %>% pull(subtitle),"\n",sep="")
subtitle <- paste(subtitle,"y is Generalized Eta-Squared measure of effect size. p-values are BH-adjusted. Peaks with ges>0.2 are labeled.",sep="\n")
pd$Effect <- factor(pd$Effect, levels = c("breed","age","sex","size"))
pdtext <- plotInfo %>% select(qf,plotname) %>% inner_join(pd) %>% filter(ges>0.17) %>% mutate(plotname=str_remove(plotname,"Q[0-9]+ "))

p <- ggplot(pd,aes(x=id,y=ges))
p <- p + geom_bar(aes(fill=questype),alpha=0.8,stat="identity",width=1, position = position_dodge(width=0.75))
p <- p + geom_hline(yintercept=log2(1),color="grey20")
p <- p + geom_text_repel(aes(label=plotname),color="grey30",segment.size=0.2,min.segment.length = 0.01,size=1.5,nudge_x=-5,nudge_y=0.05,hjust=0,data=pdtext,max.overlaps=100) 
p <- p + scale_fill_manual(values=palette)
p <- p + scale_color_manual(values=palette)

p <- p + scale_y_continuous("Effect size (ges)",breaks=c(0:5)*0.2) #,limits=c(0,1)) #breaks=yaxisF$yat)
p <- p + scale_x_continuous("Question number",breaks=c(1:6)*20)
p <- p + ggtitle("Effect of age, sex, size and breed on question scores",subtitle=subtitle)
p <- p + facet_wrap(~Effect,ncol=2)
p <- p + theme_minimal()
p <- p + theme(legend.position="bottom",strip.text.y=element_text(size=6,face="bold",angle=0,hjust=0), 
               strip.text.x=element_text(size=5,face="bold"),
               plot.title=element_text(size=8,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),axis.text.y = element_text(hjust=1,size=5),
               axis.title.y = element_text(hjust=0.5,size=6,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=5),
               legend.key.size = unit(0.1, 'in'))
ggsave(plot=p,filename="Fig_ANOVA_QUESTIONS.bar.pdf",width=4,height=4.5)

pd <- dall %>% filter(model_type=="w_breed"&ds=="conf_breed") %>% select(qf,Effect,F,p.adj,ds,facet,sig,ges)  %>% left_join(plotInfo)
pd <- pd %>% mutate(questype=if_else(questype=="physical trait"|questype=="factor",questype,"behavior"))
pd <- pd %>% select(id,questype) %>% distinct() %>% group_by(questype) %>% count() %>% rename(nqf=n) %>% inner_join(pd) %>% mutate(questype=paste(questype,"\nN=",nqf,sep=""))

pd$Effect <- factor(pd$Effect, levels = c("breed","age","sex","size"))
subtitle <- paste("Anova 1-tailed analysis of questions for dogs with age, sex, size, and breed\ndogs >",min_dog_age_yr," years old\nanova_test in rstatix 0.7.0\n",sep="")
subtitle <- paste(subtitle,dogcounts %>% filter(ds=="conf_breed" & idtype=="factor") %>% pull(subtitle),"\n",sep="")
subtitle <- paste(subtitle,"y is Generalized Eta-Squared measure of effect size\n",sep="")

p <- ggplot(pd,aes(x=Effect,y=ges))
p <- p + geom_boxplot(aes(group=Effect),width=0.75,color="grey30",size=0.3,alpha=0.75,outlier.size = 0.5, outlier.shape = 16)
p <- p + geom_hline(yintercept=log2(1),color="grey20")
p <- p + scale_color_manual(values=c("grey50","#B40F20"))
p <- p + scale_y_continuous("Effect size (ges)")
p <- p + facet_wrap(~questype,nrow=1)
p <- p + ggtitle("Effect of breed, age, sex, and size on behavior scores",subtitle=subtitle)
p <- p + theme_minimal()
p <- p + theme(legend.position = "none",strip.text=element_text(size=7,face="bold"), 
               plot.title=element_text(size=7,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),
               panel.grid.major.x=element_blank(),
               panel.grid.major.y=element_line(size=0.2,color="grey70"),
               axis.title.y = element_text(hjust=0.5,size=7),
               axis.text.x = element_text(hjust=0.5,size=6),
               axis.text.y = element_text(hjust=1,size=6),
               axis.title.x=element_blank())
ggsave(plot=p,"Fig_ANOVA_QUESTYPE.boxplot.pdf",width=3,height=3)


### compare candidate and confirmed
dcomp <- dall %>% filter(ds=="cand_breed") %>% select(qf,idtype,Effect,F,p.adj,ges,anova,plotname,questype) %>% rename(ges.cand=ges,F.cand=F,p.adj.cand=p.adj)
dcomp <- dall %>% filter(ds=="conf_breed") %>% select(qf,idtype,Effect,F,p.adj,ges,anova,plotname,questype) %>% full_join(dcomp)
dcomp <- dcomp %>% filter(Effect=="breed") %>% mutate(ratio=ges/ges.cand)
cor <- dcomp %>% group_by(idtype) %>% summarize(correlation = cor(ges,ges.cand,method="pearson"),p.correlation = cor.test(ges,ges.cand,method="pearson")$p.value)
dcomp <- dcomp %>% filter(Effect=="breed") #&idtype=="question") 

max_val <- ceiling(max(dcomp$ges,dcomp$ges.cand)*10)/10

title <- paste("Candidate and confirmed breeds have similar effect in Anova\nwith higher values for confirmed breeds (",length(unique(dcomp$qf))," questions)",sep="")

subtitle <- paste("Anova 1-tailed analysis of questions & factors  for dogs with age, sex, size, and breed\ndogs >",min_dog_age_yr," years old\nanova_test in rstatix 0.7.0\n",sep="")
subtitle <- paste(subtitle,dogcounts %>% filter(ds=="cand_breed" & idtype=="question") %>% pull(subtitle),"\n",sep="")
subtitle <- paste(subtitle,dogcounts %>% filter(ds=="conf_breed" & idtype=="question") %>% pull(subtitle),"\n",sep="")
subtitle <- paste(subtitle,"Size is excluded from model for question (Q121) because they are the same\n",sep="")
subtitle <- paste(subtitle,"Effect is Generalized Eta-Squared measure of effect size.",sep="")

cor <- cor.test(dcomp$ges,dcomp$ges.cand,exact=TRUE)
cor_string <- paste("R = ",round(cor$estimate,2),", p = ",format(cor$p.value,digits=2),sep="")

p <- ggplot(dcomp,aes(x=ges.cand,y=ges)) + geom_point(aes(color=idtype),shape=16,alpha=0.3,size=2)
p <- p + annotate(label=cor_string,geom="text",x=0,y=0.8,hjust=0,vjust=1,size=2)
#p <- p + geom_smooth(method = "lm",alpha=0.25,color="grey40",size=0.5) # + facet_wrap(~idtype)
p <- p + geom_abline(color="#B40F20",alpha=0.5) 
p <- p + scale_x_continuous("effect of breed (candidate breeds)",limits=c(0,max_val))
p <- p + scale_y_continuous("effect of breed (confirmed breed)",limits=c(0,max_val))
p <- p + ggtitle(title,subtitle=subtitle)
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom",plot.title=element_text(size=6,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=4),axis.text.x = element_text(hjust=0.5,size=6),
               axis.title = element_text(hjust=0.5,size=7),
               axis.text.y = element_text(size=6)
)
ggsave(plot=p,filename="Fig_ANOVA_CAND_v_CONF.plot.pdf",width=3,height=3.5)
write.csv(dcomp,"Fig_ANOVA_CAND_v_CONF.data.csv",row.names=F)

# compare effect and heritability

pd <- dall %>% filter(!is.na(ges)) %>% inner_join(her) %>% mutate(ratio=h2SNP/ges)
pd <- pd %>% filter(model_type=="w_breed"&ds=="conf_breed"&Effect=="breed")

pd <- pd %>% select(qf,id,idtype,ges,h2SNP,ratio)  %>% inner_join(plotInfo)
pdratio <- pd %>% inner_join(plotInfo) %>% filter(ratio>=quantile(pd$ratio,0.95)) %>% mutate(plotname=str_replace(plotname," Factor [0-9]:",""))

cor <- cor.test(pd$ges,pd$h2SNP,exact=TRUE)
cor_string <- paste("R = ",round(cor$estimate,2),", p = ",format(cor$p.value,digits=2),sep="")

pd <- pd %>% mutate(idtype=if_else(id>=120,"physical trait",idtype))
p <- ggplot(pd,aes(x=ges,y=h2SNP)) 
p <- p + geom_smooth(method = "lm",alpha=0.25,color="grey40",size=0.5) # + facet_wrap(~idtype)
p <- p + geom_point(aes(color=idtype,size=idtype,alpha=idtype,shape=idtype))
p <- p + annotate(geom="text",label=cor_string,x=max(pd$ges),y=0.01,hjust=1,vjust=0)
p <- p + ggtitle("Anova effect of breed (confirmed breed dogs) vs heritability")
p <- p+ scale_color_manual(values=c("black","#b2182b","#252525"))
p <- p + scale_size_manual(values=c(2.5,2.5,1.5))
p <- p + scale_alpha_manual(values=c(1,1,0.5))
p <- p + scale_shape_manual(values=c(16,18,21))

p <- p+ scale_x_continuous("Anova ges") 
p <- p+ scale_y_continuous("h2 (SNP)") 

#p <- p+ scale_fill_manual(values=c("#b2182b","#252525"))
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom",plot.title=element_text(size=6,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=4),axis.text.x = element_text(hjust=0.5,size=6),
               axis.title = element_text(hjust=0.5,size=7),
               axis.text.y = element_text(size=6)
)
ggsave(plot=p,filename="Fig_ANOVA_EFFECT_v_HER.pdf",width=4,height=5)
write.csv(pd,"Fig_ANOVA_EFFECT_v_HER.data.csv",row.names=F)

pd <- dall %>% filter(idtype=="factor"&ds=="conf_breed"&(Effect=="breed"|Effect=="age")&model_type=="w_breed")%>%  select(id,qf,Effect,ges) %>% pivot_wider(names_from=Effect,values_from=ges)
p <- ggplot(pd,aes(x=breed,y=age)) + geom_point(size=3,shape=21) + geom_text(aes(label=id),vjust=0.5,hjust=0.5,size=1.25) + scale_x_continuous(limits=c(0,0.2)) + scale_y_continuous(limits=c(0,0.2)) + geom_abline(color="grey50",size=1)
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom",plot.title=element_text(size=6,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=4),axis.text.x = element_text(hjust=0.5,size=6),
               axis.title = element_text(hjust=0.5,size=7),
               axis.text.y = element_text(size=6)
)
ggsave(plot=p,filename="Fig_ANOVA_AGE_V_BREED.pdf",width=2,height=2)


title <- "Effect of breed on different types of questions"
  ### compare question types
  subtitle <- paste("Anova 1-tailed analysis of questions for dogs with\nage, sex, size, and breed\n(dogs >",min_dog_age_yr," years old)\nanova_test in rstatix 0.7.0\n",sep="")
  subtitle <- paste(subtitle,dogcounts %>% filter(ds=="conf_breed" & idtype=="question") %>% pull(subtitle),"\n",sep="")
  subtitle <- paste(subtitle,"Wilcox test (alternative = \"greater\") with BH correction\n",sep="")
  subtitle <- paste(subtitle,"Size is excluded from model for question (Q121) because they are the same\n",sep="")
  subtitle <- paste(subtitle,"y is Generalized Eta-Squared measure of effect size\n",sep="")
  
  questypes <- rev(c("other behavior","motor pattern","physical trait related","physical trait"))
  my_comparisons <- rev(list(c(questypes[1],questypes[4] ),c(questypes[2],questypes[4] ),c(questypes[3],questypes[4] )))
  
  pd <- dall %>% filter(ds=="conf_breed") %>% filter(Effect=="breed") %>% filter(model_type=="w_breed") %>% filter(idtype=="question") %>% select(qf,Effect,F,p.adj,ds,facet,sig,ges) %>% inner_join(plotInfo) 
  xaxis <- pd %>% select(qf,questype) %>% distinct() %>% group_by(questype) %>% count() %>% mutate(xlabel=paste(questype," (N=",n,")",sep="")) 
  xaxis <- xaxis %>% mutate(xlabel=str_replace_all(xlabel," ","\n"))
  
  pd$questype <- factor(pd$questype,levels=questypes)
  pd$Effect <- factor(pd$Effect, levels = c("breed","age","sex","size"))
  
  p <- ggplot(pd,aes(x=questype,y=ges))
  p <- p + geom_boxplot(aes(group=questype),outlier.shape=NA,alpha=0.3,width=0.5,size=0.5,color="grey30")
  p <- p + geom_point(aes(color=sig),shape=16,size=1.5,alpha=0.25)
  p <- p + geom_hline(yintercept=log2(1),color="grey20")
  p <- p + scale_color_manual(values=c("grey50","#B40F20"))
  p <- p + stat_compare_means(label = "p",comparisons = my_comparisons,method="wilcox.test",p.adjust.method = "BH",size=2,method.args=list(alternative = "greater"))
  p <- p + scale_y_continuous("Effect size (ges)")
  p <- p + ggtitle(title,subtitle=subtitle)
  p <- p + scale_x_discrete("",breaks=xaxis$questype,labels=xaxis$xlabel)
  p <- p + theme_minimal()
  p <- p + theme(legend.position = "none",strip.text=element_text(size=6,face="bold",hjust=0), 
                 plot.title=element_text(size=5,face="bold"),panel.grid.minor=element_blank(), 
                 plot.subtitle=element_text(size=3),axis.text.y = element_text(hjust=1,size=5),
                 panel.grid.major.x=element_blank(),
                 panel.grid.major.y=element_line(size=0.2,color="grey70"),
                 axis.title.y = element_text(hjust=0.5,size=6),
                 axis.text.x = element_text(hjust=0.5,size=5),
                 axis.title.x=element_text(hjust=0.5,size=6))
  ggsave(plot=p,"Fig_ANOVA_BREED_QUESTYPE.boxplot.pdf",width=2,height=3)
  
  

