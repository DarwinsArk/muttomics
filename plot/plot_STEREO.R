#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)
permfile = args[1]

library(wesanderson)
library(tidyverse)
library(rstatix)
library(ggpubr)

ok_ndogs <- 25
ok_type <- "candidate_breed"

# define input files
indir <- "../../data_release/"
breedfile <- paste(indir,"master_breed_names.csv",sep="")
facIn <- paste(indir,"DarwinsArk_20191115_factors.csv",sep="")
rankIn <- paste(indir,"ReferenceData_stereotypes_scores.csv",sep="")
permIn <- paste(indir,"DarwinsArk_20191115_survey_permutations.csv",sep="")
breedsIn <- paste(indir,"ReferenceData_breeds.csv",sep="")
dogsIn <- paste(indir,"DarwinsArk_20191115_dogs.csv",sep="")
stereoIn <- paste(indir,"ReferenceData_stereotypes_groups.csv",sep="")
quesIn <- paste(indir,"DarwinsArk_20191115_questions.csv",sep="")

# get ok breed names
breednames <- as_tibble(read.csv(breedsIn,header=T,na.strings=c("","<NA>","NA","n/a","#N/A")))
breednames <- breednames %>% mutate(breed_muttmix=tolower(breed_muttmix)) 
breednames <- breednames %>% rename(breed=breed_name)

# get permutation scores for breeds
ppsd <- as_tibble(read.csv(permIn,header=T) ) #%>% filter(idtype=="factor")
ppsd <- ppsd %>% filter(str_detect(type,"breed"))
ppsd <- ppsd %>% mutate(set=str_replace_all(set,"_"," "))   %>% rename(breed=set)
ppsd <- ppsd %>% mutate(qf=if_else(idtype=="factor",paste("F",id,sep=""),paste("Q",id,sep="")))
ppsd <- ppsd %>% select(idtype,qf,type,ndogs,breed,pcorr,z)
ppsd <- ppsd %>% filter(ndogs==ok_ndogs,type==ok_type) %>% select(-ndogs,-type)

# get information about dogs
dogs <- as_tibble(read.csv(dogsIn,header=T),na.strings=c(""," ","  ","NA","#N/A")) %>% rename(candidate_breed=cand,confirmed_breed=conf)
dogs <- dogs %>% select(id,size,consensus_breed,candidate_breed,confirmed_breed) %>% pivot_longer(c(-id,-size,-consensus_breed)) %>% filter(value) %>% rename(breed=consensus_breed,type=name)
dogs <- dogs %>% filter(type==ok_type)
dogs <- dogs %>% filter(breed %in% breednames$breed)

# get factor info
plotInfo <- as_tibble(read.csv(facIn)) %>% mutate(idtype="factor") %>% rename(id=factor)
plotInfo <- plotInfo %>% mutate(qf=paste("F",id,sep="")) %>% filter(qf %in% ppsd$qf)
plotInfo <- plotInfo %>% mutate(plotname=str_remove(name,"Factor [0-9]+\\:")) %>% mutate(plotname=paste("F",id,plotname,sep=""))
plotInfo <- plotInfo %>% mutate(longname=paste(plotname,"\n(",negative," to ",positive,")",sep=""))
plotInfo <- plotInfo %>% select(qf,idtype,id,plotname,longname,negative,positive)
plotInfo <- plotInfo %>% mutate(questype="factor") %>% distinct() 


ppsd <- ppsd %>% filter(qf %in% plotInfo$qf)

allplotdata <- tibble()

# get breed stereotypes
stereotypes_all <- as_tibble(read.csv(stereoIn,header=T)) 


stereotypes_all <- stereotypes_all %>% filter(breed %in% ppsd$breed)



#in_grouping <- "threewords"
for (in_grouping in c("AKC_group","threewords")){
# get stereotypes for all breed / stereotype pairs

  min_breeds_per_group <- 3
if (in_grouping=="threewords"){ min_breeds_per_group <- 4 }
    
  
outroot <- paste("Fig_STEREO.",in_grouping,sep="")

stereotypes <- stereotypes_all %>% filter(grouping==in_grouping) %>% select(-grouping) %>% filter(breed %in% ppsd$breed)
groups <- crossing(stereotypes %>% select(group) %>% distinct(),ppsd %>% ungroup %>% select(breed) %>% distinct())
stereotypes <- stereotypes %>% mutate(match="in") %>% full_join(groups) %>% replace_na(list(match="out"))

subtitle <- paste("T-test comparing mean z scores for breeds based on ",str_replace_all(in_grouping,"\\_"," ")," (BH adjusted); min breeds per group=",min_breeds_per_group,sep="")
if (ok_type=="candidate_breed"){
  breedtype_subtitle <- "breed definition: candidate purebred"
}
if (ok_type=="confirmed_breed"){
  breedtype_subtitle <- "breed definition: confirmed purebred"
}
subtitle <- paste(subtitle,breedtype_subtitle,sep="\n")

d <- ppsd %>% inner_join(stereotypes)

okgrps1 <- d %>% filter(match=="in") %>% select(group,breed) %>% distinct() %>% group_by(group) %>% count() %>% filter(n>=min_breeds_per_group) %>% select(group)
d <- d %>% inner_join(okgrps1)

allgrps <- d %>% select(group,qf) %>% distinct() 
#allgrps <- d  %>% filter(match=="in") %>% select(group,breed) %>% distinct() %>% group_by(group) %>% count() %>% ungroup() %>% group_by(group) %>% summarize(max_n=max(n)) %>% filter(max_n>min_breeds_per_group) %>% select()

okgrps <- d %>% ungroup() %>% select(qf,group,match,breed) %>% distinct() %>% group_by(qf,group,match) %>% count()


#missing <- okgrps %>% group_by(group,qf) %>% summarize(min_n=min(n)) %>% filter(min_n<min_breeds_per_group)
okgrps <- okgrps %>% group_by(group,qf) %>% summarize(min_n=min(n)) %>% filter(min_n>=min_breeds_per_group) %>% select(-min_n)
## remove groups without values for all 8 factors 
okgrps <- okgrps %>% group_by(group) %>% count() %>% filter(n==8) %>% select(-n)

d <- d %>% inner_join(okgrps)

# run t-test comparing breeds in each group to other breeds
ttest <- d %>% group_by(qf,group) %>% t_test(z~match,conf.level=0.95,detailed=T)  %>% adjust_pvalue(method = "BH") %>% add_significance("p.adj")

ttest <- d  %>% ungroup() %>% group_by(qf,group,match) %>% summarize(mean=mean(z)) %>% mutate(name=if_else(match=="in","meanYes","meanNo")) %>% select(-match) %>% pivot_wider(names_from=name,values_from=mean) %>% right_join(ttest)
ttest <- plotInfo %>% select(qf,plotname,negative,positive) %>% right_join(ttest) %>% mutate(change=if_else(p<0.05,if_else(meanYes>meanNo,positive,negative),"neutral"))

pd <- allgrps %>% full_join(ttest)
pd <- pd %>% mutate(sig=if_else(p.adj<=0.05,TRUE,FALSE))
pd <- pd %>% mutate(sigthree=if_else(p.adj<=0.05,"sig_corr",if_else(p<=0.05,"sig","nonsig")))

pd <- pd %>% mutate(facet=str_replace_all(str_remove(group,"AKC "),"\\_",", "))
pd <- pd %>% mutate(facet=str_replace(facet,"intelligent, ","intelligent,\n"))
pd <- pd %>% filter(!is.na(n1)) %>% select(n1,facet) %>% distinct() %>% group_by(facet) %>% summarize(nbreeds=max(n1)) %>% inner_join(pd) %>% mutate(facet=paste(facet,"\n",nbreeds," breeds",sep=""))
pd <- pd %>% mutate(idtype=if_else(str_detect(qf,"F"),"factor","question"))
pd <- pd %>% filter(qf %in% c("F1","F2","F3","F4","F5","F6","F7","F8"))

pd <- pd %>% select(-plotname)
pd <- plotInfo %>% select(qf,plotname) %>% right_join(pd) 
#missing <- pd %>% filter(is.na(n1)) #%<% mutate(estimate=0.5)
pd <- pd %>% filter(!is.na(n1))

columns <- 5
if (length(unique(pd$facet))<=8){
  columns <- 1
}
height <- 5.5# (length(unique(pd$facet))/columns)*1.25
#missing <- missing %>% mutate(estimate=0,conf.low=0,conf.high=0)

p <- ggplot(pd,aes(y=estimate,x=plotname)) #+ geom_point(aes(color=sig))
p <- p + geom_bar(aes(fill=sigthree),alpha=1,stat="identity",width=0.75)
p <- p + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.4, colour="#252525",size=0.2)
#p <- p + geom_text(label="NA",y=0.5,size=1,data=missing)
p <- p + facet_wrap(~facet,ncol=columns)
p <- p + scale_y_continuous("t-test estimate",breaks=c(-2,-4,-5,0,2,4,5))
#p <- p + scale_x_discrete("",breaks=c("F1","F2","F3","F4","F5","F6","F7","F8"),labels=c(1:8))
p <- p + scale_fill_manual(values=c("#bdbdbd","#737373","#99000d"))
p <- p + theme_minimal()
p <- p + theme(legend.position="top",strip.text=element_text(size=5,face="bold"), 
               plot.title=element_text(size=8,face="bold"),
               panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=4),
               axis.text.x = element_text(hjust=1,size=5,angle=90,vjust=0.5),
               axis.text.y = element_text(hjust=1,size=5),
               axis.title.x=element_blank(),
               axis.title.y=element_text(hjust=0.5,size=5),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=4),
               legend.key.size = unit(0.1, 'in'),
               panel.grid.major.x=element_blank(),
               axis.ticks.y=element_line(size=0.1,color="grey30"),
               strip.text.y = element_text(size = 5,angle=0,hjust=0,vjust=0.5,face="plain"),
               strip.text.x = element_text(size = 5,angle=0,hjust=0.5,vjust=0,face="plain"),
               panel.border = element_rect(colour = "grey30", fill=NA, size=0.5))
ggsave(plot=p,filename=paste(outroot,".bars.pdf",sep=""),width=1.24*columns,height=height)

allplotdata <- ttest %>% mutate(grouping=in_grouping) %>% bind_rows(allplotdata)
# get list of breeds in each stereotype
breedstrings <- d %>% filter(match=="in") %>% select(group,breed) %>% distinct()
breedstrings <- breednames %>% select(breed,breed_name_short) %>% distinct() %>% right_join(breedstrings) %>% distinct()
breedstrings <- breedstrings %>% select(group,breed,breed_name_short) %>% distinct() %>% group_by(group) %>% summarize(breeds=paste(breed,collapse=", "),n=n()) 
breedstrings <- breedstrings %>% mutate(xlabel=paste(n," ",group," breeds: ",breeds,sep=""))

pd <- d 
pd <- plotInfo %>% select(qf,longname) %>% right_join(pd)  
pd <- pd %>% mutate(column_facet=str_replace_all(longname," to "," to \n"))
pd <- pd %>% mutate(column_facet=str_replace_all(column_facet," Motor Patterns","\nMotor Patterns"))
pd <- pd %>% mutate(column_facet=str_replace_all(column_facet," Engagement","\nEngagement"))
pd <- pd %>% mutate(y=if_else(match=="out",2,1))


row_facets <- ttest %>% group_by(group)  %>% summarize(n1=max(n1),n2=max(n2),minp=min(p.adj))
row_facets <- row_facets %>% mutate(row_facet=str_replace_all(group,",","\n")) %>% mutate(row_facet=str_replace_all(row_facet,"smart_","smart,\n")) %>% mutate(row_facet=str_replace_all(row_facet,"_",", ")) %>% mutate(row_facet=str_replace_all(row_facet,"-","-\n"))
row_facets <- row_facets %>% mutate(row_facet=str_remove(row_facet,"STEREO "))
row_facets <- row_facets %>%  mutate(row_facet=paste(n1," ",row_facet,"\n",n2," others",sep="")) 
row_facets <- row_facets %>%  arrange(minp)
row_facets$order <- c(1:length(row_facets$group)) 
row_facets <- row_facets %>% mutate(row_facet=if_else(order>=10,paste(order,"\n",row_facet,sep=""),paste("0",order,"\n",row_facet,sep="")))
row_facets <- row_facets %>% select(group,row_facet)

pd <- pd %>% left_join(row_facets)



pd_arrow <- ttest %>% mutate(y=2,yend=1) #%>% filter(p<=0.05)
pd_arrow <- pd_arrow %>% mutate(colorset=if_else(p>0.05,"(1) not sig",if_else(p.adj>0.05,"(2) not sig after corr","(3) sig")))
pd_arrow <- pd %>% select(row_facet,column_facet,group,qf) %>% distinct() %>% right_join(pd_arrow)
pd_arrow <- pd_arrow %>% mutate(p.adj.format=paste("p=",round(p,4),sep="")) %>% mutate(p.adj.format=if_else(p.adj<=0.05,paste(p.adj.format,p.adj.signif,sep=""),paste(p.adj.format," (",p.adj.signif,")",sep="")))


pd_qt <- pd  %>% group_by(group,qf,row_facet,column_facet,match,y) %>% summarize(median=median(z),mean=mean(z),Q25=quantile(z,0.25),Q75=quantile(z,0.75),QLow=max(quantile(z,0.25)-(1.5*IQR(z)),min(z)),QHigh=min(quantile(z,0.75)+(1.5*IQR(z)),max(z)))
pd_qt <- ttest %>% select(group,qf,p,p.adj) %>% right_join(pd_qt)
pd_qt <- pd_qt %>% mutate(colorset=if_else(p>0.05,"(1) not sig",if_else(p.adj>0.05,"(2) not sig after corr","(3) sig")))

xlimits <- pd %>% filter(match=="in") %>% select(qf,z)
xlimits <- pd_qt %>% filter(y==2) %>% select(qf,QLow,QHigh) %>% pivot_longer(c(-qf)) %>% select(-name) %>% rename(z=value) %>% bind_rows(xlimits)
xlimits <- xlimits %>% group_by(qf) %>% summarize(minX=min(z),maxX=max(z)) 

pd_arrowstext <- ttest %>% filter(p<=0.05) %>% left_join(xlimits) %>% mutate(just=if_else(meanYes<meanNo,0,1)) #%>% mutate(meanYes=if_else(meanYes<meanNo,meanYes-0.5,meanYes+0.5))
pd_arrowstext <- pd_arrow %>% select(column_facet,row_facet,group,qf,colorset) %>% distinct() %>% right_join(pd_arrowstext)
pd_arrowstext <- pd_arrowstext %>% mutate(xpos=if_else(meanYes>meanNo,maxX,minX),just=if_else(meanYes>meanNo,1,0))
pd_arrowstext <- pd_arrow %>% select(group,qf,p.adj.format) %>% right_join(pd_arrowstext)
pd_arrowstext <- pd_arrowstext %>% mutate(change=str_remove(change,"ment"))
pd_arrowstext <- pd_arrowstext %>% mutate(change=paste(p.adj.format,change,sep="\n"))


subtitle <- paste(subtitle,breedstrings %>% filter(group %in% pd$group) %>% select(xlabel) %>% distinct() %>% mutate(subtitle=paste(xlabel,collapse="\n")) %>% pull(subtitle),sep="\n")

columns <- length(unique(pd$column_facet))
rows <- length(unique(pd$row_facet))
width <- (columns+1)*1
height <- (rows+1)*0.75

p <- ggplot(pd,aes(y=y))
p <- p + geom_segment(aes(color=colorset,y=y-0.25,yend=yend+0.25,x=meanNo,xend=meanYes), size=0.4, arrow = arrow(length=unit(0.1,"cm"), type = "open"),data=pd_arrow)
p <- p + geom_segment(aes(x=QLow,xend=QHigh,y=y,yend=y),data=(pd_qt  %>% filter(y==2)),size=0.25,color="grey50")
p <- p + geom_rect(aes(ymax=y+0.15,ymin=y-0.15,xmin=Q25,xmax=Q75),data=(pd_qt %>% filter(y==2)),size=0.25,color="grey50",fill="white")
p <- p + geom_segment(aes(y=y+0.15,yend=y-0.15,color=colorset,xend=mean,x=mean),data=pd_qt,size=0.4,color="grey30")
p <- p + geom_point(aes(x=z),data=(pd %>% filter(y==1)),size=0.75,shape=21,alpha=0.5)
p <- p + geom_text(aes(x=xpos,label=change,color=colorset,hjust=just),lineheight=1, y=0.75,size=1.25,vjust=1,data=pd_arrowstext,alpha=0.75)
p <- p + scale_color_manual(values=c("#878787","#1a1a1a","#d6604d"),breaks=c("(1) not sig","(2) not sig after corr","(3) sig")) #"),"#67001f"))
p <- p + scale_y_continuous("group",breaks=c(1,2),labels=c("breeds in\ngroup","other\nbreeds"),limits=c(0.25,2.3))
p <- p + scale_x_continuous("mean",breaks=c(-2:2)*2,expand = expansion(mult = c(0.2,0.2)))
p <- p + ggtitle("change in ancestral groups vs all breeds",subtitle=subtitle)
p <- p + facet_grid(row_facet~column_facet,scales="free_x",space="free_x")
p <- p + theme_minimal()
p <- p + theme(legend.position="none",strip.text=element_text(size=5,face="bold"), 
               plot.title=element_text(size=8,face="bold"),
               panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=4),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.text.y = element_text(hjust=1,size=7),
               axis.title=element_blank(),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=4),
               legend.key.size = unit(0.1, 'in'),
               axis.ticks.y=element_blank(),
               panel.grid.major=element_blank(),
               axis.ticks.x=element_line(size=0.1,color="grey30"),
               strip.text.y = element_text(size = 6,angle=0,hjust=0,vjust=0.5,face="plain"),
               strip.text.x = element_text(size = 4,angle=0,hjust=0.5,vjust=0,face="plain"),
               panel.border = element_rect(colour = "grey30", fill=NA, size=0.5))

  ggsave(plot=p,filename=paste(outroot,".lines.pdf",sep=""),width=width,height=height)
}

# make heatmap of encyclopedia score vs PPS z score
rankings <- as_tibble(read.csv(rankIn,header=T,na.strings=c("","NA","#N/A"))) 

# remove breeds without PPS scores
rankings <- rankings %>% filter(breed %in% ppsd$breed)

# define plotting order
order <- tibble(trait=c("energy level","exercise requirements","playfulness","affection level","friendliness towards dogs","friendliness towards other pets","friendliness towards strangers","ease of training","watchdog ability","protection ability"),order=c(1:10))

breeds <- rankings %>% ungroup() %>% select(breed) %>% distinct()
traits <- rankings %>% ungroup() %>% select(trait) %>% distinct()
all <- crossing(breeds,traits) %>% full_join(rankings) %>% left_join(order) %>% arrange(breed,order)
d <- ppsd %>% inner_join(rankings) %>% filter(idtype=="factor")
nbreeds <- d %>% select(idtype,breed,qf) %>% distinct() %>% group_by(idtype,qf) %>% count() %>% rename(nbreeds=n)

d <- d %>% group_by(idtype,qf,trait) %>% summarize(nbreedsInc=n(),cor = cor(z,ranking,method="pearson"),p = cor.test(z,ranking,method="pearson")$p.value,
                                                   conf.low = cor.test(z,ranking,method="pearson")$conf[1],conf.high = cor.test(z,ranking,method="pearson")$conf[2],n=n(),
                                                   statistic=cor.test(z,ranking,method="pearson")$statistic,df=cor.test(z,ranking,method="pearson")$parameter) %>% adjust_pvalue(method="BH")
d <- d %>% add_significance("p.adj")
d <- d %>% mutate(p.adj.signif=if_else(p.adj<0.001,"***",if_else(p.adj<=0.01,"**",if_else(p.adj<=0.05,"*"," (ns)"))))
d <- plotInfo %>% right_join(d)
d <- d %>% mutate(change=if_else(p<=0.05,if_else(cor>0,positive,negative),"neutral"))

pd <- d 
pd <- pd %>% mutate(sigthree=if_else(p.adj<=0.05,"sig_corr",if_else(p<=0.05,"sig","nonsig")))
p <- ggplot(pd,aes(y=cor,x=plotname)) #+ geom_point(aes(color=sig))
p <- p + geom_bar(aes(fill=sigthree),alpha=1,stat="identity",width=0.75)
p <- p + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.4, colour="#252525",size=0.2)
p <- p + facet_wrap(~trait,ncol=5)
p <- p + scale_y_continuous("pearson correlation") #,breaks=c(-1,0,1))
#p <- p + scale_x_discrete("",breaks=c("F1","F2","F3","F4","F5","F6","F7","F8"),labels=c(1:8))
p <- p + scale_fill_manual(values=c("#bdbdbd","#737373","#99000d"))
p <- p + theme_minimal()
p <- p + theme(legend.position="top",strip.text=element_text(size=5,face="bold"), 
               plot.title=element_text(size=8,face="bold"),
               panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=4),
               axis.text.x = element_text(hjust=1,size=5,angle=90,vjust=0.5),
               axis.text.y = element_text(hjust=1,size=5),
               axis.title.x=element_blank(),
               axis.title.y=element_text(hjust=0.5,size=5),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=4),
               legend.key.size = unit(0.1, 'in'),
               panel.grid.major.x=element_blank(),
               axis.ticks.y=element_line(size=0.1,color="grey30"),
               strip.text.y = element_text(size = 5,angle=0,hjust=0,vjust=0.5,face="plain"),
               strip.text.x = element_text(size = 5,angle=0,hjust=0.5,vjust=0,face="plain"),
               panel.border = element_rect(colour = "grey30", fill=NA, size=0.5))
ggsave(plot=p,filename=paste("Fig_STEREO.encyclopedia",".bars.pdf",sep=""),width=6.5,height=4)


### make supplemental info file 
d1 <- as_tibble(read.csv("Fig_STEREO.encyclopedia.corr.data.csv",header=T) %>% mutate(grouping="encyclopedia scores")) %>% rename(stereotype.phenotype=trait,nbreeds=nbreedsInc,estimate=cor) %>% mutate(stat.type="pearson.corr")
d2 <- as_tibble(read.csv("Fig_STEREO.threewords.ttest.csv",header=T) %>% mutate(grouping="threewords")) %>% rename(stereotype.phenotype=group) %>% mutate(nbreeds=n1+n2,stat.type="t.test")
d3 <- as_tibble(read.csv("Fig_STEREO.AKC_group.ttest.csv",header=T) %>% mutate(grouping="AKC group")) %>% rename(stereotype.phenotype=group)%>% mutate(nbreeds=n1+n2,stat.type="t.test")
d <- d1 %>% bind_rows(d2) %>% bind_rows(d3)
d <- d %>% mutate(factor=as.integer(str_remove(qf,"F")))
d <- d %>% select(factor,plotname,grouping,stereotype.phenotype,change,p.adj.signif,nbreeds,stat.type,statistic,estimate,conf.low,conf.high,p,p.adj,df,n1,n2,meanYes,meanNo)

write.csv(d,"Table_STEREO.supp.csv",row.names=F)

