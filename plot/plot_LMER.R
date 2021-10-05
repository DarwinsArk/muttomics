#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)

library(tidyverse)
library(forcats)
library(ggrepel)
library(wesanderson)
library(rstatix)

outdir <- "plots"

indir <- "../../data_release/"
outroot<- "Fig_LMER"

breednames <- as_tibble(read.csv(paste(indir,"/ReferenceData_breeds.csv",sep=""))) %>% rename(breed=breed_name)
breednames <- breednames %>% mutate(breed_name_short=if_else(is.na(breed_name_short),breed,breed_name_short))

permIn <- paste(indir,"DarwinsArk_20191115_survey_permutations.csv",sep="")
LMERIn <- paste(indir,"DarwinsArk_20191115_LMER_models.csv",sep="")
facIn <- paste(indir,"DarwinsArk_20191115_factors.csv",sep="")
quesIn <- paste(indir,"DarwinsArk_20191115_questions.csv",sep="")
herIn <- paste(indir,"DarwinsArk_20191115_heritability_all-SNPs.csv",sep="")
validIn <- paste(indir,"ReferenceData_breed_standards_byQuestion.csv",sep="")
sizeValidIn <- paste(indir,"ReferenceData_breed_stature.csv",sep="")

d <- as_tibble(read.csv(LMERIn,header=T))
d <- d %>% mutate(idtype=if_else(str_detect(type,"factor"),"factor","question"))
d <- d %>% mutate(qf=if_else(idtype=="question",paste("Q",id,sep=""),paste("F",id,sep="")))

d <- d %>% mutate(breed=str_replace_all(breed,"\\_"," "))
d <- breednames %>% select(breed,breed_name_short) %>% distinct() %>% inner_join(d) 

# get titles etc for labelling plots
facInfo <- as_tibble(read.csv(facIn)) %>% mutate(idtype="factor")
facInfo <- facInfo %>% select(idtype,factor,name,negative,positive) %>% distinct() %>% rename(id=factor,string=name)
facInfo <- facInfo %>% filter(id <= 8) %>%  mutate(plotname=paste(string," (",negative," to ",positive,")",sep="")) %>% mutate(plotname=str_replace(plotname,"Factor ","F")) #%>% mutate(ylabel=paste(string," (",negative," to ",positive,")",sep=""),ylabel_long=paste(plotname," (",negative," to ",positive,")",sep=""))
facInfo <- facInfo %>% select(idtype,id,plotname,string,negative,positive)
facInfo <- facInfo %>% mutate(string=str_replace(string,"Factor ","F"))
facInfo <- facInfo %>% mutate(qf=paste("F",id,sep=""))
facInfo <- facInfo %>% mutate(question.type="factor")

quesInfo <- as_tibble(read.csv(quesIn,header=T)) %>% mutate(idtype="question")
quesInfo <- quesInfo %>% select(idtype,id,string,abbr_text,negative,positive,question.type) %>% distinct() 
quesInfo <- quesInfo %>% mutate(plotname=paste("Q",id,": ",abbr_text," (",negative," to ",positive,")",sep=""))  #%>% mutate(ylabel=paste(plotname_order,": ",abbr_text," (",negative," to ",positive,")",sep=""),ylabel_long=paste(plotname," (",negative," to ",positive,")",sep=""))
quesInfo <- quesInfo %>% mutate(string=paste("Q",id,": ",abbr_text,sep=""))  #%>% mutate(ylabel=paste(plotname_order,": ",abbr_text," (",negative," to ",positive,")",sep=""),ylabel_long=paste(plotname," (",negative," to ",positive,")",sep=""))
quesInfo <- quesInfo %>% select(idtype,id,plotname,question.type,string,negative,positive) %>% mutate(question.type=str_replace(question.type,"aging related surveys","other behavior")) 
quesInfo <- quesInfo %>% mutate(qf=paste("Q",id,sep=""))

plotinfo <- facInfo %>% bind_rows(quesInfo) %>% mutate(id=qf)
d <- plotinfo %>% select(qf,question.type,plotname) %>% distinct() %>% right_join(d)

# get list of breeds with and without PPS results
new <- as_tibble(read.csv(permIn,header=T)) %>% filter(str_detect(type,"breed")) %>% select(set,id,idtype) %>% distinct() %>% rename(breed=set) %>% mutate(breedtype="surveys")
new <- new %>% mutate(breed=str_replace_all(breed,"\\_"," ")) %>% filter(breed %in% breednames$breed) 
new <- d %>% select(id,idtype,breed) %>% distinct() %>% left_join(new) %>% replace_na(list(breedtype="new"))
d <- new %>% right_join(d)
d <- d %>% mutate(breedtype=if_else(str_detect(breed,"cocker"),"surveys",breedtype))
d <- d %>% group_by(qf) %>% summarize(minp=min(ML.anova.p.adj_benjhoch_FDR)) %>% inner_join(d)

d <- d %>% mutate(page=if_else(question.type!="other behavior","pg1",if_else(minp<0.05,"pg2","insig")))

pd <- d
pd <- pd %>% mutate(sig=if_else(ML.anova.p.adj_benjhoch_FDR<=0.05,TRUE,FALSE))
#pd <- pd %>% mutate(colorset=if_else(sig,paste("sig",breedtype),paste("nonsig",breedtype)))
pdtext <- pd %>% filter(sig) %>% mutate(breed_name_short=if_else(breedtype=="new",paste(breed_name_short,"*",sep=""),breed_name_short))


#order <- cor %>% filter(qf %in% pd$qf) %>% group_by(idtype,plotname,qf) %>% summarize(correlation=max(correlation)) %>% arrange(idtype,desc(correlation)) %>% pull(plotname)
order <- pd %>% select(idtype,id,plotname) %>% distinct() %>% arrange(idtype,id) %>% pull(plotname)

pd$question.type <- factor(pd$question.type,levels=c("factor","physical trait","physical trait related","motor pattern","other behavior"))
pdtext$question.type <- factor(pdtext$question.type,levels=c("factor","physical trait","physical trait related","motor pattern","other behavior"))
pd$plotname <- factor(pd$plotname,levels=order)
pdtext$plotname <- factor(pdtext$plotname,levels=order)


p <- ggplot(pd,aes(x=REML.t.val,y=plotname)) 
p <- p + geom_vline(xintercept=0,color="grey50",size=0.2)
p <- p + geom_point(aes(color=sig),size=0.75,shape=16,alpha=0.5)
p <- p + geom_text_repel(aes(label=breed_name_short),color="#A41E22",direction="both",max.overlaps=50,segment.size=0.1,size=1.25,min.segment.length = 0,data=pdtext)
p <- p + theme_minimal()
p <- p + facet_grid(question.type~.,scales="free_y",space="free_y")
p <- p + scale_color_manual(values=c("#000000","#a50f15")) 
#p <- p + scale_alpha_manual(values=c(0.5,1))
p <- p + scale_x_continuous("What the mutts say - does breed ancestry influences trait? (REML.t.val)")
p <- p + scale_y_discrete(limits=rev)
#p <- p + ggtitle(title)
p <- p + theme(legend.position="none",strip.text=element_text(size=6,face="bold"), 
               plot.title=element_text(size=6,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),
               axis.title.y = element_blank(),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.text.y = element_text(hjust=1,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"),
               strip.text.x=element_text(hjust=0.5,size=6))
nrow <- length(unique(pd$plotname))
ggsave(plot=p,filename=paste(outroot,"_all.pdf",sep=""),limitsize = FALSE,width=6.5,height=nrow*0.5)

##nrow <- length(unique((pdsig %>% filter(type!="physical trait"))$plotname))

for (inPage in unique(d$page)){
  if (inPage != "insig"){
    title <- "all other questions; only those with significant results"
    if (inPage=="pg1"){
      title <- "factors, physical traits, and motor patterns; all results"
    }
pd <- d %>% filter(page==inPage)
pd <- pd %>% mutate(sig=if_else(ML.anova.p.adj_benjhoch_FDR<=0.05,TRUE,FALSE))
pd <- pd %>% mutate(colorset=if_else(sig,paste("sig",breedtype),paste("nonsig",breedtype)))
pdtext <- pd %>% filter(sig) %>% mutate(breed_name_short=if_else(breedtype=="new",paste(breed_name_short,"*",sep=""),breed_name_short))


#order <- cor %>% filter(qf %in% pd$qf) %>% group_by(idtype,plotname,qf) %>% summarize(correlation=max(correlation)) %>% arrange(idtype,desc(correlation)) %>% pull(plotname)
order <- pd %>% select(idtype,id,plotname) %>% distinct() %>% arrange(idtype,id) %>% pull(plotname)

pd$question.type <- factor(pd$question.type,levels=c("factor","physical trait","physical trait related","motor pattern","other behavior"))
pdtext$question.type <- factor(pdtext$question.type,levels=c("factor","physical trait","physical trait related","motor pattern","other behavior"))
pd$plotname <- factor(pd$plotname,levels=order)
pdtext$plotname <- factor(pdtext$plotname,levels=order)

p <- ggplot(pd,aes(x=REML.t.val,y=plotname)) 
p <- p + geom_vline(xintercept=0,color="grey50",size=0.2)
p <- p + geom_point(aes(color=sig),size=0.75,shape=16,alpha=0.5)
p <- p + geom_text_repel(aes(label=breed_name_short),color="#A41E22",direction="both",max.overlaps=50,segment.size=0.1,size=1.25,min.segment.length = 0,data=pdtext)
p <- p + theme_minimal()
p <- p + facet_grid(question.type~.,scales="free_y",space="free_y")
p <- p + scale_color_manual(values=c("#000000","#a50f15")) 
#p <- p + scale_alpha_manual(values=c(0.5,1))
p <- p + scale_x_continuous("What the mutts say - does breed ancestry influences trait? (REML.t.val)")
p <- p + scale_y_discrete(limits=rev)
p <- p + ggtitle(title)
p <- p + theme(legend.position="none",strip.text=element_text(size=6,face="bold"), 
               plot.title=element_text(size=6,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),
               #panel.grid.major.x=element_blank(), 
               axis.title.y = element_blank(),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.text.y = element_text(hjust=1,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"),
               #legend.title = element_blank(),
               #legend.text=element_text(hjust=0.5,size=6),
               #legend.key.size = unit(0.1, 'in'),
               strip.text.x=element_text(hjust=0.5,size=6))
nrow <- length(unique(pd$plotname))

ggsave(plot=p,filename=paste(outroot,".LMER_only.",inPage,".pdf",sep=""),limitsize = FALSE,width=6.5,height=9)
}
}

her <- as_tibble(read.csv(herIn,header=T,na.strings=c("NA","#N/A",""," ","N/A")))
her <- her %>% mutate(idtype=if_else(str_detect(class,"factor"),"factor","question")) %>% rename(id=index)
her <- her %>% filter(type=="Variance"&set=="all dogs") %>% filter(trait!="Size (Tiny)"&trait!="Size (Giant)")
her <- her %>% select(id,idtype,h2SNP)
d <- her %>% inner_join(d)

perm <- as_tibble(read.csv(permIn)) %>% select(-ndogs) %>% rename(cand_or_conf=type)
perm <- perm %>% rename(breed=set) %>% mutate(breed=str_replace_all(breed,"\\_"," "))
perm <- perm %>% filter(breed %in% breednames$breed) 

pd <- perm %>% inner_join(d) 
pd <- plotinfo %>% select(qf,question.type) %>% distinct() %>% right_join(pd)

perm <- as_tibble(read.csv(permIn)) %>% filter(type=="candidate_breed") %>% filter(ndogs==25) %>% select(-type,-ndogs)
perm <- perm %>% rename(breed=set) %>% mutate(breed=str_replace_all(breed,"\\_"," "))
perm <- perm %>% filter(breed %in% breednames$breed) 

pd <- perm %>% inner_join(d) 

## MAKE VALIDATION TABLES
pd <- pd %>% mutate(qtype2=if_else(question.type=="physical trait",question.type,"behavior"))
valid <- pd %>% select(qf,qtype2,id,idtype,breed,z,p,pcorr,REML.t.val,ML.anova.p.adj_benjhoch_FDR) %>% filter(ML.anova.p.adj_benjhoch_FDR<0.05&(p<0.025|p>0.975)) %>% filter((REML.t.val<0&z<0)|(REML.t.val>0&z>0)) %>% group_by(qtype2) %>% count()  %>% rename(nmatch=n)
valid <- pd %>% select(qf,qtype2,id,idtype,breed,z,p,pcorr,REML.t.val,ML.anova.p.adj_benjhoch_FDR) %>% filter(ML.anova.p.adj_benjhoch_FDR<0.05&(p<0.025|p>0.975)) %>% group_by(qtype2) %>% count()  %>% rename(ntot=n)  %>% full_join(valid)
valid <- valid %>% mutate(frac=nmatch/ntot)
print(valid)

breedsizes <- as_tibble(read.csv(sizeValidIn,header=T) %>% mutate(breed=tolower(breed)))
breedsizes <- d %>% filter(id==121) %>% inner_join(breedsizes) 
cor.test(breedsizes$height.cm,breedsizes$REML.t.val)
breedsizes <- breedsizes %>% filter(ML.anova.p.adj_benjhoch_FDR<0.05)
cor.test(breedsizes$height.cm,breedsizes$REML.t.val)

standards <- as_tibble(read.csv(validIn,header=T,na.strings=c("NA","","#N/A","na")))
standards <- d  %>% inner_join(standards) %>% filter(ML.anova.p.adj_benjhoch_FDR<=0.05)
standards <- quesInfo %>% select(idtype,id,string,negative,positive) %>% right_join(standards)
standards <- standards %>% mutate(prediction=if_else(REML.t.val<0,negative,positive))
standards <- standards %>% select(breed,idtype,id,plotname,REML.t.val,ML.anova.p.adj_benjhoch_FDR,direction.of.change,prediction)

valid2 <- standards %>% group_by(plotname) %>% count() %>% rename(ntot=n)
valid2 <- standards %>% filter(direction.of.change==prediction) %>% group_by(plotname) %>% count() %>% rename(nmatch=n) %>% full_join(valid2)
valid2 <- standards %>% filter(direction.of.change=="other") %>% group_by(plotname) %>% count() %>% rename(nother=n) %>% full_join(valid2)
valid2 <- standards %>% group_by(plotname,direction.of.change) %>% count() %>% mutate(string=paste(n,direction.of.change)) %>% group_by(plotname) %>% summarize(string=paste(string,collapse="; ")) %>% full_join(valid2)
valid2 <- valid2 %>% replace_na(list(nother=0)) 
valid2 <- valid2 %>% summarize(ntot=sum(ntot),nother=sum(nother),nmatch=sum(nmatch)) %>% mutate(plotname="all") %>% bind_rows(valid2)
valid2 <- valid2 %>% mutate(percent.correct=nmatch/(ntot-nother))
print(valid2)

pd <- plotinfo %>% select(qf,question.type) %>% distinct() %>% right_join(pd)

cor <- pd %>% group_by(plotname,idtype,qf,question.type) %>% summarize(n=n(),correlation = cor(z,REML.t.val,method="pearson"),p = cor.test(z,REML.t.val,method="pearson")$p.value,ciL=cor.test(z,REML.t.val,method="pearson")$conf.int[1],ciH=cor.test(z,REML.t.val,method="pearson")$conf.int[2])
cor <- cor %>% adjust_pvalue(method="BH") %>% add_significance("p.adj") 
cor <- cor %>% mutate(sig=if_else(p.adj<=0.05,TRUE,FALSE))
cor <- as_tibble(cor) 

xlabels <- pd %>% group_by(qf) %>% count() 
xlabels <- plotinfo %>% select(qf,string ) %>% distinct() %>% inner_join(xlabels)
xlabels <- xlabels %>% mutate(label=paste(string," (N=",n,")",sep=""))

sigpd <- cor %>% filter(p.adj<=0.05) %>% select(question.type,idtype,qf,ciH,correlation,p.adj,p.adj.signif,sig) %>% mutate(xpos=ciH+0.02)
sigpd <- sigpd %>% mutate(pstr=paste(p.adj.signif," p=",format(p.adj,digits=2),sep=""))
cor <- cor %>% mutate(sig=if_else(p.adj<0.05,TRUE,FALSE))
limits = c(min(cor$ciL)-0.01,1.2)

order <- cor %>% group_by(idtype,qf) %>% summarize(correlation=max(correlation)) %>% arrange(idtype,correlation) %>% pull(qf)
#order <- cor %>% group_by(idtype,qf) %>% summarize(her=max(her)) %>% arrange(idtype,her) %>% pull(qf)

cor$qf <- factor(cor$qf,levels=order)
sigpd$qf <- factor(sigpd$qf,levels=order)
cor$question.type <- factor(cor$question.type,levels=c("factor","physical trait","physical trait related","motor pattern","other behavior"))
sigpd$question.type <- factor(sigpd$question.type,levels=c("factor","physical trait","physical trait related","motor pattern","other behavior"))

p <- ggplot(cor,aes(y=qf,x=correlation)) 
p <- p + geom_vline(xintercept = 0,color="grey40",size=0.2)
p <- p + geom_point(aes(color=sig),size=1,shape=16)
p <- p + geom_segment(aes(color=sig,yend=qf,x=ciL,xend=ciH),size=0.2)
p <- p + geom_text(aes(y=qf,x=xpos,label=pstr),size=1.25,data=sigpd,vjust=0.5,hjust=0) # $color=sig,hjust=0.05))
p <- p + scale_color_manual(values=c("#878787","#b2182b"))
p <- p + scale_y_discrete("",breaks=xlabels$qf,labels=xlabels$label)
p <- p + scale_x_continuous("correlation between survey permutation z and LMMR t",limits=limits,breaks=c(-2:2)/2)
p <- p + facet_grid(question.type~.,scales="free_y",space="free_y")
p <- p + theme_minimal()
p <- p + theme(legend.position="none",strip.text=element_text(size=3,face="bold"), 
               plot.title=element_text(size=8,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),axis.text.y = element_text(hjust=1,size=4.5),
               #panel.grid.major.x=element_blank(), 
               axis.title.y = element_text(hjust=0.5,size=5,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.title.x=element_text(hjust=0.5,size=5,face="bold"),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=5),
               legend.key.size = unit(0.1, 'in'))
ggsave(plot=p,filename=paste(outroot,".cor.pdf",sep=""),width=3.5,height=16)
  
