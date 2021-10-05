#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)


### make circle plots for PPS scores (and others) 

library(tidyverse)
library(forcats)
library(ggrepel)
library(wesanderson)
library(rstatix)
library(ggpubr)

sigcut <- 0.05
min_frac_qf_w_data <- 0.5

indir <- "../../data_release/"

permIn<- paste(indir,"DarwinsArk_20191115_survey_permutations.csv",sep="")

herIn <- paste(indir,"DarwinsArk_20191115_heritability_all-SNPs.csv",sep="")

facInfo <- as_tibble(read.csv(paste(indir,"/DarwinsArk_20191115_factors.csv",sep=""))) %>% mutate(idtype="factor")
facInfo <- facInfo %>% select(idtype,factor,name,negative,positive) %>% distinct() %>% rename(id=factor,string=name)
facInfo <- facInfo %>% filter(id <= 8) %>% mutate(plotname=paste("F00",id," ",string,sep="")) %>% mutate(ylabel=paste(string," (",negative," to ",positive,")",sep=""),ylabel_long=paste(plotname," (",negative," to ",positive,")",sep=""))
facInfo <- facInfo %>% select(idtype,id,plotname,ylabel,ylabel_long,negative,positive)
facInfo <- facInfo %>% mutate(qf=paste("F",id,sep=""))
facInfo <- facInfo %>% mutate(question.type="factor")

quesInfo <- as_tibble(read.csv(paste(indir,"/DarwinsArk_20191115_questions.csv",sep=""),header=T)) %>% mutate(idtype="question")
quesInfo <- quesInfo %>% select(idtype,id,string,abbr_text,negative,positive,question.type) %>% distinct() 
quesInfo <- quesInfo %>% mutate(plotname_order=if_else(id<10,paste("Q00",id,sep=""),if_else(id<100,paste("Q0",id,sep=""),paste("Q",id,sep=""))))
quesInfo <- quesInfo %>% mutate(plotname=paste(plotname_order," Question ",id,": ",abbr_text,sep=""))  %>% mutate(ylabel=paste(plotname_order,": ",abbr_text," (",negative," to ",positive,")",sep=""),ylabel_long=paste(plotname," (",negative," to ",positive,")",sep=""))
quesInfo <- quesInfo %>% select(idtype,id,plotname,ylabel,ylabel_long,question.type,negative,positive) %>% mutate(question.type=str_replace(question.type,"aging related surveys","other behavior")) 
quesInfo <- quesInfo %>% mutate(qf=paste("Q",id,sep=""))
plotInfo <- facInfo %>% bind_rows(quesInfo)

order_qfsets <- plotInfo %>% select(idtype,question.type) %>% distinct() %>% arrange(idtype,question.type) 
order_qfsets$order_qfset <- c(1,4,5,2,3)

plotInfo <- plotInfo %>% left_join(order_qfsets) %>% arrange(order_qfset,id) %>% mutate(order=row_number())
plotInfo <- plotInfo %>% mutate(idtype2=paste(order_qfset,question.type)) 


alld <- as_tibble(read.csv(permIn,header=T))



nperm <- median(alld$nperm)
npermStr <- paste((round(nperm/1000,0)),"K",sep="")

# clean up set names for plotting
alld <- alld %>% mutate(set=if_else(str_length(set)==2&str_detect(set,"Y"),str_replace(set,"Y","Y0"),set)) %>% mutate(set=as.factor(set)) 
alld <- alld %>% mutate(set=str_replace_all(set,"_"," "))

# get counts of significant differences for paper text
sigcnts <- alld %>% mutate(qtype=if_else(idtype=="factor"|id<120,"behavior","physical"),sig=if_else(pcorr<=0.05,TRUE,FALSE))
sigcntsT <- sigcnts %>% group_by(type,qtype) %>% count() %>% rename(ntot=n)
sigcntsT <- sigcnts %>% filter(sig) %>% group_by(type,qtype) %>% count() %>% rename(nsig=n) %>% full_join(sigcntsT)
sigcntsT <- sigcntsT %>% replace_na(list(nsig=0)) %>% mutate(frac=nsig/ntot)

sigcnts <- alld %>% mutate(qtype=if_else(idtype=="factor"|id<120,"behavior","physical"))
sigcnts <- sigcnts %>% group_by(type,set) %>% summarize(pcorr=min(pcorr)) %>% inner_join(sigcnts)
sigcnts <- sigcnts %>% group_by(type,set,qtype) %>% count() %>% pivot_wider(names_from=qtype,values_from=n) %>% replace_na(list(behavior=0,physical=0))
sigcnts <- sigcnts %>% mutate(tophit=if_else(behavior==1&physical==0,"behavior",if_else(behavior==0&physical==1,"physical",if_else(behavior==1&physical==1,"both","other"))))
sigcnts <- sigcnts %>% group_by(type,tophit) %>% count() %>% rename(nset=n)
sigcnts <- sigcnts %>% group_by(type) %>% summarize(ntot=sum(nset)) %>% full_join(sigcnts) %>% pivot_wider(names_from=tophit,values_from=nset) %>% replace_na(list(behavior=0,physical=0))
sigcnts <- sigcnts %>% mutate(behavioralF=behavior/ntot,physicalF=physical/ntot)

# define what order sets should be plotted in
order_dogsets <- alld %>% select(type,set) %>% distinct() %>% arrange(type,set) %>% mutate(row=row_number()) %>% rename(dogset_row=row)
alld <- alld %>% left_join(order_dogsets)

# add extra info for plot labelling to main data frame
alld <- alld %>% left_join(plotInfo)

xfacet_order <- tibble(type=c("by_year","confirmed_breed","candidate_breed"),xorder=c(1:3)) %>% mutate(facetname=paste(xorder,type))
alld <- alld %>% left_join(xfacet_order) %>% mutate(facetname=paste(facetname,"\n(N=",ndogs,")",sep=""))


# get list of breeds with low sample numbers 
lown_breeds <- alld %>% filter(idtype=="factor"|(idtype=="question"&id %in% c(121,125,127,17,54,59)))
lown_breeds <- lown_breeds %>% group_by(type,set,idtype) %>% summarize(min=min(ndogs_available)) %>% mutate(lown=if_else(min<50,TRUE,FALSE)) %>% select(-min)
alld <- alld %>% left_join(lown_breeds)




maxZ <- max(abs(alld$z))
zbrks <- c(-floor(maxZ):floor(maxZ))

pd <- alld 

pd <- pd  %>% mutate(type=str_replace_all(type,"_"," "))
outroot <- paste("Fig_PPS","_CIRCLE",sep="")

yorder <- pd %>% select(qf,order,order_qfset,idtype,idtype2) %>% distinct() %>% arrange(desc(order)) %>% mutate(nrow=row_number()) #%>% select(-order)

# don't plot sets with scores for less than 50% of questions / factors
maxcnt <- length(unique(pd$id))
pd <- pd %>% group_by(facetname,set) %>% count() %>% filter(n>max(maxcnt)*min_frac_qf_w_data) %>% select(-n) %>% inner_join(pd)

pd <- plotInfo %>% select(idtype,id,ylabel,idtype2) %>% right_join(pd) %>% mutate(ylabel=as.factor(ylabel)) 

# plot +/- sign for significant results
sig <- pd %>% filter(pcorr<=sigcut) %>% mutate(symbol=if_else(z<0,"-","+")) 

# make sure empty cells are indicated 
allxy <- expand_grid(set=unique(pd$set),ylabel=unique(pd$ylabel)) 
allxy <- pd %>% select(ylabel,id,idtype2) %>% distinct() %>% inner_join(allxy)
allxy <- pd %>% select(type,set,set,facetname) %>% distinct() %>% inner_join(allxy)
empty <- allxy %>% full_join(pd) %>% filter(is.na(z)) 

nrow<- length(unique(pd$ylabel))
pheight <- 5+(nrow*0.19) 
if (pheight<6){ pheight <- 6 }

ncol<- length(unique(pd$set))
pwidth <- 6+(ncol*0.25)

pdfname <- paste(outroot,".pdf",sep="")
#pd <- pd %>% left_join(yorder) %>% mutate(ylabel=fct_reorder(ylabel,nrow))
#empty <- empty %>% left_join(yorder) %>% mutate(ylabel=fct_reorder(ylabel,nrow))

dogset_order <- pd %>% ungroup() %>% group_by(set) %>% summarize(dogset_row=min(dogset_row)) %>% arrange(dogset_row) %>% pull(set)

print(dogset_order)

pd$set <- factor(pd$set,levels=dogset_order)
empty$set <- factor(empty$set,levels=dogset_order)

p <- ggplot(pd,aes(y=ylabel,x=set))
#p <- p + geom_point(color="#525252",size=0.2)
p <- p + geom_point(color="#f0f0f0",size=5,shape=15,data=empty)
p <- p + geom_point(aes(size=abs(z),color=z),shape=16,alpha=1)
p <- p + geom_point(aes(size=abs(z)),color="#585858",shape=21,alpha=1)
#p <- p + geom_point(aes(size=abs(z)),shape=1,data=sig,color="grey50")
p <- p + geom_text(aes(label=symbol),color="black",data=sig,alpha=0.8,hjust=0.5,vjust=0.3)
p <- p + scale_y_discrete(limit=rev)
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "#FFFFFF", high ="#e45c40", midpoint = 0,limits=c(-maxZ,maxZ),breaks=zbrks)
p <- p + theme_minimal() + theme(axis.text.x  = element_text(angle=90, hjust=1,vjust=0, size=10),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.),axis.title = element_blank(),axis.ticks=element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())
p <- p + facet_grid(idtype2 ~ facetname,scales="free",space="free")
p <- p + ggtitle(paste("permutation testing of survey traits","\nType = ","all","; ",npermStr," random permutations\ncircle size ~ abs(z); color ~ z; '+ or -' = pcorr <",sigcut,sep=""))

ggsave(plot=p,filename=pdfname,width=pwidth,height=pheight,limitsize = FALSE) 

#### make version for main text figure
okpd <- pd  %>% filter(idtype=="factor"|id %in% c(121,125,127,17,54,59)) %>% filter(type!="by year")
# keep sets if they are the top breed for that question/factor
oksetsTop <- okpd %>% group_by(idtype,id) %>% summarize(pcorr=min(pcorr)) %>% inner_join(okpd) %>% ungroup() %>% select(type,set) %>% distinct()  %>% mutate(top=5)
# keep sets with >50 dogs available
oksetsN <- okpd %>% group_by(idtype,set,type) %>% summarize(min=min(ndogs_available)) %>% filter(min>=50) %>% ungroup() %>% select(type,set) %>% distinct()  %>% mutate(top=3)
oksetsRank <- okpd %>% ungroup() %>% filter(id<120) %>% group_by(type,set) %>% summarize(top=max(abs(z))) #%>% bind_rows(oksets) 
okqf <- okpd %>% ungroup() %>% group_by(type,set,idtype) %>% count() %>% filter((idtype=="question"&n==6)|idtype=="factor"&n==8) %>% select(idtype,type,set) %>% distinct()

oksets <- oksetsTop %>% bind_rows(oksetsN) %>% bind_rows(oksetsRank) %>% inner_join(okqf)
oksets <- oksets %>% group_by(type,set) %>% summarize(top=max(top)) %>% arrange(desc(top)) %>% group_by(type) %>% mutate(row=row_number()) 
oksets <- oksets %>% filter(row <= 35) 
okpd <- okpd %>% inner_join(oksets) %>% inner_join(okqf)




for (idtype_in in c("factor","question")){
  pd2 <- okpd %>% filter(idtype==idtype_in)
  okqf <- pd2 %>% select(id,type,set)
  empty2 <- empty %>% inner_join(okqf) %>% inner_join(oksets)  %>% filter(idtype==idtype_in)
  sig2 <- sig  %>% inner_join(okqf) %>% inner_join(oksets)  %>% filter(idtype==idtype_in)
#print(length(unique(pd2$set)))
#pd2 %>% group_by(type,set) %>% summarize(min=min(ndogs_available)) %>% filter(min<=50) %>% arrange(type,set)

p <- ggplot(pd2,aes(y=ylabel,x=set))
#p <- p + geom_point(color="#525252",size=0.2)
p <- p + geom_point(color="#f0f0f0",size=5,shape=15,data=empty2)
p <- p + geom_point(aes(size=abs(z),color=z),shape=16,alpha=1)
p <- p + geom_point(aes(size=abs(z)),color="#585858",shape=21,alpha=1)
#p <- p + geom_point(aes(size=abs(z)),shape=1,data=sig2,color="grey50")
p <- p + geom_text(aes(label=symbol),color="black",data=sig2,alpha=0.8,hjust=0.5,vjust=0.3)
p <- p + scale_y_discrete(limit=rev)
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "#FFFFFF", high ="#e45c40", midpoint = 0,limits=c(-maxZ,maxZ),breaks=zbrks)
p <- p + theme_minimal() + theme(axis.text.x  = element_text(angle=90, hjust=1,vjust=0, size=10),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.),axis.title = element_blank(),axis.ticks=element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())
p <- p + facet_grid(idtype2 ~ facetname,scales="free",space="free")
p <- p + ggtitle(paste("permutation testing of survey traits","\nType = ","all","; ",npermStr," random permutations\ncircle size ~ abs(z); color ~ z; '+ or -' = pcorr <",sigcut,sep=""))
pdfname <- paste(outroot,".",idtype_in,".main_fig.pdf",sep="")

ggsave(plot=p,filename=pdfname,width=16,height=5)
}
 


### print page 1: just years 
pdfname2 <- paste(outroot,".pg1.pdf",sep="")
pd2 <- pd %>% filter(type=="by year")
empty2 <- empty %>% filter(type=="by year")
sig2 <- sig %>% filter(type=="by year")

p <- ggplot(pd2,aes(y=ylabel,x=set))
#p <- p + geom_point(color="#525252",size=0.2)
p <- p + geom_point(color="#f0f0f0",size=5,shape=15,data=empty2)
p <- p + geom_point(aes(size=abs(z)),color="#585858",shape=21,alpha=1)
p <- p + geom_point(aes(size=abs(z),color=z),shape=16,alpha=1)
#p <- p + geom_point(aes(size=abs(z)),color="#585858",shape=21,alpha=1)
p <- p + geom_text(aes(label=symbol),color="black",data=sig2,alpha=0.8,hjust=0.5,vjust=0.3)
p <- p + scale_y_discrete(limit=rev)
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "#FFFFFF", high ="#e45c40", midpoint = 0,limits=c(-maxZ,maxZ),breaks=zbrks)
p <- p + theme_minimal() + theme(axis.text.x  = element_text(angle=90, hjust=1,vjust=0, size=10),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.),axis.title = element_blank(),axis.ticks=element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())
p <- p + facet_grid(idtype2 ~ facetname,scales="free",space="free")
p <- p + ggtitle(paste("permutation testing of survey traits","\nType = ","all","; ",npermStr," random permutations\ncircle size ~ abs(z); color ~ z; '+ or -' = pcorr <",sigcut,sep=""))
ggsave(plot=p,filename=pdfname2,width=8,height=pheight,limitsize = FALSE) 

pdfname3 <- paste(outroot,".pg2.pdf",sep="")
pd3 <- pd %>% filter(type!="by year") %>% filter(idtype2!="5 other behavior")
empty3 <- empty %>% filter(type!="by year") %>% filter(idtype2!="5 other behavior")
sig3 <- sig %>% filter(type!="by year") %>% filter(idtype2!="5 other behavior")

p <- ggplot(pd3,aes(y=ylabel,x=set))
#p <- p + geom_point(color="#525252",size=0.2)
p <- p + geom_point(color="#f0f0f0",size=5,shape=15,data=empty3)
p <- p + geom_point(aes(size=abs(z),color=z),shape=16,alpha=1)
p <- p + geom_point(aes(size=abs(z)),color="#585858",shape=21,alpha=1)
#p <- p + geom_point(aes(size=abs(z)),shape=1,data=sig3,color="grey50")
p <- p + geom_text(aes(label=symbol),color="black",data=sig3,alpha=0.8,hjust=0.5,vjust=0.3)
p <- p + scale_y_discrete(limit=rev)
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "#FFFFFF", high ="#e45c40", midpoint = 0,limits=c(-maxZ,maxZ),breaks=zbrks)
p <- p + ggtitle("Population peculiarity scores (page 2/4)")
p <- p + theme_minimal() 
p <- p + theme(axis.text.x  = element_text(angle=90, hjust=1,vjust=0, size=10),
               axis.text.y  = element_text(hjust=1,vjust=0, size=10),
               strip.text = element_text(hjust=0.5,vjust=0.5, size=10),
               panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0,size=14,face="bold"),
               axis.title = element_blank(),axis.ticks=element_blank(),
               panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())
p <- p + facet_grid(idtype2 ~ facetname,scales="free",space="free")
ggsave(plot=p,filename=pdfname3,width=18,height=11,limitsize = FALSE) 

pdfname4 <- paste(outroot,".pg3.pdf",sep="")
pd4 <- pd %>% filter(type!="by year") %>% filter(idtype2=="5 other behavior"&id<=53)
empty4 <- empty %>% filter(type!="by year") %>% filter(idtype2=="5 other behavior"&id<=53)
sig4<- sig %>% filter(type!="by year") %>% filter(idtype2=="5 other behavior"&id<=53)
pdfname5 <- paste(outroot,".pg4.pdf",sep="")
pd5 <- pd %>% filter(type!="by year") %>% filter(idtype2=="5 other behavior"&id>53)
empty5 <- empty %>% filter(type!="by year") %>% filter(idtype2=="5 other behavior"&id>53)
sig5 <- sig %>% filter(type!="by year") %>% filter(idtype2=="5 other behavior"&id>53)


p <- ggplot(pd4,aes(y=ylabel,x=set))
#p <- p + geom_point(color="#525252",size=0.2)
p <- p + geom_point(color="#f0f0f0",size=5,shape=15,data=empty4)
p <- p + geom_point(aes(size=abs(z),color=z),shape=16,alpha=1)
#p <- p + geom_point(aes(size=abs(z)),shape=1,data=sig4,color="grey50")
p <- p + geom_point(aes(size=abs(z)),color="#585858",shape=21,alpha=1)
p <- p + geom_text(aes(label=symbol),color="black",data=sig4,alpha=0.8,hjust=0.5,vjust=0.3)
p <- p + scale_y_discrete(limit=rev)
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "#FFFFFF", high ="#e45c40", midpoint = 0,limits=c(-maxZ,maxZ),breaks=zbrks)
p <- p + ggtitle("Population peculiarity scores (page 3/4)")
p <- p + theme_minimal() 
p <- p + theme(axis.text.x  = element_text(angle=90, hjust=1,vjust=0, size=10),
               axis.text.y  = element_text(hjust=1,vjust=0, size=10),
               strip.text = element_text(hjust=0.5,vjust=0.5, size=10),
               panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0,size=14,face="bold"),
               axis.title = element_blank(),axis.ticks=element_blank(),
               panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())
p <- p + facet_grid(idtype2 ~ facetname,scales="free",space="free")
ggsave(plot=p,filename=pdfname4,width=18,height=13,limitsize = FALSE) 


p <- ggplot(pd5,aes(y=ylabel,x=set))
#p <- p + geom_point(color="#525252",size=0.2)
p <- p + geom_point(color="#f0f0f0",size=5,shape=15,data=empty5)
p <- p + geom_point(aes(size=abs(z),color=z),shape=16,alpha=1)
#p <- p + geom_point(aes(size=abs(z)),shape=1,data=sig5,color="grey50")
p <- p + geom_point(aes(size=abs(z)),color="#585858",shape=21,alpha=1)
p <- p + geom_text(aes(label=symbol),color="black",data=sig5,alpha=0.8,hjust=0.5,vjust=0.3)
p <- p + scale_y_discrete(limit=rev)
#p <- p + scale_size(limits=c(0,maxsize))
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "#FFFFFF", high ="#e45c40", midpoint = 0,limits=c(-maxZ,maxZ),breaks=zbrks)
p <- p + ggtitle("Population peculiarity scores (page 4/4)")
p <- p + theme_minimal() 
p <- p + theme(axis.text.x  = element_text(angle=90, hjust=1,vjust=0, size=10),
               axis.text.y  = element_text(hjust=1,vjust=0, size=10),
               strip.text = element_text(hjust=0.5,vjust=0.5, size=10),
               panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0,size=14,face="bold"),
               axis.title = element_blank(),axis.ticks=element_blank(),
               panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())
p <- p + facet_grid(idtype2 ~ facetname,scales="free",space="free")
ggsave(plot=p,filename=pdfname5,width=18,height=13,limitsize = FALSE) 


### compare stringent and lenient sets
outroot <- paste("Fig_PPS","_CONF_v_CAND",sep="")
pdfname <- paste(outroot,".scatter.pdf",sep="")

pd <- alld   %>% filter((ndogs==50&type=="confirmed_breed")|(ndogs==25&type=="candidate_breed")) %>% select(id,idtype,plotname,type,set,z) %>% distinct()
pd <- pd %>% pivot_wider(names_from=type,values_from=z) %>% filter(!is.na(confirmed_breed)&!is.na(candidate_breed))  %>% mutate(diff=confirmed_breed-candidate_breed)
pd_n100 <- pd %>% group_by(set) %>% count() %>% filter(n>=100) %>% select(-n)
pd <- pd %>% inner_join(pd_n100)
cor <- pd %>% group_by(set,idtype) %>% summarize(correlation = cor(candidate_breed,confirmed_breed,method="pearson"),p.correlation = cor.test(candidate_breed,confirmed_breed,method="pearson")$p.value) 

cor <- cor %>% mutate(pstr=if_else(p.correlation<0.001,paste("p=",format(p.correlation,digits=2,scientific=T),sep=""),paste("p=",round(p.correlation,3),sep="")))
minx <- pd %>% group_by(idtype) %>% summarize(minx=min(candidate_breed))
maxy <- pd %>% group_by(set) %>% summarize(maxy=max(confirmed_breed))
cor <- cor %>% left_join(minx)  %>% left_join(maxy) 

pr_pd <- cor %>% select(set,idtype,correlation,p.correlation) %>% right_join(pd)
write.csv(pr_pd,paste(outroot,".data.csv",sep=""),)
max <- ceiling(max(abs(pd$candidate_breed),abs(pd$confirmed_breed)))
p<- ggplot(pd,aes(x=candidate_breed,y=confirmed_breed))  
p <- p + geom_abline(color="#3188a2",size=0.25,linetype=2)  
p <- p + geom_point(alpha=0.5,shape=16,size=1)
p <- p + scale_x_continuous("candidate purebred",limits=c(-max,max))
p <- p + scale_y_continuous("confirmed purebred",limits=c(-max,max))
p <- p + geom_text(aes(x=minx,y=maxy,label=paste("pearson R=",round(correlation,3),"\n",pstr,sep="")),data=cor,hjust=0,vjust=1,size=1.75,color="blue")
p <- p  + theme_minimal() + theme(panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0,size=7),
                             axis.text  = element_text(size=6),panel.grid.minor.x = element_blank(),
                             strip.text=element_text(hjust = 0.5,vjust=0.5,size=6),axis.title = element_text(size=7))
p <- p + facet_grid(set~idtype) #,scales="free")
p <- p + ggtitle(paste("Comparing permutation z scores\nfor candidate (25) and confirmed (50) breed populations\n",npermStr," permutations",sep=""))
ggsave(plot=p,filename=pdfname,width=4,height=6) 

### MAKE line plots comparing age and z score for all ages under 11

okndogs <- 100
age_outroot <- paste("year_v_z.",okndogs,"dogs.",npermStr,"_perm",sep="")
pd <- alld %>% filter(type=="by_year"&ndogs==okndogs) %>% select(id,idtype,plotname,ylabel,type,set,z) %>% distinct()
pd <- pd %>% mutate(year=as.integer(str_replace(set,"Y",""))) %>% mutate(ylabel=str_replace(ylabel," \\(","\n\\(")) %>% filter(year<=12)
cor <- pd %>% group_by(idtype,id) %>% summarize(ncor=n(),correlation = cor.test(year,z,method="pearson")$estimate,t = cor.test(year,z,method="pearson")$statistic,miny=min(z),p = cor.test(year,z,method="pearson")$p.value)  %>% adjust_pvalue(method="BH")
cor <- cor  %>% arrange(desc(correlation)) %>% mutate(nrow=row_number()) 

pd <- pd %>% inner_join(cor)
pd <- pd %>% mutate(pstr=if_else(p.adj<0.001,paste("p=",format(p.adj,digits=2,scientific=T),sep=""),paste("p=",round(p.adj,3),sep="")))
corfile <- paste("Fig_PPS","_AGE.cor.csv",sep="")
cor_pr <- cor %>% select(idtype,id,ncor,correlation,t,p,p.adj)  %>% rename(n=ncor) %>%  arrange(idtype,id)
write.csv(cor_pr,corfile,row.names=F)

cor_pr <- cor_pr %>% mutate(sig=if_else(p.adj<0.05,TRUE,FALSE))
cor_pr <- plotInfo %>% select(id,idtype,ylabel) %>% right_join(cor_pr) %>% rename(plotname=ylabel)
cor_pr <- cor_pr %>% select(id,idtype) %>% distinct() %>% group_by(idtype) %>% count() %>% mutate(xlabel=paste(idtype,"\nN=",n,sep="")) %>% select(-n) %>% inner_join(cor_pr)
cor_high <- cor_pr %>% group_by(idtype) %>% summarize(cut=quantile(abs(correlation),0.9)) %>% inner_join(cor_pr) %>% filter(abs(correlation)>=cut)

pdfname <- paste("Fig_PPS","_AGE_CORR.violin.pdf",sep="")
p <- ggplot(cor_pr,aes(x=xlabel,y=abs(correlation))) + geom_violin(fill="grey30",alpha=0.1,color=NA,scale="count") + geom_point(aes(shape=sig),alpha=0.5,size=2)
p <- p + geom_text_repel(aes(label=plotname),data=cor_high,nudge_x = 0.25, direction = "y", hjust = "left",size=2,segment.color="grey60",segment.size = 0.2,lineheight=0.9,max.overlaps=30,min.segment.length = 0)
#p <- p + scale_color_manual(values=c("#252525","#b2182b"))
p <- p + scale_x_discrete(expand = expansion(mult = 1.5))
p <- p + scale_shape_manual(values=c(21,16))
p <- p + theme_minimal()
p <- p + theme(legend.position="bottom",strip.text=element_text(size=5,face="bold"), 
               plot.title=element_text(size=8,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),axis.text.y = element_text(hjust=1,size=6),
               panel.grid.major.x=element_blank(), 
               axis.title.y = element_text(hjust=0.5,size=7,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=6,face="bold"),
               axis.title.x=element_blank(),
               axis.ticks.x=element_blank(),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=5),
               legend.key.size = unit(0.1, 'in'))
ggsave(plot=p,filename=pdfname,width=8,height=4) 



pd_all <- pd

### PLOT AGE V FACTORS

pdfname <- paste("Fig_PPS","_AGE_FAC.line.pdf",sep="")
pd <- pd_all %>% filter(idtype=="factor") 
pdcor <- pd %>% select(correlation,ylabel,pstr) %>% distinct()

ylabel_order <- pd %>% select(ylabel,correlation) %>% distinct() %>% arrange(desc(correlation)) %>% pull(ylabel)
pd$ylabel <- factor(pd$ylabel,levels=ylabel_order)
pdcor$ylabel <- factor(pdcor$ylabel,levels=ylabel_order)

maxy <- ceiling(max(pd$z))+0.5
miny <- floor(min(pd$z))-0.5
#maxy <- max(maxy,abs(miny))+0.2
#miny <- -maxy
scales <- plotInfo %>% filter(idtype=="factor") %>% filter(id %in% pd$id) %>% select(qf,id,negative,positive) %>% pivot_longer(c(-qf,-id)) 
scales <- scales %>% mutate(y=if_else(name=="negative",miny+0.2,maxy-0.2)) %>% mutate(vjust=if_else(name=="negative",1,0))
scales <- pd %>% select(id,ylabel) %>% distinct() %>% inner_join(scales) %>% distinct()

p <- ggplot(pd) + geom_point(aes(x=year,y=z,color=correlation),shape=16,size=1) + geom_line(aes(group=id,x=year,y=z,color=correlation))
p <- p + geom_text(aes(label=paste("R=",round(correlation,3),"; ",pstr,sep="")),color="black",size=1.6,lineheight=0.9,x=-1,y=maxy+0.75,hjust=0,vjust=0,data=pdcor) 
p <- p + geom_text(aes(y=y,label=value,vjust=vjust),hjust=0,size=1.6,x=-1,data=scales,color="grey30")
p <- p + geom_segment(x=-0.8,y=0,yend=maxy-0.5,xend=-0.8,arrow=arrow(length = unit(0.05, "inches")),color="grey30",size=0.3,alpha=0.5)
p <- p + geom_segment(x=-0.8,y=0,yend=miny+0.5,xend=-0.8,arrow=arrow(length = unit(0.05, "inches")),color="grey30",size=0.3,alpha=0.5)
p <- p + geom_hline(yintercept=0,color="grey30",size=0.1)
p <- p + scale_y_continuous("z score",limits=c(miny,maxy+1.1))
p <- p + scale_x_continuous("age (year)",breaks=c(0,2,4,6,8,10),limits=c(-1,10.5))
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "grey70", high ="#e45c40", midpoint = 0,limits=c(-1,1))
p <- p + ggtitle(paste("z scores over ",npermStr," permutations; by year",sep=""),subtitle="pearson correlation, with adjusted p-values (BH)\nz score plotted for breeds is most extreme observed in permutation testing")
p <- p + facet_wrap(~ylabel,ncol=2)
p <- p + theme_minimal()
p <- p + theme(legend.position="bottom",strip.text=element_text(size=5,face="bold"), 
               plot.title=element_text(size=8,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),axis.text.y = element_text(hjust=1,size=5),
               #panel.grid.major.y=element_blank(), 
               axis.title.y = element_text(hjust=0.5,size=6,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=5),
               legend.key.size = unit(0.1, 'in'))
ggsave(plot=p,filename=pdfname,width=4,height=7) 

### PLOT AGE V QUESTIONS ### 
pdfname <- paste("Fig_PPS","_AGE_QUES.line.pdf",sep="")
pd <- pd_all %>% filter(idtype=="question") 
pdcor <- pd %>% select(correlation,ylabel,pstr) %>% distinct()

ylabel_order <- pd %>% select(ylabel,correlation) %>% distinct() %>% arrange(desc(correlation)) %>% pull(ylabel)
pd$ylabel <- factor(pd$ylabel,levels=ylabel_order)
pdcor$ylabel <- factor(pdcor$ylabel,levels=ylabel_order)

maxy <- (ceiling(max(pd$z)*10))/10
miny <- ((floor(min(pd$z)*10))/10)-0.2

p <- ggplot(pd) + geom_point(aes(x=year,y=z),shape=16,size=1) + geom_line(aes(x=year,y=z))
p <- p + geom_text(aes(label=paste("R=",round(correlation,3),"\n",pstr,sep=""),color=correlation),x=max(pd$year),y=min(pd$z),hjust = 1,vjust=0,data=pdcor) 
p <- p + geom_hline(yintercept=0,color="grey40",size=0.25)
p <- p + scale_y_continuous("z score",limits=c(miny,maxy))
p <- p + scale_x_continuous("age (year)",breaks=c(0,2,4,6,8,10,12))
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "grey70", high ="#e45c40", midpoint = 0,limits=c(-1,1))
p <- p + ggtitle(paste("z scores over ",npermStr," permutations; by year",sep=""),subtitle="pearson correlation, with adjusted p-values (BH)")
p <- p + facet_wrap(~ylabel,ncol=7)
p <- p + theme_minimal()
p <- p + theme(legend.position="bottom",strip.text=element_text(size=5,face="bold"), 
               plot.title=element_text(size=8,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),axis.text.y = element_text(hjust=1,size=5),
               panel.grid.major.y=element_blank(), 
               axis.title.y = element_text(hjust=0.5,size=6,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=5),
               legend.key.size = unit(0.1, 'in'))

ggsave(plot=p,filename=pdfname,width=20,height=26)



pd <- pd_all %>% filter(idtype=="question") 
cut <- quantile(abs(pd$correlation),0.75)
pd <- pd %>% filter(abs(correlation)>=cut)
pd <- pd_all %>% inner_join(pd)
pdcor <- pd %>% select(correlation,ylabel,pstr) %>% distinct()

ylabel_order <- pd %>% select(ylabel,correlation) %>% distinct() %>% arrange(desc(correlation)) %>% pull(ylabel)
pd$ylabel <- factor(pd$ylabel,levels=ylabel_order)
pdcor$ylabel <- factor(pdcor$ylabel,levels=ylabel_order)
pdcor <- pdcor %>% mutate(y=if_else(correlation>0,maxy-1.5,miny))

nframes <- pd  %>% select(id) %>% distinct() %>% count()
ncols <- 5 #as.integer(floor((nframes/(11/8.5))^0.5))
nrows <- as.integer(ceiling(nframes/ncols))
okwidth <- ncols*2+0.5
okheight <- nrows*2+0.5

p <- ggplot(pd) + geom_point(aes(x=year,y=z,color=correlation),shape=16,size=1) + geom_line(aes(group=id,x=year,y=z,color=correlation))
p <- p + geom_text(aes(label=paste("R=",round(correlation,3),"\n",pstr,sep=""),y=y,color=correlation),size=2,lineheight=0.9,x=min(pd$year),hjust = "left",vjust=0,data=pdcor) 
p <- p + geom_hline(yintercept=0,color="grey40",size=0.25)
p <- p + scale_y_continuous("z score")
p <- p + scale_x_continuous("age (year)",breaks=c(0,2,4,6,8,10,12))
p <- p + scale_colour_gradient2(low = "#3188a2", mid = "grey70", high ="#e45c40", midpoint = 0,limits=c(-1,1))
#p <- p  + theme_minimal() + theme(panel.grid.minor.y=element_blank(),plot.title = element_text(hjust = 0),axis.ticks=element_blank(),panel.grid.major.y = element_blank())
p <- p + ggtitle(paste("z scores over ",npermStr," permutations; by year",sep=""),subtitle=paste("pearson correlation, with adjusted p-values (BH)\nz score plotted for breeds is most extreme observed in permutation testing\nTop 20% most correlated questions shown",sep=""))
p <- p + facet_wrap(~ylabel,ncol=ncols)
p <- p + theme_minimal()
p <- p + theme(legend.position="top",strip.text=element_text(size=5,face="bold"), 
               plot.title=element_text(size=8,face="bold"),panel.grid.minor=element_blank(), 
               plot.subtitle=element_text(size=5),axis.text.y = element_text(hjust=1,size=5),
               panel.grid.major.y=element_blank(), 
               axis.title.y = element_text(hjust=0.5,size=6,face="bold"),
               axis.text.x = element_text(hjust=0.5,size=5),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=5),
               legend.key.size = unit(0.1, 'in'))

pdfname <- paste("Fig_PPS","_AGE_QUES_TOP.line.pdf",sep="")
ggsave(plot=p,filename=pdfname,width=6.5,height=8)

quesgrps <- c("physical trait","physical trait related","motor pattern","factor","other behavior")
my_comparisons <- (list(c("other behavior",quesgrps[4] ),c("other behavior",quesgrps[3] ),c("other behavior",quesgrps[2] ),c("other behavior",quesgrps[1])))




pd <- alld %>% rename(questype=question.type) %>% filter(str_detect(type,"breed"))
pd <- pd %>% mutate(facet=paste(type,ndogs))
pd <- pd %>% mutate(sig=if_else(pcorr<=0.05,TRUE,FALSE)) %>% select(-avg_real_value,-p,-pcorr,-nperm) %>% distinct()
pd$questype <- factor(pd$questype,levels=quesgrps)
xlabels <- tibble(questype=quesgrps,Xorder=c(1:length(quesgrps)))
xlabels <- pd %>% select(id,questype) %>% distinct() %>% group_by(questype) %>% count() %>% mutate(xlabel=paste(questype,"\nN=",n,sep="")) %>% inner_join(xlabels)
xlabels <- xlabels %>% mutate(xlabel=as.factor(xlabel)) #%>% mutate(xlabel=fct_reorder(xlabel,order))
#pd <- pd %>% inner_join(xlabels)
pd_ndogs <- pd %>% group_by(facet,questype) %>% count() %>% mutate(str=paste("N=",n,sep=""))
#pd_pts <- pd %>% inner_join(pd_ndogs) %>% filter(n<=50)

p <- ggplot(pd,aes(x=questype,y=abs(z))) 
p <- p  + geom_violin(color="grey50",fill="grey50",alpha=0.2,size=0.1)#,draw_quantiles=c(0.25,0.5,0.75))
p <- p + geom_boxplot(outlier.shape=NA,fill=NA,color="black",width=0.1,size=0.2)
p <- p + geom_text(aes(label=str),y=-0.1,vjust=1,hjust=0.5,data=pd_ndogs,color="#b2182b",size=1.5)
p <- p + stat_compare_means(comparisons = my_comparisons,method="t.test",p.adjust.method = "bh",size=1.5,label = "p.format")
p <- p + scale_x_discrete("question type",breaks=xlabels$questype,labels=xlabels$xlabel)
p <- p + ggtitle("Compare question classifications and\nsurvey permutation z scores",subtitle=paste("t.test, adjusted p (BH)",sep=""))
p <- p + facet_wrap(.~facet,ncol=1)
p <- p + theme_minimal()
p <- p + theme(legend.position="none",strip.text=element_text(size=7,face="bold"), 
               plot.title=element_text(size=8,face="bold"),
               plot.subtitle=element_text(size=5),
               panel.grid.minor=element_blank(), 
               panel.grid.major.x=element_blank(),
               axis.text.y = element_text(hjust=1,size=6),
               axis.title.y = element_text(hjust=0.5,size=7,face="bold"),
               axis.title.x=element_text(hjust=0.5,size=6,face="bold"),
               axis.text.x = element_text(size=6))
pdfname <- paste("Fig_PPS","_Z_QTYPE.violin.pdf",sep="")
ggsave(plot=p,filename=pdfname,width=2.75,height=5)

  



