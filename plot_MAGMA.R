#### NOT YET UPDATED #####
#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)
permfile = args[1]

library(wesanderson)
library(tidyverse)
library(rstatix)

## point to location of folder with paper data release files.
indir <- "../../../data_release/"

facIn <- paste(indir,"DarwinsArk_20191115_factors.csv",sep="")
quesIn <- paste(indir,"DarwinsArk_20191115_questions.csv",sep="")
gtexNamesIn <- "plot_MAGMA.gtex_sets.input.csv"
magIn <-  paste(indir,"DarwinsArk_20191115_magma.csv",sep="")
d <- as_tibble(read.csv(magIn,header=T,na.strings=c("NA","#N/A",""))) #%>% rename(p=P)
d <- d %>% group_by(source) %>% adjust_pvalue(method="BH") %>% rename(p.adj.bh=p.adj)
d <- d %>% mutate(source=if_else(source=="Dog Size","Dog GWAS",str_replace_all(source,"\\_"," ")))

# get question / factor info
plotInfo <- as_tibble(read.csv(facIn)) %>% mutate(idtype="factor",qtype="behavior factor") %>% rename(id=factor) %>% filter(id <= 8)
plotInfo <- plotInfo %>% select(id,idtype,qtype,name,negative,positive) %>% distinct()  %>% mutate(name=str_remove(name,"Factor [0-9]: ")) %>% mutate(long=name)
tmp <- as_tibble(read.csv(quesIn),header=T) %>% mutate(idtype="question",qtype=if_else(id>=120,"physical trait","behavior question"))
plotInfo <- tmp  %>% rename(name=abbr_text,long=string) %>% select(id,idtype,qtype,name,long,negative,positive) %>% bind_rows(plotInfo)
plotInfo <- plotInfo %>% mutate(qlabel=if_else(idtype=="factor",paste("F",id,sep=""),if_else(str_length(id)==1,paste("Q00",id,sep=""),if_else(str_length(id)==2,paste("Q0",id,sep=""),paste("Q",id,sep="")))))

names <- read.csv(gtexNamesIn,header=T) %>% rename(geneset=MAGMA)
d <- d %>% left_join(names) %>% mutate(geneset=if_else(!is.na(GTEX),GTEX,geneset)) %>% select(-GTEX)
d <- d %>% mutate(geneset=str_replace(geneset,"Dogsize","Q121 (size) GWAS regions"))


d <- d %>% left_join(plotInfo)
d <- d %>% mutate(trait=str_replace_all(trait,"\\_"," ")) %>% mutate(trait=str_remove(trait," neg-v-pos")) 
d <- d %>% mutate(trait=str_remove(trait,"coat-color ")) %>% mutate(trait=str_remove(trait,"coat-pattern "))
d <- d %>% mutate(qlabel=paste(qlabel,name))
suppd <- d %>% select(source,geneset) %>% distinct() %>% group_by(source) %>% summarize(n_genesets=n()) %>% full_join(d)
suppd <- suppd %>% select(source,n_genesets,geneset,ngenes,qtype,id,long,trait,beta,beta_std,se,p,p.adj.bh)
suppd <- suppd %>% arrange(source,geneset,qtype,id)
write.csv(suppd,"suppTable_MAGMA.csv",row.names=F)

pd <- d %>% mutate(qlabel=if_else(is.na(trait),qlabel,paste(qlabel," (",trait,")",sep="")))
pd <- pd %>% mutate(sig=if_else(p.adj.bh<=0.05,TRUE,FALSE))
pd <- pd %>% filter(id!=121)

pd <- pd %>% group_by(qlabel) %>% summarize(minp=min(p)) %>% filter(minp<=0.005) %>% select(-minp) %>% inner_join(pd)
pd <- pd %>% mutate(source=str_replace_all(source," ","\n"))

p <- ggplot(pd,aes(x=geneset,y=qlabel))
p <- p + geom_point(aes(size=-log10(p),alpha=-log10(p)),color="#525252",shape=16,data=pd %>% filter(!sig))
p <- p + geom_point(aes(size=-log10(p)),color="#cb181d",shape=16,data=pd %>% filter(sig))
p <- p + geom_point(aes(size=-log10(p)),color="black",shape=21,data=pd %>% filter(p<=0.005))
p <- p + facet_grid(qtype~source,scales="free",space="free")
p <- p + scale_y_discrete(limits=rev)
p <- p + scale_alpha_continuous(range=c(0.1,0.75))
p <- p + scale_size(range=c(1,3))
p <- p + scale_fill_manual(values=c("black","red","black"))
p <- p + scale_color_manual(values=c("black","red","black"))
p <- p + ggtitle("MAGMA gene set enrichment analysis",subtitle="Factors/questions with at least 1 enrichment with p<0.005 (black outlines)\nRed=significant with BH correction")
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom", panel.grid.minor=element_blank(),
               plot.title = element_text(size=7, face="bold"),plot.subtitle = element_text(size=6),
               axis.text.y = element_text(size=6),axis.text.x = element_text(angle=90,size=6,hjust=1,vjust=0.5),
               axis.title.y=element_blank(),axis.title.x=element_blank(),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=6),
               legend.key.size = unit(0.1, 'in'),
               strip.text.y=element_text(size=5,vjust=0,hjust=0.5),
               strip.text.x=element_text(size=5,vjust=0,hjust=0.5))
ggsave(plot=p,filename="Fig_MAGMA.all_circles.pdf",width=6.5,height=6.5 ) 

