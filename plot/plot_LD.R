library(ggplot2)
library(tidyverse)

indir <- "../../data_release/"

distIn <- paste(indir,"MendelsMutts_extent-of-LD.csv",sep="")
tagIn <- paste(indir,"MendelsMutts_variants-tagged.csv",sep="")

colors <- rev(c("#1a1a1a","#878787","#b2182b","#a6bddb","#3690c0","#0570b0","#045a8d"))

d <- as_tibble(read.csv(distIn,header=T))
d <-d %>% mutate(population=sub("_"," ",POP))
d <-d %>% mutate(population=if_else((POP=="wolf"|POP=="village_dog"|POP=="MendelsMutts"),population,paste("breed (",population,")",sep="")))
d <-d %>% mutate(population=if_else(POP=="village_dog","village dogs",population))
d <-d %>% mutate(population=if_else(POP=="MendelsMutts","mixed breed dogs",population))

d <- d %>% mutate(BINSET=if_else(BIN<=10000,"100bp","500bp"))
p <- ggplot(d,aes(x=BIN,y=AVG_R2,group=population))
p <- p + geom_line(aes(color=population,size=population))
p <- p + ggtitle("LD in dogs (25 dogs / population)",subtitle=paste("Extent of LD in different populations measured from 20,000 randomly sampled markers filtered for maf>0.025\nPLINK tags: --ld-window-kb 105 --ld-window-r2 0  --maf 0.025 --dog --ld-window 5000",sep=""))
p <- p + scale_y_continuous("Average r2",limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))
p <- p + scale_x_continuous("Distance between variants",breaks=c(0,5000,10000,25000,50000,75000,100000),labels=c("0kb","5kb","10kb","25kb","50kb","75kb","100kb"))
p <- p + scale_color_manual(values=c(colors))
p <- p + scale_linetype_manual(values=c(1,1,1,1,1,1,2))
p <- p + facet_wrap(~BINSET,ncol=2,scales="free_x")
p <- p + scale_size_manual(values=c(0.5,0.5,0.5,0.5,0.75,0.75,0.75))
p <- p + theme_bw() 
p <- p + theme(plot.title = element_text(lineheight=1, face="bold",size=8),plot.subtitle = element_text(lineheight=1,size=8),panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
ggsave(plot=p,filename="Fig_LD_DIST.pdf",width=8,height=5) 

colors <- c("#48BC96","#637FBF","#F2632A","#1A1A1A")
d <- as_tibble(read.csv(tagIn,header=T))

title <- "Tags in dogs (25 dogs / population)"
d <-d %>% mutate(SET=if_else(SET=="affy","2 Axiom Canine Genotyping Array Set A & B",if_else(SET=="illumina","1 CanineHD BeadChip",if_else(SET=="darwinsdogs","3 Low pass and imputation","ERROR"))))
d <-d %>% mutate(population=if_else((POP=="wolf"|POP=="village_dog"|POP=="MendelsMutts"),POP,"4 breeds"))
d <-d %>% mutate(population=if_else(POP=="village_dog","village dogs",population))
d <-d %>% mutate(population=if_else(POP=="MendelsMutts","mixed breed dogs",population))
d <- d %>% select(-FRAC) %>% group_by(SET,DIST,R2,population) %>% summarize(NTOT=sum(NTOT),N=sum(N)) %>% mutate(FRAC=N/NTOT)
d <- d %>% mutate(R2STR=paste("r2>",as.character(R2),sep=""))

p <- ggplot(d,aes(x=DIST,y=FRAC,group=population))
p <- p + geom_line(aes(color=population,linetype=R2STR))
p <- p + ggtitle("Tags in dogs (25 dogs / population)",subtitle="Fraction of all SNPs tagged by genotyped SNPs with three different technologies\nMeasured from 20,000 randomly sampled markers filtered for maf>0.025\nPLINK tags: --ld-window-kb 255 --ld-window-r2 0.2 --maf 0.025 --dog --ld-window 5000")
p <- p + scale_y_continuous("fraction tagged",limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))
p <- p + scale_x_continuous("Distance between variants (kb)",breaks=c(0,50000,100000,150000,200000,250000),labels=c(0,50,100,150,200,250))
p <- p + scale_color_manual(values=c(colors))
p <- p + scale_linetype_manual(values=c(1,2))
p <- p + facet_grid(R2STR~SET)  
p <- p + theme_bw() 
p <- p + theme(legend.position="bottom",legend.text = element_text(size=6),legend.title = element_text(size=6,face="bold"),
               plot.title = element_text(lineheight=1, face="bold",size=8),plot.subtitle = element_text(lineheight=1,size=8),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),strip.text.x=element_text(size=5,face="bold",hjust=0.5,vjust=0.5),strip.text.y=element_text(size=6,hjust=0.5,vjust=0.5))
ggsave(plot=p,filename="Fig_LD_TAGGING.pdf",width=6,height=5) 

