# Libraries
library(tidyverse)

# define input files
indir <- "../../data_release/"
entIn <- paste(indir,"MuttMix_20180616_pheno_entropy.csv",sep="")

# data cleaning: import entropy values and standardize breed names
d <- as_tibble(read.csv(entIn,header=T))

# mark each row as significant or not
d <- d %>% mutate(sig=if_else(fraction.explained>=sigcut,TRUE,FALSE))

# rearrange data frame so that each row contains the min and max values from the leave-one-out analysis
minmax <- d %>% group_by(breed,dog.pheno) %>% summarize(min_xpos=min(fraction.explained),max_xpos=max(fraction.explained))
tmp <- d %>% filter(dog.removed!="none") %>% select(dog.removed,fraction.explained,breed,dog.pheno)
minmax <- tmp %>% rename(min_name=dog.removed,min_xpos=fraction.explained) %>% right_join(minmax)
minmax <- tmp %>% rename(max_name=dog.removed,max_xpos=fraction.explained) %>% right_join(minmax)

pd <- d %>% filter(dog.removed=="none") %>% select(-dog.removed) %>% left_join(minmax)

# make names data frame with dog name labels for plot. Only label breed/phenotype pairs where the leave-one-out analysis spanned more that 0.1 of entropy explained, and the max value was significant
names <- pd  %>% filter((max_xpos-min_xpos)>0.1) %>% filter(max_xpos >= sigcut) %>% select(breed,sig,dog.pheno,sigcut,max_name,min_name) %>% pivot_longer(c(max_name,min_name)) %>% mutate(name=str_remove(name,"_name")) %>% rename(dogname=value)
names <- pd  %>% filter((max_xpos-min_xpos)>0.1) %>% filter(max_xpos >= sigcut) %>% select(breed,sig,dog.pheno,sigcut,max_xpos,min_xpos) %>% pivot_longer(c(max_xpos,min_xpos)) %>% mutate(name=str_remove(name,"_xpos")) %>% inner_join(names)
names <- names %>% mutate(sig=if_else(value>=sigcut,TRUE,FALSE))

# abbreviate longer dog names
names <- names %>% mutate(dogname=if_else(str_length(dogname)>6,str_sub(dogname,0,4),dogname)) %>% distinct()


# make segments data frame for plotting lines in red and gray, depending on whether they are significant
segments <- pd %>% select(breed,dog.pheno,sigcut,max_xpos,min_xpos)
tmp <- segments  %>% filter(sigcut>min_xpos&sigcut<max_xpos) %>% mutate(min_xpos=sigcut,sig=TRUE)
tmp <- segments  %>% filter(sigcut>min_xpos&sigcut<max_xpos) %>% mutate(max_xpos=sigcut,sig=FALSE) %>% bind_rows(tmp)
segments <- segments  %>% filter(!(sigcut>min_xpos&sigcut<max_xpos)) %>% mutate(sig=if_else(sigcut<min_xpos,TRUE,FALSE)) %>% bind_rows(tmp)

# make plot with all data and no labels
p <- ggplot(pd, aes(y=breed))
p <- p + geom_segment(aes(x=min_xpos,xend=max_xpos,yend=breed,color=sig),size=1.5,data=segments)
p <- p + geom_point(aes(x=fraction.explained,shape=sig,color=sig),shape=16,size=2)
p <- p + geom_point(aes(x=fraction.explained,shape=sig),shape=16,size=1,color="white",alpha=1) #,data=points) # %>% filter(!sig))
p <- p + ggtitle("Entropy analysis of Mutt Mix guesses for different breed/trait pairs",subtitle="Red = significant; grey = not significant\n")
p <- p + scale_color_manual(values=c("#bdbdbd","#a50f15"))
p <- p + scale_x_continuous("entropy explained",breaks=c(0,0.1,0.2,0.4))
p <- p + scale_y_discrete("breed",limits=rev)
p <- p + facet_grid(.~dog.pheno,scales="free_x",space="free_x")
p <- p + theme_minimal()
p <- p + theme(legend.position="none",
               strip.text.y=element_text(size=5,angle=0,hjust=0), 
               strip.text.x=element_text(size=5,vjust=0,hjust=0.5),
               plot.title=element_text(size=6,face="bold"),
               panel.grid.minor=element_blank(), 
               panel.grid.major.x=element_line(size=0.1,color="grey70"),
               panel.grid.major.y=element_line(size=0.1,color="grey70"),
               plot.subtitle=element_text(size=4),
               axis.text.y = element_text(hjust=1,size=6),
               axis.title.y = element_blank(), 
               axis.text.x = element_text(hjust=0.5,size=6),
               axis.title.x=element_text(hjust=0.5,size=6),
               panel.border = element_rect(colour = "grey20", fill=NA, size=0.3))
ggsave(plot=p,filename="Fig_ENTROPY_all.pdf",width=6.5,height=3.25)


