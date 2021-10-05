# Libraries
library(tidyverse)
library(ggrepel)
library(wesanderson)
library(ggpubr)

indir <- "../../data_release/"

breedsIn <- paste(indir,"/ReferenceData_breeds.csv",sep="")
geneticsIn <- paste(indir,"DarwinsArk_20191115_breedcalls.csv",sep="")
dogsIn <- paste(indir,"DarwinsArk_20191115_dogs.csv",sep="")
akcIn <- paste(indir,"ReferenceData_AKC_registrations.csv",sep="")

# get standardized breed names
breednames <- as_tibble(read.csv(breedsIn,header=T)) %>% rename(breed=breed_name)

# get breed calling results (genetic ancestry)
breed_calling_percents <- as_tibble(read.csv(geneticsIn,header=T)) %>% rename(breed_calling=breed,percent=pct)
breed_calling_percents <- breednames %>% select(breed,breed_calling) %>% distinct() %>% right_join(breed_calling_percents) 
all_breed_calling_percents <- breed_calling_percents

# get list of breeds included in breed calling reference panel
breeds_called <- breed_calling_percents %>% select(breed) %>% distinct() %>% pull(breed) 

# get dogs
dogs <- as_tibble(read.csv(dogsIn,header=T))  %>% rename(dog=id)
all_dogs <- dogs

# if breed2 == breed1, then set breed2 = NA
dogs <- dogs %>% mutate(breed2=na_if(breed2,breed1))
#f breed1 is not in breednames (e.g. not a real breed) set to NA
dogs <- dogs %>% mutate(breed1=replace(breed1,!(breed1 %in% breednames$breed),NA))
#if breed2 is not in breednames (e.g. not a real breed) set to NA
dogs <- dogs %>% mutate(breed2=replace(breed2,!(breed2 %in% breednames$breed),NA))
## annotate whether there is sequence data 
dogs <- dogs %>% mutate(sequenced=if_else(geno_wgs|geno_axiom|geno_lowpass,TRUE,FALSE))  
dogs <- dogs %>% mutate(purebred=if_else(is.na(purebred)|purebred=="no",FALSE,TRUE))

#dogs <- dogs %>% select(-breed1,-breed2) 

# put in sets based on owner declared ancestry
dogsets <- dogs %>% filter(sequenced)
dogsets <- dogsets %>% mutate(conf_surv=if_else(!is.na(breed1)&is.na(breed2)&purebred&breed1%in%breeds_called,TRUE,FALSE))
dogsets <- dogsets %>% mutate(cand_surv=if_else(!is.na(breed1)&is.na(breed2)&breed1%in%breeds_called,TRUE,FALSE))
dogsets <- dogsets %>% mutate(mutt_surv=if_else((!is.na(breed1)&!is.na(breed2)&breed1!=breed2)|(is.na(breed1)&is.na(breed2)),TRUE,FALSE))

dogsets <- dogsets %>% select(dog,conf_surv,cand_surv,mutt_surv) %>% pivot_longer(-dog) %>% filter(value)
dogsets <- dogsets %>% select(-value) %>% rename(dogset=name)
dogsets <- dogs %>% filter(is.na(breed2)) %>% select(dog,breed1) %>% right_join(dogsets)

# add genetic ancestry 
dogs_called <- dogsets %>% inner_join(breed_calling_percents) #%>% filter(percent>=0.05)

# make empty rows to make sure we count dogs that have no ancestry detected from top breed
dogs_called_empty <- dogs_called %>% filter(dogset!="mutt_surv") %>% select(dog,breed1,dogset) %>% mutate(breed=breed1,breed_calling=breed1,percent=0)   %>% distinct()
dogs_called <- dogs_called %>% bind_rows(dogs_called_empty)
dogs_called <- dogs_called %>% group_by(dog,breed1,dogset,breed,breed_calling) %>% summarize(percent=max(percent)) %>% inner_join(dogs_called)
dogs_called <- dogs_called %>% select(-breed_calling)
dogs_called_top_breed <- dogs_called %>% group_by(dog) %>% summarize(max=max(percent))
dogs_called_top_breed <- dogs_called_top_breed %>%  inner_join(dogs_called) #@#%>% inner_join(dogsets) %>% distinct()

## get numbers for Breed Ancestry Assignment section (first paragraph)
matchcnt <- dogs_called_top_breed %>% filter(dogset!="mutt_surv") %>% group_by(dog,dogset) %>% summarize(percent=max(percent)) %>% inner_join(dogs_called_top_breed) %>% mutate(match=if_else((str_detect(breed1,"poodle")&str_detect(breed,"poodle"))|breed1==breed,TRUE,FALSE))
matchper <- matchcnt %>% group_by(dogset,match) %>% count() %>% pivot_wider(names_from=match,values_from=n) %>% mutate(total=`FALSE`+`TRUE`) %>% select(-`FALSE`) %>% mutate(percent_match=round((`TRUE`/total)*100,1))
print(matchper)

# if not mutt, get percentage of owner-reported breed; otherwise, get max. 
dogs_called_top_breed <- dogs_called_top_breed %>% filter((dogset=="mutt_surv"&percent==max)|breed1==breed|(str_detect(breed1,"poodle")&str_detect(breed,"poodle"))) 
dogs_called_top_breed <- dogs_called_top_breed %>% select(-max) %>% group_by(dog,dogset) %>% summarize(percent=max(percent)) %>% inner_join( dogs_called_top_breed)
#####
set_descriptions <- tibble(dogset=c("conf_surv","cand_surv","mutt_surv"),group=c("1. Confirmed (surveys)","2. Candidate (surveys)","3. Mutts (surveys)"))

pd <- dogs_called_top_breed %>% select(dog,dogset,percent) %>% left_join(set_descriptions)
pd <- pd %>% group_by(group) %>% count() %>% inner_join(pd)
pd <- pd %>% mutate(ylabel=paste(group," (N=",n,")",sep="")) %>% distinct()

pd_threshold <- pd %>% filter(percent>=0.85&dogset!="other") %>% group_by(ylabel,group) %>% count()
pd_threshold <- pd %>% group_by(ylabel,group) %>% count() %>% rename(ntot=n) %>% inner_join(pd_threshold) %>% mutate(frac=n/ntot,percent=0.85)

# plot line plot of % dogs over certain ancestry cutoff
thresholds <- tibble(cutoff=c(c(0:100)*0.01))
thresholds <- crossing(thresholds,pd) %>% filter(percent>=cutoff) %>% group_by(cutoff,ylabel) %>% count()
thresholds <- pd %>% group_by(ylabel) %>% count() %>% rename(ntot=n) %>% inner_join(thresholds) %>% mutate(freq=n/ntot)
thresholds <- thresholds %>% rename(percent=cutoff)

cand_wrong <- pd %>% filter(dogset=="cand_surv"&percent==0) %>% group_by(dogset) %>% count() %>% pull(n)
conf_wrong <- pd %>% filter(dogset=="conf_surv"&percent==0) %>% group_by(dogset) %>% count() %>% pull(n)

p <- ggplot(thresholds,aes(x=percent,y=freq,group=ylabel)) + geom_line(aes(color=ylabel))
p <- p + geom_point(aes(color=ylabel),shape=16,data=(thresholds %>% filter(percent==0.85)))
p <- p + geom_text(aes(label=paste(round(freq*100,1),"%",sep=""),color=ylabel),data=(thresholds %>% filter(percent==0.85)),size=3,vjust=0,hjust=0,nudge_x=0.01,nudge_y=0.02,alpha=0.75)
p <- p + geom_vline(xintercept=0.85,color="#1a1a1a",linetype=2,size=0.5)
p <- p + scale_color_manual(values=c("black","#fc8d62","#66c2a5","#8da0cb"))
p <- p + scale_x_continuous("minimum fraction ancestry",limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))
p <- p + scale_y_continuous("fraction of dogs",limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))
p <- p + ggtitle("Fraction of ancestry from top breed",subtitle=paste(cand_wrong," candidate breed dogs and ",conf_wrong," confirmed breed dogs had no ancestry detected from owner-reported breed",sep=""))
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom", panel.grid.minor=element_blank(),
               plot.title = element_text(size=7, face="bold"),plot.subtitle = element_text(size=5),
               axis.text.y = element_text(size=7),axis.text.x = element_text(size=7),
               legend.title = element_blank(),
               legend.text=element_text(hjust=0.5,size=7),
               legend.key.size = unit(0.1, 'in'))
ggsave(p,filename="Fig_ANC_FRAC_TOP_BREED.line.pdf",width=3,height=3)


breeds_per_mutt <- dogs %>% filter(mutt) %>% inner_join(dogs_called) 
breeds_per_mutt <- breeds_per_mutt %>% filter(percent>0.05) %>% group_by(dog) %>% count()  %>% rename(nbreeds=n)
counts <- tibble(nbreeds=c(1:10))
counts <- breeds_per_mutt %>% group_by(nbreeds) %>% summarize(ndogs=n()) %>% full_join(counts) %>% replace_na(list(ndogs=0))
tot <- sum(counts$ndogs)
counts <- counts %>% arrange(desc(nbreeds)) %>% mutate(ndogs_cum=cumsum(ndogs))  %>% mutate(ntot=tot) %>% mutate(frac_dogs=ndogs/ntot,frac_cum=ndogs_cum/ntot) %>% arrange(nbreeds)

pd <- counts %>% filter(nbreeds>1&nbreeds<9)
pd <- pd %>% mutate(label=paste(round(frac_cum*100,0),"%",sep=""))
p <- ggplot(pd,aes(x=nbreeds,y=frac_cum)) + geom_bar(stat="identity",alpha=1) + geom_text(aes(label=label,y=frac_cum+0.01),color="black",hjust=0.5,vjust=0,size=2.5)
p <- p + ggtitle("number of breeds in non-purebred dogs")
p <- p + scale_y_continuous("fraction of mutts",breaks=c(0,0.5,1))
p <- p + scale_x_continuous("# breeds detected at >5%",breaks=c(1:10),labels=paste(c(1:10),"+",sep=""))
p <- p + theme_minimal()
p <- p + theme(legend.position = "none",
               plot.title = element_text(size=7, face="bold"),
               panel.grid.minor=element_blank(),
               panel.grid.major=element_blank(),
               axis.ticks.x=element_blank(),
               axis.text.y = element_text(size=6),
               axis.text.x = element_text(size=7,face="bold"))
ggsave(p,filename="Fig_ANC_NBREEDS_MUTTS.histogram.pdf",width=3,height=2.5)


#### Make plots of breed frequency in breed calling #### 
pd <- breed_calling_percents %>% mutate(bin=if_else(percent<0.05,0,if_else(percent>=0.85,0.85,if_else(percent>=0.45,0.45,0.05)))) %>% group_by(breed,bin) %>% summarize(percent=sum(percent)/sum(breed_calling_percents$percent))
pd <- pd %>% group_by(breed) %>% summarize(percentTot=sum(percent)) %>% inner_join(pd)
pd <- pd %>% mutate(percentC=percent/percentTot)
pd <- pd %>% select(breed,percentTot) %>% distinct() %>% arrange(desc(percentTot)) %>% mutate(row=row_number()) %>% full_join(pd)
pd <- pd %>% mutate(ylabel=paste(row,breed))

colors <- rev(wes_palette("Zissou1",5))
colors <- c(colors[1],colors[3],colors[4],"grey40")
levels <- rev(pd %>% select(bin) %>% distinct() %>% arrange(bin) %>% pull(bin))

topbreeds <- pd %>% filter(percentTot>0.005) %>% select(breed,ylabel,percentTot) %>% distinct() 
pd <- pd %>% filter(breed %in% topbreeds$breed)
pd <- pd %>% pivot_longer(c(percent,percentC)) %>% mutate(name=if_else(name=="percentC","percent of breed","percent of population"))
pd$name <- factor(pd$name,levels=c("percent of breed","percent of population"))
p <- ggplot(pd, aes(x=value, y=fct_reorder(ylabel, row))) + geom_bar(aes(fill=factor(bin, levels=levels)),position="stack", stat="identity")
p <- p + scale_fill_manual(values=colors)
p <- p + ggtitle(paste("breed ancestry for all dogs"),subtitle="grey = under 5%; blue = 5%-45%;\nyellow = 45%-85%; red = 85%-100%\nbreeds representing >0.05% of DA population")
p <- p + scale_x_continuous("fraction ancestry")
p <- p + scale_y_discrete("",limits=rev)
p <- p + facet_wrap(~name,nrow=1,scales="free_x")
p <- p + theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),plot.subtitle = element_text(size=8),strip.text = element_text(size=7,face="bold",lineheight=0.75))
ggsave(p,filename="Fig_ANC_PROP_POP_MIN005.pdf",width=7,height=9)
 
#### MAKE plot of breed frequency differences by region of US ###### 
dogs <- all_dogs
pd <- dogs %>% select(dog,region) %>% filter(!is.na(region)) %>% inner_join(breed_calling_percents) %>% group_by(region) %>% summarize(totanc=sum(percent),ndogs_region=n()) 
pd <- dogs %>% select(dog,region) %>% filter(!is.na(region)) %>% inner_join(breed_calling_percents) %>% group_by(region,breed) %>% summarize(n_region_breed=n(),perc_anc=sum(percent)) %>% inner_join(pd) %>% mutate(perc_anc=perc_anc/totanc) 
pd <- dogs %>% select(dog,region) %>% filter(!is.na(region))  %>% inner_join(breed_calling_percents) %>% group_by(breed) %>% summarize(tot=sum(percent)) %>% filter(tot>=50) %>% select(-tot) %>% inner_join(pd)
pd <- pd <- pd %>% filter(ndogs_region>2000)
pd <- topbreeds %>% arrange(desc(percentTot))  %>% slice_head(n=5) %>% inner_join(pd)
#pd <- pd %>% select(region,breed) %>% inner_join(breed_calling_percents) %>% group_by(region) %>% count()
pd <- pd %>% mutate(group=region) %>% mutate(strip=str_replace_all(breed," ","\n")) 
pd <- pd %>% mutate(group=str_replace(group,"\\(","\n("))
pdtext1 <- pd %>% group_by(ylabel,percentTot,group,strip,region,breed) %>% summarize(perc_anc=sum(perc_anc))  %>% mutate(percent=paste(round(perc_anc*100,1),"%",sep="")) %>% mutate(textcolor=if_else(perc_anc<0.01,"black","white"))
pd$group <- factor(pd$group, levels = rev(unique(pd$group)))

palette <- wes_palette("Darjeeling1",5,type="continuous")
p <- ggplot(pd, aes(x=perc_anc, y=fct_reorder(ylabel, percentTot))) + geom_bar(aes(fill=breed),position="stack", stat="identity",width=0.75)
p <- p + geom_text(aes(label=percent),color="black",vjust=0.5,hjust=0,size=2,data=pdtext1,nudge_x=0.001)
p <- p + ggtitle(paste("breed ancestry by","region","(all dogs)"))
p <- p + scale_x_continuous("fraction ancestry in population",limits=c(0,max(pd$perc_anc)*1.2))
p <- p + scale_y_discrete("")
p <- p + scale_fill_manual(values=palette)
p <- p + facet_wrap(~group,nrow=1)
p <- p + theme_minimal() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),strip.text = element_text(size=7,face="bold",lineheight=0.75))
ggsave(p,filename="Fig_ANC_BY_REGION.pdf",width=8,height=2)

#### MAKE plot of breed frequency in population vs AKC registrations ###### 

akc <- as_tibble(read.csv(akcIn,header=T)) %>% select(breed,AKC)  %>% filter(AKC>0)
akc <- akc %>% filter(breed %in% breednames$breed)
akcTot <- sum(akc$AKC)
akc <- akc %>% mutate(f_AKC=AKC/akcTot) 

real <- all_breed_calling_percents %>% group_by(breed) %>% summarize(DA=sum(percent)) 
realTot <- sum(real$DA)
real <- real %>% mutate(f_DA=DA/realTot)

pd <- akc %>% inner_join(real) %>% select(-AKC,-DA)
high <- pd %>% select(breed,f_DA,f_AKC) %>% filter(f_DA>0.05|(f_DA>0.015&f_DA/f_AKC>4)) %>% select(breed) %>% distinct() %>% inner_join(pd)
high <- breednames %>% select(breed,abbr) %>% right_join(high) %>% mutate(abbr=str_remove_all(abbr,"\\-"))
correlations <- pd %>% summarize(cor=cor(f_DA,f_AKC),p=cor.test(f_DA,f_AKC)$p.value,conf.low=(cor.test(f_DA,f_AKC)$conf.int)[1],conf.high=(cor.test(f_DA,f_AKC)$conf.int)[2]) 
palette <- wes_palette("Darjeeling1",5,type="continuous")

p <- ggplot(pd,aes(y=f_DA,x=f_AKC))+ geom_smooth(method=lm,alpha=0.2,color=palette[1],size=0.5)
p <- p + geom_point(color="grey40",alpha=0.5,shape=16,size=1.5)
p <- p + geom_text_repel(aes(label=abbr),data=high,size=2,min.segment.length = 0.02, segment.size = 0.2,lineheight=0.9)
p <- p + stat_cor(label.x=0,label.y=0.085,color=palette[1],size=2.5)
p <- p + ggtitle("AKC breed registrations (2000-2015) vs ancestry in pet dog population")
p <- p + scale_x_continuous("fraction of AKC registrations",breaks=c(0,0.05,0.1,0.15))
p <- p + scale_y_continuous("fraction of ancestry in U.S. dogs",breaks=c(0,0.025,0.05,0.075,0.1))
p <- p + theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),axis.text=element_text(size=7),axis.title=element_text(size=8),plot.title = element_text(size=7, face="bold"),strip.text = element_text(size=7,face="bold",lineheight=0.75))
ggsave(p,filename="Fig_ANC_DA_v_AKC.pdf",width=4,height=4)
