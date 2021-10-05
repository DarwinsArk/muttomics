
#!/usr/bin/env Rscript
## Plotting permutations as circle plots. 
args = commandArgs(trailingOnly=TRUE)
permfile = args[1]

library(tidyverse)
indir <- "../../data_release/"
pbsIn <- paste(indir,"DarwinsArk_20191115_GWASloci_breedPBS_permutations.csv",sep="")
d <- as_tibble(read.csv(pbsIn,header=T))
d <- d %>% mutate(z=qnorm(pbs.max.perm.percentile,lower.tail=FALSE)) %>% filter(pbs.max.perm.percentile<1)
#d <- d %>% mutate(type=if_else(type!="physical trait","behavioral trait",type))

my_comparisons <- list(c("physical trait","behavioral trait"))
pd <- d %>% select(type,z)  %>% filter(!is.na(z)) %>% mutate(type=as.factor(type))
means <- compare_means(z~type,comparisons = my_comparisons,method="t.test",data=d)
print(means) 
print(paste("#regions for physical traits:",round(length((pd %>% filter(type=="physical trait"))$z),6)))
print(paste("mean z for physical traits:",round(mean((pd %>% filter(type=="physical trait"))$z),6)))
print(paste("sd z for physical traits:",round(sd((pd %>% filter(type=="physical trait"))$z),6)))
print(" ")
print(paste("#regions for behavioral traits:",round(length((pd %>% filter(type=="behavioral trait"))$z),6)))
print(paste("mean z for behavioral traits:",round(mean((pd %>% filter(type=="behavioral trait"))$z),6)))
print(paste("sd z for behavioral traits:",round(sd((pd %>% filter(type=="behavioral trait"))$z),6)))


# remove anything weakly associated
input <- d  %>% filter(gwas.p<1e-6) 

# get the minimum PBS p value reported in any breed for each region
counts <- input %>% group_by(type,chromosome,start,end) %>% summarize(minp=min(pbs.max.perm.percentile.adj.fdr))
counts_sig <- counts %>% filter(minp<0.05) %>% group_by(type) %>% count() %>% rename(noverlap=n)

counts <- counts %>% group_by(type) %>% count() %>% full_join(counts_sig) %>% replace_na(list(noverlap=0))
counts <- counts %>% mutate(frac=noverlap/n)
counts <- counts %>% arrange(type)
print(counts)

regions <- d %>% filter(gwas.p<1e-6) %>% filter(pbs.max.perm.percentile.adj.fdr<=0.05) %>% mutate(region=paste("chr",chromosome,":",start,"-",end,sep=""))
regions <- regions %>% group_by(type,region,trait,chromosome,start,end) %>% summarize(minp=min(pbs.max.perm.percentile.adj.fdr)) %>% arrange(minp)
regions <- regions %>% filter(type!="physical trait") %>% select(-type)

stats <- d %>% group_by(type) %>% 
  summarize(n=n(),p=t.test(z,mu=0,alternative = "greater")$p.value,mean=t.test(z,mu=0,alternative = "greater")$estimate,t=t.test(z,mu=0,alternative = "greater")$statistic) %>% 
  adjust_pvalue(method="BH") %>% add_significance("p.adj") %>% mutate(pTF=if_else(p<=0.05,TRUE,FALSE))
stats <- stats %>% mutate(pchar=if_else(p>0.01,as.character(round(p,3)),format(p,digits=2)))
stats <- stats %>% mutate(label=paste("N=",n,"\nmean=",round(mean,2),"\np=",pchar,"\n",p.adj.signif,sep=""))

pd <- stats %>% select(type,pTF) %>% distinct() %>% full_join(d) 
ypos <- max(d$z)*1.05
ymin <- floor(min(d$z*10))/10
ymax <- ceiling((ypos+1)*10)/10

p <- ggplot(pd,aes(x=type,y=z)) + geom_violin(aes(fill=pTF),width=0.5,alpha=0.2,draw_quantiles=c(0.25,0.5,0.75))
p <- p + stat_summary(fun=mean, geom="point", size=2, color="red")
p <- p + scale_fill_manual(values=c("grey40","red"))

p <- p + geom_text(aes(label=label),y=ypos,hjust=0.5,vjust=0,data=stats,size=2,lineheight=0.9)
p <- p + scale_y_continuous("z",limits=c(ymin,ymax))
p <- p + ggtitle("Overlap between PBS regions and GWAS hits in breeds",subtitle=('Assumption: "Percentile of max(PBS)" can be converted into normally distributed z score with qnorm\nt-test; red dot = mean'))
p <- p + theme_bw() + theme(axis.title.x=element_blank(),plot.title=element_text(size=9,face="bold"),plot.subtitle=element_text(size=7))
ggsave(plot=p,filename="Fig_PBS.violin.pdf",width=6,height=4)

