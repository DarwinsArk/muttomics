# Libraries
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(RColorBrewer)

## point to location of folder with paper data release files.
indir <- "../../../data_release/"


# input data files from data_release folder
surveyIn <- paste(indir,"MuttMix_20180616_survey_data.csv",sep="")
geneticIn <- paste(indir,"MuttMix_20180616_genetic_data.csv",sep="")
DAIn <- paste(indir,"DarwinsArk_20191115_breedcalls.csv",sep="")
breedsIn <- paste(indir,"ReferenceData_breeds.csv",sep="")
akcIn <- paste(indir,"ReferenceData_AKC_registrations.csv",sep="")

# get standardized breednames
breednames <- as_tibble(read.csv(breedsIn,header=T,na.strings=c("","<NA>","NA","n/a","#N/A")))
breednames <- breednames %>% mutate(breed_muttmix=tolower(breed_muttmix)) 
breednames <- breednames %>% rename(breed=breed_name)

# get ancestry calling results for Mutt mix dogs
genetics <- as_tibble(read.csv(geneticIn,header=T,na.strings=c("","<NA>","NA"))) 
genetics <- genetics %>% rename(breed_muttmix=survey_option,DogID=dog) %>% mutate(breed=tolower(breed),breed_muttmix=tolower(breed_muttmix)) 
genetics <- genetics %>% left_join(breednames) %>% select(DogID,breed_muttmix,breed,pct) #%>% filter(!is.na(breed_muttmix))

genetics.top3 <- genetics %>% filter(!is.na(breed_muttmix)) %>% arrange(DogID,desc(pct)) %>% group_by(DogID) %>% mutate(genetics_top3.i=row_number()) %>% ungroup() 
genetics <- genetics %>% left_join(genetics.top3)                          
genetics.top3 <- genetics.top3 %>% filter(genetics_top3.i<=3) %>% select(DogID,breed,breed_muttmix,genetics_top3.i,pct) 

# read in Mutt Mix survey responses
survey_data <- as_tibble(read.csv(surveyIn,header=T,na.strings=c("","<NA>","NA","n/a","#N/A")))

# remove Sophie, the accidental purebred dog
survey_data <- survey_data %>% filter(DogFirstName!="Sophie")
survey_data <- survey_data %>% pivot_longer(c(BreedChoice1,BreedChoice2,BreedChoice3)) %>% rename(breed_muttmix=value) %>% filter(!is.na(breed_muttmix))
survey_data <- survey_data %>% mutate(breed_muttmix=tolower(breed_muttmix)) 
survey_data <- survey_data %>% distinct() %>% pivot_wider(names_from=name,values_from=breed_muttmix)

# update mutt call names to be unique
muttnames <- as_tibble(survey_data) %>% select(DogID,DogFirstName) %>% distinct() 
muttnames <- muttnames %>% mutate(DogFirstName=if_else(DogFirstName=="Jack",if_else(DogID==1021,"Jack1","Jack2"),DogFirstName))
survey_data <- survey_data %>% mutate(DogFirstName=if_else(DogFirstName=="Jack",if_else(DogID==1021,"Jack1","Jack2"),DogFirstName))

answers <- as_tibble(survey_data) %>% select(-DateAdded) %>% pivot_longer(c(-UserID,-ProfDogTrainer,-DogFirstName,-DogID)) %>% distinct() %>% rename(breed_muttmix=value)
answers <- answers  %>% filter(!is.na(breed_muttmix))
answers <- answers %>% mutate(guess.i=as.integer(str_remove(name,"BreedChoice"))) %>% select(-name,-DogFirstName)
#answers <- breednames %>% select(breed,breed_muttmix) %>% inner_join(answers)

# check how often users guessed ok breed
allguesses <- genetics %>% mutate(match=TRUE) %>% right_join(answers) %>% mutate(match=if_else(is.na(match),FALSE,match)) %>% select(DogID,UserID,breed_muttmix,pct,genetics_top3.i,match,pct) %>% rename(rank.i=genetics_top3.i) %>% replace_na(list(pct=0))


########## Make Fig_MM_CORRECT.pdf ##########
# check how much of the mutt's total ancestry the users guessed accurately 
correct_by_anc_tot <- allguesses %>% group_by(DogID,UserID) %>% summarize(pct=sum(pct))
correct_by_anc_tot_stat <- correct_by_anc_tot %>% ungroup() %>% group_by(DogID) %>% summarize(n=n(),mean=mean(pct),median=median(pct)) #%>% inner_join(correct_by_anc_tot)
correct_by_anc_tot <- correct_by_anc_tot %>% inner_join(correct_by_anc_tot_stat) 
correct_by_anc_tot <- survey_data %>% select(DogID,DogFirstName) %>% distinct() %>% inner_join(correct_by_anc_tot)
correct_by_anc_tot <- correct_by_anc_tot %>% mutate(ylabel=paste(DogFirstName," (N=",n,")",sep=""))
order <- correct_by_anc_tot %>% ungroup() %>% select(DogID,median,mean) %>% distinct() %>% arrange(mean,median) %>% mutate(nrow=row_number()) %>% select(DogID,nrow)
correct_by_anc_tot <- correct_by_anc_tot %>% inner_join(order)
correct_by_anc_tot <- correct_by_anc_tot %>% ungroup() %>% mutate(ylabel = fct_reorder(ylabel, nrow))
#pd_mean <- correct_by_anc_tot %>% ungroup() %>% select(ylabel,mean) %>% rename(pct=mean) %>% distinct()

pd <- crossing(tibble(bin=c(1:10)/10),correct_by_anc_tot %>% select(DogID,DogFirstName,n,ylabel,nrow) %>% distinct())
pd <- correct_by_anc_tot %>% mutate(bin=floor(pct*10)/10) %>% group_by(DogID,DogFirstName,n,ylabel,nrow,bin) %>% summarize(nid=n()) %>% mutate(fnid=nid/n) %>% full_join(pd)
pd <- pd %>% mutate(bin=if_else(bin==1,0.9,bin))
pd <- pd %>% replace_na(list(fnid=0))
pd <- correct_by_anc_tot %>% select(DogID,median,mean) %>% distinct() %>% inner_join(pd)
pd <- pd %>% mutate(ylabel=as.factor(ylabel))

mean_all <- mean(correct_by_anc_tot$pct)

p <- ggplot(pd,aes(x=bin,y=fct_reorder(ylabel, median))) #+ geom_tile(aes(fill=fnid))
p <- p + geom_segment(aes(xend=bin+1/10,yend=ylabel,color=fnid),size=3)
p <- p + geom_point(aes(x=median),color="#08306b",fill="white",shape=21)
p <- p + geom_vline(xintercept=mean_all,color="#de2d26")
p <- p + scale_fill_gradient(low="#f7fbff",high="#08306b",limits=c(0,1))
p <- p + scale_color_gradient(low="#f7fbff",high="#08306b",limits=c(0,1))
p <- p + scale_x_continuous(limits=c(0,1),breaks=c(0,0.5,1))
p <- p + theme_minimal()  + theme(legend.position="bottom",axis.ticks=element_blank(),panel.grid.major.x=element_line(color="grey50",size=0.2),panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),plot.title = element_text(lineheight=1, face="bold",size=12),axis.text.y=element_text(size=6),axis.title=element_blank())
ggsave(plot=p,filename="Fig_MM_CORRECT.pdf",width=3,height=4.5)


########## Make Fig_MM_vANC.pdf ########## 

users_per_dog <- answers %>% select(DogID,UserID) %>% distinct() %>% group_by(DogID) %>% count() %>% rename(nuser=n)
frac_users_correct_by_breed <- allguesses %>% filter(rank.i<=3) %>% filter(match) %>% distinct() %>% group_by(DogID,breed_muttmix,pct) %>% count() %>% inner_join(users_per_dog) %>% mutate(frac_users=n/nuser) # %>% left_join(muttnames) %>% distinct()
frac_users_correct_by_breed <- survey_data %>% select(DogID,DogFirstName) %>% distinct() %>% inner_join(frac_users_correct_by_breed)

p <- ggplot(frac_users_correct_by_breed,aes(x=pct,y=frac_users)) + geom_text(aes(label=DogFirstName),hjust=0.5,vjust=0.5,size=1.75,alpha=0.75)
p <- p + stat_cor(method = "pearson",color="#99000d",size=3,label.y=1)
p <- p + geom_vline(xintercept=0.45,color="#99000d",linetype=2) 
p <- p + geom_text(label=">45%",x=0.46,y=0,hjust=0,size=2,color="#99000d")
p <- p + scale_x_continuous("% ancestry by genetics") + scale_y_continuous("Fraction of users with correct guess") 
p <- p + ggtitle("Fraction of users that correctly guessed dog's breed",subtitle=paste("Pearson correlation for",nrow(frac_users_correct_by_breed),"dog/breed pairs"))
p <- p + theme_bw() + theme(plot.title = element_text(lineheight=1, face="bold",size=8),plot.subtitle = element_text(lineheight=1,size=6))
ggsave(plot=p,filename="Fig_MM_vANC.pdf",width=4,height=4)
write.csv(frac_users_correct_by_breed,"Fig_MM_vANC.csv",row.names=F)



########## Make Fig_MM_GUESS_vAKC_DA.pdf ########## 

  # get AKC registration rates
  akc <- as_tibble(read.csv(akcIn,header=T)) %>% select(breed,AKC)  %>% filter(AKC>0)
  akc <- akc %>% inner_join(breednames)%>% filter(!is.na(breed_muttmix))
  akctotal <- sum(akc$AKC)
  akc <- akc  %>% mutate(frac_AKC=AKC/akctotal) %>% select(-AKC)
  akc<- akc  %>% group_by(breed_muttmix) %>% summarize(frac_AKC=sum(frac_AKC))

  # get overall ancestry percentages for whole Darwin's Ark population
  DA_data <- as_tibble(read.csv(DAIn,header=T))
  DA_pop_percents <- breednames %>% select(breed_muttmix,breed_calling) %>% distinct() %>% rename(breed=breed_calling) %>% inner_join(DA_data) %>% filter(!is.na(breed_muttmix)) 
  pop_total <- sum(DA_pop_percents$pct)
  DA_pop_percents <- DA_pop_percents %>% group_by(breed,breed_muttmix) %>% summarize(sum=sum(pct)) %>% mutate(DA_freq=sum/pop_total)
  DA_pop_percents <- DA_pop_percents %>% select(breed,breed_muttmix,DA_freq) %>% arrange(desc(DA_freq))

nguesses_total <- length(answers$UserID)

exp_breed_freqs <- breednames %>% select(breed_muttmix) %>% inner_join(allguesses) %>% group_by(breed_muttmix) %>% count()
exp_breed_freqs <- exp_breed_freqs %>% mutate(guess_freq=n/nguesses_total) %>% select(-n) 
exp_breed_freqs <- exp_breed_freqs %>% left_join(DA_pop_percents)
exp_breed_freqs <- exp_breed_freqs %>% left_join(akc)
exp_breed_freqs <- exp_breed_freqs %>% arrange(breed_muttmix) %>% filter(!is.na(breed_muttmix))
exp_breed_freqs$order <- c(1:nrow(exp_breed_freqs))
maxorder <- max(exp_breed_freqs$order)
exp_breed_freqs <- exp_breed_freqs  %>% mutate(order=order/maxorder)

pd <- exp_breed_freqs %>% rename(`frac ancestry in Darwin's Ark dogs`=DA_freq,`position in Mutt Mix dropdown menu`=order,`frac registrations with AKC (2000-2015)`=frac_AKC)
pd <- pd %>% pivot_longer(c(-breed_muttmix,-guess_freq,-breed)) %>% filter(!is.na(value))
#outliers <- pd %>% filter((name=="% ancestry in Darwin's Ark dogs"&(value>0.05|guess_freq>0.05))|(name=="position in Mutt Mix dropdown menu"&guess_freq>0.05)|(name=="% registrations with AKC (2000-2015)"&(value>0.05|guess_freq>0.05)))
outliers <- pd %>% filter(guess_freq>0.06|(str_detect(name,"Ark")&value>0.09))
p <- ggplot(pd,aes(x=value,y=guess_freq)) + geom_point() + facet_wrap(~name,scales="free_x",nrow=1) + theme_bw()
p <- p + geom_text_repel(aes(label=breed),data=outliers,size=2,min.segment.length = 0, segment.size = 0.2,lineheight=0.9)
p <- p + stat_cor(method = "pearson", label.y = max(exp_breed_freqs$guess_freq)*1.2, label.x = 0,color="black",size=3.5)
p <- p + ggtitle("Breeds guessed correlate with overall popularity in the US",subtitle="with pearson correlation")
p <- p + scale_y_continuous("frequency guessed in Mutt Mix")

p <- p + theme(axis.title.x = element_blank(),panel.grid.minor=element_blank(),strip.text.y = element_text(size=3))
ggsave(plot=p,filename="Fig_MM_GUESS_vAKC_DA.pdf",width=8,height=3.5)
write.csv(exp_breed_freqs,"Fig_MM_GUESS_vAKC_DA.csv",row.names=F)

######### comparing observed and expected #########
# Check how well users performed compared to expections 
## keep only dogs with >5% ancestry from at least two breeds (30 dogs)
mutts_all <-  genetics  %>% filter(!is.na(breed_muttmix)&pct>=0.05) %>% group_by(DogID) %>% count() %>% filter(n>1) %>% select(DogID) %>% distinct()
nbreeds_tot <- length(unique(answers$breed_muttmix))

## get expected from guess rate 
exp_breed_freqs <- breednames %>% select(breed_muttmix) %>% inner_join(allguesses) %>% group_by(breed_muttmix) %>% count() %>% mutate(guess_freq=n/nguesses_total) %>% select(-n) 
exp_breed_freqs <- exp_breed_freqs %>% left_join(DA_pop_percents) %>% select(-breed)
exp_breed_freqs <- exp_breed_freqs %>% filter(!is.na(breed_muttmix))  %>% arrange(breed_muttmix)
exp_breed_freqs$order <- c(1:length(exp_breed_freqs$breed_muttmix))


# get all possible combinations of answer types
combRaw <- tibble(nguess=c(2,2,2,3,3,3,3),nright=c(c(0:2),c(0:3)))
combRaw <- crossing(mutts_all,combRaw)

#nbreeds <- length(guesses %>% filter(DogID %in% mutts_all$DogID) %>% select(breed_muttmix) %>% distinct() %>% pull(breed_muttmix))
obs_by_dog_user <- allguesses %>% select(DogID,UserID,breed_muttmix,match) %>% distinct()
obs_by_dog_user_total <- obs_by_dog_user %>% group_by(DogID,UserID) %>% count() %>% rename(nguess=n)
obs_by_dog_user <- obs_by_dog_user %>% filter(match) %>% group_by(DogID,UserID) %>% count() %>% rename(nright=n) %>% full_join(obs_by_dog_user_total) %>% replace_na(list(nright=0))

obsTot <- obs_by_dog_user %>% group_by(DogID,nguess) %>% count() %>% rename(ntot=n)
observed <- obs_by_dog_user %>% ungroup() %>% group_by(DogID,nright,nguess) %>% count() %>% rename(nobs=n)
observed <- combRaw %>% full_join(observed) %>% replace_na(list(nobs=0)) %>% full_join(obsTot) %>% mutate(obs=nobs/ntot)
observed <- observed %>% ungroup() %>% group_by(DogID,nguess) %>% summarize(cum_obs=order_by(desc(nright),cumsum(obs)),nright=nright) %>% inner_join(observed)

#########
# every breed has probability of being picked ~ overall frequency in Darwin's Ark population
unorderedP3 <- function(f1,f2,f3) {
  prob <- (f1*(f2/(1-f1))*(f3/(1-(f2+f1))))+(f1*(f3/(1-f1))*(f2/(1-(f3+f1))))+(f2*(f1/(1-f2))*(f3/(1-(f2+f1))))+(f2*(f3/(1-f2))*(f1/(1-(f2+f3))))+(f3*(f2/(1-f3))*(f1/(1-(f2+f3))))+(f3*(f1/(1-f3))*(f2/(1-(f3+f1))))
  return(prob)
}
unorderedP2 <- function(f1,f2) {
  prob <- f1*(f2/(1-f1)) + f2*(f1/(1-f2))
  return(prob)
}
#########
input <- exp_breed_freqs %>% filter(!is.na(breed_muttmix)) %>% group_by(breed_muttmix) %>% summarize(freq=sum(DA_freq))
tot <- sum(input$freq)
input <- input %>% mutate(freq=freq/tot)

freq_guessed <- input %>% ungroup() %>% arrange(breed_muttmix)
freq_guessed$row <- c(1:length(freq_guessed$breed_muttmix))
br1 <- c(1:length(unique(freq_guessed$breed_muttmix)))
br2 <- br1
br3 <- br1

rcombs <- crossing(br1, br2, br3) %>% filter(br1<br2&br2<br3)
rcombs <- rcombs %>% inner_join(freq_guessed,by=c("br1"="row")) %>% rename(f1=freq,breed1=breed_muttmix)
rcombs <- rcombs %>% inner_join(freq_guessed,by=c("br2"="row")) %>% rename(f2=freq,breed2=breed_muttmix)
rcombs <- rcombs %>% inner_join(freq_guessed,by=c("br3"="row")) %>% rename(f3=freq,breed3=breed_muttmix)
rcombs <- rcombs %>% mutate(exp=unorderedP3(f1,f2,f3))
rcombs <- rcombs %>% select(breed1,breed2,breed3,exp) %>% mutate(nguess=3) %>% distinct()
rcombs2 <- crossing(br1, br2) %>% filter(br1<br2)
rcombs2 <- rcombs2 %>% inner_join(freq_guessed,by=c("br1"="row")) %>% rename(f1=freq,breed1=breed_muttmix)
rcombs2 <- rcombs2 %>% inner_join(freq_guessed,by=c("br2"="row")) %>% rename(f2=freq,breed2=breed_muttmix)
rcombs2 <- rcombs2 %>% mutate(exp=unorderedP2(f1,f2))
rcombs2 <- rcombs2 %>% select(breed1,breed2,exp) %>% mutate(nguess=2) %>% distinct()
rcombs_exp <- rcombs %>% bind_rows(rcombs2) %>% distinct()
rcombs_real <- genetics.top3 %>% ungroup() %>% filter(!is.na(breed_muttmix)) %>% group_by(DogID,breed_muttmix) %>% summarize(pct=sum(pct),genetics_top3.i=min(genetics_top3.i)) %>% filter(pct>=0.05&genetics_top3.i<=3)
rcombs_real <- rcombs_real  %>% group_by(DogID) %>% mutate(row=row_number()) %>% inner_join(rcombs_real)
rcombs_real <- rcombs_real %>% mutate(row=paste("r.breed",row,sep=""))%>% select(DogID,breed_muttmix,row) %>% pivot_wider(names_from = row,values_from=breed_muttmix)
rcombs_all <- crossing(rcombs_real,rcombs_exp)
rcombs_n3 <- rcombs_all %>% filter(nguess==3) %>% mutate(r1=if_else(breed1==r.breed1|breed2==r.breed1|breed3==r.breed1,1,0),r2=if_else(breed1==r.breed2|breed2==r.breed2|breed3==r.breed2,1,0),r3=if_else(is.na(r.breed3),0,if_else(breed1==r.breed3|breed2==r.breed3|breed3==r.breed3,1,0)))
rcombs_n2 <- rcombs_all %>% filter(nguess==2) %>% mutate(r1=if_else(breed1==r.breed1|breed2==r.breed1,1,0),r2=if_else(breed1==r.breed2|breed2==r.breed2,1,0),r3=if_else(!is.na(r.breed3),if_else(breed1==r.breed3|breed2==r.breed3,1,0),0))
rcombs_all <- rcombs_n3 %>% bind_rows(rcombs_n2) %>% mutate(nright=r1+r2+r3)
rcombs_all <- rcombs_all %>% group_by(DogID,nguess,nright) %>% summarize(exp=sum(exp))
rcombs_all <- rcombs_all %>% ungroup() %>% group_by(DogID,nguess) %>% summarize(cum_exp=order_by(desc(nright),cumsum(exp)),nright=nright) %>% inner_join(rcombs_all)

# combine expected frequencies from each approach
d <- observed  %>% inner_join(rcombs_all) %>% filter(DogID %in% mutts_all$DogID) # %>% filter(!is.na(exp)) 
d <- d %>% inner_join(muttnames)
dcum <- d %>% select(DogFirstName,DogID,nguess,nright,ntot,cum_obs,cum_exp) %>% mutate(type="cumulative") %>% rename(obs=cum_obs,exp=cum_exp)
d <- d %>% select(DogFirstName,DogID,nguess,nright,ntot,obs,exp) %>% mutate(type="noncum") %>% bind_rows(dcum)

# combine 2 guesses and 3 guesses
d <- d %>% mutate(nobs=obs*ntot,nexp=exp*ntot) %>% group_by(DogFirstName,DogID,nright,type) %>% summarize(ntot=sum(ntot),obs=sum(nobs)/sum(ntot),exp=sum(nexp)/sum(ntot))
d <- d %>% mutate(obs=if_else(obs==0,1/(ntot+1),obs))
d <- d  %>% mutate(ratio=obs/exp)
d <- d %>% filter(type=="cumulative"&nright>0)
d <- d %>% mutate(nobs=obs*ntot,nexp=exp*ntot)
pchisq <- d %>% ungroup() %>% group_by(DogID,nright) %>% summarize(pchi=chisq.test(matrix(c(nobs,ntot-nobs,nexp,ntot-nexp),nrow=2),simulate.p.value=T)$p.value,n=n())
pchisq  <- pchisq %>% mutate(adjp=p.adjust(pchi,method="BH"))
d <- pchisq %>% right_join(d) %>% mutate(pchi=if_else(obs==1&exp==1,1,pchi)) %>% mutate(adjp=if_else(obs==1&exp==1,1,adjp))
d <- d %>% mutate(direc=if_else(ratio<1,"under","over"))
d <- d %>% select(DogID,nright) %>% distinct() %>% group_by(nright)  %>% count() %>% rename(ntests=n) %>% inner_join(d)
d <- d %>% mutate(sig=if_else(pchi<=0.05/ntests,"sig","nonsig")) 

######### Make Fig_MM_OBS_EXP.pdf ######### 

pd <- d %>% filter(type=="cumulative"&nright>0) %>% mutate(logratio=log10(ratio))


pd <- pd %>% mutate(strip=if_else(nright<3,paste(nright," or more correct breed guesses",sep=""),paste(nright,"/3 correct breed guesses",sep="")))

ylabels <- c(0.01,0.05,0.1,0.5,1,5,10,100,1000,10000)
ybreaks <- log10(ylabels)
miny <- floor(min(pd$logratio)*10)/10

p <- ggplot(pd,aes(x=obs,y=logratio)) + geom_hline(yintercept=0,color="grey30") + geom_point(aes(color=direc,shape=sig),size=1.25,alpha=0.75)
p <- p + geom_text_repel(aes(label=DogFirstName),size=2,segment.color="grey60",segment.size = 0.2,lineheight=0.9,max.overlaps=30,min.segment.length = 0)
p <- p + facet_wrap(~strip,scales="free",ncol=3)
p <- p + scale_color_manual(values=c("#1F78B4","#252525"))
p <- p + scale_shape_manual(values=c(21,16))
p <- p + scale_y_continuous("ratio of observed vs expected",breaks=ybreaks,labels=ylabels)
p <- p + scale_x_continuous("fraction of participants") #,breaks=c(0,0.2,0.4,0.6,0.8,1),labels=c("0%","2%","4%","%6","8%","10%"))
p <- p + ggtitle("Observed rate of correct guesses vs. expected",subtitle="All mutts\n")
p <- p + theme_minimal()  + theme(legend.position="bottom",panel.grid.minor=element_blank(),plot.title = element_text(lineheight=1, face="bold",size=12),axis.text.y=element_text(size=8))
ggsave(plot=p,filename="Fig_MM_OBS_EXP.horizontal.pdf",width=6.5,height=4.5)
write.csv(pd,"Fig_MM_OBS_EXP.csv",row.names=F)

######### Make Fig_MM_OBS_EXP.pdf ######### 
ylabels <- c(0.01,0.05,0.1,0.5,1,2,5,10,20,50,100,1000,10000)
ybreaks <- log10(ylabels)

p <- ggplot(pd,aes(x=obs,y=logratio)) + geom_hline(yintercept=0,color="grey30") + geom_point(aes(color=direc,shape=sig),size=1.25,alpha=0.75)
p <- p + geom_text_repel(aes(label=DogFirstName),size=2,segment.color="grey60",segment.size = 0.2,lineheight=0.9,max.overlaps=30,min.segment.length = 0)
p <- p + facet_wrap(~strip,scales="free",nrow=3)
p <- p + scale_color_manual(values=c("#1F78B4","#252525"))
p <- p + scale_shape_manual(values=c(21,19))
p <- p + scale_y_continuous("ratio of observed vs expected",breaks=ybreaks,labels=ylabels)
p <- p + scale_x_continuous("fraction of participants") #,breaks=c(0,0.2,0.4,0.6,0.8,1),labels=c("0%","2%","4%","%6","8%","10%"))
p <- p + ggtitle("Observed rate of correct guesses\nvs. expected",subtitle="All mutts\n")
p <- p + theme_minimal()  + theme(legend.position="bottom",panel.grid.minor=element_blank(),plot.title = element_text(lineheight=1, face="bold",size=12),axis.text.y=element_text(size=8))
ggsave(plot=p,filename="Fig_MM_OBS_EXP.vertical.pdf",width=3.5,height=11)

######### Make Fig_MM_OBS_EXP.main_text.pdf ######### 

pd <- pd %>% filter(nright<=2)
shown_in_plot <- c("Maxine","Jack1","Bella")
cuts <- pd %>% group_by(nright,strip) %>% summarize(cutH=quantile(ratio,0.90),cutL=quantile(ratio,0.1))
shown_in_plot <- pd %>% inner_join(cuts) %>% filter((ratio>=cutH|ratio<=cutL)|DogFirstName %in% shown_in_plot) %>% ungroup() %>% select(DogFirstName) %>% distinct() %>% pull(DogFirstName)
pd_text <- pd %>% filter(DogFirstName %in% shown_in_plot)
p <- ggplot(pd,aes(x=obs,y=logratio)) + geom_hline(yintercept=0,color="grey30") + geom_point(aes(color=direc,shape=sig),size=1.25,alpha=0.75)
p <- p + geom_text_repel(aes(label=DogFirstName),size=2,segment.color="grey60",segment.size = 0.2,lineheight=0.9,max.overlaps=30,min.segment.length = 0,data=pd_text)
p <- p + facet_wrap(~strip,scales="free",nrow=2)
p <- p + scale_color_manual(values=c("#1F78B4","#252525"))
p <- p + scale_shape_manual(values=c(21,19))
p <- p + scale_y_continuous("ratio of observed vs expected",breaks=ybreaks,labels=ylabels)
p <- p + scale_x_continuous("fraction of participants",breaks=c(0,0.25,0.5,0.75,1),labels=c("0%","25%","50%","%75","100%"),limits=c(0,1))
#p <- p + ggtitle("Observed rate of correct guesses\nvs. expected",subtitle="All mutts\n")
p <- p + theme_minimal()  + theme(legend.position="bottom",panel.grid.minor=element_blank(),plot.title = element_text(lineheight=1, face="bold",size=12),axis.text.y=element_text(size=8))
ggsave(plot=p,filename="Fig_MM_OBS_EXP.main_text.pdf",width=3,height=5.25)

######### Make Fig_MM_PROF_NON.pdf ######### 

users <- answers %>% select(UserID, ProfDogTrainer) %>% distinct()

combRaw <- tibble(nguess=c(2,2,2,3,3,3,3),nright=c(c(0:2),c(0:3)))
combRaw <- crossing(mutts_all,combRaw)

nbreeds <- length(allguesses %>% select(breed_muttmix) %>% distinct() %>% pull(breed_muttmix))
obs_by_dog_user <- allguesses %>% select(DogID,UserID,breed_muttmix,match) %>% distinct()
obs_by_dog_user_total <- obs_by_dog_user %>% group_by(DogID,UserID) %>% count() %>% rename(nguess=n)
obs_by_dog_user <- obs_by_dog_user %>% filter(match) %>% group_by(DogID,UserID) %>% count() %>% rename(nright=n) %>% full_join(obs_by_dog_user_total) %>% replace_na(list(nright=0)) %>% inner_join(users)
obsTot <- obs_by_dog_user  %>% group_by(DogID,ProfDogTrainer,nguess) %>% count() %>% rename(ntot=n)

observed <- obs_by_dog_user %>% ungroup() %>% group_by(DogID,ProfDogTrainer,nright,nguess) %>% count() %>% rename(nobs=n) 
observed <- combRaw %>% full_join(observed) %>% replace_na(list(nobs=0)) %>% full_join(obsTot) %>% mutate(obs=nobs/ntot)
observed <- observed %>% ungroup() %>% group_by(DogID,ProfDogTrainer,nguess) %>% summarize(cum_obs=order_by(desc(nright),cumsum(obs)),nright=nright) %>% inner_join(observed)
observed <- observed  %>% inner_join(rcombs_all) 
observed <- observed %>% inner_join(muttnames)

d <- observed %>% select(ProfDogTrainer,DogFirstName,DogID,nguess,nright,ntot,cum_obs,cum_exp) %>% mutate(type="cumulative") %>% rename(obs=cum_obs,exp=cum_exp)

# combined nguess=2 and nguess=3
d <- d %>% mutate(nobs=obs*ntot,nexp=exp*ntot) %>% group_by(ProfDogTrainer,DogFirstName,DogID,nright,type) %>% summarize(ntot=sum(ntot),obs=sum(nobs)/sum(ntot),exp=sum(nexp)/sum(ntot))
d <- d  %>% mutate(ratio=obs/exp)
pd <- d %>% filter(obs>0,type=="cumulative"&nright>0) %>% mutate(logratio=log10(ratio))
pd <- pd %>% mutate(strip=if_else(nright<3,paste(nright," or more correct breed guesses",sep=""),paste(nright,"/3 correct breed guesses",sep="")))
pd <- pd %>% mutate(guessers=if_else(ProfDogTrainer==1,"Professional","Nonprofessional"))

#remove data points without a prof / nonprof pair
pd <- pd %>% group_by(DogID,type,nright) %>% count() %>% filter(n>1) %>% select(-n) %>% inner_join(pd)

pd_wide <- pd %>% ungroup() %>%  select(DogFirstName,logratio,strip,guessers) %>% pivot_wider(names_from=guessers,values_from=logratio)
pd_wide <- pd_wide %>% mutate(guessers="Nonprofessional",guessersEnd="Professional",logratio=Nonprofessional,logratioEnd=Professional)

my_comparisons <- list( c("Professional","Nonprofessional"))

p <- ggplot(pd,aes(x=guessers,y=logratio))
p <- p + geom_segment(aes(xend=guessersEnd,yend=logratioEnd),data=pd_wide,color="#e31a1c",alpha=0.75,size=0.25)
p <- p + geom_violin(alpha=0.25,width=0.5,color="grey30",fill="grey70",size=0.25,draw_quantiles=c(0.5))
p <- p + geom_point(size=1,alpha=0.5,shape=16)
p <- p + stat_compare_means(method = "wilcox.test",size=2,comparisons = my_comparisons,paired = TRUE,exact=TRUE)
p <- p + facet_wrap(~strip,scales="free",ncol =3)
p <- p + scale_y_continuous("ratio of observed vs expected",breaks=ybreaks,labels=ylabels)
p <- p + ggtitle("Professionals vs nonprofessionals",subtitle="ratio of observed vs expected (>1 = better than random)\nWilcoxon non-parametric test")
p <- p + theme_bw()  + theme(legend.position="bottom",panel.grid.minor=element_blank(),plot.subtitle = element_text(lineheight=1,size=6),plot.title = element_text(lineheight=1, face="bold",size=8),axis.text.y=element_text(size=8))
ggsave(plot=p,filename="Fig_MM_PROF_NON.pdf",width=6.5,height=3)
write.csv(pd,"Fig_MM_PROF_NON.csv",row.names=F)
  

