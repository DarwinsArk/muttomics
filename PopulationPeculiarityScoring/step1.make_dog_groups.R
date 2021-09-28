
args = commandArgs(trailingOnly=TRUE)
library(tidyverse)

inputOut <- "input.all.scores.csv"
setsOnlyOut <- "input.all.sets.csv"
inputGroupsOut <- "input.all.scores_and_sets.csv"
inputGroupsSumOut <- "input.all.scores_and_sets.summary.csv"

indir <- "../../data_release"
dogsIn <- paste(indir,"/DarwinsArk_20191115_dogs.csv",sep="")
dogs <- as_tibble(read.csv(dogsIn,header=T),na.strings=c("NA","#N/A",""," ","  "))

breednames <- as_tibble(read.csv(paste(indir,"/ReferenceData_breeds.csv",sep=""))) %>% rename(breed=breed_name)
breednames <- breednames %>% mutate(breed_name_short=if_else(is.na(breed_name_short),breed,breed_name_short))

## get survey responses to factors and questions 
quesIn <- paste(indir,"/DarwinsArk_20191115_answers.csv",sep="") # input_files/input.all.scores.csv"
allQ <- as_tibble(read.csv(quesIn,header=T,na.strings=c("NA","#N/A","","N/A")))

factorsIn <- paste(indir,"/DarwinsArk_20191115_factor_scores.csv",sep="")
allF <-  as_tibble(read.csv(factorsIn,header=T,na.strings=c("NA","#N/A","","N/A")))

## normalize question responses and standardize column headers
ques <- allQ %>% mutate(idtype="question",qf=paste("Q",question,sep="")) %>% select(-time,-option) %>% rename(id=question,age=age_years) 
ques <- ques %>% rename(value=ans.norm) %>% select(idtype,qf,id,dog,value,age)   %>% filter(!is.na(value))

## remove factor responses without age and standardize column headers
fac <- allF %>% mutate(qf=paste("F",factor,sep=""),idtype="factor") %>% rename(id=factor,value=score.norm) %>% select(qf,idtype,id,dog,value,age)
fac <- fac %>% filter(!is.na(value))  #%>% filter(!is.na(age))

## make input file with scores for all dogs
input <- ques %>% select("dog","qf","value","age")
input <- fac %>% select("dog","qf","value","age") %>% bind_rows(input)
input <- input %>% filter(dog %in% dogs$id)
write.csv(input %>% select(-age),inputOut,row.names=F)

breeds <- dogs %>% select(id,consensus_breed,cand,conf) %>% pivot_longer(c(-id,-consensus_breed)) %>% filter(value) %>% select(-value)
breeds <- breeds %>% filter(consensus_breed %in% breednames$breed)

# make breed sets
sets <- breeds %>% rename(type=name,set=consensus_breed,dog=id) %>% mutate(set=str_replace_all(set," ","_"))
sets <- input %>% inner_join(sets) %>% select(-age)

# make age sets
input.tmp <- input %>% filter(!is.na(age)) %>% mutate(type="age",set=paste("Y",floor(age),sep="")) %>% select(-age)
sets <- sets %>% bind_rows(input.tmp)

# remove sets with too few dogs
oksets <- sets %>% select(dog,type,set,qf) %>% distinct() %>% group_by(set,type,qf) %>% count()
oksets <- oksets %>% filter((type=="cand"&n>=25)|(type=="conf"&n>=100)|(type=="age"&n>=200)) %>% select(-n)
sets <- sets %>% inner_join(oksets)

### make output file with all sets with scores  -- check why this is needed
### alld <- sets %>% select(dog,type,set,qf,value)
### write.csv(alld,inputGroupsOut,row.names=F)

# make output file with all sets (no scores) 
setsOnly <- sets %>% select(set,type,dog,qf) %>% distinct()
write.csv(setsOnly,setsOnlyOut,row.names=F)

stats <- alld %>% group_by(type,set,qf) %>% summarize(n=n(),min=min(value),max=max(value),mean=mean(value),median=median(value),sd=sd(value),q25=quantile(value,0.25),q75=quantile(value,0.75))
write.csv(stats,inputGroupsSumOut,row.names=F)

if (!dir.exists("runs")){
  dir.create("runs")
}
for (intype in c("cand","conf","age")){
  out <- paste("runs/",intype,".sets.csv",sep="")
  write.csv(alld %>% filter(type==intype) %>% select(dog,type,set,qf),out,row.names=F)
  print(out)
}


