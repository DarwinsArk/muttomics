
args = commandArgs(trailingOnly=TRUE)
library(tidyverse)
library("R.utils")


min_dogs_per_breed <- 50
indir <- "../../data_release/"

okqfs <- c("F1","F2","F3","F4","F5","F6","F7","F8","Q121","Q123","Q124","Q125","Q127","Q17","Q54","Q60" )

inQuesScores <- "make_WEBDATA.questions.input.csv"
quesOut <- paste(indir,"DarwinsArk_20191115_website.questions.csv",sep="")
factOut <- paste(indir,"DarwinsArk_20191115_website.factors.csv",sep="")
descOut <- paste(indir,"DarwinsArk_20191115_website.descriptions.csv",sep="")

breednames <- as_tibble(read.csv(paste(indir,"/ReferenceData_breeds.csv",sep=""))) %>% rename(breed=breed_name)
breednames <- breednames %>% mutate(breed_name_short=if_else(is.na(breed_name_short),breed,breed_name_short))

answersIn <- paste(indir,"DarwinsArk_20191115_answers.csv",sep="")
factorsIn <- paste(indir,"DarwinsArk_20191115_factor_scores.csv",sep="")

facInfo <- as_tibble(read.csv(paste(indir,"/DarwinsArk_20191115_factors.csv",sep=""))) %>% mutate(idtype="factor")
facInfo <- facInfo %>% mutate(qf=paste("F",factor,sep="")) %>% select(qf,name,negative,positive) %>% distinct() 
facInfo <- facInfo %>% rename(desc=name) %>% filter(qf %in% okqfs)

### quesRaw <- as_tibble(read.csv(answersIn,header=T,na.strings=c("NA","#N/A","")))  %>% mutate(qf=paste("Q",question,sep="")) %>%  filter(qf %in% okqfs)  
ques <- quesRaw %>% filter(!is.na(answer))  %>% select(qf,dog,question,answer,option) #%>% rename(age=age_years)  #%>% inner_join(ques)  %>% rename(age=age_years)
ques <- ques %>% select(dog,qf,answer) %>% rename(value=answer) %>% mutate(type="question")

facRaw <- as_tibble(read.csv(factorsIn,header=T,na.strings=c("NA","#N/A",""))) %>% mutate(qf=paste("F",factor,sep="")) %>%  filter(qf %in% okqfs)  
fac <- facRaw %>% select(dog,qf,score.norm) %>% rename(value=score.norm) %>% mutate(type="factor")

input <- fac %>% bind_rows(ques) %>% filter(!is.na(value)) %>% distinct()

# only keep dogs with complete infomation
input <- input %>% group_by(dog) %>%   count() %>% filter(n==length(okqfs)) %>% select(-n) %>% inner_join(input)

dogsIn <- paste(indir,"DarwinsArk_20191115_dogs.csv",sep="")
dogs <- as_tibble(read.csv(dogsIn,header=T),na.strings=c("NA","#N/A",""))
dogs <- dogs %>% rename(dog=id) %>% select(dog,owner_breed,regseq_breed) %>% rename(candidate_breed=owner_breed,confirmed_breed=regseq_breed)
mutts <- dogs %>% filter(is.na(candidate_breed)&is.na(confirmed_breed)) %>% pull(dog)
breeds <- dogs %>% select(dog,candidate_breed,confirmed_breed) %>% pivot_longer(c(-dog)) %>% filter(!is.na(value)) %>% mutate(value=str_trim(value)) %>% filter(str_length(value)>0)
breeds <- breeds %>% mutate(value=if_else(value=="flat-coated retrievers","flat-coated retriever",value))

# group cocker spaniel, american cocker spaniel, and english cocker spaniel together, because usage by owners is confused
breeds <- breeds %>% mutate(value=if_else(str_detect(value,"cocker spaniel"),"cocker spaniel",value))

# check that breed name is ok breed name
breeds <- breeds %>% filter(value %in% breednames$breed)

breeds <- breeds %>% rename(breed=value,breedtype=name)
breeds  <- breeds %>% pivot_wider(names_from=breedtype,values_from=breed)
breeds <- breeds %>% mutate(candidate_breed=if_else(candidate_breed==confirmed_breed|is.na(confirmed_breed),candidate_breed,confirmed_breed))
breeds <- breeds %>% mutate(breed_confirmed=if_else(is.na(confirmed_breed),FALSE,TRUE)) %>% rename(breed=candidate_breed) %>% select(-confirmed_breed)
breeds <- dogs %>% filter(dog %in% mutts) %>% select(dog) %>% mutate(breed="mixed breed",breed_confirmed=FALSE) %>% bind_rows(breeds)


# add negative positive labels to factors
fInput <- input %>% filter(type=="factor") %>% group_by(qf) %>% summarize(Q25=quantile(value,0.25),Q75=quantile(value,0.75))
fInput <- fInput %>% right_join(input) 
fInput <- fInput %>% filter(value <= Q25 | value >= Q75) %>% mutate(group=if_else(value <= Q25,"negative",if_else(value>=Q75,"positive","other")))
fInput <- fInput %>% filter(group!="other") %>% select(qf,dog,type,group)

# add negative positive labels to questions
qText <- read.csv(inQuesScores,header=T) %>% rename(value=answer)
qInput <- input %>% filter(type=="question") %>% inner_join(qText) %>% select(qf,dog,type,group)
quesInfo <- qText %>% select(-value) %>% distinct() %>% pivot_wider(names_from=group,values_from=description)

dall <- input %>% mutate(group="any") %>% select(-value) %>% bind_rows(fInput) %>% bind_rows(qInput)
dall <- dall %>% inner_join(breeds)

# remove breeds with < min_dogs_per_breed dogs
breedcounts <- dall %>% ungroup() %>% select(dog,breed) %>% distinct() %>% group_by(breed) %>% count() %>% rename(ndogs.in.breed=n)
breedcounts <- breedcounts %>% filter(ndogs.in.breed>=min_dogs_per_breed) %>% rename(ndogs.in.breed=ndogs.in.breed) %>% arrange(desc(ndogs.in.breed))

# compile dataset for all dogs
dall <- breedcounts %>% select(-ndogs.in.breed) %>% inner_join(dall)

#make empty array 
arr  <- tibble("F1"=c("any","negative","positive"))
arr2 <- arr %>% rename(F2=F1) %>% crossing(arr)
arr2 <- arr %>% rename(F3=F1) %>% crossing(arr2)
arr2 <- arr %>% rename(F4=F1) %>% crossing(arr2)
arr2 <- arr %>% rename(F5=F1) %>% crossing(arr2)
arr2 <- arr %>% rename(F6=F1) %>% crossing(arr2)
arr2 <- arr %>% rename(F7=F1) %>% crossing(arr2)
arr2 <- arr %>% rename(F8=F1) %>% crossing(arr2)
empty_no_breed_fac <- arr2
empty <- dall %>% select(breed) %>% distinct() %>% crossing(arr2) %>% select("breed","F1","F2","F3","F4","F5","F6","F7","F8")
emptyFac <- empty

dFac <- dall %>% filter(type=="factor") %>% select(-type) %>% select(-breed_confirmed)
F1 <- dFac %>% filter(qf=="F1") %>% rename(F1=group) %>% select(-qf)
F2 <- dFac %>% filter(qf=="F2") %>% rename(F2=group) %>% select(-qf)
F3 <- dFac %>% filter(qf=="F3") %>% rename(F3=group) %>% select(-qf)
F4 <- dFac %>% filter(qf=="F4") %>% rename(F4=group) %>% select(-qf)
F5 <- dFac %>% filter(qf=="F5") %>% rename(F5=group) %>% select(-qf)
F6 <- dFac %>% filter(qf=="F6") %>% rename(F6=group) %>% select(-qf)
F7 <- dFac %>% filter(qf=="F7") %>% rename(F7=group) %>% select(-qf)
F8 <- dFac %>% filter(qf=="F8") %>% rename(F8=group) %>% select(-qf)
dFac <- F1 %>% full_join(F2) %>% full_join(F3) %>% full_join(F4) %>% full_join(F5) %>% full_join(F6) %>% full_join(F7) %>% full_join(F8) #%>% replace_na()
fcounts <- dFac %>% group_by(breed,F1,F2,F3,F4,F5,F6,F7,F8) %>% count() %>% full_join(emptyFac) %>% replace_na(list(n=0))
fcounts <- breedcounts %>% full_join(fcounts) # %>% filter(!(F1=="any"&F2=="any"&F3=="any"&F4=="any"))
fcounts <- fcounts %>% mutate(frac_of_breed=n/ndogs.in.breed) %>% select(-ndogs.in.breed)
fcounts <- fcounts %>% select(-n) %>% distinct()
write.csv(fcounts,factOut,row.names=F)
gzip(factOut,overwrite=TRUE)
dim(fcounts)

#fcounts <- fcounts %>% filter(frac_of_breed!=0)
#write.csv(fcounts,"data.WEBDATA.factor_data.for_viz.frac_gt_zero.csv",row.names=F)
#dim(fcounts)

#write.csv(facInfo,"data.WEBDATA.factor_descriptions.for_viz.csv",row.names=F)
#write.csv(breedcounts,"data.WEBDATA.factor_breed_counts.for_viz.csv",row.names=F)

colnames(empty) <- c("breed","Q121","Q123","Q124","Q125","Q127","Q17","Q54","Q60")
empty_no_breed_ques <- empty %>% select(-breed) %>% distinct()

dQues <- dall %>% filter(type=="question") %>% select(-type) %>% select(-breed_confirmed)
Q121 <- dQues %>% filter(qf=="Q121") %>% rename(Q121=group) %>% select(-qf)
Q125 <- dQues %>% filter(qf=="Q125") %>% rename(Q125=group) %>% select(-qf)
Q127 <- dQues %>% filter(qf=="Q127") %>% rename(Q127=group) %>% select(-qf)
Q124 <- dQues %>% filter(qf=="Q124") %>% rename(Q124=group) %>% select(-qf)
Q54 <- dQues %>% filter(qf=="Q54") %>% rename(Q54=group) %>% select(-qf)
Q123 <- dQues %>% filter(qf=="Q123") %>% rename(Q123=group) %>% select(-qf)
Q60 <- dQues %>% filter(qf=="Q60") %>% rename(Q60=group) %>% select(-qf)
Q17 <- dQues %>% filter(qf=="Q17") %>% rename(Q17=group) %>% select(-qf)
dQues <- Q121 %>% full_join(Q125) %>% full_join(Q127) %>% full_join(Q124) %>% full_join(Q54) %>% full_join(Q123) %>% full_join(Q60) %>% full_join(Q17) #%>% replace_na()
qcounts <- dQues %>% group_by(breed,Q121,Q125,Q127,Q124,Q54,Q123,Q60,Q17) %>% count() %>% full_join(empty) %>% replace_na(list(n=0))
qcounts <- breedcounts %>% right_join(qcounts) # %>% filter(!(Q121=="any"&Q125=="any"&Q127=="any"&Q124=="any"))
qcounts <- qcounts %>% mutate(frac_of_breed=n/ndogs.in.breed) %>% select(-ndogs.in.breed) 
qcounts <- qcounts %>% select(-n)
write.csv(qcounts,quesOut,row.names=F)
gzip(quesOut,overwrite=TRUE)


#write.csv(qcounts,"data.WEBDATA.question_data.for_viz.csv",row.names=F)
#qcounts <- qcounts %>% filter(frac_of_breed!=0)
#write.csv(qcounts,"data.WEBDATA.question_data.for_viz.frac_gt_zero.csv",row.names=F)

write.csv(quesInfo,"data.WEBDATA.question_descriptions.for_viz.csv",row.names=F)
#write.csv(breedcounts,"data.WEBDATA.question_breed_counts.for_viz.csv",row.names=F)

info <- quesInfo %>% rename(desc=short) %>% mutate(site="questions")
info <- facInfo  %>% mutate(site="factors") %>% bind_rows(info)
write.csv(info,descOut,row.names=F)


