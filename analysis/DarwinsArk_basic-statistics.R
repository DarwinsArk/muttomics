# Libraries
library(readr)
library(tidyverse)
library(dplyr)
library(data.table)
library(grid)
library(ggpubr)
library(wesanderson)
library(EnvStats)
library(ggrepel)
library(svglite)
library(ggupset)
library(gg.gap)
library(svglite)
library(stringr)

dogs = read_csv("DarwinsArk_20191115_dogs.csv")
questions = read_csv("DarwinsArk_20191115_questions.csv")
answers = read_csv("DarwinsArk_20191115_answers.csv")
breedcalls =  read_csv("DarwinsArk_20191115_breedcalls.csv")
factors = read_csv("DarwinsArk_20191115_factors.csv")
factor_scores = read_csv("DarwinsArk_20191115_factor_scores.csv")
breeds = read_csv("ReferenceData_breeds.csv")
inbreeding = read_csv("DarwinsArk_20191115_inbreeding.csv")
muttmix_surveys = read_csv("MuttMix_20180616_survey_data.csv")

dogs_surveyed = answers %>% pull(dog) %>% unique()

# upset plot
pUpset = dogs %>%
  filter(id %in% dogs_surveyed | id %in% breedcalls$dog) %>%
  summarise(dog = id,
            surveyed = (id %in% dogs_surveyed),
            full_sex_age_size = (!is.na(sex) & !is.na(birth_date) & !is.na(size)),
            candidate_purebred = (cand == T & id %in% dogs_surveyed),
            confirmed_purebred = (conf == T & id %in% dogs_surveyed),
            mutt = (mutt == T & id %in% dogs_surveyed),
            genetic_data = (id %in% unique(breedcalls$dog))) %>%
  pivot_longer(cols = -dog,
               names_to = "feature",
               values_to = "membership") %>%
  filter(membership) %>%
  group_by(dog) %>%
  summarize(states = list(feature)) %>%
  ggplot(aes(x=states)) +
  geom_bar() +
  geom_text(stat='count', aes(label=after_stat(count))) +
  scale_x_upset(n_intersections = 20) +
  scale_y_continuous(breaks = NULL, name = "") +
  theme_pubclean()

# set plot
pSets = dogs %>%
  filter(id %in% dogs_surveyed| id %in% breedcalls$dog) %>%
  summarise(dog = id,
            surveyed = (id %in% dogs_surveyed),
            full_sex_age_size = (!is.na(sex) & !is.na(birth_date) & !is.na(size)),
            candidate_purebred = (cand == T & id %in% dogs_surveyed),
            confirmed_purebred = (conf == T & id %in% dogs_surveyed),
            mutt = (mutt == T & id %in% dogs_surveyed),
            genetic_data = (id %in% unique(breedcalls$dog))) %>%
  pivot_longer(cols = -dog,
               names_to = "feature",
               values_to = "membership") %>%
  filter(membership) %>%
  dplyr::select(feature) %>%
  unnest(cols = feature) %>%
  dplyr::count(feature) %>%
  mutate(feature = fct_reorder(as.factor(feature), n)) %>%
  ggplot(aes(y = n, x = feature)) +
  geom_col() +
  geom_text(aes(label=n)) +
  coord_flip() +
  scale_y_reverse() +
  xlab("") + ylab("") +
  theme_pubclean() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

# histogram of survey responses
pHist = ggplot(dogs, aes(x = response_rate)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "") +
  theme_pubclean()

p = ggarrange(ggarrange(pHist,pSets,nrow=2,ncol=1,heights = c(2,1)),
          pUpset,
          ncol = 2,
          nrow = 1,
          widths = c(1,3), align = "h")

ggsave(plot = p, filename = "DarwinsArk_UpsetPlot_Full_surveyed-dogs.svg",
       device = "svg",
       units = "in",
       width = 2.5*3,
       height = 1.5*3)

dogs %>%
  filter(id %in% answers$dog) %>%
  count() %>%
  print() # 18,385 dogs with any survey data

dogs %>%
  filter(id %in% answers$dog) %>%
  group_by(sex) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n / sum(n)) %>%
  print() # 50.5% female, 49.2% male, 0.0025% unreported

dogs %>%
  filter(id %in% answers$dog) %>%
  group_by(sterilized) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n / sum(n)) %>%
  print() # 89.6% sterilized, 10.1% unsterilized, 2.35% unreported

dogs %>%
  filter(id %in% answers$dog) %>%
  group_by(!is.na(owner_breed)) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n / sum(n)) %>%
  print() # 49.2% report a single breed, 50.8% do not

dogs %>%
  filter(id %in% answers$dog) %>%
  group_by(!is.na(owner_breed) & purebred == "yes") %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n / sum(n)) %>%
  print() # 18.2% are registered purebred

dogs %>%
  filter(id %in% answers$dog & id %in% breedcalls$dog) %>%
  group_by(conf) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n / sum(n)) %>%
  print() # of dogs with survey data, 28.7% (601) of genetic data set are confirmed purebred, 71.3% (1,496) are mutts

dogs %>%
  filter(id %in% answers$dog & id %in% breedcalls$dog) %>%
  filter(!is.na(birth_date) & !is.na(sex) & !is.na(size)) %>%
  group_by(conf) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n / sum(n)) %>%
  print() # of dogs with surveys, genetics, and age/sex/size, 547 are confirmed purebred

  breedcalls %>%
    group_by(dog) %>%
    summarise(max(pct)) %>%
    ungroup() %>%
    merge(dogs, by.x = "dog", by.y = "id") %>%
    group_by(`max(pct)` < 0.45, conf) %>%
    count() %>%
    ungroup() %>%
    mutate(freq = n / sum(n)) %>%
    print() # 633 confirmed purebreds, 1,522 non-purebred with 1,059 at <45% breed ancestry
  
  breedcalls %>%
    group_by(dog) %>%
    filter(pct == max(pct)) %>%
    ungroup() %>%
    merge(dogs, by.x = "dog", by.y = "id") %>%
    filter(conf == T) %>%
    select(breed) %>%
    distinct() %>%
    count() %>%
    print() # of those 633 confirmed purebreds with survey data, 89 breeds (we can detect) represented
  
  breedcalls %>%
    group_by(dog) %>%
    filter(pct == max(pct)) %>%
    ungroup() %>%
    merge(dogs, by.x = "dog", by.y = "id") %>%
    filter(conf == T) %>%
    select(consensus_breed) %>%
    distinct() %>%
    count() %>%
    print() # of those 633 confirmed purebreds with survey data, 102 breeds (including non-reference breeds) represented

breed_freq_DA = read_csv("DarwinsArk_20191115_breed-frequency.csv") %>% as.data.table()

breed_freq_AKC = read_csv("ReferenceData_AKC_registrations.csv") %>% as.data.table()
breed_freq_AKC$freq = breed_freq_AKC$AKC / sum(breed_freq_AKC$AKC)

breed_freq = merge(breed_freq_DA[sample == "owner_breed"], breed_freq_AKC, by = "breed")

cor.test(x = breed_freq$freq.x,
         y = breed_freq$freq.y,
         method = "pearson")

cor.test(x = breed_freq$freq.x,
         y = breed_freq$freq.y,
         method = "pearson")$p.value

# Pearson's R = 0.88, p = 1.48e-32 between breed frequency in candidate purebred dogs and breed frequency in AKC registrations from 2000-2015

# get survey responses to factors and questions 
dogs_info <- dogs %>%
  select(id,sex,owner_breed,regseq_breed,size) %>%
  distinct() %>%
  rename(dog=id)

#### Owner-reported Breed Validation ####
topBreeds = breedcalls %>%
  group_by(dog) %>%
  slice_max(order_by = pct, n = 1) %>%
  dplyr::rename(top_breed = breed) %>%
  dplyr::rename(top_pct = pct) %>%
  as.data.table()

refBreeds = unique(breedcalls$breed)
dogs_breedValidation = dogs %>%
  merge(topBreeds, by.x = "id", by.y = "dog") %>%
  mutate(top_bin = cut(top_pct,
                       breaks = c(0,0.85,1),
                       include.lowest = T,
                       right = F)) %>%
  mutate(breed_ref = (breed1 %in% refBreeds | breed2 %in% refBreeds | owner_breed %in% refBreeds | reg_breed %in% refBreeds)) %>%
  mutate(top_reported = (top_breed == breed1 | top_breed == breed2 | top_breed == owner_breed | top_breed == reg_breed)) %>%
  as.data.table()

dogs_breedValidation[is.na(top_reported), top_reported := F]

# discrepencies
dogs %>%
  merge(topBreeds, by.x = "id", by.y = "dog") %>%
  filter(!is.na(reg_breed)) %>%
  group_by(top_breed == reg_breed) %>%
  summarize(n = n()) %>%
  mutate(pct=n/sum(n))

dogs %>%
  merge(topBreeds, by.x = "id", by.y = "dog") %>%
  filter(!is.na(owner_breed)) %>%
  group_by(top_breed == owner_breed) %>%
  summarize(n = n()) %>%
  mutate(pct=n/sum(n))

# preliminary: non-purebred
dogs_breedValidation %>%
  filter(is.na(owner_breed) & is.na(reg_breed)) %>%
  group_by(top_bin,top_reported) %>%
  summarise(dogsN = n()) %>%
  ungroup() %>%
  mutate(totalN = sum(dogsN)) %>%
  group_by(top_reported) %>%
  mutate(topN = sum(dogsN)) %>%
  ungroup() %>%
  arrange(top_reported)

# preliminary: single breed
dogs_breedValidation %>%
  filter(!is.na(owner_breed) | !is.na(reg_breed)) %>%
  group_by(top_bin,top_reported,breed_ref) %>%
  summarise(dogsN = n()) %>%
  ungroup() %>%
  mutate(totalN = sum(dogsN)) %>%
  group_by(breed_ref,top_bin) %>%
  mutate(binN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(breed_ref) %>%
  mutate(refN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(top_reported) %>%
  mutate(topN = sum(dogsN)) %>%
  ungroup() %>%
  arrange(breed_ref,top_reported)

# preliminary: registered purebred
dogs_breedValidation %>%
  filter(!is.na(reg_breed)) %>%
  group_by(top_bin,top_reported,breed_ref) %>%
  summarise(dogsN = n()) %>%
  ungroup() %>%
  mutate(totalN = sum(dogsN)) %>%
  group_by(breed_ref,top_bin) %>%
  mutate(binN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(breed_ref) %>%
  mutate(refN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(top_reported) %>%
  mutate(topN = sum(dogsN)) %>%
  ungroup() %>%
  arrange(breed_ref,top_reported)

# final: mutt
dogs_breedValidation %>%
  filter(mutt == T) %>%
  group_by(top_bin,top_reported,breed_ref) %>%
  summarise(dogsN = n()) %>%
  ungroup() %>%
  mutate(totalN = sum(dogsN)) %>%
  group_by(breed_ref,top_bin) %>%
  mutate(binN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(breed_ref) %>%
  mutate(refN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(top_reported) %>%
  mutate(topN = sum(dogsN)) %>%
  ungroup() %>%
  arrange(breed_ref,top_reported)

# final: candidate purebred
dogs_breedValidation %>%
  filter(cand == T) %>%
  group_by(top_bin,top_reported,breed_ref) %>%
  summarise(dogsN = n()) %>%
  ungroup() %>%
  mutate(totalN = sum(dogsN)) %>%
  group_by(breed_ref,top_bin) %>%
  mutate(binN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(breed_ref) %>%
  mutate(refN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(top_reported) %>%
  mutate(topN = sum(dogsN)) %>%
  ungroup() %>%
  arrange(breed_ref,top_reported)

# final: confirmed purebred
dogs_breedValidation %>%
  filter(conf == T) %>%
  group_by(top_bin,top_reported,breed_ref) %>%
  summarise(dogsN = n()) %>%
  ungroup() %>%
  mutate(totalN = sum(dogsN)) %>%
  group_by(breed_ref,top_bin) %>%
  mutate(binN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(breed_ref) %>%
  mutate(refN = sum(dogsN)) %>%
  ungroup() %>%
  group_by(top_reported) %>%
  mutate(topN = sum(dogsN)) %>%
  ungroup() %>%
  arrange(breed_ref,top_reported)

#### Breed Ancestry Classification ####
# counts for candidate purebred, confirmed purebred, and non-purebred dogs
num_candidate <- dogs %>%
  filter(cand == T) %>%
  filter(id %in% answers$dog) %>% # has 1+ survey responses
  dplyr::count() %>%
  print() # 9009 final candidate purebred dogs

num_confirmed <- dogs %>%
  filter(conf == T) %>%
  filter(id %in% answers$dog) %>% # has 1+ survey responses
  dplyr::count() %>%
  print() # 3637 final confirmed purebred dogs

num_neither <- dogs %>%
  filter(mutt == T) %>%
  filter(id %in% answers$dog) %>% # has 1+ survey responses
  dplyr::count() %>%
  print() # 9276 final mutts

print(paste("Candidate:", num_candidate, sep = " "))
print(paste("Confirmed:", num_confirmed, sep = " "))
print(paste("Mutts:", num_neither, sep = " "))
print(paste("Total (candidates + mutts):", num_candidate + num_neither, sep = " "))

#### Inbreeding ####
inbreeding %>%
  merge(dogs, by.x = "dog", by.y = "id", all.x = T) %>%
  group_by(conf) %>%
  summarise(mean=mean(F.roh), sd=sd(F.roh), n = n())

p = inbreeding %>%
  merge(dogs, by.x = "dog", by.y = "id", all.x = T) %>%
  mutate(class = if_else(conf == T, "confirmed purebred", if_else(mutt == T, "mutt", "candidate purebred"))) %>%
  filter(class != "candidate purebred") %>%
  ggplot(aes(x = class, y = F.roh)) +
  geom_point(position = position_jitter(), alpha = 0.2) +
  geom_boxplot(outlier.shape = NA, fill = "white", alpha = 0.5) +
  coord_flip() +
  stat_n_text() + 
  theme_pubr()

test = inbreeding %>%
  merge(dogs, by.x = "dog", by.y = "id", all.x = T) %>%
  mutate(class = if_else(conf == T, "confirmed purebred", if_else(mutt == T, "mutt", "candidate purebred"))) %>%
  filter(class %in% c("confirmed purebred","mutt"))

test %>%
  group_by(class) %>%
  summarise(mean = mean(F.roh),
            sd = sd(F.roh),
            n = n())

t.test((test %>% filter(class == "confirmed purebred") %>% pull(F.roh)), (test %>% filter(class == "mutt") %>% pull(F.roh)))$p.value

#### Breed Frequencies ####
  # rank by genetic dogs
  # include other breeds
  genFreq = breedcalls %>%
    group_by(breed) %>%
    dplyr::summarize(genFreq = sum(pct)/sum(breedcalls$pct)) %>%
    ungroup() %>%
    mutate(genRank = dense_rank(desc(genFreq))) %>% as.data.table()
  
  candFreq = dogs %>%
    filter(id %in% dogs_surveyed & (!is.na(owner_breed) | !is.na(regseq_breed))) %>%
    mutate(breed = owner_breed) %>%
    select(id,breed) %>%
    filter(!is.na(breed)) %>%
    dplyr::count(breed) %>%
    mutate(candFreq = n / sum(n)) %>%
    mutate(candRank = dense_rank(desc(candFreq))) %>%
    dplyr::rename(candN = n) %>%
    select(breed, candN, candFreq, candRank) %>% as.data.table()
    
  confFreq = dogs %>%
    filter(id %in% dogs_surveyed & (!is.na(regseq_breed))) %>%
    mutate(breed = regseq_breed) %>%
    select(id,breed) %>%
    filter(!is.na(breed)) %>%
    dplyr::count(breed) %>%
    mutate(confFreq = n / sum(n)) %>%
    mutate(confRank = dense_rank(desc(confFreq))) %>%
    dplyr::rename(confN = n) %>%
    select(breed, confN, confFreq, confRank) %>% as.data.table()
  
  # get MuttMix guess frequency:
  muttmix_surveys = muttmix_surveys %>%
    pivot_longer(cols = c("BreedChoice1","BreedChoice2","BreedChoice3"),
                 names_to = "choice",
                 names_prefix = "BreedChoice",
                 values_to = "survey_breed",
                 values_drop_na = T) %>% 
    merge(breeds[,list(breed_name,breed_muttmix)], by.x = "survey_breed", by.y = "breed_muttmix", all.x = T)
  
  mmixFreq = muttmix_surveys %>%
    dplyr::rename(breed = breed_name) %>%
    select(UserID,breed) %>%
    filter(!is.na(breed)) %>%
    dplyr::count(breed) %>%
    mutate(mmixFreq = n / sum(n)) %>%
    mutate(mmixRank = dense_rank(desc(mmixFreq))) %>%
    dplyr::rename(mmixN = n) %>%
    select(breed, mmixN, mmixFreq, mmixRank) %>% as.data.table()
  
  breedRanks = genFreq %>%
    merge(candFreq, by = "breed", all = T) %>%
    merge(confFreq, by = "breed", all = T) %>%
    merge(mmixFreq, by = "breed", all = T)
  
  breedOrder = breedRanks %>%
    arrange(genRank, mmixRank, candRank, confRank) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    select(breed,order)
  
  breedRanks %>%
    filter(genRank <= 20 | candRank <= 30 | confRank <= 30 | mmixRank <= 30) %>%
    View()
  
# breed frequency: genetic dogs
  pd = breedcalls %>%
    dplyr::mutate(bin=if_else(pct<0.05,0,
                       if_else(pct>=0.85,0.85,
                               if_else(pct>=0.45,0.45,0.05)))) %>%
    group_by(breed,bin) %>%
    dplyr::summarize(pct=sum(pct)/sum(breedcalls$pct))
  
  pd <- pd %>% group_by(breed) %>% dplyr::summarize(pctTot=sum(pct)) %>% inner_join(pd)
  pd <- pd %>% dplyr::mutate(pctC=pct/pctTot)
  
  
  # top breeds...
  okbreeds <- pd %>% select(breed,pctTot) %>% distinct() %>% arrange(desc(pctTot)) %>% top_n(30) %>% pull(breed)

  colors <- rev(wes_palette("Zissou1",5))
  colors <- c(colors[1],colors[3],colors[4],"grey40")
  levels <- rev(pd %>% select(bin) %>% distinct() %>% arrange(bin) %>% pull(bin))
  
  # assign breed order:
  pd = pd %>%
    merge(breedOrder, by = "breed", all = T)
  
  pd %>%
    ggplot(aes(x = pct, y = reorder(breed,-order))) +
    geom_bar(aes(fill=factor(bin, levels=levels)),
             position="stack", stat="identity") +
    scale_fill_manual(values=colors) +
    ggtitle(paste("breed ancestry for all dogs"),
            subtitle="grey = under 5%; blue = 5%-45%;\nyellow = 45%-85%; red = 85%-100%") +
    scale_x_continuous("fraction ancestry in population",breaks=(c(1:5)/100)*2) +
    scale_y_discrete("") +
    theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),plot.subtitle = element_text(size=8),strip.text = element_text(size=7,face="bold",lineheight=0.75))
  
pAllFreq = pd %>%
  ggplot(aes(x = pct, y = reorder(breed,-order))) +
  geom_bar(aes(fill=factor(bin, levels=levels)),
           position="stack", stat="identity") +
  scale_fill_manual(values=colors) +
  scale_x_continuous("",breaks=(c(1:5)/100)*2) +
  scale_y_discrete("") +
  theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),plot.subtitle = element_text(size=8),strip.text = element_text(size=7,face="bold",lineheight=0.75))

pTopFreq = pd %>%
  filter(breed %in% okbreeds) %>%
  ggplot(aes(x = pct, y = reorder(breed,-order))) +
  geom_bar(aes(fill=factor(bin, levels=levels)),
           position="stack", stat="identity") +
  scale_fill_manual(values=colors) +
  scale_x_continuous("",breaks=(c(1:5)/100)*2) +
  scale_y_discrete("") +
  theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),plot.subtitle = element_text(size=8),strip.text = element_text(size=7,face="bold",lineheight=0.75))

pd = as.data.table(pd)
breedFreqGenetics = pd[, list(panel = "genetics",
                              breed = breed,
                              pct = pct,
                              order = order,
                              bin = bin)]
colorTable = data.table(bin = levels,
                        color = colors)
breedFreqGenetics = breedFreqGenetics %>%
  merge(colorTable, by = "bin", all.x = T) %>%
  as.data.table()

# breed frequency: perceptions (muttmix guesses)
mmixFreq %>%
  merge(breedOrder, by = "breed", all.x = T) %>%
  ggplot(aes(x = mmixFreq, y = reorder(breed,-order))) +
  geom_bar(stat="identity") +
  scale_x_continuous("",breaks=(c(1:5)/100)*2) +
  scale_y_discrete("") +
  theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),plot.subtitle = element_text(size=8),strip.text = element_text(size=7,face="bold",lineheight=0.75))
  
mmixFreq = as.data.table(mmixFreq)
breedFreqMuttMix = mmixFreq[, list(panel = "muttmix",
                                   breed = breed,
                                   pct = mmixFreq)]

breedFreqMuttMix = breedFreqMuttMix %>% 
  merge(breedOrder, by = "breed", all = T) %>%
  mutate(panel = "muttmix") %>%
  mutate(bin = NA) %>%
  mutate(color = "grey40") %>%
  as.data.table()
  
# breed frequency: purebred dogs (from owner reports)
# label: candidate purebred only, confirmed purebred
dogs %>%
  filter(id %in% dogs_surveyed) %>%
  dplyr::mutate(candidate_purebred = !is.na(owner_breed),
         confirmed_purebred = !is.na(regseq_breed),
         breed = owner_breed) %>%
  select(id,breed,candidate_purebred,confirmed_purebred) %>%
  filter(!is.na(breed)) %>%
  group_by(candidate_purebred,confirmed_purebred) %>%
  dplyr::count(breed) %>%
  ungroup() %>%
  dplyr::mutate(freq = n / sum(n)) %>%
  merge(breedOrder, by = "breed", all.x = T) %>%
  ggplot(aes(x = freq, y = reorder(breed,-order))) +
  geom_bar(aes(fill=confirmed_purebred),
           position="stack", stat="identity") +
  scale_x_continuous("",breaks=(c(1:5)/100)*2) +
  scale_y_discrete("") +
  theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),plot.subtitle = element_text(size=8),strip.text = element_text(size=7,face="bold",lineheight=0.75))
  
breedFreqPurebreds = dogs %>%
  filter(id %in% dogs_surveyed) %>%
  dplyr::mutate(candidate_purebred = !is.na(owner_breed),
         confirmed_purebred = !is.na(regseq_breed),
         breed = owner_breed) %>%
  select(id,breed,candidate_purebred,confirmed_purebred) %>%
  filter(!is.na(breed)) %>%
  group_by(candidate_purebred,confirmed_purebred) %>%
  dplyr::count(breed) %>%
  ungroup() %>%
  dplyr::mutate(freq = n / sum(n)) %>%
  merge(breedOrder, by = "breed", all = T) %>% as.data.table()

confColor = "#4A3466"
candColor = "#7E55A3"
breedFreqPurebreds = breedFreqPurebreds[, list(panel = "purebreds",
                                               breed = breed,
                                               pct = freq,
                                               order = order,
                                               bin = confirmed_purebred)]
breedFreqPurebreds[bin==TRUE, color := confColor]
breedFreqPurebreds[bin!=TRUE, color := candColor]

# combine breed frquencies
breedFreqGenetics[, bin := as.character(bin)]
breedFreqMuttMix[, bin := as.character(bin)]
breedFreqPurebreds[, bin := as.character(bin)]
breedFreq = as.data.table(rbindlist(list(breedFreqGenetics,
                                         breedFreqMuttMix,
                                         breedFreqPurebreds),
                                    use.names = T,
                                    fill = T))
p = breedFreq %>%
  ggplot(aes(y = reorder(paste("#", as.character(order), " ", breed, sep = ""),-order), x = pct, fill = color)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_identity() +
  scale_x_continuous("frequency", breaks=(c(1:5)/100)*2, expand = c(0,0)) +
  scale_y_discrete("") +
  facet_wrap(.~panel) +
  theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),plot.subtitle = element_text(size=8),strip.text = element_text(size=7,face="bold",lineheight=0.75))

ggsave(plot = p,
       filename = "DarwinsArk_BreedFrequency_Full.pdf",
       device = "pdf",
       units = "in",
       width = 4.75 * 2,
       height = 10 * 2)

keepBreeds = breedRanks %>%
  filter(confN >= 50) %>%
  filter(genRank <= 20 | candRank <= 30 | confRank <= 30 | mmixRank <= 30) %>% select(breed)

keepBreeds = breedRanks %>%
  filter(candN >= 25) %>%
  filter(genRank <= 20 | candRank <= 20 | confRank <= 20 | mmixRank <= 20) %>% select(breed)

p = breedFreq %>%
  filter(breed %in% keepBreeds$breed) %>%
  ggplot(aes(y = reorder(paste("#", as.character(order), " ", breed, sep = ""),-order), x = pct, fill = color)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_identity() +
  scale_x_continuous("frequency", breaks=(c(1:5)/100)*2, expand = c(0,0)) +
  scale_y_discrete("") +
  facet_wrap(.~panel) +
  theme_bw() + theme(legend.position = "none",panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(size=7),plot.title = element_text(size=10, face="bold"),plot.subtitle = element_text(size=8),strip.text = element_text(size=7,face="bold",lineheight=0.75))

gt = ggplot_gtable(ggplot_build(p))
gt$widths[1] = 1.2*gt$widths[1]
gt$widths[5] = 1.5*gt$widths[5]
gt$widths[9] = .8*gt$widths[9]
gt$widths[13] = .8*gt$widths[13]
grid.draw(gt)

ggsave(plot = gt,
       filename = "DarwinsArk_BreedFrequency_Top.pdf",
       device = "pdf",
       units = "in",
       width = 4.75 * 2,
       height = 3 * 2)

ggsave(plot = gt,
       filename = "DarwinsArk_BreedFrequency_Top.svg",
       device = "svg",
       units = "in",
       width = 4.75 * 2,
       height = 3 * 2)

