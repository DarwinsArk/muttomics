# Linear mixed effects regression models for survey scores

# Conditions:
# 1. only breed ancestry up to 50% from a breed
# 2. only dogs with <50% ancestry from any one breed
# 3. only breeds with a SD >5% ancestry across dogs carrying that breed

# Variables:
# independent variable: factor score
# fixed effects: ancestry from each breed
# random effects: covariance matrix of genetic relatedness, age bracket (2 years and under, between 2 and 12 years, and 12 years and older)

# Method:
# 1. build models with restricted maximum likelihood (REML) to obtain unbiased estimates, standard deviations, and Wald statistics (t.val) for the fixed effects of breed on factor scores
# 2. perform analysis-of-variance (ANOVA) on each factor model to obtain the breed F-statistics
# 3. contribute models with maximum likelihood (ML) - breed.i
# 4. perform ANOVA to obtain likelihood ratio for each breed and report p-values for likelihood tests between the model with ML (all breeds) and models with ML -breed.i
# NOTE! We report p-values for the likelihood tests between the ML models but not for REML models. Why? See: https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
# 5. obtain the proportion of variance explained by the fixed effects (breed ancestries) as the marginal Nakagawa's R2 for each factor modeled with and without restricted maximum likelihood
# 6. obtain the conditional R2, which is the variance explained by both the random effects, which are the age bracket and kinship covariance matrix, and fixed effects, which are the breed ancestries
# NOTE! We could not always ascertain the conditional R2 due to singularity in some models from the random effects structure being too complex.

# Load Libraries
# plotting and visualization
require(ggplot2)
require(ggpubr)

# data management
require(tidyr)
require(dplyr)
require(plyr)
require(readr)
require(data.table)
require(reshape2)
require(zoo)

# modeling
require(devtools)
require(lme4)
# remotes::install_github("palday/coefplot2", subdir = "pkg")
require(coefplot2)
# devtools::install_github("variani/lme4qtl")
require(lme4qtl)
require(psych)
require(insight)
require(performance)

# R script to read the GRM binary file
ReadGRMBin=function(prefix, AllN=F, size=4){
  sum_i=function(i){
    return(sum(1:i))
  }
  BinFileName=paste(prefix,".grm.bin",sep="")
  NFileName=paste(prefix,".grm.N.bin",sep="")
  IDFileName=paste(prefix,".grm.id",sep="")
  id = read.table(IDFileName)
  n=dim(id)[1]
  BinFile=file(BinFileName, "rb");
  grm=readBin(BinFile, n=n*(n+1)/2, what=numeric(0), size=size)
  NFile=file(NFileName, "rb");
  if(AllN==T){
    N=readBin(NFile, n=n*(n+1)/2, what=numeric(0), size=size)
  }
  else N=readBin(NFile, n=1, what=numeric(0), size=size)
  i=sapply(1:n, sum_i)
  return(list(diag=grm[i], off=grm[-i], id=id, N=N))
}

#### Load Data ####
# see Drive: https://drive.google.com/drive/folders/1IwYpapfqhGaOcNKmKdZ5fjs-STGsFfLB?usp=sharing

GRM = ReadGRMBin("")

dogs = read_csv("DarwinsArk_20191115_dogs.csv") %>% as.data.table()

breedcalls =  read_csv("DarwinsArk_20191115_breedcalls.csv") %>% as.data.table()

factor_scores_melt.filled = read.delim("DarwinsArk_20191115_factor_scores.csv",
                                       sep = ",",
                                       header = T) %>% as.data.table()

factors = read.delim("DarwinsArk_20191115_factors.csv",
                     sep = ",",
                     header = T) %>% as.data.table()

factors_use = unique(factors[!is.na(name), c("factor","name")])

#### Mixed Linear Effects Models with Covariance Matrix ####
# get the covariance matrix from genetic relationship matrix (GRM)
GRMCorr <- matrix(NA, ncol = length(GRM$diag), nrow = length(GRM$diag))
GRMCorr[lower.tri(GRMCorr)] <- GRM$off
GRMCorr[upper.tri(GRMCorr)] <- t(GRMCorr)[upper.tri(t(GRMCorr))]
diag(GRMCorr) <- 1 # GRM not exact 1 on diagonal due to some nonsense I spoke with Diane about though can't recall exactly why
rownames(GRMCorr) = GRM$id$V1
colnames(GRMCorr) = GRM$id$V1

# as this is not a positive definite matrix, smooth correlations
GRMCorr = cor.smooth(GRMCorr)

# build model for different ancestry cutoffs:
# 55%, 45%, 35%, 25%, 15%, 5%
# assess at each cutoff, how many breeds remain
# this was for when I submitted jobs with M as a variable
M = 0.45

# settings for model
min = 0 # minimum pct considered
max = M # maximum pct considered

# get breed calls for dogs listed in the GRM
BreedLoad = copy(breedcalls[, list(pct = mean(pct, na.rm = T)), by = c("dog","breed")])

# remove spaces from breed
BreedLoad[, breed := gsub(" ","_",breed)]

# eliminate calls below min%, set to 0%
BreedLoad[pct < min, pct := 0]

# keep only dogs that have factor scores
BreedLoad = BreedLoad[dog %in% unique(factor_scores_melt.filled$dog)]

# keep only dogs in the GRM
BreedLoad = BreedLoad[dog %in% GRM$id$V1]

# set 0% to 0.01% for purpose of correlation matrix
BreedLoad[pct == 0, pct := 0.01]

# remove dogs with any call not within [min,max] %
BreedLoad = BreedLoad[!dog %in% unique(BreedLoad[pct > max]$dog)]
print("Dogs kept in model:")
print(length(unique(BreedLoad$dog)))

# retain calls from breeds with SD >= 5% of ancestry between 5% and 45%
BreedLoad = BreedLoad[breed %in% BreedLoad[pct >= 0.05 & pct <= 0.45, sd(pct, na.rm = T), by = breed][V1 >= 0.05]$breed]
print("Breeds kept in model:")
print(length(unique(BreedLoad$breed)))

# get correlation
BreedLoadCorr = pivot_wider(data = BreedLoad,
                            id_cols = c("dog"),
                            names_from = "breed",
                            values_from = "pct",
                            values_fill = 0) %>% as.data.table()
BreedLoad_dogs = BreedLoadCorr$dog
BreedLoadCorr[, dog := NULL]
BreedLoadCorr = round(cor(t(BreedLoadCorr)),3)


BreedLoadCorr = cor.smooth(BreedLoadCorr)
rownames(BreedLoadCorr) = BreedLoad_dogs
colnames(BreedLoadCorr) = BreedLoad_dogs

# reset 0.01% to 0%
BreedLoad[pct < min, pct := 0]

# list breeds col names
formula_paste = paste(unique(BreedLoad$breed), collapse = " + ")

# breed info
BreedLoad_breeds = data.table(breed = unique(BreedLoad$breed),
                              Nbreed = BreedLoad[pct > 0, .N, by = "breed"]$N)
breeds_run = unique(BreedLoad$breed)

# scale percent ancestry by breed
BreedLoad[, pct := scale(pct), by = "breed"]

# pivot by breed
BreedLoad = pivot_wider(data = BreedLoad,
                        id_cols = "dog",
                        names_from = "breed",
                        values_from = "pct",
                        values_fill = 0) %>% as.data.table()

# merge with factor info
BreedLoad = merge(factor_scores_melt.filled,
                  BreedLoad,
                  by = "dog", allow.cartesian = T) %>% as.data.table()

# dog info
BreedLoad_dogs = unique(BreedLoad$dog)

# dog as factor
BreedLoad$dog = as.factor(BreedLoad$dog)

# age brackets
# NOTE: THIS WILL ASSUME THAT DOGS WITHOUT AGE ARE BETWEEN 2 and 12 YEARS OLD
# <=2yr, 12>age>2, >=12 yr
BreedLoad[, age_bracket := "2yr_to_12yr"]
BreedLoad[age>=12, age_bracket := "12yr_min"]
BreedLoad[age<=2, age_bracket := "2yr_max"]

BreedLoad_FactorScore_LMER_CovMat_byFactor = list()
BreedLoad_FactorScore_LMER_CovMat_byBreed = list()
modelType = "LMER_CovMat-GRM"

fi = 1 # factor index for outer loop
bi = 1 # breed index for inner loop

for (fac in factors_use$factor){
  
  formula_allDogs = as.formula(paste("score.norm ~ (1 | dog) + (1 | age_bracket) +", paste(BreedLoad_breeds$breed, collapse = " + ")))
  
  # model with REML, all breeds:
  model_withBreed_REML = relmatLmer(data = BreedLoad[factor == fac],
                                    formula = formula_allDogs,
                                    relmat = list(dog = GRMCorr),
                                    na.action = "na.exclude",
                                    REML = T)
  model_summ = data.table(coef(summary(model_withBreed_REML)), keep.rownames = T)
  
  # model with ML, all breeds:
  model_withBreed_ML = relmatLmer(data = BreedLoad[factor == fac],
                                  formula = formula_allDogs,
                                  relmat = list(dog = GRMCorr),
                                  na.action = "na.exclude",
                                  REML = F)
  
  BreedLoad_FactorScore_LMER_CovMat_byFactor[[fi]] = compare_performance(model_withBreed_ML, model_withBreed_REML) %>% as.data.table()
  
  BreedLoad_FactorScore_LMER_CovMat_byFactor[[fi]]$factor = fac
  
  # perform ANOVA on model with REML to obtain F-stats for each breed:
  model_anova = as.data.table(anova(model_withBreed_REML), keep.rownames = T)
  
  # perform ANOVA of model with ML +/- breed to get likelihood ratio:
  for (B in BreedLoad_breeds$breed){
    print(B)
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]] = data.table(factor = fac,
                                                                 breed = B,
                                                                 N = BreedLoad_breeds[breed == B]$Nbreed,
                                                                 modelType = modelType,
                                                                 modelMinPctCutoff = min,
                                                                 modelMaxPctCutoff = max)
    
    model_withoutBreed_ML = relmatLmer(data = BreedLoad[factor == fac],
                                       formula = as.formula(paste("score.norm ~ (1 | dog) + (1 | age_bracket) +", paste(BreedLoad_breeds[breed!=B]$breed, collapse = " + "))),
                                       relmat = list(dog = GRMCorr),
                                       na.action = "na.exclude",
                                       REML = F)
    
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]]$REML.effect = model_summ[rn==B]$Estimate
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]]$REML.SD = model_summ[rn==B]$`Std. Error`
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]]$REML.t.val = model_summ[rn==B]$`t value`
    
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]]$REML.anova.sumSq = model_anova[rn==B]$`Sum Sq`
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]]$REML.anova.meanSq = model_anova[rn==B]$`Mean Sq`
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]]$REML.anova.F.val = model_anova[rn==B]$`F value`
    
    BreedLoad_anova = anova(model_withBreed_ML,model_withoutBreed_ML)
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]]$ML.anova.chisq = BreedLoad_anova$Chisq[2]
    BreedLoad_FactorScore_LMER_CovMat_byBreed[[bi]]$ML.anova.p = BreedLoad_anova$`Pr(>Chisq)`[2]
    
    bi = bi+1 
  }
  fi = fi+1
}

# collapse all breed-factor results:
BreedLoad_FactorScore_LMER_CovMat_byBreed = rbindlist(BreedLoad_FactorScore_LMER_CovMat_byBreed) %>% as.data.table()

# adjust p-values from model with ML +/- breed ANOVA:
BreedLoad_FactorScore_LMER_CovMat_byBreed[, ML.anova.p.adj_bonferroni := p.adjust(ML.anova.p, method = "bonferroni"), by = "factor"]
BreedLoad_FactorScore_LMER_CovMat_byBreed[, ML.anova.p.adj_benjhoch_FDR := p.adjust(ML.anova.p, method = "fdr"), by = "factor"]

BreedLoad_FactorScore_LMER_CovMat_byBreed$ancestryCutoff = M

# collapse all factor results:
BreedLoad_FactorScore_LMER_CovMat_byFactor = rbindlist(BreedLoad_FactorScore_LMER_CovMat_byFactor, fill=TRUE) %>% as.data.table()
BreedLoad_FactorScore_LMER_CovMat_byFactor$ancestryCutoff = M

write.table(x = BreedLoad_FactorScore_LMER_CovMat_byBreed,
            file = paste("BreedLoad_FactorScore_LMER_CovMat_byBreed_ancCutoff-",M,".csv",sep=""),
            sep = ",",
            row.names = F)

write.table(x = BreedLoad_FactorScore_LMER_CovMat_byFactor,
            file = paste("BreedLoad_FactorScore_LMER_CovMat_byFactor_ancCutoff-",M,".csv",sep=""),
            sep = ",",
            row.names = F)

save.image(file = paste("DD_FactorAnalysis_LMER_ancCutoff-",M,".Rdata", sep = ""))
