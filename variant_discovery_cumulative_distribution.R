# This code generates the variant-discovery cumulative distributions and 95% CI shown in Figure 3B of Morrill et al., Ancestry-inclusive dog genomics challenges popular breed stereotypes

# Step 1. Use PLINK to generate tped file from .bed/.bim/.fam files for all 557 WGS dogs. Family IDs refer to breed for samples in the Broad-UMass Canid Variants VCF and can be used to partition purebred and mutt cohorts. Dimensions of  .tped file: columns = row name + 2* number of variants; row= # of dogs. 

#Step 2. In R, use the .ped file to identify, for each dog, genomic position that contain at least one instance of variant. Each position in each dog is coded as either "no" (both alleles are reference) or "var" (at least one of the two alleles at this position is the variant). 

for(p in 1:dim(simple)[1])
{
  listforthem<-c()
  for(i in 1:length(firstpos))
  {
    a<-simple[p,firstpos[i]]
    b<-simple[p,firstpos[i]+1]
    if(a==1||b==1){set<-"var"} else {set<-"no"}
    listforthem<-c(listforthem, set)
  }
  receive.geno[p,]<-listforthem
}

write.table(receive.geno, "chr13.trial.genos")

#Step 3. In R, specify which individual dogs that meet criteria for inclusion in some cohort. Here, for example, include one randomly chosen representative for each of 128 breeds. 

pure.spots<-c(seq(1:382), seq(409:557))

pure.locations<-pure.spots

#Step 4. In  R, read in the file, generated in Step 2, above, containing information on which sites contain variants in a given dog

variants<-read.table("chr13.trial.genos",header=F)


#Step 5. In R, extract genotypes for dogs cohort of interest. 

pure.var.only<-variants[,pure.locations]
df.pure.var<-data.frame(pure.var.only)

#Step 6. In R, specify function to identify the first dog (in a ordered list) that has at least one copy of variant at a given site.

first.occur<-function(row) {which(row=="var")[1]}

#Step 7. In R, specify number of random reorderings  ("reps") and, for each, identify cumulative number of unique variants discovered through incremental addition of dogs to the data set. The code here implements cumulative tracking of variants discovered through sequencing of the first 10 dogs randomly selected and ordered from a given cohort.

d<-list()

for(z in 1:reps)
{
  new.take<-df.pure.var[ , sample(1:ncol(df.pure.var), 10)]
  pure.var.only<-new.take
  list.of.pure<-c()
  for(i in 1:dim(pure.var.only)[1])
  {
    list.of.pure<-c(list.of.pure,first.occur(pure.var.only[i,]))                                                                                     }
  a<-seq(1:10)
  d[[z]]<-sapply(a, function(x) sum(list.of.pure %in% x))
}
i

write.table(d, "/seq/vgb/diane/genereux/pure.tables.any.indiv.2")

# Output table gives # of variants for which each dog in a given ordering of list enables discovery of a new variant. It has the following dimensions: width=# random draws/reorderings of individual dogs to be included. length=# of dogs used to compute cumulative distribution of variants discovered. 