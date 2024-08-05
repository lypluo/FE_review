#################################################################
##Aim: data tidy the NSC and phenology data from Zahnd et al., 2024
#################################################################
# data from Zahnd et al., 2024:
#https://academic.oup.com/treephys/advance-article/doi/10.1093/treephys/tpae005/7515139
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

#--------------------
#(1) load the data
#--------------------
data.path<-"./data-raw/"

#a. read NSC data:
#--species_id (Aa = Abies alba; Pa = Picea abies; Ps = Pinus sylvestris;
#Fs = Fagus sylvatica; Qs = Quercus sp. (robur x petreae); Ap = Acer pseudoplatanus;
#Fe = Fraxinus excelsior; Cb = Carpinus betulus; St = Sorbus torminalis)

#crown_pos: t(top of crown),b(bottom of crown)
#tissue: X="wood"(xylem for branches), "N1"= 1-year-old needles
df.NSC<-read_xlsx(paste0(data.path,"NSC.xlsx"))
df.NSC<-df.NSC %>%
  mutate(DOY=yday(date))

#b. read phenology data
#spring phenolgy:
#----------------------
#Broadleaved species (Fs, Qs, Fe, Ap, Cb, St):
#----------------------
#pheno1 = bud swelling (buds swollen but fully closed);
#pheno2 = budbreak (buds opening, green leaf tips visible);
#pheno3 = leaf unfolding (buds fully open, leaves still partially folded);
#pheno4 = leaves fully unfolded

#-------------------------
#Evergreen species(Pa, Aa, Ps)
#-------------------------
# Picea and Abies (Pa, Aa): pheno1 = bud swelling (buds swollen but fully closed);
#pheno2 = budbreak (buds opening, needles still in tight bundle);
#pheno3 = needles fully open (needles spread out on the new shoot); pheno4 does not apply (NA)

# Pinus (Ps): pheno1 = bud swelling (buds swollen but fully closed);
#pheno2 = budbreak of the longshoot;
#pheno3 = budbreak of the short shoots carrying the young needles; pheno4 does not apply (NA)

df.pheno_spring<-read_xlsx(paste0(data.path,"spring_phenology.xlsx"))
df.pheno_spring<-df.pheno_spring %>%
  rename_with(.cols = c(pheno1:pheno4),~paste0("spring_",.x,recycle0 = TRUE))

#autumn phenolgy:
#for deciduous forests:
#The exact phenological stages were:
#pheno1 = percentage of largely discolored leaves (paleing or yellowing, depending on species);
#pheno2 = percentage of shed leaves.

df.pheno_autumn<-read_xlsx(paste0(data.path,"autumn_phenology.xlsx"))
df.pheno_autumn<-df.pheno_autumn %>%
  rename_with(.cols = c(pheno1:pheno2),~paste0("autumn_",.x,recycle0 = TRUE))

#-------------------
#(2) merge the phenology data
#-------------------
df.pheno<-full_join(df.pheno_spring,df.pheno_autumn)
#average the phenological data in differnt years for each species:
df.pheno_species_sum<-df.pheno %>%
  group_by(species_id,crown_pos,DOY)%>%
  # summarise(spring_pheno1=mean(spring_pheno1,na.rm = T))
  summarize(across(spring_pheno1:autumn_pheno2, ~ mean(.x,na.rm=TRUE)))
#further summarize the pheno-->find out the minimum and maximum pheno:
find_doy_range<-function(df,pheno_name){
  # df<-df.pheno_species_sum
  # pheno_name<-"spring_pheno1"

  df_subset<-df %>%
    select(species_id:DOY,pheno_name)
  names(df_subset)<-c("species_id","crown_pos","DOY","pheno")
  df_start<-df_subset %>%
    filter(pheno>0 & pheno<100) %>%
    group_by(species_id,crown_pos)%>%
    summarise(pheno_start=min(DOY))
  df_end<-df_subset %>%
    filter(pheno==100) %>%
    group_by(species_id,crown_pos)%>%
    summarise(pheno_end=min(DOY))
  #
  df_pheno<-left_join(df_start,df_end)
  #names:
  names(df_pheno)<-c("species_id","crown_pos",paste0(pheno_name,c("_start","_end")))
  #
  return(df_pheno)
}
###
df.spring_pheno1<-find_doy_range(df.pheno_species_sum,"spring_pheno1")
df.spring_pheno2<-find_doy_range(df.pheno_species_sum,"spring_pheno2")
df.spring_pheno3<-find_doy_range(df.pheno_species_sum,"spring_pheno3")
df.spring_pheno4<-find_doy_range(df.pheno_species_sum,"spring_pheno4")
df.autumn_pheno1<-find_doy_range(df.pheno_species_sum,"autumn_pheno1")
df.autumn_pheno2<-find_doy_range(df.pheno_species_sum,"autumn_pheno2")
##merge the data:
df_pheno1<-left_join(left_join(df.spring_pheno1,df.spring_pheno2),df.spring_pheno3)
df_pheno<-left_join(left_join(left_join(df_pheno1,df.spring_pheno4),df.autumn_pheno1),df.autumn_pheno2)

#-------------------
#summarize the phenology data
#-------------------
#differentiate the deciduous and evergreen
df_pheno<-df_pheno %>%
  mutate(leaf_type=ifelse(species_id %in% c("Fs","Qs","Fe","Ap","Cb","St"), "deciduous","evergreen"))

#mean of leaftype
df_pheno_leaftype_mean<-df_pheno %>%
  group_by(leaf_type,crown_pos)%>%
  summarise(across(spring_pheno1_start:autumn_pheno2_end,~round(mean(.x,na.rm = T),0)))
#sd of leaftype
df_pheno_leaftype_sd<-df_pheno %>%
  group_by(leaf_type,crown_pos)%>%
  summarise(across(spring_pheno1_start:autumn_pheno2_end,~round(sd(.x,na.rm = T),0)))
df_pheno_leaftype<-list(df_pheno_leaftype_mean,df_pheno_leaftype_sd)
names(df_pheno_leaftype)<-c("mean","sd")

#-------------------
#(3) summary the NSC data
#-------------------
df_NSC<-df.NSC %>%
  group_by(species_id,crown_pos,tissue,DOY) %>%
  summarise(sugar=mean(sugar,na.rm=T),
            starch=mean(starch,na.rm=T),
            nsc=mean(nsc,na.rm = T))

#-------------------
#summarize the NSC for different leaf type
#-------------------
df_NSC<-df_NSC %>%
  mutate(leaf_type=ifelse(species_id %in% c("Fs","Qs","Fe","Ap","Cb","St"), "deciduous","evergreen"))
#mean of leaftype
df_NSC_leaftype_mean<-df_NSC %>%
  group_by(leaf_type,crown_pos,tissue,DOY)%>%
  summarise(across(sugar:nsc,~round(mean(.x,na.rm = T),2)))
#sd of leaftype
df_NSC_leaftype_sd<-df_NSC %>%
  group_by(leaf_type,crown_pos,tissue,DOY)%>%
  summarise(across(sugar:nsc,~round(sd(.x,na.rm = T),2)))
df_NSC_leaftype<-list(df_NSC_leaftype_mean,df_NSC_leaftype_sd)
names(df_NSC_leaftype)<-c("mean","sd")

#-------------------
#(4) save the data
#-------------------
save.path<-"./data/"
#for phenology
save(df.pheno,file=paste0(save.path,"pheno.allspecies.RDA"))
save(df_pheno_leaftype,file=paste0(save.path,"pheno.leaftypes.RDA"))
#for NSC
save(df_NSC,file=paste0(save.path,"NSC.allspecies.RDA"))
save(df_NSC_leaftype,file=paste0(save.path,"NSC.leaftypes.RDA"))

