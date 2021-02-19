
###
#title: Life form abundance Glmm
#author: "Yuri Souza"
#data: "24/04/2020"
#content: Create a life form abundance table and use it in glmm models. This script also test the best normality for the data and best glmm option for it, suggested and commented by Nachoman.
###

rm(list = ls())



# load packages and table ----------------------------------------------------------


library(tidyverse)
library(rcompanion)
library(stringr)
library(MASS)
library(lmtest)
library(lme4) 
library(car)


#data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.1.csv")
glimpse(data_biota)



# life form abundance table
bt.lf.abn <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::select(-c(4,24:26,28)) %>% 
  gather(key = "Month", value = "value", 4:22) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::group_by(Site, Treatment, Plot, Month, Time, life_form) %>%
  summarise(Abundances = sum(value)) %>% 
  ungroup(Site, Treatment, Plot, Month, Time,life_form) %>%
  spread(life_form, Abundances) %>% 
  replace(is.na(.), 0) %>% 
  rename(bamboo.abn = bamboo,
         herb.abn = herb,
         liana.abn = liana,
         palm.abn = palm,
         shrub.abn = shrub,
         tree.abn = tree) %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("open", "closed")))
bt.lf.abn


# GLMM abundance ----------------------------------------------------------



# trees -------------------------------------------------------------------

#trees clearly follow a Poisson distribution
plotNormalHistogram(bt.lf.abn$tree.abn)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(bt.lf.abn$tree.abn+1))

# modelo com interação para a anova
bt.div.sim.tree.glmm.poisson02<- glmer(tree.abn~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(bt.div.sim.tree.glmm.poisson02)
car::Anova(bt.div.sim.tree.glmm.poisson02, type = "III")

# modelo sem interação para a anova
mo1lf.tree <- glmer(tree.abn~ Treatment+log(Time+1) + (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.tree)

# anova para testar a significancia da interação do modelo
anova(bt.div.sim.tree.glmm.poisson02,mo1lf.tree)

# palms -------------------------------------------------------------------


#palms seems slightly zero unfalted but when we log it it behaves very well (see below)
plotNormalHistogram(bt.lf.abn$palm.abn)
#when logged the distribution is almost perfectlly gaussian
plotNormalHistogram(log(bt.lf.abn$palm.abn+1))
#so we can run the glmer model with poisson distribution with no problem 

# modelo com interação para a anova
bt.div.sim.palm.glmm.poisson02<- glmer(palm.abn~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(bt.div.sim.palm.glmm.poisson02)
car::Anova(bt.div.sim.palm.glmm.poisson02, type = "III")

# modelo sem interação para a anova
mo1lf.palm<- glmer(palm.abn~ Treatment+log(Time+1) + (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.palm)

# anova para testar a significancia da interação do modelo
anova(bt.div.sim.palm.glmm.poisson02,mo1lf.palm)


# lianas ------------------------------------------------------------------


#liana seems slightly ZERO INFLATED
plotNormalHistogram(bt.lf.abn$liana.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.lf.abn$liana.abn+1))

# modelo com interação para a anova
nbglmer.liana <- glmer.nb(liana.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(nbglmer.liana)
car::Anova(nbglmer.liana, type = "III")

# modelo sem interação para a anova
mo1lf.liana <- glmer.nb(liana.abn~ Treatment+log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(mo1lf.liana)
# anova para testar a significancia da interação do modelo
anova(nbglmer.liana,mo1lf.liana)



# shrubs ------------------------------------------------------------------


#shrubs seems slightly ZERO INFLATED
plotNormalHistogram(bt.lf.abn$shrub.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.lf.abn$shrub.abn+1))


# modelo com interação para a anova

nbglmer.shrub<- glmer.nb(shrub.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(nbglmer.shrub)
car::Anova(nbglmer.shrub, type = "III")

# modelo sem interação para a anova
mo1lf.shrub<- glmer.nb(shrub.abn~ Treatment+log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(mo1lf.shrub)

# anova para testar a significancia da interação do modelo
anova(nbglmer.shrub,mo1lf.shrub)


# herbs ------------------------------------------------------------------

#herbs seems slightly ZERO INFLATED
plotNormalHistogram(bt.lf.abn$herb.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.lf.abn$herb.abn+1))


# modelo com interação para a anova
nbglmer.herb<- glmer.nb(herb.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(nbglmer.herb)
car::Anova(nbglmer.herb, type = "III")


# modelo sem interação para a anova
mo1lf.herb<- glmer.nb(herb.abn~ Treatment+log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(mo1lf.herb)

# anova para testar a significancia da interação do modelo
anova(nbglmer.herb,mo1lf.herb)




# bamboos -----------------------------------------------------------------


#bamboos seems slightly ZERO INFLATED
plotNormalHistogram(bt.lf.abn$bamboo.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.lf.abn$bamboo.abn+1))


# modelo com interação para a anova

nbglmer.bamboo<- glmer.nb(bamboo.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(nbglmer.bamboo)
car::Anova(nbglmer.bamboo, type = "III")


# modelo sem interação para a anova
mo1lf.bamboo<- glmer.nb(bamboo.abn~ Treatment+log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(mo1lf.bamboo)

# anova para testar a significancia da interação do modelo
anova(nbglmer.bamboo,mo1lf.bamboo)

