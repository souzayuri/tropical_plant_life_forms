###
#title: Inverse Simpson eveness and glmm
#author: "Yuri Souza"
#data: "12/08/2020"
#content: Create a life-form species matriz to calculate inverse simpson diversity to apply and glmms
###

rm(list = ls())



# load packages and table ----------------------------------------------------------


library(tidyverse)
library(hillR)
library(hablar)
library(stringr)
library(lme4) 
library(car)
library(naniar)
library(ggpubr)
library(grid)
library(codyn)
library(rcompanion)
library(MASS)
library(lmtest)



data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.1_para_abundancia.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota)


# table with inverse simpsons calculated  -------------------------------------------------------------------


bt.invsim.mtz <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::select(-c(4, 24:26,28)) %>% 
  gather(key = "Month", value = "value", 4:22) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  mutate(site=Site, treatment = Treatment, plot = Plot, month = Month, time = Time, lifeform = life_form) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time, life_form) %>% rename(PlotID = Site) %>% 
  dplyr::group_by(PlotID, site, treatment, plot, month, time, Species) %>%
  dplyr::summarise(abundances = sum(value)) %>% 
  ungroup(PlotID, site, treatment, plot, month, time, Species) %>%
  spread(Species, abundances) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  dplyr::select(-c(2:6)) %>% 
  column_to_rownames(var="PlotID") %>% 
  hillR::hill_taxa(q = 2, MARGIN = 1) %>% 
  as.data.frame() %>% rownames_to_column(var="PlotID") %>% 
  separate(PlotID, c("Site", "Treatment", "Plot", "Month", "Time", "Life_form"), convert = TRUE) %>% 
  rename(simp.inv.div = ".") %>% 
  hablar::rationalize() %>% 
  spread(Life_form, simp.inv.div) %>%
  hablar::rationalize() %>% 
  mutate(Month = as.factor(Month)) %>%
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("open", "closed"))) %>% 
  mutate(site = Site,
         treatment = Treatment,
         plot = Plot,
         month = Month,
         time = Time) %>%
  unite(site, site, treatment, plot, month, time) %>% rename(PlotID = site) %>% 
  rename(bamboo.invs = bamboo,
         herb.invs = herb,
         liana.invs = liana,
         palm.invs = palm,
         shrub.invs = shrub,
         tree.invs = tree)
bt.invsim.mtz



# total abundance table  -------------------------------------------------------------------


bt.abu.mtz <- data_biota %>% 
  dplyr::select(-c(4, 24:26,28)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:22) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, Time, life_form) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>% 
  dplyr::mutate(site = Site, plot = Plot, month = Month,
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, month,
                time, treatment, abundances,life_form) %>% 
  spread(life_form, abundances) %>% 
  rename(bamboo.abn = bamboo,
         herb.abn = herb,
         liana.abn = liana,
         palm.abn = palm,
         shrub.abn = shrub,
         tree.abn = tree) %>% 
  dplyr::select(1,7:12)

bt.abu.mtz

# join inverse simpsons, abundances and richness table --------------------

bt.invsim.abu.mtz <-left_join(bt.invsim.mtz, bt.abu.mtz, by = "PlotID") %>% 
  dplyr::select(12, 1:11, 13:18) %>% 
  hablar::rationalize() %>%
  mutate_all(replace_na, 0) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x)) %>% 
  hablar::rationalize() %>%
  mutate_all(replace_na, 0)


bt.invsim.abu.mtz


# glmm life-form species evenness -----------------------------------------



# trees -------------------------------------------------------------------

plotNormalHistogram(bt.invsim.abu.mtz$tree.invs)

plotNormalHistogram(log(bt.invsim.abu.mtz$tree.invs+1))

# time
sp.lf.even.time.tree <- glmer(tree.invs~ Treatment*log(Time+1) + (1 |Site/Plot/Month), 
                          family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.time.tree)
car::Anova(sp.lf.even.time.tree, type = "III")


# como fica
sp.lf.even.tree <- glmer(tree.invs~ Treatment+log(Time+1) + (1 |Site/Plot/Month), 
                              family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.tree)

anova(sp.lf.even.time.tree, sp.lf.even.tree)



# palms -------------------------------------------------------------------


plotNormalHistogram(bt.invsim.abu.mtz$palm.invs)

plotNormalHistogram(log10(bt.invsim.abu.mtz$palm.invs+1))

# time
sp.lf.even.time.palm <- glmer(palm.invs~ Treatment*log(Time+1) + (1 |Site/Plot/Month), 
                              family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.time.palm)
car::Anova(sp.lf.even.time.palm, type = "III")


sp.lf.even.palm <- glmer(palm.invs~ Treatment+log(Time+1) + (1 |Site/Plot/Month), 
                              family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.palm)
car::Anova(sp.lf.even.palm, type = "III")

anova(sp.lf.even.time.palm,sp.lf.even.palm)

#sp.lf.even.time.palm.nb <- glmer.nb(palm.invs~ Treatment*log(Time+1) + (1 |Site/Plot/Month), 
#                               data = bt.invsim.abu.mtz)
#summary(sp.lf.even.time.palm.nb)
#car::Anova(sp.lf.even.time.palm.nb, type = "III")

#lrtest(sp.lf.even.time.palm, sp.lf.even.time.palm.nb)

# abundance
sp.lf.even.abun.palm <- glmer(palm.invs~ Treatment*palm.abn + (1 |Site/Plot/Month), 
                              family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.abun.palm)
car::Anova(sp.lf.even.abun.palm, type = "III")



#sp.lf.even.abun.palm.nb <- glmer.nb(palm.invs~ Treatment*palm.abn + (1 |Site/Plot/Month), 
#                                    data = bt.invsim.abu.mtz)
#summary(sp.lf.even.abun.palm.nb)
#car::Anova(sp.lf.even.time.palm.nb, type = "III")

#lrtest(sp.lf.even.abun.palm, sp.lf.even.abun.palm.nb)

anova(sp.lf.even.time.palm, sp.lf.even.abun.palm)


# lianas -------------------------------------------------------------------


plotNormalHistogram(bt.invsim.abu.rich.mtz$liana.invs)

plotNormalHistogram(log(bt.invsim.abu.rich.mtz$liana.invs+1))


sp.lf.even.time.liana.nb <- glmer.nb(liana.invs~ Treatment*log(Time+1) + (1 |Site/Plot/Month), 
                                    data = bt.invsim.abu.mtz)
summary(sp.lf.even.time.liana.nb)
car::Anova(sp.lf.even.time.liana.nb, type = "III")


sp.lf.even.liana.nb <- glmer.nb(liana.invs~ Treatment+log(Time+1) + (1 |Site/Plot/Month), 
                                     data = bt.invsim.abu.mtz)

anova(sp.lf.even.time.liana.nb,sp.lf.even.liana.nb)



# shrubs -------------------------------------------------------------------


plotNormalHistogram(bt.invsim.abu.rich.mtz$shrub.invs)

plotNormalHistogram(log(bt.invsim.abu.rich.mtz$shrub.invs+1))

# time
sp.lf.even.time.shrub <- glmer(shrub.invs~ Treatment*log(Time+1) + (1 |Site/Plot/Month), 
                               family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.time.shrub)
car::Anova(sp.lf.even.time.shrub, type = "III")

sp.lf.even.shrub <- glmer(shrub.invs~ Treatment+log(Time+1) + (1 |Site/Plot/Month), 
                               family = gaussian, data = bt.invsim.abu.mtz)

anova(sp.lf.even.time.shrub,sp.lf.even.shrub)


# herbs -------------------------------------------------------------------


plotNormalHistogram(bt.invsim.abu.rich.mtz$herb.invs)

plotNormalHistogram(log(bt.invsim.abu.rich.mtz$herb.invs+1))

# time
sp.lf.even.time.herb <- glmer(herb.invs~ Treatment*log(Time+1) + (1 |Site/Plot/Month), 
                               family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.time.herb)
car::Anova(sp.lf.even.time.herb, type = "III")



sp.lf.even.herb <- glmer(herb.invs ~ Treatment+log(Time+1) + (1 |Site/Plot/Month), 
                              family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.herb)
car::Anova(sp.lf.even.herb, type = "III")


anova(sp.lf.even.time.herb,sp.lf.even.herb)





# bamboos -------------------------------------------------------------------


plotNormalHistogram(bt.invsim.abu.rich.mtz$bamboo.invs)

plotNormalHistogram(log(bt.invsim.abu.rich.mtz$bamboo.invs+1))

# time
sp.lf.even.time.bamboo <- glmer(bamboo.invs~ Treatment*log(Time+1) + (1 |Site/Plot/Month), 
                              family = gaussian, data = bt.invsim.abu.mtz)
summary(sp.lf.even.time.bamboo)
car::Anova(sp.lf.even.time.bamboo, type = "III")

sp.lf.even.bamboo <- glmer(bamboo.invs~ Treatment+log(Time+1) + (1 |Site/Plot/Month), 
                                family = gaussian, data = bt.invsim.abu.mtz)

anova(sp.lf.even.time.bamboo, sp.lf.even.bamboo)
