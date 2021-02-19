
# among life-forms --------------------------------------------------------


# Informations ------------------------------------------------------------

### title: Temporal evenness diversity graphic with bootstrap  ###
### author: Yuri (desouza.s.yuri@gmail.com)
### data: 09/11/2020
### Description: this script plot 2 graphics with time variation and inverse simpson diversity

rm(list = ls())


# packages -----------------------------------------------------------------


library(tidyverse)
library(textclean)
library(vegan)
library(ggpubr)
library(gridExtra)
library(gtable)
library(grid)
library(codyn)
library(ggthemes)

# load table ------------------------------------------------

data_biota_diver <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/diversity/bt_lf_invsim_abund_rich_15-05-20.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota_diver)


# manipulation table and graphic one ------------------------------------------------------


bt.lf.invs <- data_biota_diver %>% 
  dplyr::select(c(1:6,19:24)) %>% 
  gather(key = "Life_form", value = "inv_simp", 7:12) %>%
  mutate(Life_form = recode(Life_form, 
                            "bamboo.invs" = "Bamboo","herb.invs" = "Herb",
                            "shrub.invs" = "Shrub","liana.invs" = "Liana",
                            "tree.invs" = "Tree", "palm.invs" = "Palm")) %>% 
  arrange(match(Life_form, c("Tree", "Palm","Liana",
                             "Shrub","Herb","Bamboo"))) %>% 
  mutate(Time = recode(Time, 
                       "0" = 2010, "6" = 2011,
                       "12" = 2011, "18" = 2012, 
                       "24" = 2012, "30" = 2013,
                       "36" = 2013, "42" = 2014,
                       "48" = 2014, "54" = 2015,
                       "60" = 2015, "66" = 2016,
                       "72" = 2016, "78" = 2017,
                       "84" = 2017, "90" = 2018,
                       "96" = 2018, "102" = 2019,
                       "108" = 2019)) %>% 
  mutate(Life_form = factor(Life_form, level = c("Tree", "Palm","Liana",
                                                 "Shrub","Herb","Bamboo"))) %>% 
  mutate(Treatment = factor(Treatment, level = c("open", "closed"))) %>% 
  mutate(Treatment = recode(Treatment, "open" = "Open","closed" = "Closed")) %>% 
  mutate(treatment = Treatment,
         life_form = Life_form,
         time = Time,
         month = Month) %>% 
  unite(treatment, treatment, month, time, life_form) %>% rename(ID = treatment) %>% 
  select(!9)




bt.lf.invs


# graph -------------------------------------------------------------------



among <- ggplot(bt.lf.invs, aes(as.factor(Month), inv_simp, color = Life_form, group = Life_form)) +
  facet_grid(Treatment~., space="free", switch="both") +
  scale_color_manual(values = c("springgreen4","yellowgreen",
                                "#fb81bf","tan1","sienna3","brown"), 
                     name = "Life-forms") + 
  #scale_shape_discrete(name = "Life-forms") +
  theme_bw() + 
  stat_summary(fun.data = mean_se,
               geom = "ribbon",
               aes(group = Life_form),
               color = "0.12",
               alpha = 0.2) +
  stat_summary(fun = mean,
               geom = "point",
               size = 3,
               stroke = 0.5,
               show.legend = TRUE) +
  stat_summary(fun = mean, 
               geom = "line",
               aes(group = Life_form),
               size = 1.5) +
  labs(x = "Sampled period (months)", y = "Inverse Simpson index") +
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        #legend.position = c(0.15, .85),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        strip.text.y = element_text(size=12),
        legend.position="top", legend.box = "horizontal") +
  #annotate("text", label = 'atop(bold("B"))', parse= TRUE, size = 7, x = 1, y = 3.34) +
  #annotate("text", label = "2009", size = 2.5, x = 1, y = 2.29) +
  #annotate("text", label = "2010", size = 2.5, x = 3, y = 2.399) +
  #annotate("text", label = "2011", size = 2.5, x = 5, y = 2.4) +
  #annotate("text", label = "2012", size = 2.5, x = 7, y = 2.47) +
  #annotate("text", label = "2013", size = 2.5, x = 9, y = 2.435) +
  #annotate("text", label = "2014", size = 2.5, x = 11, y = 2.389) +
  #annotate("text", label = "2015", size = 2.5, x = 13, y = 2.477) +
  #annotate("text", label = "2016", size = 2.5, x = 15, y = 2.53) +
  #annotate("text", label = "2017", size = 2.5, x = 17, y = 2.49) +
  #annotate("text", label = 'atop(bold("A"))', parse= TRUE, size = 7, x = 1, y = 20.7) +
  #expand_limits(y=2.5) +
scale_x_discrete(labels = c("00","","12","","24","","36","","48","","60","","72","","84","","96", "", "108"))

among
ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/evenness/biota_lf_inv_simpsons_index_diversity_bootstrap_time_24-11-20.jpeg", height = 15, width = 20, units = "cm", dpi = 500)



# within life-forms -------------------------------------------------------


###
#title: Inverse Simpson eveness
#author: "Yuri Souza"
#data: "09/11/2020"
#content: Create a life-form species matriz to calculate inverse simpson diversity to apply to pielous eveness
###


# load table ----------------------------------------------------------

data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.1_para_abundancia.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota)


# table with inverse simpsons calculated  -------------------------------------------------------------------


bt.invsim.mtz <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::select(-c(4, 24:26,28)) %>% 
  gather(key = "Month", value = "value", 4:22) %>% 
  dplyr::mutate(Time = Month %>% stringr::str_replace("T", "")) %>% 
  mutate(site=Site, treatment = Treatment, plot = Plot, month = Month, time = Time, lifeform = life_form) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::group_by(PlotID, site, treatment, plot, month, time, lifeform) %>%
  summarise(abundances = sum(value)) %>% 
  ungroup(PlotID, site, treatment, plot, month, time, lifeform) %>%
  spread(lifeform, abundances) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  dplyr::select(-c(2:6)) %>% 
  column_to_rownames(var="PlotID") %>% 
  hillR::hill_taxa(q = 2, MARGIN = 1) %>% 
  as.data.frame() %>% rownames_to_column(var="PlotID") %>% 
  separate(PlotID, c("Site", "Treatment", "Plot", "Month", "Time"), convert = FALSE) %>% 
  rename(simp.inv.div = ".") %>% 
  hablar::rationalize() %>%
  #dplyr::select(c(1:6)) %>% 
  group_by(Site, Treatment, Plot, Time, Month) %>% 
  dplyr::summarize(Mean = mean(simp.inv.div)) %>% 
  ungroup(Site, Treatment, Plot, Time, Month) %>% 
  mutate(treatment = Treatment,
         time = Time) %>% 
  unite(treatment,treatment, time) %>% rename(PlotID = treatment) %>%
  dplyr::select(c(7,1:6)) %>% 
  mutate(Treatment = factor(Treatment, level = c("open", "closed")))


bt.invsim.mtz

# graphic -----------------------------------------------------------------



within <- ggplot(bt.invsim.mtz, aes(Time, Mean, color = Treatment, fill = Treatment)) +
  scale_color_manual(values = c("#556B2F","#663399"), name = "Treatment", labels = c("Open","Closed")) + 
  scale_fill_manual(values = c("#556B2F","#663399"), name = "Treatment", labels = c("Open","Closed")) + 
  
  theme_classic() +
  stat_summary(fun.data = mean_se,
               geom = "ribbon",
               aes(group = Treatment, fill = Treatment),
               color = "0.12",
               alpha = 0.12) +
  stat_summary(fun = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Inverse Simpson index") +
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = c(0.15, .9),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14)) +
  #annotate("text", label = 'atop(bold("A"))', parse= TRUE, size = 7, x = 1, y = 3.34) +
  #annotate("text", label = "2009", size = 2.5, x = 1, y = 2.29) +
  #annotate("text", label = "2010", size = 2.5, x = 3, y = 2.399) +
  #annotate("text", label = "2011", size = 2.5, x = 5, y = 2.4) +
  #annotate("text", label = "2012", size = 2.5, x = 7, y = 2.47) +
  #annotate("text", label = "2013", size = 2.5, x = 9, y = 2.435) +
  #annotate("text", label = "2014", size = 2.5, x = 11, y = 2.389) +
  #annotate("text", label = "2015", size = 2.5, x = 13, y = 2.477) +
  #annotate("text", label = "2016", size = 2.5, x = 15, y = 2.53) +
  #annotate("text", label = "2017", size = 2.5, x = 17, y = 2.49) +
  #expand_limits(y=2.5) +
  scale_x_discrete(labels = c("00","","12","","24","","36","","48","","60","","72","","84","","96", "", "108"))

within

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/evenness/life_forms_time_evenness_24-11-20.jpeg", width = 25, height = 15, units = "cm", dpi = 300)



# ggarrange ---------------------------------------------------------------
ggarrange(within, among, ncol = 2, labels = c("A", "B"), nrow = 1, font.label = list(size = 18))
ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/evenness/life_forms_time_evenness_among_and_within_24-11-20.png", w = 30, h = 15, units = "cm", dpi = 300)



