# Informations ------------------------------------------------------------

### title: Temporal relative abundance ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 09/11/2020
### Description: this script create the graph of plant life-form relative abundance by time

rm(list = ls())

# Load packages and open datatable -------------------------------------
if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("textclean")) install.packages("textclean", dependencies = TRUE)
if(!require("plotrix")) install.packages("plotrix", dependencies = TRUE)

data.biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.1_para_abundancia.csv")

# Calculate relative abundances -----------------------------------------


rel.abun <- data.biota %>% 
  dplyr::rename(life_form = `Life Form`) %>%
  textclean::drop_row("life_form", c("indeterminate", "fern")) %>%
  gather(key = "Month", value = "value", 5:23) %>% 
  dplyr::mutate(time = Month %>% stringr::str_replace("T", "")) %>% 
  dplyr::mutate(Month = as.factor(Month)) %>% 
  dplyr::mutate(Treatment = factor(Treatment)) %>% 
  dplyr::mutate(Treatment = fct_relevel(Treatment, "open", "closed")) %>% 
  dplyr::mutate(Treatment = recode(Treatment, "open" = "Open", "closed" = "Closed")) %>% 
  dplyr::mutate(life_form = fct_relevel(life_form, "tree", "palm","liana","shrub","herb","bamboo")) %>%
  dplyr::mutate(life_form = recode(life_form, "tree" = "Tree", "palm" = "Palm","liana" = "Liana","shrub" = "Shrub","herb" = "Herb","bamboo" = "Bamboo")) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, time, life_form) %>% 
  dplyr::summarise(abs.abun = sum(value)) %>% 
  dplyr::ungroup(Site, Plot, Treatment, Month, time, life_form) %>% 
  dplyr::group_by(Site,Plot,Treatment,Month,time) %>%
  dplyr::mutate(rel.abun = abs.abun / sum(abs.abun)) %>% 
  dplyr::ungroup(Site,Plot,Treatment,Month,time) #%>% 
# for error bar range
  #dplyr::group_by(Treatment, Month, time, life_form) %>% 
  #dplyr::select(-c(1:2,7)) %>% 
  #summarise_each(funs(mean (rel.abun), sd(rel.abun), se=sd(rel.abun)/sqrt(n()))) %>% 
  #mutate(rel.abun.mean_round = round(mean, 2)) %>% 
  #mutate(rel.abun_sd_round = round(sd, 2)) %>% 
  #mutate(rel.abun_se_round = round(se, 2))
rel.abun 



# graphic line with SE in bar range ---------------------------------------


ggplot(rel.abun, aes(x = time, y = rel.abun.mean_round, color = life_form, group = life_form)) + 
  geom_errorbar(aes(ymin=rel.abun.mean_round-rel.abun_se_round, 
                    ymax=rel.abun.mean_round+rel.abun_se_round), 
                width=1,
                position=position_dodge(0.1)) +
  geom_line(size = 1.5, position=position_dodge(0.1)) +
  geom_point(size = 1, stroke = 2.0, position=position_dodge(0.1)) +
  theme_bw() +
  facet_grid(Treatment~., space="free", switch="both") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(y = "Relative abundance", x = "Sampled period (months)", color = "Life-forms") +
  theme(strip.background = element_rect(color="grey50", fill="gray90"),
        axis.title = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.y = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position="top", legend.box = "horizontal", 
        plot.background=element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = unit(c(1,1,1,1), "lines")) + #top, right, botton, left
  scale_color_manual(values = c("springgreen4","yellowgreen",
                                "#fb81bf","tan1","sienna3","brown")) +
  scale_x_discrete(labels = c("00","","12","","24","","36","","48","","60","","72","","84","","96", "", "108"))



# graphic line with SE in smooth range ---------------------------------------


ggplot(rel.abun, aes(x = time, y = rel.abun, color = life_form, group = life_form)) + 
  stat_summary(fun.data = mean_se,
               geom = "ribbon",
               aes(group = life_form),
               color = "0.12",
               alpha = 0.2) +
  stat_summary(fun = mean,
               geom = "point",
               size = 3,
               stroke = 0.5,
               show.legend = TRUE) +
  stat_summary(fun = mean, 
               geom = "line",
               aes(group = life_form),
               size = 1.5) +
  theme_bw() +
  facet_grid(Treatment~., space="free", switch="both") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(y = "Relative abundance", x = "Sampled period (months)", color = "Life-forms") +
  theme(strip.background = element_rect(color="grey50", fill="gray90"),
        axis.title = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.y = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position="top", legend.box = "horizontal", 
        plot.background=element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = unit(c(1,1,1,1), "lines")) + #top, right, botton, left
  scale_color_manual(values = c("springgreen4","yellowgreen",
                                "#fb81bf","tan1","sienna3","brown")) +
  scale_x_discrete(labels = c("00","","12","","24","","36","","48","","60","","72","","84","","96", "", "108"))


ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/relative_abundance/relative_abun_lf_geom_line_facetwrap_24-11-20.jpeg", width = 15, height = 20, units = "cm", dpi = 300)

