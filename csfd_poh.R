rm(list = ls())

knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)
library(readr)
library(readxl)

########################## Načtení dat a nastavení ############################
poh <- read_excel("csfd_data.xlsx", sheet = "ftr_sample")

#Colorblind-friendly palette
safe_colorblind_palette2 <- c("#008607","#68023F","#008169","#FFCFE2","#FF71FD","#EF0096","#00DCB5","#003C86","#9400E6","#F60239","#009FFA","#FFDC3D","#7CFFFA","#00E307","#6A0213")
#draw colors
scales::show_col(safe_colorblind_palette2)

########################## Data transformation ###################
poh$poradi <- as.numeric(poh$poradi)

poh_set <- poh %>%
  group_by(rok) %>%
  mutate(rank = rank(poradi),
         hodnoceni_adj = hodnoceni,
         poh_lbl = paste0(" ",hodnoceni*100)) %>%
  group_by(nazev) %>% 
  filter(rank <= 10) %>%
  ungroup()
poh_set$poradi <- as.numeric(poh_set$poradi)

############################### Static plot #############################
static_plot <- ggplot(poh_set, aes(rank, group = nazev))  +
  #rectangles
  geom_tile(aes(y = hodnoceni_adj/2,
                height = hodnoceni_adj, fill = nazev,
                width = 0.9), alpha = 0.8, color = NA) +
  #coloring rectangles
  scale_fill_manual(values = safe_colorblind_palette2) +
  #film names labels
  geom_text(aes(y = 0.008, label = paste(nazev, " ")), vjust = 0.2, hjust = 0, size = 8, fontface = "bold", 
            color = ifelse(poh_set$nazev %in% c("Sněhurka a sedm trpaslíků (1937)", "Coco (2017)", "Fimfárum Jana Wericha (2002)", "Shrek (2001)", "Klaus (2019)"), "black", "white")) +
  #% values on the rights side
  geom_text(aes(y=hodnoceni_adj,label = poh_lbl, hjust=0), size = 8) +
  #year label
  geom_text(aes(x=11.5, y=max(hodnoceni_adj) , label = as.factor(rok)), hjust = 1, alpha = 0.5, fontface = "italic",  col = "black", size = 20) +
  #transposing vertical chart to horizontal
  coord_flip(clip = "off", expand = FALSE) +
  
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  #setting themes
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(), #element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_blank(), #element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=1, face="italic", color="grey"),
        plot.caption =element_text(size=14, hjust=0.995, vjust = -7, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,7, 2, 4, "cm"),
        legend.position="none")

#setting the animation
animated <- static_plot + transition_states(rok,
                                            transition_length = 4, state_length = 1, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  ease_aes('linear')+
  enter_fade()+
  exit_fade() +
  labs(title = "Nejlepší pohádky v top 300 podle ČSFD",  
       subtitle  =  "Hodnocení v %",
       caption  = "Data Source: ČSFD")
  
#animation + render gif
animate(animated, 150, start_pause = 7, end_pause = 7, width = 1080, height = 1350, 
        
        renderer = gifski_renderer("anim_pohb.gif"))


#the most useful links:
#https://stackoverflow.com/questions/54926358/animated-sorted-bar-chart-problem-with-overlapping-bars
#https://rpubs.com/haginta/709479
#http://mkweb.bcgsc.ca/colorblind/palettes.mhtml