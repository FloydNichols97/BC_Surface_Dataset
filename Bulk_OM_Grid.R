rm(list=ls())

library(tidyverse)
library(readr)
library(gridExtra)

All_Lake_Data <- read_csv("C:/Users/14128/Desktop/Osburn Lab/Analysis/All_Lake_Data.csv")

Surface <- All_Lake_Data %>% 
  filter(Category == "Surface") %>% 
  filter(Lake != "Salt Lake Spring") %>%  
  filter(Type == "Brine" | Type == "Sediment" | Type == "Mat" | Type == "Salt") %>% 
  mutate(TLE_TOC = TLE/TOC)

# Manual Color Scheme #
Colors <- c("Basque Lake 1" = "orange2", "Basque Lake 2" = "red1", "Basque Lake 4" = "red4", "Last Chance Lake" = "yellow", "Salt Lake" = "blue4")

# TOC #
TOC_boxplot <- ggplot(Surface, aes(x = Lake, y = TOC)) + 
  geom_boxplot(aes(fill = Lake)) + 
  geom_jitter(aes(shape = Type)) + 
  scale_fill_manual(values = Colors) + 
  guides(shape=guide_legend(ncol=2)) + 
  guides(fill=guide_legend(ncol=2)) +
  theme_bw() +
  theme(legend.position = c(0.68,0.67),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        legend.key.height= unit(0.1, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.text = element_text(size=8)) + 
  labs(y = "TOC (%)")
plot(TOC_boxplot)

# d13C #
d13C_boxplot <- ggplot(Surface, aes(x = Lake, y = `d13C (VPDB)`)) + 
  geom_boxplot(aes(fill = Lake)) + 
  geom_jitter(aes(shape = Type)) +
  scale_fill_manual(values = Colors) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  labs(y = expression(delta~{}^13*"C (\u2030)"))
plot(d13C_boxplot)

# TN #
TN_boxplot <- ggplot(Surface, aes(x = Lake, y = TON)) + 
  geom_boxplot(aes(fill = Lake)) + 
  geom_jitter(aes(shape = Type)) +
  scale_fill_manual(values = Colors) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  labs(y = "TN (%)")
plot(TN_boxplot)

# d15N #
d15N_boxplot <- ggplot(Surface, aes(x = Lake, y = d15N)) + 
  geom_boxplot(aes(fill = Lake)) + 
  geom_jitter(aes(shape = Type)) +
  scale_fill_manual(values = Colors) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  labs(y = expression(delta~{}^15*"N (\u2030)"))
plot(d15N_boxplot)

# TLE #
TLE_boxplot <- ggplot(Surface, aes(x = Lake, y = TLE)) + 
  geom_boxplot(aes(fill = Lake)) + 
  geom_jitter(aes(shape = Type)) +
  scale_fill_manual(values = Colors) + 
  theme_bw() + 
  theme(legend.position="none",
        #axis.text.x = element_text(angle = 45, vjust = 0.5)
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  labs(y = "TLE (mg/g dry wt)")
plot(TLE_boxplot)

# TLE/TOC #
TLE_TOC_boxplot <- ggplot(Surface, aes(x = Lake, y = TLE_TOC)) + 
  geom_boxplot(aes(fill = Lake)) + 
  geom_jitter(aes(shape = Type)) +
  scale_fill_manual(values = Colors) + 
  theme_bw() + 
  theme(legend.position="none",
        #axis.text.x = element_text(angle = 45, vjust = 0.5)
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  labs(y = "TLE/TOC")
plot(TLE_TOC_boxplot)

# Grid Figure #
grid.arrange(TOC_boxplot, d13C_boxplot, TN_boxplot, d15N_boxplot, TLE_boxplot, TLE_TOC_boxplot, ncol = 2, nrow = 3)

# Statistics #
library(purrr)
summary_OM <- Surface %>% split(.$Lake) %>% map(summary)
print(summary_OM)
