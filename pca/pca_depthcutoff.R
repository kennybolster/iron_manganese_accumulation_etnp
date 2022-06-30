library(tidyverse)
library(factoextra)
library(patchwork)
etsp <- read_csv('etsp_full.csv')
# reading in the spreadsheets with gtnum, station, and depth
etnp <- read_csv('rbnutrients.csv')
depthcutoff <- 1000
# max depth for cutoff in m
etsp <- etsp %>% rename('Oxygen' = oxy, 'Nitrite' = no2, 'Phosphate' = po4,
                        'Nitrate' = no3, 'Iron(II)' = fe2, 'Manganese' = mn,
                        'Iron' = dfe) %>% 
  filter(depth <= depthcutoff) %>% 
  select(-gtnum, -station, -depth) %>% 
  drop_na()
etnp <- etnp %>% filter(nominal.depth <= depthcutoff) %>% 
  select('Oxygen' = oxygen_uM, 'Nitrite' = no2,
                        'Phosphate' = po4, 'Nitrate' = no3, 'Iron(II)' = fe2,
                        'Manganese' = mn, 'Iron' = fe) %>% drop_na()
np_pca <- prcomp(etnp, scale. = TRUE)
sp_pca <- prcomp(etsp, scale. = TRUE)
np <- fviz_pca_biplot(np_pca, label = 'var', col.var = 'black', repel = TRUE, 
                      col.ind = 'darkgrey', title = '')
sp <- fviz_pca_biplot(sp_pca, label = 'var', col.var = 'black', repel = TRUE, 
                      col.ind = 'darkgrey', title = '')
np / sp
ggsave('pca_cutoff.pdf', width = 3.5, height = 7)
