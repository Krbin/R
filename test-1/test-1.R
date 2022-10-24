library(tidyverse)
library(dplyr)
library(forcats)
library(patchwork)

x <- readRDS("01_data_pro_test.RDS")

x %>%
count(kraj)



x <- x %>%
  mutate(kraj = factor(kraj)) %>%
  mutate(kraj = fct_relevel(kraj, c("Praha", "Středočeský", "Jihočeský",  "Plzeňský", "Karlovarský", "Ústecký", "Liberecký", "Královéhradecký", "Pardubický", "Vysočina", "Jihomoravský", "Olomoucký", "Zlínský", "Moravskoslezský"))) %>%
  arrange(kraj)

x
