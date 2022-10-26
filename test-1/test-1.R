library(tidyverse)
library(dplyr)
library(forcats)
library(patchwork)

f <- readRDS("01_data_pro_test.RDS")

f %>%
count(kraj)



f <- f %>%
  mutate(kraj = factor(kraj) %>% fct_relevel(c("Praha", "Středočeský", "Jihočeský",  "Plzeňský", "Karlovarský", "Ústecký", "Liberecký", "Královéhradecký", "Pardubický", "Vysočina", "Jihomoravský", "Olomoucký", "Zlínský", "Moravskoslezský"))) %>%
  arrange(kraj) %>%
  
  left_join(
    f %>%
      filter(kraj == "Praha") %>%
      mutate(a = "Praha") %>%
      select(ID, a), by = c("ID" = "ID")) %>%
  
  left_join(
    f %>%
      filter(kraj == "Středočeský") %>%
      mutate(a = "Střední Čechy") %>%
      select(ID, a), by = c("ID" = "ID")) %>%
  
  left_join(
    f %>%
      filter(kraj == "Jihočeský" | kraj == "Plzeňský") %>%
      mutate(a = "Jihozápad") %>%
      select(ID, a), by = c("ID" = "ID")) %>%
  
  left_join(
    f %>%
      filter(kraj == "Karlovarský" | kraj == "Ústecký") %>%
      mutate(a = "Severozápad") %>%
      select(ID, a), by = c("ID" = "ID")) %>%
  
  left_join(
    f %>%
      filter(kraj == "Liberecký" | kraj == "Královéhradecký" | kraj == "Pardubický") %>%
      mutate(a = "Severovýchod") %>%
      select(ID, a), by = c("ID" = "ID")) %>%
  
  left_join(
    f %>%
      filter(kraj == "Vysočina" | kraj == "Jihomoravský") %>%
      mutate(a = "Jihovýchod") %>%
      select(ID, a), by = c("ID" = "ID")) %>%
  
  left_join(
    f %>%
      filter(kraj == "Olomoucký" | kraj == "Zlínský") %>%
      mutate(a = "Střední Morava") %>%
      select(ID, a), by = c("ID" = "ID")) %>%
  
  left_join(
    f %>%
      filter(kraj == "Moravskoslezský") %>%
      mutate(a = "Moravskoslezsko") %>%
      select(ID, a), by = c("ID" = "ID")) %>%
  
  unite("NUTS_2", a.x:a.y:a.x.x:a.y.y:a.x.x.x:a.y.y.y:a.x.x.x.x:a.y.y.y.y, na.rm = TRUE, remove = FALSE) %>%
  select(-a.x, -a.y, -a.x.x, -a.y.y, -a.x.x.x, -a.y.y.y, -a.x.x.x.x, -a.y.y.y.y)
  
  



 f

NUTS_3 <- 
  f %>%
  count(kraj, wt = pocet_zaku) %>%
  rename("pocet_zaku" = "n") %>%
  
  inner_join(
    f %>% 
      count(kraj, wt = pocet_asistentu) %>% 
      rename("pocet_asistentu" = "n"), 
    by = c("kraj" = "kraj")) %>%
  
  mutate( zaci_na_asistenta = pocet_zaku/ pocet_asistentu) %>%
  
  inner_join(
    f %>%
      group_by(kraj) %>%
      count(psycholog) %>%
      filter(psycholog == "ano") %>% 
      rename("pocet_psychologu" = "n") %>%
      select(-psycholog), 
    by = c("kraj" = "kraj")) %>%
  
  inner_join(
    f %>% 
      filter(spec_pedagog == "ano" | psycholog == "ano") %>%
      count(kraj) %>%
      mutate("a" = n / f %>%
               count(kraj) %>% 
               filter(!is.na(kraj)) %>%
               select(n)) %>%
      mutate(n = a$n) %>%
      rename(spy_spec = n) %>%
      select(kraj, spy_spec), 
    by = c("kraj" = "kraj")) %>%
  
  inner_join(
    f %>%
      group_by(NUTS_2) %>%
      count(kraj) %>%
      select(-n), 
    by = c("kraj" = "kraj"))  %>%
  filter(!is.na(kraj))

NUTS_3


NUTS_2 <- 
  f %>%
  count(NUTS_2, wt = pocet_zaku) %>%
  rename("pocet_zaku" = "n") %>%
  
  inner_join(
    f %>% 
      count(NUTS_2, wt = pocet_asistentu) %>% 
      rename("pocet_asistentu" = "n"), 
    by = c("NUTS_2" = "NUTS_2")) %>%
  
  mutate(zaci_na_asistenta = pocet_zaku/ pocet_asistentu) %>%
  
  mutate(NUTS_2 = factor(NUTS_2) %>% fct_relevel(c("Praha", "Střední Čechy", "Jihozápad",  "Severozápad", "Severovýchod", "Jihovýchod", "Střední Morava", "Moravskoslezsko"))) %>%
  arrange(NUTS_2) %>%
  filter(!is.na(NUTS_2)) 



six <- f %>%
  count(psycholog, wt = pocet_zaku) %>%
  rename("b" = "psycholog") %>%
  rename("psycholog" = "n") %>%
  mutate(psycholog, psycholog =  psycholog / nrow(f)) %>%

  inner_join(
    f %>%
      count(spec_pedagog, wt = pocet_zaku) %>%
      rename("b" = "spec_pedagog") %>%
      rename("spec_pedagog" = "n") %>%
      mutate(spec_pedagog, spec_pedagog =  spec_pedagog / nrow(f)),
  by = c("b" = "b")) %>%
  filter(!is.na(b))

  

  