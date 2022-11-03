library(tidyverse)
library(xlsx)

f <- readRDS("01_data_pro_test.RDS")


f <- f %>%
  mutate(kraj = kraj %>% fct_relevel(c("Praha", "Středočeský", "Jihočeský",  "Plzeňský", "Karlovarský", "Ústecký", "Liberecký", "Královéhradecký", "Pardubický", "Vysočina", "Jihomoravský", "Olomoucký", "Zlínský", "Moravskoslezský"))) %>%
  arrange(kraj) %>%
  
  left_join( tibble(NUTS3 = c("Praha", "Středočeský", "Jihočeský", "Plzeňský", 
                              "Karlovarský","Ústecký", "Liberecký","Královéhradecký", "Pardubický", 
                              "Vysočina", "Jihomoravský", "Olomoucký", "Zlínský", "Moravskoslezský"),
                    NUTS2 = c("Praha", "Střední Čechy", "Jihozápad", "Jihozápad", 
                              "Severozápad", "Severozápad", "Severovýchod", "Severovýchod", "Severovýchod",
                              "Jihovýchod", "Jihovýchod", "Střední Morava", "Střední Morava", "Moravskoslezsko")), 
             by = c("kraj" = "NUTS3")) 

f


NUTS_3 <- 
  f %>%
  count(kraj, wt = pocet_zaku) %>%
  rename("pocet_zaku" = "n") %>%
  
  inner_join(
    f %>% 
      count(kraj) %>% 
      rename("pocet_skol" = "n"), 
    by = c("kraj" = "kraj")) %>%
  
  inner_join(
    f %>%
      group_by(kraj) %>%
      count(psycholog) %>%
      filter(psycholog == "ano") %>% 
      rename("pocet_psych" = "n") %>%
      select(-psycholog), 
    by = c("kraj" = "kraj")) %>%
  
  inner_join(
    f %>% 
      count(kraj, wt = pocet_asistentu) %>% 
      rename("pocet_asistentu" = "n"), 
    by = c("kraj" = "kraj")) %>%
  
  mutate( zna = pocet_zaku/ pocet_asistentu)  %>%
  
  inner_join(
    f %>% 
      filter(spec_pedagog == "ano" | psycholog == "ano") %>%
      count(kraj) %>%
      mutate("a" = 100 * n / f %>%
               count(kraj) %>% 
               filter(!is.na(kraj)) %>%
               select(n)) %>%
      mutate(n = a$n) %>%
      rename(pcsp = n) %>%
      select(kraj, pcsp), 
    by = c("kraj" = "kraj")) %>%
  
  inner_join(
    f %>%
      group_by(NUTS2) %>%
      count(kraj) %>%
      select(-n), 
    by = c("kraj" = "kraj"))  %>%
  filter(!is.na(kraj))




NUTS2 <- NUTS_3 %>%
  count(NUTS2, wt = pocet_zaku) %>%
  rename(pocet_zaku = n) %>%
  
  inner_join(NUTS_3 %>%
            count(NUTS2, wt = pocet_asistentu) %>%
              rename(pocet_asistentu = n),
            by = c("NUTS2" = "NUTS2")
  ) %>%
  
  mutate(zna = pocet_zaku/ pocet_asistentu) %>%
  mutate(NUTS2 = factor(NUTS2) %>% fct_relevel(c("Praha", "Střední Čechy", "Jihozápad",  "Severozápad", "Severovýchod", "Jihovýchod", "Střední Morava", "Moravskoslezsko"))) %>%
  
  arrange(NUTS2) %>%
  filter(pocet_zaku != 0) 




six <- f %>%
  count(psycholog, wt = pocet_zaku) %>%
  rename("b" = "psycholog") %>%
  rename("psychologa" = "n") %>%
  mutate(psychologa, psychologa =  psychologa / nrow(f)) %>%

  inner_join(
    f %>%
      count(spec_pedagog, wt = pocet_zaku) %>%
      rename("b" = "spec_pedagog") %>%
      rename("Specálního_pedagoga" = "n") %>%
      mutate(Specálního_pedagoga, Specálního_pedagoga =  Specálního_pedagoga / nrow(f)),
  by = c("b" = "b")) %>%
  filter(!is.na(b))
  
six[1,1] = "Mají: "
six[2,1] = "Nemají: "



write.xlsx(f, "data.xlsx")
write.xlsx(NUTS2, "NUTS-2.xlsx")
write.xlsx(NUTS_3, "NUTS-3.xlsx")
write.xlsx(six, "six.xlsx")

  print("IV. Počet škol v krajích")
  print(NUTS_3 %>% select(kraj, pocet_skol))
  
  
  print("IV.a Počet žáků za kraj: ")
  print(NUTS_3 %>% select(kraj, pocet_zaku))
  
  
  print("IV.b Počet žáků za NUTS2: ")
  print(NUTS2 %>% select(NUTS2, pocet_zaku))
  
  
  print("V. Počet žáků na assistenta/ku pedagoga/žky")
  print("a. za kraj")
  print(NUTS_3 %>% select(kraj, zna))
  
  
  print("b. za NUTS2")
  print(NUTS2 %>% select(NUTS2, zna))
  
  
  
  print("VI. Průměrný počet žáků ve školách, které (ne)mají: ")
  print(six)
  
  
  
  print("VII. Procento škol ve kraji, ve kterých je psycholog či speciální pedagog.")
  print(NUTS_3 %>% select(kraj, pcsp))
  
  