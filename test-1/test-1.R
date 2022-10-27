library(tidyverse)
library(xlsx)

f <- readRDS("01_data_pro_test.RDS")


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
      group_by(NUTS_2) %>%
      count(kraj) %>%
      select(-n), 
    by = c("kraj" = "kraj"))  %>%
  filter(!is.na(kraj))




NUTS_2 <- NUTS_3 %>%
  count(NUTS_2, wt = pocet_zaku) %>%
  rename(pocet_zaku = n) %>%
  
  inner_join(NUTS_3 %>%
            count(NUTS_2, wt = pocet_asistentu) %>%
              rename(pocet_asistentu = n),
            by = c("NUTS_2" = "NUTS_2")
  ) %>%
  
  mutate(zna = pocet_zaku/ pocet_asistentu) %>%
  
  mutate(NUTS_2 = factor(NUTS_2) %>% fct_relevel(c("Praha", "Střední Čechy", "Jihozápad",  "Severozápad", "Severovýchod", "Jihovýchod", "Střední Morava", "Moravskoslezsko"))) %>%
  arrange(NUTS_2) %>%
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



write.xlsx(f, "C:/Users/kryst/OneDrive/Documents/GitHub/R/test-1/data.xlsx")
write.xlsx(NUTS_2, "C:/Users/kryst/OneDrive/Documents/GitHub/R/test-1/NUTS-2.xlsx")
write.xlsx(NUTS_3, "C:/Users/kryst/OneDrive/Documents/GitHub/R/test-1/NUTS-3.xlsx")
write.xlsx(six, "C:/Users/kryst/OneDrive/Documents/GitHub/R/test-1/six.xlsx")

  print("IV. Počet škol v krajích")
  print(NUTS_3 %>% select(kraj, pocet_skol))
  
  
  print("IV.a Počet žáků za kraj: ")
  print(NUTS_3 %>% select(kraj, pocet_zaku))
  
  
  print("IV.b Počet žáků za NUTS2: ")
  print(NUTS_2 %>% select(NUTS_2, pocet_zaku))
  
  
  print("V. Počet žáků na assistenta/ku pedagoga/žky")
  print("a. za kraj")
  print(NUTS_3 %>% select(kraj, zna))
  
  
  print("b. za NUTS2")
  print(NUTS_2 %>% select(NUTS_2, zna))
  
  
  
  print("VI. Průměrný počet žáků ve školách, které (ne)mají: ")
  print(six)
  
  
  
  print("VII. Procento škol ve kraji, ve kterých je psycholog či speciální pedagog.")
  print(NUTS_3 %>% select(kraj, pcsp))
  
  