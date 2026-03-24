library(tidyverse)
library(lubridate)
library(anytime)
library(stringr)
library(magrittr)
library(kableExtra)
library(viridis)
library(plotly)
library(tidylog)

Sys.setenv(TZ = "America/Los_Angeles")

gemgen <- read_csv("data_raw/2025_YBFMP_results.csv")
gem2 <- read_csv("data_raw/YBFMP_Reruns_20260316.csv")
salmgen <- read_csv("data_raw/SalmGenetics_20250128.csv")

str(gemgen)
str(salmgen)

gemgen1 <- gemgen %>% 
  mutate(SalmGeneticRowID = 3095:3319) %>% 
  relocate(SalmGeneticRowID) %>% 
  rename(FishTagID= "Sample ID",
         BestEstimate = final_call,
         Comments = Notes,
         Prob1 = probability)

gem2a <- gem2 %>% 
  mutate(SalmGeneticRowID = 3320:3357) %>% 
  relocate(SalmGeneticRowID) %>% 
  rename(FishTagID= "Sample ID",
         BestEstimate = final_call,
         Prob1 = "Final Call probability")

str(gemgen1)
str(salmgen)

resample <- gemgen1 %>% 
  filter(Status == "Rerunning")

final <- left_join(gem2a, resample, by = "FishTagID") %>% 
  select(-c(tributary.x, tributary.y, Prob1.y, Fall, Late_fall, Spring, Winter))

needid <- final %>% 
  filter(is.na(SalmGeneticRowID.y)) %>% 
  select(-c(SalmGeneticRowID.y, BestEstimate.y, Status)) 

needid <- needid %>% 
  rename(SalmGeneticRowID = SalmGeneticRowID.x,
         BestEstimate = BestEstimate.x)

finala <- final %>% 
  filter(!is.na(SalmGeneticRowID.y)) %>% 
  select(-c(SalmGeneticRowID.x, BestEstimate.y, Status, Comments)) %>% 
  rename(SalmGeneticRowID = SalmGeneticRowID.y,
         BestEstimate = BestEstimate.x,
         Fall = "Fall Prob.",
         Late_fall = "Late_fall Prob.",
         Spring = "Spring Prob.",
         Winter = "Winter Prob.",
         Prob1 = Prob1.x) %>% 
  relocate(SalmGeneticRowID)

gemgen2 <- gemgen1 %>% 
  filter(!(Status == "Rerunning")) %>% 
  select(-c(tributary, Status))

str(gemgen2)
str(finala)

library(purrr)

gemgen2$Prob1 <- as.numeric(gemgen2$Prob1)

gemgenetics <- bind_rows(gemgen2, finala)

write.csv(gemgenetics, "data_raw/gemlabgenetics.csv")

# resample <- gemgen1$FishTagID %in% gem2a$FishTagID
# as_tibble(resample)
# print(resample)



gemgen2 <-gemgen1 %>% 
  select(-c(Fall, Late_fall, Spring, Winter, tributary, Status)) %>% 
  case_when(Prob1 == "Data" ~ "", TRUE ~ as.numeric(Prob1))
