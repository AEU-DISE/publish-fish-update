library(tidyverse)
library(lubridate)
library(tidylog)

#bring in trap effort from YBFMP Access DB query of "TrapEffort" and "Sample" tables
sample <- read.csv("data_raw/Sample_20260310.csv")
effort <- read.csv("data_raw/TrapEffort_20260310.csv")
filenames <- list.files("data_raw/Catch", pattern="*.csv", full.names=TRUE)
Catchread <- lapply(filenames, read.csv)
catch <- bind_rows(Catchread, .id = "column_label")

# ------------------------------

# Clean up columns from previous and join together
stationsOfInterest <- c("PCS", "STTD")
methods <- c("FKTR", "RSTR")
DateFirst <- as.Date("1998-01-01")
DateLast <- as.Date("2025-10-01")


#confirm all pcs and sttd are labeled correctly methodcode
checkmethod <- sample %>% 
  filter(StationCode == "PCS" | StationCode == "STTD")

PCScheck <- checkmethod %>% 
  filter(StationCode == "PCS") %>% 
  filter(MethodCode == "BSEIN" | MethodCode == "RSTR")

STTDcheck <- checkmethod %>% 
  filter(StationCode == "STTD") %>% 
  filter(MethodCode == "BSEIN" | MethodCode == "FKTR")

sample$MethodCode[sample$SampleID == 10648] <- "RSTR"
sample$GearID[sample$SampleID == 10648] <- "RSTR8"
sample$StationCode[sample$SampleID == 8396] <- "BL1"
sample$MethodCode[sample$SampleID == 10643] <- "FKTR"
sample$GearID[sample$SampleID == 10643] <- "FKTR"

f_cleancolumnsandmerge <- function(stationsOfInterest, methods, DateFirst, DateLast) {
  fykerstr <- sample %>% 
    dplyr::mutate(Date = lubridate::mdy(SampleDate)) %>%
    dplyr::select(-c(VegetationRank:DataCorrectionComments)) %>%
    dplyr::filter(StationCode %in% stationsOfInterest) %>%
    dplyr::filter(MethodCode %in% methods) %>%
    dplyr::filter(Date > DateFirst & Date < DateLast)
  effort2 <- dplyr::select(effort, -c(s_ColLineage:s_Lineage))
  sampeffort <- left_join(fykerstr, effort2, by = "SampleID") 
}
#updating line due to join issue removing status, etc that did not happen in previous publishings

sampeffort <- f_cleancolumnsandmerge(stationsOfInterest, methods, DateFirst, DateLast)
# Failed to parse is (sample ID 6378) that does not have a date included; digging in the Access database shows there was no fish or environmental data associated with this date so it was probably a mis-entry & can be removed

#renaming and cleaning up duplicate samplerowid column
sampeffort <- sampeffort %>% 
  select(-c(SampleRowID.y)) %>% 
  rename(SampleRowID = SampleRowID.x)
