#check what error occurred for eventID 11151

checkdates <- fish_catch_QC %>% 
  filter(EventID == 11151 )

fish_date <- fish_sample_cleaned %>% 
  filter(SampleDate == "2025-04-16")

check <- fish_catch_QC %>% 
  filter(EventID == 10637)

salmon <- fish_catch_clean %>% 
  filter(EventID == 9869)

salmon <- fish_catch_clean %>% 
  filter(EventID == 10953)


# figure out adding in rev counter 
library(tidyverse)
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(ggridges)
library(lubridate)
library(ggpubr)
library(tidylog)

sample <- read.csv("data_raw/Sample_20260310.csv")
effort <- read.csv("data_raw/TrapEffort_20260310.csv")
filenames <- list.files("data_raw/Catch", pattern="*.csv", full.names=TRUE)
Catchread <- lapply(filenames, read.csv)
catch <- bind_rows(Catchread, .id = "column_label")


#fix known gear issues before proceeding (scientific review etc)  
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
  rename(SampleRowID = SampleRowID.x,
         RevCounter = Rev.Counter)

summary(sampeffort)
status.na <- filter(sampeffort, is.na(TrapStatus) | TrapStatus == "")
time.na <- filter(sampeffort, is.na(SampleTime) | SampleTime == "")
date.na <- filter(sampeffort, is.na(Date) | SampleDate == "")


PCS <- filter(status.na, StationCode == "PCS")
STTD <- filter(status.na, StationCode == "STTD")

#remove rows with comments that indicate the fyke was not set
# 7641, 8050, 9261, 9505, 9515, 10558 - screw trap not set
# 10746 was check date
fykeNotSet_ids <- c(3192, 7118, 7641, 8050, 8051, 8361, 9261, 
                    9406, 9407, 9408, 9409, 9410, 9485, 9505,
                    9515, 9920, 9950, 10490, 10506, 10558)


gear.na <- filter(sampeffort, is.na(GearConditionCode) | GearConditionCode == "")

#search records from gear.na and remove duplicated/mis-entered samples
dataEntry_ids <- c(1698, 1858, 1862, 2009, 2477, 2826, 3485, 6971, 11123)

#entries with condition code not noted
# 10137, 10161, 10225, 10257, 10348, 10367. 11129 (WQ wasn't taken at set, entered into db without WQ just date/time)

f_qaqc1 <- function(fykeNotset_ids, dataEntry_ids) {
  sampeffort %>%
    dplyr::filter(!(SampleID %in% fykeNotSet_ids)) %>%
    dplyr::filter(!(SampleID %in% dataEntry_ids))
}

sampeffort2 <- f_qaqc1(fykeNotSet_ids, dataEntry_ids)

sampeffort3 <- sampeffort2
#Some samples didn't have status entered - confirm with scanned data sheet
sampeffort3$TrapStatus[sampeffort2$SampleID == 7447] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 3485] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 5028] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 1917] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 6820] <- "Pull"
sampeffort3$TrapStatus[sampeffort2$SampleID == 10746] <- "Check"


sampeffortcatch <- left_join(sampeffort3, catch)


#some marked as set end up having fish data with them so change to "check" for the purpose of not removing them
sampeffortcatch <- left_join(sampeffort3, catch) %>%
  select(-c(WeatherCode:SampleRowID, TotalLength:FishTagID, s_ColLineage:s_Lineage))
set <- filter(sampeffortcatch, TrapStatus == "Set" & !is.na(OrganismCode))

# Set: 1001, 2811, 2797, 3308, 2363, 2352, 2354, 2358
# These should be set to catch = "NA" in the future
sampeffort3$TrapStatus[sampeffort2$SampleID == 2810] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 23] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 6237] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 2892] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 8195] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 8981] <- "Check"

# Older data (from Nicole)
sampeffort3$TrapStatus[sampeffort2$SampleID == 1697] <- "Pull"
sampeffort3$TrapStatus[sampeffort2$SampleID == 1626] <- "Set"
sampeffort3$TrapStatus[sampeffort2$SampleID == 2510] <- "Set"
sampeffort3$TrapStatus[sampeffort2$SampleID == 3011] <- "Set"

# Incorrectly entered
sampeffort3$TrapStatus[sampeffort2$SampleID == 3590] <- "Check"
sampeffort3$TrapStatus[sampeffort2$SampleID == 3589] <- "Set"
sampeffort3$TrapStatus[sampeffort2$SampleID == 6237] <- "Set"
sampeffort3$TrapStatus[sampeffort2$SampleID == 8195] <- "Set"

#two dates with no time recorded (sample ID 6954), add in an approximate time based on check times the day before and after so hours for that day and the day after can be appropriately calculated
sampeffort3$SampleTime[sampeffort2$SampleID == 6954] <- "11:00:00"
sampeffort3$SampleTime[sampeffort2$SampleID == 6431] <- "10:40:00"
sampeffort3$SampleTime[sampeffort2$SampleID == 7657] <- "10:40:00" # looks like 16:40 but probably 10:40

# Incorrect Dates
sampeffort3$Date[sampeffort2$SampleID == 3501] <- as.Date("2009-07-09")
sampeffort3$SampleDate[sampeffort2$SampleID == 3501] <- "7/9/2009"# Missing entry from 7/8/2008 and other entries in July 2008. It says pulled for the season on 6/27/2008, but there are more data sheets from July in the scanned data sheets. Listed in effort and samp, but for some reason not showing up 
sampeffort3$Date[sampeffort2$SampleID == 3589] <- as.Date("2010-03-22")
sampeffort3$SampleDate[sampeffort2$SampleID == 3589] <- "3/22/2010"

sampeffort3$Date[sampeffort2$SampleID == 7558] <- as.Date("2017-05-24")
sampeffort3$SampleDate[sampeffort2$SampleID == 7558] <- "5/24/2017"

sampeffort3$Date[sampeffort2$SampleID == 7558] <- as.Date("2017-05-24")
sampeffort3$SampleDate[sampeffort2$SampleID == 7558] <- "5/24/2017"

# There are two entries for 6/13/2017. Chose this one to be the next day because of SampleID pattern, but not 100% sure which one was which day.
sampeffort3$Date[sampeffort2$SampleID == 7620] <- as.Date("2017-06-14")
sampeffort3$SampleDate[sampeffort2$SampleID == 7620] <- "6/14/2017"

# There are two entries for 6/27/2017. Chose this one to be the next day because of the sample crew but not 100% sure.
# sampeffort3$Date[sampeffort2$SampleID == 7657] <- as.Date("2017-06-28")
# sampeffort3$SampleDate[sampeffort2$SampleID == 7657] <- "6/28/2017"

# 2/19 was president's day and fyke was set 2/20 (Tuesday), so guessing screw trap was also set 2/20 and not 2/19. 
sampeffort3$Date[sampeffort2$SampleID == 7966] <- as.Date("2018-02-20")
sampeffort3$SampleDate[sampeffort2$SampleID == 7966] <- "2/20/2018"


#create a column with date & time together
sampeffort3$DateTime = mdy_hms(paste(sampeffort3$SampleDate, sampeffort3$SampleTime))
str(sampeffort3)

# sampeffort4 <- sampeffort3 %>% 
#   mutate(SampleDate = as.Date("SampleDate",format ="%Y/%m/%d"))

sampeffort3$SampleDate <- mdy(sampeffort3$SampleDate)

sampeffort4 <- sampeffort3 %>% 
  rename(RevCounter = Rev.Counter) %>% 
  mutate(RevCounter=as.character(RevCounter))%>%
  drop_na(RevCounter)%>%
  mutate(Week=isoweek(SampleDate))%>%
  arrange(DateTime)%>%
  mutate(PrevRevs=as.numeric(dplyr::lag(RevCounter, n=1)))%>%
  mutate(TotalOldRevs=as.numeric(RevCounter)-PrevRevs)

# sampeffort5 <- sampeffort4 %>% 
#   filter(TrapStatus == "Set" & MethodCode == "RSTR") %>% 
#   mutate(TotalRevs = case_when(TotalOldRevs != 0 ~ 0,
#                                TRUE ~ TotalOldRevs))

sampeffort5 <- sampeffort4 %>% 
  mutate(TotalRevs = case_when(TrapStatus =="Set"&MethodCode =="RSTR" & TotalOldRevs !=0 ~ 0,
                               TRUE ~ TotalOldRevs))


sampeffort5 <- sampeffort5 %>% 
  relocate(RevCounter, .after = Week) %>% 
  relocate(Comments, .before = DateTime)

check <- sampeffort5 %>% 
  filter(TotalRevs != 0)

negatives <- subset(sampeffort5, TotalRevs <0) %>% 
  arrange(MethodCode)

highvalues <- subset(sampeffort5, TotalRevs > 100000) %>% 
  arrange(MethodCode)

march <- sample %>% 
  filter(SampleDate == "3/9/2017")

february <- sample %>% 
  filter(SampleDate == "2/28/2013")

feb2 <- sampeffort3 %>% 
  filter(Date == "2013-02-28")

#5226 missing a check day the day before
#5379 may be entered wrong, 19860 instead of 14860?
#5811 note on data sheet states rev counter seems wrong but data sheets confirm revs
#6173 data sheets confirm rev counter number recorded
#6435 rev counter entered with an extra 0 confirmed on data sheet, should be 69832
#6823, 6824, 6825, 6979 all same day - 2 set, 2 pull numbers match data sheets
#6868, 6869, 6870, 6871 all same day - 2 set 2 pull - numbers match data sheets
# need to ensure formula for calculating revs is in order of date AND time to calculate correct revs
#6898 likely missing a 0 in rev counter number due to spaces on datasheet - flag
#7413 no set rev counter as noted in comments - flag
#7657 date was entered wrong, is 6/27/17 (2 data sheets in a day) not 6/28/17
#8668 revs are 321484 from datasheet not 321984
#10704 revs should be 104604 based on datasheet


sampeffort3$Rev.Counter[sampeffort3$SampleID == 6435] <- 69832
sampeffort3$Rev.Counter[sampeffort3$SampleID == 6925] <- 103007 #confirmed with datasheet
sampeffort3$Rev.Counter[sampeffort3$SampleID == 10704] <- 104604 #confirmed with datasheet
sampeffort3$Rev.Counter[sampeffort3$SampleID == 8668] <- 321484 # confirmed with datasheet


sampeffort6 <- sampeffort3 %>% 
  filter(is.na(RevCounter)) %>% 
  mutate(RevCounter=as.character(RevCounter))
str(sampeffort5)

sampeffort3 <- bind_rows(sampeffort6, sampeffort5) %>% 
  select(-c(Week, PrevRevs, TotalOldRevs))
  

#calculate effort in hours
sampeffortHours <- sampeffort3 %>%
  arrange(DateTime) %>%
  select(-SampleDate) %>%
  group_by(MethodCode) %>%
  mutate(effort.hrs = ifelse(TrapStatus == "Set", 0, as.numeric(difftime(DateTime,lag(DateTime), units = "hours")))) %>%
  arrange(MethodCode, DateTime) 


rev <- sampeffortHours %>% 
  filter(Date == "2013-02-22")
