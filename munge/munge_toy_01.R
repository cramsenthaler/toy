#Data munging for SENIORS-I to get to masterfile

# List of packages for session
.packages = c("lubridate", "tidyverse","here")
# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#alternatively pacman::load?

#Read in csv file import2
sen <- read.csv2(here("data", "seniorsi_import2.csv"), header = TRUE)
sen <- read.csv2(here("data", "seniorsi_import3.csv"), header = TRUE)
names(sen)[1] <- "data_nr"

#Datumsvariable
sen$date <- dmy(sen$date)
#KW
sen$kw <- lubridate::isoweek(sen$date)
#convert to extract year as new variable
sen$year <- year(sen$date) 
sen$month <- month(sen$date)

str(sen)

#Days between assessments per case
sen <- sen %>%
  group_by(fall_id) %>% 
  arrange(fall_id, date) %>% 
  mutate(diff_date = c(0,diff(date)))
table(sen$diff_date)
#weitere Datenbereinigung nötig:
which(sen$diff_date == 79)
which(sen$diff_date == 347)
which(sen$diff_date == 43)
sen[735, ]
#Diese zwei Fälle bereinigt - war noch doppelte ID-Nummer und 
#einmal falsches Jahr (2021 anstatt 2020).
#xlsx und csv verändert und erneut eingelesen
#Der letzte Fall mit der langen Verweildauer ist in Ordnung.

#Gesamtliegedauer per case
#intermediate dataset with first and last date per patient
senim <- sen[, c(3,14,17)]
senim <- subset(senim, subset = ereignis != 2)
table(senim$ereignis)
#convert this to other format
senim <- senim %>%
  pivot_wider(
    names_from = "ereignis",
    values_from = "date"
  )
#calculate time diff
str(senim)
senim$date_diff <- interval(senim$`1`, senim$`3`)  
table(senim$date_diff)
senim$date_diff <- senim$date_diff %/% days(1)
#Datenbereinigung Fall 1271
which(senim$date_diff == -25)
senim[271, ]
#Fall_ID 1271 is the culprit --> need to check initial data
#drop columns
senim <- senim[,c(1,4)]
#bind back to original dataset
sen <- sen %>%
  left_join(senim, by = c("fall_id" = "fall_id"))
#Categorise Liegedauer
names(sen)[51] <- "liegedauer"
table(sen$liegedauer)
sen$ld_cat <- NULL
sen$ld_cat[(sen$liegedauer == 0)] <- 1
sen$ld_cat[(sen$liegedauer >=1) & (sen$liegedauer <=3)] <- 2
sen$ld_cat[(sen$liegedauer >=4) & (sen$liegedauer <=7)] <- 3
sen$ld_cat[(sen$liegedauer >=8) & (sen$liegedauer <=14)] <- 4
sen$ld_cat[(sen$liegedauer >=15) & (sen$liegedauer <=21)] <- 5
sen$ld_cat[(sen$liegedauer >=22) & (sen$liegedauer <=30)] <- 6
sen$ld_cat[(sen$liegedauer >=31)] <- 7
sen$ld_cat <- factor(sen$ld_cat, levels = c(1:7),
                       labels = c("<1", "1-3", "4-7",
                                  "8-14","15-21","22-30",">30"))
table(sen$ld_cat, useNA = "always")
table(sen$liegedauer, sen$ld_cat, useNA = "always")
#sen$'NULL' <- NULL

#Age from yob to date
#convert yob variable
sen$yob <- as.Date(as.character(sen$yob), format = "%Y")
senim <- sen[, c(3,7,14,17)]
senim <- subset(senim, subset = ereignis == 1)
#calculate time diff
senim$age <- interval(senim$yob, senim$date)  
table(senim$age)
senim$age <- senim$age %/% years(1)
#drop columns
senim <- senim[,c(1,5)]
#bind back to original dataset
sen <- sen %>%
  left_join(senim, by = c("fall_id" = "fall_id")) 

#Categorise age
table(sen$age)
sen$age_cat <- NULL
sen$age_cat[(sen$age <= 39)] <- 1
sen$age_cat[(sen$age >=40) & (sen$age <=49)] <- 2
sen$age_cat[(sen$age >=50) & (sen$age <=59)] <- 3
sen$age_cat[(sen$age >=60) & (sen$age <=69)] <- 4
sen$age_cat[(sen$age >=70) & (sen$age <=79)] <- 5
sen$age_cat[(sen$age >=80) & (sen$age <=89)] <- 6
sen$age_cat[(sen$age >=90)] <- 7
sen$age_cat <- factor(sen$age_cat, levels = c(1:7),
                     labels = c("<40", "40-49", "50-59",
                                "60-69","70-79","80-89",">=90"))
table(sen$age_cat)
table(sen$age, sen$age_cat)

#Faktorvariablen
#sex
sen$sex <- factor(sen$sex, levels = c(1,2),
                  labels = c("m","w"))
#hdiagc
sen$hdiagc <- factor(sen$hdiagc, levels = c(1,2),
                     labels = c("Krebs","Nichtkrebs"))
#diag_cancer
table(sen$diag_cancer)
sen$diag_cancer <- factor(sen$diag_cancer, levels=c(1:13),
                          labels=c("HNO","Magen/Darm",
                                   "Leber", "Pankreas",
                                   "Lunge","Knochen",
                                   "Mamma","Uterus",
                                   "Prostata","Niere",
                                   "Gehirn", "CUP",
                                   "Blutkrebs"))
#diag_noncancer
table(sen$diag_noncancer)
sen$diag_noncancer <- factor(sen$diag_noncancer, levels=c(1:11),
                             labels = c("Herz","HerzIns",
                                        "Apoplex","Neurologisch",
                                        "Motoneuron","COPD/ILD",
                                        "Leber","Niere","Sepsis",
                                        "Demenz","Andere"))
#egrundkat
table(sen$egrundkat)
sen$egrundkat <- factor(sen$egrundkat, levels=c(1:6),
                        labels = c("Symptomlast","Az-Verschlechterung",
                                   "Therapie","Symptomkontrolle",
                                   "ACP","Überforderung"))
#month (character)
table(sen$month, useNA = "always")
sen$month <- factor(sen$month, levels=c(1:12),
                    labels=c("Januar", "Februar","Maerz",
                             "April","Mai","Juni","Juli",
                             "August","September", "Oktober",
                             "November","Dezember"))

#poi
table(sen$poi)
sen$poif <- factor(sen$poi, levels=c(1:5),
                   labels=c("stabil","instabil","verschlechternd",
                            "sterbend","verstorben")) 
#tod
sen$tod <- factor(sen$tod, levels=c(0,1),
                  labels=c("lebend","gestorben"))

#IPOS '5' as missing code?
#two versions of items
#work with left_join again
senim <- sen[,c(2,3,24:33,40:46)]
senim <- senim %>% rename_at(vars(-c(1,2)), ~ paste0(., 's5'))
senim <- senim %>% 
  mutate_at(vars(-c(1,2)), 
            funs(na_if(., "5")))
#NAs aufzählen
senim <- senim %>%
  rowwise() %>% 
  mutate(total_na = sum(is.na(c_across(i1s5:i17s5))))
table(senim$total_na, useNA = "always")
#30 mit NA
senim <- senim %>%
  rowwise() %>% 
  mutate(symptom_na = sum(is.na(c_across(i1s5:i10s5))))
table(senim$symptom_na)
#36 mit 10 NAs
senim <- senim %>%
  rowwise() %>% 
  mutate(emotion_na = sum(is.na(c_across(i11s5:i14s5))))
table(senim$emotion_na)
#49 mit 4 NAs
senim <- senim %>%
  rowwise() %>% 
  mutate(praktisch_na = sum(is.na(c_across(i15s5:i17s5))))
table(senim$praktisch_na)
#85 mit 3 NAs

#IPOS total
senim <- senim %>%
  rowwise() %>% 
  mutate(total = sum(c_across(i1s5:i17s5), na.rm = TRUE))
table(senim$total, useNA = "always")
senim <- senim %>% 
  mutate(total = replace(total, total_na == "17", NA))

#IPOS Symptom
senim <- senim %>%
  rowwise() %>% 
  mutate(symptom = sum(c_across(i1s5:i10s5), na.rm = TRUE))
table(senim$symptom, useNA = "always")
senim <- senim %>% 
  mutate(symptom = replace(symptom, symptom_na == "10", NA))

#IPOS Emotion
senim <- senim %>%
  rowwise() %>% 
  mutate(emotion = sum(c_across(i11s5:i14s5), na.rm = TRUE))
table(senim$emotion, useNA = "always")
senim <- senim %>% 
  mutate(emotion = replace(emotion, emotion_na == "4", NA))

#IPOS Praktisch
senim <- senim %>%
  rowwise() %>% 
  mutate(praktisch = sum(c_across(i15s5:i17s5), na.rm = TRUE))
table(senim$praktisch, useNA = "always")
senim <- senim %>% 
  mutate(praktisch = replace(praktisch, praktisch_na == "3", NA))

#drop columns for left_join
senim <- senim[,c(1,3:27)]
#left_join on sort_id
#bind back to original dataset
sen <- sen %>%
  left_join(senim, by = c("sort_id" = "sort_id"))

#reorder variables in dataframe
names(sen)
master <- sen %>% 
  select(data_nr, sort_id, fall_id, patient_id, episode_nr, 
         sex, yob, age, age_cat, hdiagc, diag_cancer, 
         diag_noncancer, egrundkat, ereignis, tod, phasenwechsel, 
         date, year, month, kw, diff_date, liegedauer, ld_cat, 
         poi, poif, akps, i1:i10,
         i11:i17, i1s5:praktisch)
names(master)
str(master)

#.RData file
rio::export(master, here("data", "master.rds"))
