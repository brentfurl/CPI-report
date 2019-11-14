file1 <- paste0(filebu, "analyses and cleaning report/")
file2 <- paste0(filebu2, "nps pdfs/")

library(pander)


ALL_group  <- group_by(df, ALL)
TENURE_group <- group_by(df, TENURE)
GENERATION_group <- group_by(df, GENERATION)
ROLE_group <- group_by(df, ROLE)
LOCATION_group <- group_by(df, LOCATION)

ALL_means <- summarise(ALL_group, round(mean(CPI_Index, na.rm=TRUE),2), round(mean(DIRECTION, na.rm=TRUE),2),	round(mean(OPERATIONS, na.rm=TRUE),2),	round(mean(PEOPLE, na.rm=TRUE),2),	round(mean(ENGAGEMENT, na.rm=TRUE),2),	round(mean(VisionValues, na.rm=TRUE),2),	round(mean(Strategy, na.rm=TRUE),2),	round(mean(Leadership, na.rm=TRUE),2),	round(mean(Adaptability, na.rm=TRUE),2),	round(mean(PerfMan, na.rm=TRUE),2),	round(mean(SysProc, na.rm=TRUE),2),	round(mean(Teamwork, na.rm=TRUE),2),	round(mean(TalMan, na.rm=TRUE),2),	round(mean(CoachDev, na.rm=TRUE),2),	round(mean(EmpClarFit, na.rm=TRUE),2),	round(mean(CustFoc, na.rm=TRUE),2),	round(mean(Comm, na.rm=TRUE),2), round(mean(Positivity, na.rm=TRUE),2), round(mean(satisfaction_1, na.rm=TRUE),2),  round(mean(effort_2, na.rm=TRUE),2),	round(mean(nps, na.rm=TRUE),2))
ALL_means <- add_column(ALL_means, Factor = "ALL", .before = 1)
ALL_means <- ALL_means %>% rename(Level = ALL)
ALL_means <- add_column(ALL_means, Count = (count(ALL_group)[[2]]), .after = 2)
ALL_means <- ALL_means %>% filter(Level != "NA")
nps_score_ALL <- ALL_group %>% group_by(ALL, nps_NPS_GROUP) %>% count(nps_NPS_GROUP, .drop = FALSE) %>% filter(nps_NPS_GROUP != "NA")# & nps_NPS_GROUP != "NA")
nps_spread <- nps_score_ALL %>% spread(nps_NPS_GROUP, n)  %>%
  filter(ALL != "NA")
ALL_nps <- nps_spread %>% rowwise() %>%  
  mutate(nps_calc = ((Promoter/sum(c(Detractor, Neutral, Promoter)))-(Detractor/sum(c(Detractor, Neutral, Promoter)))) *100) %>% 
  filter(ALL != "NA")

write_csv(ALL_nps, paste0(file1, "ALL_nps.csv"))
ALL_nps_pdf <- Pandoc$new("","","")
ALL_nps_pdf$add(ALL_nps)
ALL_nps_pdf$export(paste0(file2, "ALL_nps"))


TENURE_means <- summarise(TENURE_group, round(mean(CPI_Index, na.rm=TRUE),2), round(mean(DIRECTION, na.rm=TRUE),2),	round(mean(OPERATIONS, na.rm=TRUE),2),	round(mean(PEOPLE, na.rm=TRUE),2),	round(mean(ENGAGEMENT, na.rm=TRUE),2),	round(mean(VisionValues, na.rm=TRUE),2),	round(mean(Strategy, na.rm=TRUE),2),	round(mean(Leadership, na.rm=TRUE),2),	round(mean(Adaptability, na.rm=TRUE),2),	round(mean(PerfMan, na.rm=TRUE),2),	round(mean(SysProc, na.rm=TRUE),2),	round(mean(Teamwork, na.rm=TRUE),2),	round(mean(TalMan, na.rm=TRUE),2),	round(mean(CoachDev, na.rm=TRUE),2),	round(mean(EmpClarFit, na.rm=TRUE),2),	round(mean(CustFoc, na.rm=TRUE),2),	round(mean(Comm, na.rm=TRUE),2), round(mean(Positivity, na.rm=TRUE),2), round(mean(satisfaction_1, na.rm=TRUE),2),  round(mean(effort_2, na.rm=TRUE),2),	round(mean(nps, na.rm=TRUE),2))
TENURE_means <- add_column(TENURE_means, Factor = "TENURE", .before = 1)
TENURE_means <- TENURE_means %>% rename(Level = TENURE)
TENURE_means <- add_column(TENURE_means, Count = (count(TENURE_group)[[2]]), .after = 2)
TENURE_means <- TENURE_means %>% filter(Level != "NA")
nps_score_TENURE <- TENURE_group %>% group_by(TENURE, nps_NPS_GROUP) %>% count(nps_NPS_GROUP, .drop = FALSE) %>% filter(nps_NPS_GROUP != "NA")# & nps_NPS_GROUP != "NA")
nps_spread <- nps_score_TENURE %>% spread(nps_NPS_GROUP, n)  %>%
   filter(TENURE != "NA")
TENURE_nps <- nps_spread %>% rowwise() %>%  
  mutate(nps_calc = ((Promoter/sum(c(Detractor, Neutral, Promoter)))-(Detractor/sum(c(Detractor, Neutral, Promoter)))) *100) %>% 
  filter(TENURE != "NA")

write_csv(TENURE_nps, paste0(file1, "TENURE_nps.csv"))
TENURE_nps_pdf <- Pandoc$new("","","")
TENURE_nps_pdf$add(TENURE_nps)
TENURE_nps_pdf$export(paste0(file2, "TENURE_nps"))
                             
                             
GENERATION_means <- summarise(GENERATION_group, round(mean(CPI_Index, na.rm=TRUE),2), round(mean(DIRECTION, na.rm=TRUE),2),	round(mean(OPERATIONS, na.rm=TRUE),2),	round(mean(PEOPLE, na.rm=TRUE),2),	round(mean(ENGAGEMENT, na.rm=TRUE),2),	round(mean(VisionValues, na.rm=TRUE),2),	round(mean(Strategy, na.rm=TRUE),2),	round(mean(Leadership, na.rm=TRUE),2),	round(mean(Adaptability, na.rm=TRUE),2),	round(mean(PerfMan, na.rm=TRUE),2),	round(mean(SysProc, na.rm=TRUE),2),	round(mean(Teamwork, na.rm=TRUE),2),	round(mean(TalMan, na.rm=TRUE),2),	round(mean(CoachDev, na.rm=TRUE),2),	round(mean(EmpClarFit, na.rm=TRUE),2),	round(mean(CustFoc, na.rm=TRUE),2),	round(mean(Comm, na.rm=TRUE),2), round(mean(Positivity, na.rm=TRUE),2), round(mean(satisfaction_1, na.rm=TRUE),2),  round(mean(effort_2, na.rm=TRUE),2),	round(mean(nps, na.rm=TRUE),2))
GENERATION_means <- add_column(GENERATION_means, Factor = "GENERATION", .before = 1)
GENERATION_means <- GENERATION_means %>% rename(Level = GENERATION)
GENERATION_means <- add_column(GENERATION_means, Count = (count(GENERATION_group)[[2]]), .after = 2)
GENERATION_means <- GENERATION_means %>% filter(Level != "NA")
nps_score_GENERATION <- GENERATION_group %>% group_by(GENERATION, nps_NPS_GROUP) %>% count(nps_NPS_GROUP, .drop = FALSE) %>% filter(nps_NPS_GROUP != "NA")# & nps_NPS_GROUP != "NA")
nps_spread <- nps_score_GENERATION %>% spread(nps_NPS_GROUP, n)  %>%
  filter(GENERATION != "NA")
GENERATION_nps <- nps_spread %>% rowwise() %>%  
  mutate(nps_calc = ((Promoter/sum(c(Detractor, Neutral, Promoter)))-(Detractor/sum(c(Detractor, Neutral, Promoter)))) *100) %>% 
  filter(GENERATION != "NA")

write_csv(GENERATION_nps, paste0(file1, "GENERATION_nps.csv"))
GENERATION_nps_pdf <- Pandoc$new("","","")
GENERATION_nps_pdf$add(GENERATION_nps)
GENERATION_nps_pdf$export(paste0(file2, "GENERATION_nps"))


ROLE_means <- summarise(ROLE_group, round(mean(CPI_Index, na.rm=TRUE),2), round(mean(DIRECTION, na.rm=TRUE),2),	round(mean(OPERATIONS, na.rm=TRUE),2),	round(mean(PEOPLE, na.rm=TRUE),2),	round(mean(ENGAGEMENT, na.rm=TRUE),2),	round(mean(VisionValues, na.rm=TRUE),2),	round(mean(Strategy, na.rm=TRUE),2),	round(mean(Leadership, na.rm=TRUE),2),	round(mean(Adaptability, na.rm=TRUE),2),	round(mean(PerfMan, na.rm=TRUE),2),	round(mean(SysProc, na.rm=TRUE),2),	round(mean(Teamwork, na.rm=TRUE),2),	round(mean(TalMan, na.rm=TRUE),2),	round(mean(CoachDev, na.rm=TRUE),2),	round(mean(EmpClarFit, na.rm=TRUE),2),	round(mean(CustFoc, na.rm=TRUE),2),	round(mean(Comm, na.rm=TRUE),2), round(mean(Positivity, na.rm=TRUE),2), round(mean(satisfaction_1, na.rm=TRUE),2),  round(mean(effort_2, na.rm=TRUE),2),	round(mean(nps, na.rm=TRUE),2))
ROLE_means <- add_column(ROLE_means, Factor = "ROLE", .before = 1)
ROLE_means <- ROLE_means %>% rename(Level = ROLE)
ROLE_means <- add_column(ROLE_means, Count = (count(ROLE_group)[[2]]), .after = 2)
ROLE_means <- ROLE_means %>% filter(Level != "NA")
nps_score_ROLE <- ROLE_group %>% group_by(ROLE, nps_NPS_GROUP) %>% count(nps_NPS_GROUP, .drop = FALSE) %>% filter(nps_NPS_GROUP != "NA")# & nps_NPS_GROUP != "NA")
nps_spread <- nps_score_ROLE %>% spread(nps_NPS_GROUP, n)  %>%
  filter(ROLE != "NA")
ROLE_nps <- nps_spread %>% rowwise() %>%  
  mutate(nps_calc = ((Promoter/sum(c(Detractor, Neutral, Promoter)))-(Detractor/sum(c(Detractor, Neutral, Promoter)))) *100) %>% 
  filter(ROLE != "NA")

write_csv(ROLE_nps, paste0(file1, "ROLE_nps.csv"))
ROLE_nps_pdf <- Pandoc$new("","","")
ROLE_nps_pdf$add(ROLE_nps)
ROLE_nps_pdf$export(paste0(file2, "ROLE_nps"))


LOCATION_means <- summarise(LOCATION_group, round(mean(CPI_Index, na.rm=TRUE),2), round(mean(DIRECTION, na.rm=TRUE),2),	round(mean(OPERATIONS, na.rm=TRUE),2),	round(mean(PEOPLE, na.rm=TRUE),2),	round(mean(ENGAGEMENT, na.rm=TRUE),2),	round(mean(VisionValues, na.rm=TRUE),2),	round(mean(Strategy, na.rm=TRUE),2),	round(mean(Leadership, na.rm=TRUE),2),	round(mean(Adaptability, na.rm=TRUE),2),	round(mean(PerfMan, na.rm=TRUE),2),	round(mean(SysProc, na.rm=TRUE),2),	round(mean(Teamwork, na.rm=TRUE),2),	round(mean(TalMan, na.rm=TRUE),2),	round(mean(CoachDev, na.rm=TRUE),2),	round(mean(EmpClarFit, na.rm=TRUE),2),	round(mean(CustFoc, na.rm=TRUE),2),	round(mean(Comm, na.rm=TRUE),2), round(mean(Positivity, na.rm=TRUE),2), round(mean(satisfaction_1, na.rm=TRUE),2),  round(mean(effort_2, na.rm=TRUE),2),	round(mean(nps, na.rm=TRUE),2))
LOCATION_means <- add_column(LOCATION_means, Factor = "LOCATION", .before = 1)
LOCATION_means <- LOCATION_means %>% rename(Level = LOCATION)
LOCATION_means <- add_column(LOCATION_means, Count = (count(LOCATION_group)[[2]]), .after = 2)
LOCATION_means <- LOCATION_means %>% filter(Level != "NA")
nps_score_LOCATION <- LOCATION_group %>% group_by(LOCATION, nps_NPS_GROUP) %>% count(nps_NPS_GROUP, .drop = FALSE) %>% filter(nps_NPS_GROUP != "NA")# & nps_NPS_GROUP != "NA")
nps_spread <- nps_score_LOCATION %>% spread(nps_NPS_GROUP, n)  %>%
  filter(LOCATION != "NA")
LOCATION_nps <- nps_spread %>% rowwise() %>%  
  mutate(nps_calc = ((Promoter/sum(c(Detractor, Neutral, Promoter)))-(Detractor/sum(c(Detractor, Neutral, Promoter)))) *100) %>% 
  filter(LOCATION != "NA")

write_csv(LOCATION_nps, paste0(file1, "LOCATION_nps.csv"))
LOCATION_nps_pdf <- Pandoc$new("","","")
LOCATION_nps_pdf$add(LOCATION_nps)
LOCATION_nps_pdf$export(paste0(file2, "LOCATION_nps"))


means <- bind_rows(ALL_means, TENURE_means, GENERATION_means, ROLE_means, LOCATION_means)

write_csv(means, paste0(file1, "means.csv"))
