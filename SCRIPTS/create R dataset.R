## set the company name here.  should match directory name also

company_name <- "CPI report"

##########################################################

wd <- paste0("~/Dropbox/R/", eval(company_name))
setwd(eval(wd))
library(tidyverse)
library(forcats)
library(lubridate)


columnsSG <- c("ResponseId",	"StartDate",	"EndDate",	"Status",	"Contact ID",	"Legacy Comments",	"Comments",	"Language",	"Referer",	"SessionID",	"User Agent",	"Tags",	"IP Address",	"Longitude",	"Latitude",	"Country",	"City",	"State/Region",	"Postal",
               "DIVISION",	"LOCATION",	"TENURE",	"GENERATION",	"ROLE",	"cpi_vv_1",	"cpi_vv_2",	"cpi_vv_3",	"cpi_vv_4",	"cpi_vv_5",	"cpi_strat_1",	"cpi_strat_2",	"cpi_strat_3",	"cpi_strat_4",	"cpi_strat_5",	"cpi_lead_1",	"cpi_lead_2",	"cpi_lead_3",	"cpi_lead_4",	"cpi_lead_5",	"cpi_adapt_1",	"cpi_adapt_2",	"cpi_adapt_3",	"cpi_adapt_4",	"cpi_adapt_5",	"cpi_perfman_1",	"cpi_perfman_2",	"cpi_perfman_3",	"cpi_perfman_4",	"cpi_perfman_5",	"cpi_sysproc_1",	"cpi_sysproc_2",	"cpi_sysproc_3",	"cpi_sysproc_4",	"cpi_sysproc_5",	"cpi_team_1",	"cpi_team_2",	"cpi_team_3",	"cpi_team_4",	"cpi_team_5",	"cpi_talman_1",	"cpi_talman_2",	"cpi_talman_3",	"cpi_talman_4",	"cpi_talman_5",	"cpi_cd_1",	"cpi_cd_2",	"cpi_cd_3",	"cpi_cd_4",	"cpi_cd_5",	"cpi_ecf_1",	"cpi_ecf_2",	"cpi_ecf_3",	"cpi_ecf_4",	"cpi_ecf_5",	"cpi_custfoc_1",	"cpi_custfoc_2",	"cpi_custfoc_3",	"cpi_custfoc_4",	"cpi_custfoc_5",	"cpi_comm_1",	"cpi_comm_2",	"cpi_comm_3",	"cpi_comm_4",	"cpi_comm_5",	
               "positivity_1",	"positivity_2",	"positivity_3",	"positivity_4",	"positivity_5",	"satisfaction_1",	"effort_2",	"nps","change", "Duration (in seconds)",	"nps_NPS_GROUP", "Company")


###################################### upload raw data from SG and QUALTRICS ###############################################

SG <- read_csv(paste0(wd,"/sgizmo download/SG-Ergon.csv"))


#################################### MAKE CHANGES TO TOP LINES  ###################################################

SG <- add_column(SG, yy = 0, zz = 200, Company = eval(company_name))
colnames(SG) <- columnsSG

### double check which values to fileter.  e.g. take out test data.  In this example, the numbers 18 and below are removed.
data <- SG %>% filter(ResponseId > 18) %>%
  select(ResponseId, Company, DIVISION:nps, "Duration (in seconds)",	nps_NPS_GROUP)
data[, c("ResponseId",	"nps", "Duration (in seconds)", "nps_NPS_GROUP") ]<- lapply(SG[,c("ResponseId",	"nps", "Duration (in seconds)", "nps_NPS_GROUP")], as.character)

# SG[, c("ResponseId") ]<- lapply(SG[,c("ResponseId")], as.character)
# SG[, c("StartDate") ]<- lapply(SG[,c("StartDate")], as.Date.POSIXct)


data <- add_column(data, ALL = "All")

data <- data %>% mutate(cpi_adapt_1 = recode(cpi_adapt_1, "Strongly Disagree" = "Strongly Agree",
                                               "Disagree" = "Agree",
                                               "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                               "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                               "Agree" = "Disagree",
                                               "Strongly Agree" = "Strongly Disagree"),
                          cpi_adapt_3 = recode(cpi_adapt_3, "Strongly Disagree" = "Strongly Agree",
                                               "Disagree" = "Agree",
                                               "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                               "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                               "Agree" = "Disagree",
                                               "Strongly Agree" = "Strongly Disagree"),
                          cpi_sysproc_1 = recode(cpi_sysproc_1, "Strongly Disagree" = "Strongly Agree",
                                                 "Disagree" = "Agree",
                                                 "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                 "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                                 "Agree" = "Disagree",
                                                 "Strongly Agree" = "Strongly Disagree"),
                          cpi_sysproc_2 = recode(cpi_sysproc_2, "Strongly Disagree" = "Strongly Agree",
                                                 "Disagree" = "Agree",
                                                 "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                 "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                                 "Agree" = "Disagree",
                                                 "Strongly Agree" = "Strongly Disagree"),
                          cpi_sysproc_3 = recode(cpi_sysproc_3, "Strongly Disagree" = "Strongly Agree",
                                                 "Disagree" = "Agree",
                                                 "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                 "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                                 "Agree" = "Disagree",
                                                 "Strongly Agree" = "Strongly Disagree"),
                          cpi_talman_4 = recode(cpi_talman_4, "Strongly Disagree" = "Strongly Agree",
                                                "Disagree" = "Agree",
                                                "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                                "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                                "Agree" = "Disagree",
                                                "Strongly Agree" = "Strongly Disagree"),
                          cpi_ecf_2 = recode(cpi_ecf_2, "Strongly Disagree" = "Strongly Agree",
                                             "Disagree" = "Agree",
                                             "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                             "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                             "Agree" = "Disagree",
                                             "Strongly Agree" = "Strongly Disagree"),
                          cpi_comm_5 = recode(cpi_comm_5, "Strongly Disagree" = "Strongly Agree",
                                              "Disagree" = "Agree",
                                              "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                              "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                              "Agree" = "Disagree",
                                              "Strongly Agree" = "Strongly Disagree")
)

data <- data %>% mutate(
  cpi_vv_1 = recode(cpi_vv_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_vv_2 = recode(cpi_vv_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_vv_3 = recode(cpi_vv_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_vv_4 = recode(cpi_vv_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_vv_5 = recode(cpi_vv_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_1 = recode(cpi_strat_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_2 = recode(cpi_strat_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_3 = recode(cpi_strat_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_4 = recode(cpi_strat_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_strat_5 = recode(cpi_strat_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_1 = recode(cpi_lead_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_2 = recode(cpi_lead_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_3 = recode(cpi_lead_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_4 = recode(cpi_lead_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_lead_5 = recode(cpi_lead_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_1 = recode(cpi_adapt_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_2 = recode(cpi_adapt_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_3 = recode(cpi_adapt_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_4 = recode(cpi_adapt_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_adapt_5 = recode(cpi_adapt_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_1 = recode(cpi_perfman_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_2 = recode(cpi_perfman_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_3 = recode(cpi_perfman_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_4 = recode(cpi_perfman_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_perfman_5 = recode(cpi_perfman_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_1 = recode(cpi_sysproc_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_2 = recode(cpi_sysproc_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_3 = recode(cpi_sysproc_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_4 = recode(cpi_sysproc_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_sysproc_5 = recode(cpi_sysproc_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_1 = recode(cpi_team_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_2 = recode(cpi_team_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_3 = recode(cpi_team_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_4 = recode(cpi_team_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_team_5 = recode(cpi_team_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_1 = recode(cpi_talman_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_2 = recode(cpi_talman_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_3 = recode(cpi_talman_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_4 = recode(cpi_talman_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_talman_5 = recode(cpi_talman_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_1 = recode(cpi_cd_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_2 = recode(cpi_cd_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_3 = recode(cpi_cd_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_4 = recode(cpi_cd_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_cd_5 = recode(cpi_cd_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_1 = recode(cpi_ecf_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_2 = recode(cpi_ecf_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_3 = recode(cpi_ecf_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_4 = recode(cpi_ecf_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_ecf_5 = recode(cpi_ecf_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_1 = recode(cpi_custfoc_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_2 = recode(cpi_custfoc_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_3 = recode(cpi_custfoc_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_4 = recode(cpi_custfoc_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_custfoc_5 = recode(cpi_custfoc_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_1 = recode(cpi_comm_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_2 = recode(cpi_comm_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_3 = recode(cpi_comm_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_4 = recode(cpi_comm_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  cpi_comm_5 = recode(cpi_comm_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_1 = recode(positivity_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_2 = recode(positivity_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_3 = recode(positivity_3, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_4 = recode(positivity_4, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  positivity_5 = recode(positivity_5, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  satisfaction_1 = recode(satisfaction_1, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"),
  effort_2 = recode(effort_2, "Strongly Disagree" = "0", "Disagree" = "25", "Neither Agree nor Disagree" = "50", "Neither Agree Nor Disagree" = "50", "Agree" = "75", "Strongly Agree" = "100"))


# RECODE TO NUMER
data <- data %>% mutate(cpi_vv_1 = as.integer(cpi_vv_1),cpi_vv_2 = as.integer(cpi_vv_2),cpi_vv_3 = as.integer(cpi_vv_3),cpi_vv_4 = as.integer(cpi_vv_4),cpi_vv_5 = as.integer(cpi_vv_5),cpi_strat_1 = as.integer(cpi_strat_1),cpi_strat_2 = as.integer(cpi_strat_2),cpi_strat_3 = as.integer(cpi_strat_3),cpi_strat_4 = as.integer(cpi_strat_4),cpi_strat_5 = as.integer(cpi_strat_5),cpi_lead_1 = as.integer(cpi_lead_1),cpi_lead_2 = as.integer(cpi_lead_2),cpi_lead_3 = as.integer(cpi_lead_3),cpi_lead_4 = as.integer(cpi_lead_4),cpi_lead_5 = as.integer(cpi_lead_5),cpi_adapt_1 = as.integer(cpi_adapt_1),cpi_adapt_2 = as.integer(cpi_adapt_2),cpi_adapt_3 = as.integer(cpi_adapt_3),cpi_adapt_4 = as.integer(cpi_adapt_4),cpi_adapt_5 = as.integer(cpi_adapt_5),cpi_perfman_1 = as.integer(cpi_perfman_1),cpi_perfman_2 = as.integer(cpi_perfman_2),cpi_perfman_3 = as.integer(cpi_perfman_3),cpi_perfman_4 = as.integer(cpi_perfman_4),cpi_perfman_5 = as.integer(cpi_perfman_5),cpi_sysproc_1 = as.integer(cpi_sysproc_1),cpi_sysproc_2 = as.integer(cpi_sysproc_2),cpi_sysproc_3 = as.integer(cpi_sysproc_3),cpi_sysproc_4 = as.integer(cpi_sysproc_4),cpi_sysproc_5 = as.integer(cpi_sysproc_5),cpi_team_1 = as.integer(cpi_team_1),cpi_team_2 = as.integer(cpi_team_2),cpi_team_3 = as.integer(cpi_team_3),cpi_team_4 = as.integer(cpi_team_4),cpi_team_5 = as.integer(cpi_team_5),cpi_talman_1 = as.integer(cpi_talman_1),cpi_talman_2 = as.integer(cpi_talman_2),cpi_talman_3 = as.integer(cpi_talman_3),cpi_talman_4 = as.integer(cpi_talman_4),cpi_talman_5 = as.integer(cpi_talman_5),cpi_cd_1 = as.integer(cpi_cd_1),cpi_cd_2 = as.integer(cpi_cd_2),cpi_cd_3 = as.integer(cpi_cd_3),cpi_cd_4 = as.integer(cpi_cd_4),cpi_cd_5 = as.integer(cpi_cd_5),cpi_ecf_1 = as.integer(cpi_ecf_1),cpi_ecf_2 = as.integer(cpi_ecf_2),cpi_ecf_3 = as.integer(cpi_ecf_3),cpi_ecf_4 = as.integer(cpi_ecf_4),cpi_ecf_5 = as.integer(cpi_ecf_5),cpi_custfoc_1 = as.integer(cpi_custfoc_1),cpi_custfoc_2 = as.integer(cpi_custfoc_2),cpi_custfoc_3 = as.integer(cpi_custfoc_3),cpi_custfoc_4 = as.integer(cpi_custfoc_4),cpi_custfoc_5 = as.integer(cpi_custfoc_5),cpi_comm_1 = as.integer(cpi_comm_1),cpi_comm_2 = as.integer(cpi_comm_2),cpi_comm_3 = as.integer(cpi_comm_3),cpi_comm_4 = as.integer(cpi_comm_4),cpi_comm_5 = as.integer(cpi_comm_5),positivity_1 = as.integer(positivity_1),positivity_2 = as.integer(positivity_2),positivity_3 = as.integer(positivity_3),positivity_4 = as.integer(positivity_4),positivity_5 = as.integer(positivity_5),satisfaction_1 = as.integer(satisfaction_1),effort_2 = as.integer(effort_2), nps = as.integer(nps)
)
## RECODE FACTORS
DIVISIONLevs <- c("Asphalt + Emulsions (i.e., Ergon Asphalt, Paragon Technical Services, etc.)",
             "Construction + Real Estate (i.e., Alliant Construction, ISO Services, Ergon Maintenance Services, etc.)",
             "Corporate + Other",
             "Crafco",
             "Midstream + Logistics (i.e., Terminaling, Oil Purchasing, Trucking, Magnolia Marine, EMIS)",
             "Oil + Gas (i.e., Lampton-Love Inc., Ergon Exploration, Ergon Production, etc.)",
             "Refining + Marketing",
             "Specialty Chemicals (Resinall)")
data <- data %>% mutate(DIVISION = factor(DIVISION, levels = DIVISIONLevs))


LOCATIONLevs <- c("Asia", "Europe", "Latin America", "US - Midwest", "US - Jackson MS", "US - Northeastern", "US - Southeastern", "US - West/Southwest")
data <- data %>% mutate(LOCATION = factor(LOCATION, levels = LOCATIONLevs))

TENURELevs <- c("Less than a year", "1-4 years", "4-8 years", "8-12 years", "12+ years")
data <- data %>% mutate(TENURE = factor(TENURE, levels = TENURELevs))

ROLELevs <- c("Admin + Corporate Support", "Board of Directors", "Management", "Finance + Accounting", "Operations", "Sales + Marketing", "Health, Safety + Environment", 
              "Information Technology", "Research + Technology Development")
data <- data %>% mutate(ROLE = factor(ROLE, levels = ROLELevs))

GENERATIONLevs <- c("Greatest Generation (born between 1925 - 1945)", "Baby Boomer (born between 1946 - 1965)", "Generation X  (born between 1966 - 1980)", "Generation Y (born between 1981 - 2000)", "Generation Z (born between 2001 - 2020)")
data <- data %>% mutate(GENERATION = factor(GENERATION, levels = GENERATIONLevs))

data <- data %>% mutate(nps_NPS_GROUP = factor(nps)) # create NPS groups
data <- data %>% mutate(nps_NPS_GROUP = fct_collapse(nps_NPS_GROUP, "Detractor" = c("0","1","2","3","4","5","6"), "Neutral"=c("7","8"),  "Promoter"=c("9","10")))

## CALUCULATE MEANS

data <- data %>% rowwise() %>%  
  mutate(CPI_Index = mean(c(cpi_vv_1, 	cpi_vv_2, 	cpi_vv_3, 	cpi_vv_4, 	cpi_vv_5, 	
                            cpi_strat_1, 	cpi_strat_2, 	cpi_strat_3, 	cpi_strat_4, 	cpi_strat_5, 	
                            cpi_lead_1, 	cpi_lead_2, 	cpi_lead_3, 	cpi_lead_4, 	cpi_lead_5, 	
                            cpi_adapt_1, 	cpi_adapt_2, 	cpi_adapt_3, 	cpi_adapt_4, 	cpi_adapt_5, 	
                            cpi_perfman_1, 	cpi_perfman_2, 	cpi_perfman_3, 	cpi_perfman_4, 	cpi_perfman_5, 	
                            cpi_sysproc_1, 	cpi_sysproc_2, 	cpi_sysproc_3, 	cpi_sysproc_4, 	cpi_sysproc_5, 	
                            cpi_team_1, 	cpi_team_2, 	cpi_team_3, 	cpi_team_4, 	cpi_team_5, 	
                            cpi_talman_1, 	cpi_talman_2, 	cpi_talman_3, 	cpi_talman_4, 	cpi_talman_5, 	
                            cpi_cd_1, 	cpi_cd_2, 	cpi_cd_3, 	cpi_cd_4, 	cpi_cd_5, 	
                            cpi_ecf_1, 	cpi_ecf_2, 	cpi_ecf_3, 	cpi_ecf_4, 	cpi_ecf_5, 	
                            cpi_custfoc_1, 	cpi_custfoc_2, 	cpi_custfoc_3, 	cpi_custfoc_4, 	cpi_custfoc_5, 	
                            cpi_comm_1, 	cpi_comm_2, 	cpi_comm_3, 	cpi_comm_4, 	cpi_comm_5)),
         DIRECTION = mean(c(cpi_vv_1, 	cpi_vv_2, 	cpi_vv_3, 	cpi_vv_4, 	cpi_vv_5, 	
                            cpi_strat_1, 	cpi_strat_2, 	cpi_strat_3, 	cpi_strat_4, 	cpi_strat_5, 	
                            cpi_lead_1, 	cpi_lead_2, 	cpi_lead_3, 	cpi_lead_4, 	cpi_lead_5)),
         OPERATIONS = mean(c( cpi_adapt_1, 	cpi_adapt_2, 	cpi_adapt_3, 	cpi_adapt_4, 	cpi_adapt_5, 	
                              cpi_perfman_1, 	cpi_perfman_2, 	cpi_perfman_3, 	cpi_perfman_4, 	cpi_perfman_5, 	
                              cpi_sysproc_1, 	cpi_sysproc_2, 	cpi_sysproc_3, 	cpi_sysproc_4, 	cpi_sysproc_5)),
         PEOPLE = mean(c(cpi_team_1, 	cpi_team_2, 	cpi_team_3, 	cpi_team_4, 	cpi_team_5, 	
                         cpi_talman_1, 	cpi_talman_2, 	cpi_talman_3, 	cpi_talman_4, 	cpi_talman_5, 	
                         cpi_cd_1, 	cpi_cd_2, 	cpi_cd_3, 	cpi_cd_4, 	cpi_cd_5)),
         ENGAGEMENT = mean(c(cpi_ecf_1, 	cpi_ecf_2, 	cpi_ecf_3, 	cpi_ecf_4, 	cpi_ecf_5, 	
                             cpi_custfoc_1, 	cpi_custfoc_2, 	cpi_custfoc_3, 	cpi_custfoc_4, 	cpi_custfoc_5, 	
                             cpi_comm_1, 	cpi_comm_2, 	cpi_comm_3, 	cpi_comm_4, 	cpi_comm_5)),
         VisionValues = mean(c(cpi_vv_1, 	cpi_vv_2, 	cpi_vv_3, 	cpi_vv_4, 	cpi_vv_5)), 	
         Strategy = mean(c(           cpi_strat_1, 	cpi_strat_2, 	cpi_strat_3, 	cpi_strat_4, 	cpi_strat_5)), 	
         Leadership = mean(c(          cpi_lead_1, 	cpi_lead_2, 	cpi_lead_3, 	cpi_lead_4, 	cpi_lead_5)),
         Adaptability = mean(c( cpi_adapt_1, 	cpi_adapt_2, 	cpi_adapt_3, 	cpi_adapt_4, 	cpi_adapt_5)), 	
         PerfMan = mean(c(         cpi_perfman_1, 	cpi_perfman_2, 	cpi_perfman_3, 	cpi_perfman_4, 	cpi_perfman_5)), 	
         SysProc = mean(c(         cpi_sysproc_1, 	cpi_sysproc_2, 	cpi_sysproc_3, 	cpi_sysproc_4, 	cpi_sysproc_5)),
         Teamwork = mean(c(cpi_team_1, 	cpi_team_2, 	cpi_team_3, 	cpi_team_4, 	cpi_team_5)), 	
         TalMan = mean(c(         cpi_talman_1, 	cpi_talman_2, 	cpi_talman_3, 	cpi_talman_4, 	cpi_talman_5)), 	
         CoachDev = mean(c(            cpi_cd_1, 	cpi_cd_2, 	cpi_cd_3, 	cpi_cd_4, 	cpi_cd_5)),
         EmpClarFit = mean(c(cpi_ecf_1, 	cpi_ecf_2, 	cpi_ecf_3, 	cpi_ecf_4, 	cpi_ecf_5)), 	
         CustFoc = mean(c(                 cpi_custfoc_1, 	cpi_custfoc_2, 	cpi_custfoc_3, 	cpi_custfoc_4, 	cpi_custfoc_5)), 	
         Comm = mean(c(             cpi_comm_1, 	cpi_comm_2, 	cpi_comm_3, 	cpi_comm_4, 	cpi_comm_5)),
         Positivity = mean(c(positivity_1, positivity_2, positivity_3, positivity_4, positivity_5)))



##############################  $$$$$$$$$$$$$$$$$  ^^^^^^^^^^^^^^^^^^6  &&&&&&&&&&&&&&&&&&&

source(paste0(wd,"SCRIPTS/themes.R"))
source(paste0(wd,"SCRIPTS/plot script index.R"))
source(paste0(wd,"SCRIPTS/app.old.R"))
source(paste0(wd,"SCRIPTS/means.R"))
source(paste0(wd,"SCRIPTS/combine pdfs.R"))

   
