# RUN ANALYSES


# set libraries
library(tidyverse)
library(broom)
library(shiny)
library(Hmisc)


file <- paste0(filepath, "analyses and cleaning report/")
demo_vars <- c("Location", "Tenure", "Role", "Generation")
ANOVA_IVs <- demo_vars


# global vars used in all analyses
confirmatory_outcomes <- c("ENGAGEMENT", "EmpClarFit", "CustFoc", "Comm","nps", "Positivity", "satisfaction_1", "effort_2")
exploratory_outcomes <- c("DIRECTION", "OPERATIONS", "PEOPLE", "VisionValues", "Strategy", "Leadership", "Adaptability", "PerfMan", "SysProc", "Teamwork", "TalMan", "CoachDev")
confirmatory_predictors <- c("DIRECTION", "OPERATIONS", "PEOPLE", "VisionValues", "Strategy", "Leadership", "Adaptability", "PerfMan", "SysProc", "Teamwork", "TalMan", "CoachDev", "Tenure","Division","Level","Gender")
# analysis vars: lists for the IVs, DVs, demo groups, and set the df
dv<-c(exploratory_outcomes, confirmatory_outcomes)

####### Vars for this analysis: ONE-Factor ANOVA with no covariates.  loop vars and analysis vars

# set loop vars
dv_inner_loop = list()
demo_var_level_loop = list()
demo_vars_loop=list()
iv_level_loop=list()





###################################################### BEGIN FUNCTION ################################################
############## 3 outer loops

for(a in 1:length(ANOVA_IVs)){                                             # loop throught the list of IV vars
   IV<-ANOVA_IVs[a]
  
   
  #   iv_demo_diff <- setdiff(demo_groups, IV)                                 # for each IV this creates a list of demo vars to loop through
  #  
  # for (z in 1:length(iv_demo_diff)){                                         # loop through the demo vars
  #   demo <- iv_demo_diff[z]

    # for (l in 1:length(levels(df[[demo]]))) {                              #loop through each level of the demo var
    #   level <- levels(df[[demo]])[l]
    #   df_demo_cut <- df %>% 
    #          filter(get(demo)==level)
    #       
    #   if (length(unique(df_demo_cut[[eval(IV)]]))>1) {                     # make sure that the IV has at least 2 levels in this particular demo cut
    #   
          
          
  #######################################################INNER LOOP##########################################################################
          
          
          
        for(y in 1:length(dv)){                                            # analyses, plots, etc...loop through the dvs.  NOTE: I MAY COME BACK AND SEPARATE BY CONFIRMATORY VS EXPLORATORY BUT THAT IS MORE IMPORTANT FOR OTHER ANALYSES, BUT MIGHT AS WELL GET PRACTICE AND SET THE COLUMN HERE.
            
          #####
          # IV <- ANOVA_IVs[1]
          # y <-1
          
            DV <- dv[y]
            pars<-tidy(aov(get(DV) ~ get(IV), data = df))    ######  3 !!!!!!!!!!!!!!! analysis
            # plot <- list(ggplot(df_demo_cut, aes(x=get(IV), y=get(DV))) +  #plot
             # geom_boxplot())
            
            # create tibble and organize it
            pars_plot_desc <- tibble(IV = character(), df = integer(), sumsq = double(), meansq = double(), statistic = double(), 
                                     p.value = double(), DV = character()) #, plot = list())
            if ("term" %in% names(pars)) {pars_plot_desc[1,1]<- IV}
            if ("df" %in% names(pars)) {pars_plot_desc[1,2]<- pars[1,2]}
            if ("sumsq" %in% names(pars)) {pars_plot_desc[1,3]<- pars[1,3]}
            if ("meansq" %in% names(pars)) {pars_plot_desc[1,4]<- pars[1,4]}
            if ("statistic" %in% names(pars)) {pars_plot_desc[1,5]<- pars[1,5]}
            if ("p.value" %in% names(pars)) {pars_plot_desc[1,6]<- pars[1,6]}
            
            pars_plot_desc[1,7]<-DV
            # pars_plot_desc[1,8]<-"ANOVA"
            # pars_plot_desc[1,9]<-"no_cov"
            # pars_plot_desc[1,10]<-demo     #!!!!!!!1  4  $###!!!!!!!!!
            # pars_plot_desc[1,11]<-level
           # pars_plot_desc[1,12]<- list(plot)
            
            dv_inner_loop[[y]]=pars_plot_desc  # each loop makes a pars_plot_desc which becomes row number "y" in dv_inner_loop
            
        }     #closes inner loop dv
          
          
    ################################################ END ANALYSES, PLOT, DF, INNER LOOP #######################################      
          
          
    
          all_inner <- do.call(dplyr::bind_rows, dv_inner_loop) # this creates object to hold 1 level of a demo
          iv_level_loop[[a]]= all_inner   # all_inner becomes 1 object (row), row "l" of object, representing 1 demo level, that will be bound into object of the entire demo
 
}     #closes outermost loop for list of iv's using x

all_ivs <- do.call(dplyr::bind_rows, iv_level_loop)  # the final df...just ANOVAs for now
 all_ivs <- all_ivs %>% 
  mutate(IV = factor(IV), DV = factor(DV))

 all_ivs<- all_ivs[, c(1,7, 6)] %>% arrange(p.value)
 
 predict_nps <- tidy(lm(nps ~ VisionValues + Strategy + Leadership + Adaptability + PerfMan + SysProc + 
                          Teamwork + TalMan + CoachDev + EmpClarFit + CustFoc + Comm,  data=df)) %>%
                          arrange(p.value) %>% 
                          add_column(criterion = "nps", .before = 1)
 
 predict_satisfaction <- tidy(lm(nps ~ VisionValues + Strategy + Leadership + Adaptability + PerfMan + SysProc +
                                   Teamwork + TalMan +CoachDev +EmpClarFit + CustFoc + Comm,  data=df)) %>%
                                    arrange(p.value) %>% 
                                    add_column(criterion = "satisfaction", .before = 1)
 predict_effort <- tidy(lm(nps ~ VisionValues + Strategy + Leadership + Adaptability + PerfMan + SysProc +
                             Teamwork + TalMan +CoachDev +EmpClarFit + CustFoc + Comm,  data=df)) %>%
                              arrange(p.value) %>% 
                              add_column(criterion = "effort", .before = 1)
 
 predictors <- bind_rows(predict_nps, predict_satisfaction, predict_effort)
 
 
 
 saveRDS(predictors, file = paste0(file,"regressions.rds"))
 write_csv(predictors, paste0(file,"regressions.csv"))
 predictors
 
# saveRDS(all_ivs, file = "C:/Users/Brent/Dropbox/6.25.19/view_analyses/analyses.rds")
# saveRDS(all_ivs, file = "~/Dropbox/Clients/Mays/view_analyses/analyses.rds")
# write_csv(all_ivs, "~/Dropbox/Clients/Mays/analyses/anovas.csv")
# runApp("C:/Users/Brent/Dropbox/6.25.19/view_analyses")
# runApp("~/Dropbox/Clients/Mays/view_analyses")
 
 saveRDS(all_ivs, file = paste0(file,"main anovas.rds"))
 write_csv(all_ivs, paste0(file,"main anovas.csv"))
 # runApp("C:/Users/Brent/Dropbox/6.25.19/view_analyses")
 #runApp("C:/Users/Brent/Dropbox/Clients/Mays/view_analyses")
 