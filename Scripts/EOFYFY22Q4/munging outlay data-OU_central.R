g# PURPOSE: Munge and Analysis of Central Mechanisms
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2022-11-30
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(extrafont)
    library(tidytext)
    library(here)
    library(gt)
    library(janitor)
library(glue)
    
    
  
  # Set paths  
# folder set up
# Path where you want to save your PDF outputs, log files, and temp files
save_dir <- "C:/Users/bkasdan/Documents/GitHub/i-need-a-dollar/Images"
# Add path to where folders should be placed
OU <- "central"
OU <- glue("{save_dir}/{OU}")


# Build necessary directories if they are not present
dir.create(OU, showWarning=F)
   
    si_paths 
    ref_id <- "aa98349c"
    
  # Functions  
  
    percent_clean <- function(x, y) {
      ifelse(y > 0.000, (x / y), NA_real_)
    }

# LOAD DATA ============================================================================  

    budget<-list.files("~/Data/Budget Files/FY22Q4 EOFY/Central Workbooks for Visuals",full.names = TRUE) #can try and automate via google folder
    glimpse(budget)


    # MUNGE ============================================================================
    get_be_central<-function(df){
      df<-read_xlsx(df, 
                    sheet = "OUMechanisms", skip = 2)%>%
        clean_names()%>%
        select(c(operating_unit, last_active_cop,previous_cop_total_planned_funding,total_outlays_during_previous_fy,delta_between_plan_and_previous_fy_outlays ))%>%
        mutate(funding_agency="USAID-Central")
      df<-df%>%
        mutate(mechs_overspend=delta_between_plan_and_previous_fy_outlays)%>%
        mutate(mechs_overspend = ifelse(delta_between_plan_and_previous_fy_outlays >=0, "0",
                                        ifelse(delta_between_plan_and_previous_fy_outlays<0, "1",NA)))%>%
        
        mutate(mechs_overspend=as.numeric(mechs_overspend))%>%
        group_by(operating_unit,funding_agency)%>%
        summarise_at(vars(previous_cop_total_planned_funding,total_outlays_during_previous_fy,delta_between_plan_and_previous_fy_outlays, mechs_overspend,), sum, na.rm = TRUE)
      df<-df%>%
        mutate("unoutlayed"=previous_cop_total_planned_funding-total_outlays_during_previous_fy)%>%
        mutate(budget_execution=percent_clean(total_outlays_during_previous_fy,previous_cop_total_planned_funding))%>%
        ungroup()
      
      return(df)
    }
    
  
# VIZ ============================================================================

    df_cent <- purrr::map_dfr(.x = budget,
                          .f = ~ get_be_central(.x)) 

# SPINDOWN ============================================================================
    target_location <- gs4_get(" https://docs.google.com/spreadsheets/d/1FSnWTGbxw9Xu2M9vBd8JAiK5RcpFJ5eHjDCinhhNnH4/edit#gid=0")
    sheet_write(df_cent, target_location, sheet = "Central OU outlays_2022")
