# PURPOSE: Munge and Analysis of outlay data OU WCF
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2022-12-01
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(tidyverse)
    library(glitr)
    library(glamr)
    library(gophr)
    library(extrafont)
    library(scales)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library (janitor)
    
    
  
     # folder set up
     # Path where you want to save your PDF outputs, log files, and temp files
     save_dir <- "C:/Users/bkasdan/Documents/GitHub/i-need-a-dollar/Images"
     # Add path to where folders should be placed
     OU <- "WCF"
     OU <- glue("{save_dir}/{OU}")
     
     
     # Build necessary directories if they are not present
     dir.create(OU, showWarning=F)
   
    
    ref_id <- "341b53f1"
    
  # Functions  
    get_be_WCF<-function(df){
      df<-read_xlsx(df, 
                    sheet = "OUMechanisms", skip = 2)%>%
        clean_names()%>%
        select(c(operating_unit, last_active_cop,previous_cop_total_planned_funding,total_outlays_during_previous_fy,delta_between_plan_and_previous_fy_outlays ))%>%
        mutate(funding_agency="USAID-WCF")
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
  
    
    percent_clean <- function(x, y) {
      ifelse(y > 0.000, (x / y), NA_real_)
    }

# LOAD DATA ============================================================================  

    budget<-list.files("~/Data/Budget Files/FY22Q4 EOFY/WCF",full.names = TRUE) #can try and automate via google folder
    glimpse(budget)

# MUNGE ============================================================================
  
  df1 <- purrr::map_dfr(.x = budget,
                             .f = ~ get_be_WCF(.x))
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================
    target_location <- gs4_get(" https://docs.google.com/spreadsheets/d/1FSnWTGbxw9Xu2M9vBd8JAiK5RcpFJ5eHjDCinhhNnH4/edit#gid=0")
    sheet_write(df1, target_location, sheet = "WCF  OU outlays_2022")
