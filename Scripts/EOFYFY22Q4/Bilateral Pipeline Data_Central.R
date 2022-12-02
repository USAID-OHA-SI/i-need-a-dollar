# PURPOSE: Munge and Analysis of Central Pipeline Data
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2022-12-01
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
library(tidyverse)
library(gophr)
library(scales)
library(extrafont)
library(tidytext)
library(here)
library(gt)
library(googledrive)
library(googlesheets4)
library(janitor)
library(readxl)
library(data.table)
library(glue)
  
# Functions  

save_dir <- "C:/Users/bkasdan/Documents/GitHub/i-need-a-dollar/Images"
# Add path to where folders should be placed
OU <- "central"
OU <- glue("{save_dir}/{OU}")
# Build necessary directories if they are not present
dir.create(OU, showWarning=F)
   
    si_paths 
    ref_id <- "02f119d4"
    
 
    
    
    percent_clean <- function(x, y) {
      ifelse(y > 0.000, (x / y), NA_real_)
    }
    
    
    # Functions  
    #weirdly I have to open up a file and re-copy in the Funds on CODB line each time for this to work
    filters<-c("Operating Unit",
               "Funding Agency",
               "FY 2004-2022 Q4 Total Pipeline (to include unobligated and unliquidated obligation funding)",
               "Approved Funds in COP matrix not yet transferred",
               "Unliquidated Obligations (ULO) / Obligations Pending Outlay (OPO): Expired Award on Non-Expired Fund Accounts (Untouchable Pipeline)",
               "ULO / OPO: Expired Awards on Expired Fund Accounts as of the Last calendar day of Previous FY (Untouchable Pipeline)",
               "Funds on CODB obligations that have not been fully outlaid and can't be used to support Current COP approved activities (Untouchable Pipeline)",
               "Additional Outlays for Current COP: Other outlays approved by S/GAC but not relfected in COP Matrix (This is not a request field.)",
               "Other ULO / OPO",
               "Other Untouchable Unobligated",
               "Total Untouchable Pipeline",
               "Total Current COP Approved Amount",
               "Total Projected Current FY Outlays",
               "Projected Remaining Pipeline at Current FY Close",
               "Months of Allowable Pipeline Needed",
               "Total Allowable Buffer Pipeline",
               "New Adjusted Pipeline",
               "Adjusted Excess Pipeline",
               "Notes",
               "Further work in OU intended?")
    
    
    
    
    
    
    budget_central<-list.files("~/Data/Budget Files/FY22Q4 EOFY/Central Workbooks for Visuals",full.names = TRUE)
    get_ou_pipeline_central<-function(df){
      df<-read_xlsx(df, 
                    sheet = "OU")
      df<-df%>%
        dplyr::filter(Attribute %in% filters)%>%
        rename_at(vars(contains('Data Entry')), ~ 'data_entry')%>%
        select(c("Attribute","data_entry"))
      
      
      df<-t(df)%>%as.data.frame()
      colnames(df) <- filters
      df=df[-1,]
      
      df<-df%>%
        clean_names()%>%
        mutate_at(c(3:19), as.numeric)%>%
        
        # rename("pipeline_available_at_fy22_end"=projected_remaining_pipeline_at_current_fy_close)%>%
        # rename("cop23_applied_pipeline"=adjusted_excess_pipeline)%>%
        # mutate(ulos_on_expired_awards=as.numeric(ulos_on_expired_awards))%>%
        # mutate(ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline=as.numeric(ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline))%>%
        # mutate("pipeline_in_expired_awards"=ulos_on_expired_awards+ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline )%>%
        mutate("pipeline_in_expired_awards"=unliquidated_obligations_ulo_obligations_pending_outlay_opo_expired_award_on_non_expired_fund_accounts_untouchable_pipeline )%>%
        rename("expired award_expired_accounts"=ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline)%>%
        mutate("total_expired_awards"= `expired award_expired_accounts`+unliquidated_obligations_ulo_obligations_pending_outlay_opo_expired_award_on_non_expired_fund_accounts_untouchable_pipeline)%>%
        rename("codb_untouchable_pipeline"=funds_on_codb_obligations_that_have_not_been_fully_outlaid_and_can_t_be_used_to_support_current_cop_approved_activities_untouchable_pipeline)%>%
        dplyr::relocate(pipeline_in_expired_awards, .before = notes)%>%
        dplyr::relocate(total_expired_awards, .after=`expired award_expired_accounts`)%>%
        mutate(funding_agency="USAID-central")%>%
        rename("fy23_startup_pipeline"= fy_2004_2022_q4_total_pipeline_to_include_unobligated_and_unliquidated_obligation_funding)
      
      return(df)
      
    }
    
    df_ou_pipeline_central <- purrr::map_dfr(.x = budget_central,
                                             .f = ~ get_ou_pipeline_central(.x))

# LOAD DATA ============================================================================  

# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

    target_location <- gs4_get(" https://docs.google.com/spreadsheets/d/1FSnWTGbxw9Xu2M9vBd8JAiK5RcpFJ5eHjDCinhhNnH4/edit#gid=0")
    sheet_write(df_ou_pipeline_central, target_location, sheet =  "Central OU pipeline_2022")
    