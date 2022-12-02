# PURPOSE: Munge and Analysis of Bilat Pipeline
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2022-11-29
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
  
  # Set paths  
save_dir <- "C:/Users/bkasdan/Documents/GitHub/i-need-a-dollar/Images"
# Add path to where folders should be placed
OU <- "bilateral"
OU <- glue("{save_dir}/{OU}")


# Build necessary directories if they are not present
dir.create(OU, showWarning=F)
   
    si_paths 
    
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
    
    
    percent_clean <- function(x, y) {
      ifelse(y > 0.000, (x / y), NA_real_)
    }
    
    
    budget<-list.files("~/Data/Budget Files/FY22Q4 EOFY/Bilateral",full.names = TRUE) #can try and automate via google folder
    glimpse(budget) 
    
    # budget_central<-list.files("~/Data/Budget Files/FY22Q4 EOFY/Central Workbooks for Visuals",full.names = TRUE)

# LOAD DATA ============================================================================  
    # #testing for one file to build the function 
    #    df <- read_excel("Data/Budget Files/FY22Q4 EOFY/Bilateral/Angola_FY22 EOFY (Bilateral) for Visuals.xlsx",sheet = "OU")
    #  df1<-df%>%
    #         dplyr::filter(Attribute %in% filters)%>%
    #         rename_at(vars(contains('Data Entry')), ~ 'data_entry')%>%
    #         select(c("Attribute","data_entry"))
    # 
    # 
    #       df2<-t(df1)%>%as.data.frame()
    #      colnames(df2) <- filters
    #  df3=df2[-1,]
    #  
    #  df3<-df3%>%
    #    clean_names()%>%
    #    mutate_at(c(3:16), as.numeric)%>%
    #     
    #    # rename("pipeline_available_at_fy22_end"=projected_remaining_pipeline_at_current_fy_close)%>%
    #    # rename("cop23_applied_pipeline"=adjusted_excess_pipeline)%>%
    #    # mutate(ulos_on_expired_awards=as.numeric(ulos_on_expired_awards))%>%
    #    # mutate(ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline=as.numeric(ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline))%>%
    #    # mutate("pipeline_in_expired_awards"=ulos_on_expired_awards+ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline )%>%
    #    mutate("pipeline_in_expired_awards"=unliquidated_obligations_ulo_obligations_pending_outlay_opo_expired_award_on_non_expired_fund_accounts_untouchable_pipeline )%>%
    #    rename("expired award_expired_accounts"=ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline)%>%
    #    rename("codb_untouchable_pipeline"=funds_on_codb_obligations_that_have_not_been_fully_outlaid_and_can_t_be_used_to_support_current_cop_approved_activities_untouchable_pipeline)%>%
    #    dplyr::relocate(pipeline_in_expired_awards, .before = notes)%>%
    # 
    #    rename("fy23_startup_pipeline"= fy_2004_2022_q4_total_pipeline_to_include_unobligated_and_unliquidated_obligation_funding)
    # 
    #  
    #  
     #function build
     
     get_ou_pipeline_bilat<-function(df){
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
         mutate(funding_agency="USAID-bilateral")%>%
         rename("fy23_startup_pipeline"= fy_2004_2022_q4_total_pipeline_to_include_unobligated_and_unliquidated_obligation_funding)
       
       return(df)
       
     }
    
    
   
  
 
    
    

# MUNGE ============================================================================
  
    df_ou_pipeline <- purrr::map_dfr(.x = budget,
                              .f = ~ get_ou_pipeline_bilat(.x)) 
     
    # adorn_totals("row",,,, -funding_agency,-operating_unit)%>%
# VIZ ============================================================================
    ## excess pipeline
    ref_id <- "db70b99a"
    
    df_excess<-df_ou_pipeline%>%
      adorn_totals("row",,,, -funding_agency,-operating_unit)%>%
      arrange(desc(adjusted_excess_pipeline)) %>%
      slice_max(adjusted_excess_pipeline,n=6)%>%
      select(operating_unit,fy23_startup_pipeline, total_untouchable_pipeline,total_projected_current_fy_outlays,total_allowable_buffer_pipeline,new_adjusted_pipeline,adjusted_excess_pipeline)
    
    df_excess<-df_excess%>%
      gt()%>%
      fmt_currency( # add dolar signs
        columns = c(fy23_startup_pipeline, total_untouchable_pipeline,total_projected_current_fy_outlays,total_allowable_buffer_pipeline,new_adjusted_pipeline,adjusted_excess_pipeline),
        decimals = 0,
        currency = "USD")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      cols_width(
        everything() ~ px(120))%>%
      cols_label(
        operating_unit = "Operating Unit",
        fy23_startup_pipeline ="Total Funds Avail on 10/1/22",
        total_untouchable_pipeline="Untouchable Pipeline",
        total_projected_current_fy_outlays="Projected Current COP22 Outlays",
        total_allowable_buffer_pipeline ="Buffer Pipeline",
        new_adjusted_pipeline = "New Adjusted Pipeline",
        adjusted_excess_pipeline="Adjusted Excess Pipeline",
        #pipeline_in_expired_awards="Expired Awards Pipeline"
      )%>%
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),) %>%
      
      tab_style(
        style = cell_borders(
          sides = "right",
          weight = px(1.5),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))%>%
      
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_align(
        align = "left",
        columns = tidyselect::contains("operating")
      )%>%
      tab_header(
        title = glue::glue(" COP21 USAID End of Fiscal Year Financial Performance Summary"),
        subtitle = "Top 5 OUs with Adjusted Excess Pipeline")%>%
      #gt::tab_source_note(
      # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | ref: {ref_id}"))
      )  %>%
      tab_footnote(
        footnote = "Total Gobal of USAID/PEPFAR",
        locations = cells_body(
          columns = vars("operating_unit"),
          rows = 1)
      )%>%
      tab_style(
        style = list(cell_text(weight = "bold")
        ),
        locations = cells_body(
          rows = 1
        )
      ) %>%
      opt_table_outline()%>%
      
      opt_row_striping(row_striping = TRUE)%>%
      gtsave(.,path=OU,filename = "FY22_excess_bilateral.png")
    
    
    #untouchable  pipeline graphic breakdown
    ref_id <- "3669e8a8"   
    df_pipe_top_10<-df_ou_pipeline%>%
      adorn_totals("row",,,, -funding_agency,-operating_unit)%>%
      arrange(desc(total_untouchable_pipeline)) %>%
      slice_max(total_untouchable_pipeline,n=5)%>%
      select(operating_unit,total_untouchable_pipeline,codb_untouchable_pipeline,total_expired_awards,other_ulo_opo)
    
    
    df_pipe_top_10<- df_pipe_top_10%>%
      gt()%>%
      fmt_currency( # add dolar signs
        columns = c(total_untouchable_pipeline,codb_untouchable_pipeline,total_expired_awards,other_ulo_opo ),
        decimals = 0,
        currency = "USD")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      cols_width(
        everything() ~ px(120))%>%
      cols_label(
        operating_unit = "Operating Unit",
        total_untouchable_pipeline="Total Untouchable Pipeline",
        codb_untouchable_pipeline="CODB",
        total_expired_awards="Total Expired Awards",
        other_ulo_opo="Other ULO/OPO"
      )%>%
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),) %>%
      
      tab_style(
        style = cell_borders(
          sides = "right",
          weight = px(1.5),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))%>%
      
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_align(
        align = "left",
        columns = tidyselect::contains("operating")
      )%>%
      tab_header(
        title = glue::glue(" COP21 USAID End of Fiscal Year Financial Performance Summary"),
        subtitle = "OUs with untouchable pipeline above $20 million")%>%
      #gt::tab_source_note(
      # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | ref: {ref_id}"))
      )  %>%
      tab_footnote(
        footnote = "Total Gobal of USAID/PEPFAR",
        locations = cells_body(
          columns = vars("operating_unit"),
          rows = 1),
      )%>%
      tab_style(
        style = list(cell_text(weight = "bold")
        ),
        locations = cells_body(
          rows = 1
        )
      ) %>%
      opt_table_outline()%>%
      
      opt_row_striping(row_striping = TRUE)%>%
      
      gtsave(.,path=OU,filename = "FY22_untouchable_pipeline_bilat.png")
    
    
    
    
    ## other ULO
    ref_id <- "1c56a09f"
    
    df_other_ulo_op_top_10<-df_ou_pipeline%>%
      adorn_totals("row",,,, -funding_agency,-operating_unit)%>%
      arrange(desc(other_ulo_opo)) %>%
      slice_max(other_ulo_opo,n=11)%>%
      select(operating_unit,other_ulo_opo)
    
    df_other_ulo_op_top_10<-df_other_ulo_op_top_10%>%
      gt()%>%
      fmt_currency( # add dolar signs
        columns = c(other_ulo_opo ),
        decimals = 0,
        currency = "USD")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      cols_width(
        everything() ~ px(120))%>%
      cols_label(
        operating_unit = "Operating Unit",
        other_ulo_opo="Other ULO",
        #pipeline_in_expired_awards="Expired Awards Pipeline"
      )%>%
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),) %>%
      
      tab_style(
        style = cell_borders(
          sides = "right",
          weight = px(1.5),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))%>%
      
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_align(
        align = "left",
        columns = tidyselect::contains("operating")
      )%>%
      tab_header(
        title = glue::glue(" COP21 USAID End of Fiscal Year Financial Performance Summary"),
        subtitle = "Top 10 OUs with ULOs that have been spent by IPs but not liquidated in Phoenix")%>%
      #gt::tab_source_note(
      # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | ref: {ref_id}"))
      )  %>%
      tab_footnote(
        footnote = "Total Gobal of USAID/PEPFAR",
        locations = cells_body(
          columns = vars("operating_unit"),
          rows = 1),
      )%>%
      tab_style(
        style = list(cell_text(weight = "bold")
        ),
        locations = cells_body(
          rows = 1
        )
      ) %>%
      opt_table_outline()%>%
      
      opt_row_striping(row_striping = TRUE)%>%
      gtsave(.,path=OU,filename = "FY22_ULO_bilat.png")
    
  #Expired awards pipeline graphic
    ref_id <- "ea537dde"
    
    df_expired_top_10<-df_ou_pipeline%>%
      adorn_totals("row",,,, -funding_agency,-operating_unit)%>%
      arrange(desc(pipeline_in_expired_awards)) %>%
      slice_max(pipeline_in_expired_awards,n=11)%>%
      select(operating_unit,pipeline_in_expired_awards)
    df_expired_top_10<-df_expired_top_10%>%
      gt()%>%
      fmt_currency( # add dolar signs
        columns = c(pipeline_in_expired_awards ),
        decimals = 0,
        currency = "USD")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      cols_width(
        everything() ~ px(120))%>%
      cols_label(
        operating_unit = "Operating Unit",
        # projected_remaining_pipeline_at_current_fy_close="COP23 Applied Pipeline",
        pipeline_in_expired_awards="Expired Awards Pipeline"
      )%>%
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),) %>%
      
      tab_style(
        style = cell_borders(
          sides = "right",
          weight = px(1.5),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))%>%
      
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_align(
        align = "left",
        columns = tidyselect::contains("operating")
      )%>%
      tab_header(
        title = glue::glue(" COP21 USAID End of Fiscal Year Financial Performance Summary"),
        subtitle = "Top 10 OUs with most pipeline stuck in expired awards")%>%
      #gt::tab_source_note(
      # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: EOFY Workbooks ref: {ref_id}"))
      )  %>%
      tab_footnote(
        footnote = "Total of USAID/PEPFAR",
        locations = cells_body(
          columns = vars("operating_unit"),
          rows = 1)
      )%>%
      tab_style(
        style = list(cell_text(weight = "bold")
        ),
        locations = cells_body(
          rows = 1
        )
      ) %>%
      opt_table_outline()%>%
      
      opt_row_striping(row_striping = TRUE)%>%
      
      gtsave(.,path=OU,filename = "FY23_expired_award_pipeline.png")
    
    
    
    ## largest untouchable pipeline
    ref_id <- "1c56a09K"
    
    df_untouch<-df_ou_pipeline%>%
      adorn_totals("row",,,, -funding_agency,-operating_unit)%>%
      arrange(desc(total_untouchable_pipeline)) %>%
      slice_max(total_untouchable_pipeline,n=11)%>%
      select(operating_unit,total_untouchable_pipeline)
    
    df_untouch<- df_untouch%>%
      gt()%>%
      fmt_currency( # add dolar signs
        columns = c(total_untouchable_pipeline),
        decimals = 0,
        currency = "USD")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      cols_width(
        everything() ~ px(120))%>%
      cols_label(
        operating_unit = "Operating Unit",
        total_untouchable_pipeline="Total Untouchable Pipeline",
        #pipeline_in_expired_awards="Expired Awards Pipeline"
      )%>%
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),) %>%
      
      tab_style(
        style = cell_borders(
          sides = "right",
          weight = px(1.5),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))%>%
      
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_align(
        align = "left",
        columns = tidyselect::contains("operating")
      )%>%
      tab_header(
        title = glue::glue(" COP21 USAID End of Fiscal Year Financial Performance Summary"),
        subtitle = "Top 10 OUs with largest untouchable pipeline")%>%
      #gt::tab_source_note(
      # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | ref: {ref_id}"))
      )  %>%
      opt_table_outline()%>%
      
      opt_row_striping(row_striping = TRUE)%>%
      tab_footnote(
        footnote = "Total of USAID/PEPFAR",
        locations = cells_body(
          columns = vars("operating_unit"),
          rows = 1)
      )%>%
      tab_style(
        style = list(cell_text(weight = "bold")
        ),
        locations = cells_body(
          rows = 1
        )
      )%>%
      gtsave(.,path=OU,filename = "FY22_largest_pipe_bilat.png")
    
    
    ## excess pipeline
    ref_id <- "0eCeb3ee"
    
    df_excess<-df_ou_pipeline%>%
      # adorn_totals("row",,,, -funding_agency,-operating_unit)%>%
      arrange(desc(adjusted_excess_pipeline)) %>%
      slice_max(adjusted_excess_pipeline,n=5)%>%
      select(operating_unit,adjusted_excess_pipeline)
    
    df_excess<-df_excess%>%
      gt()%>%
      fmt_currency( # add dolar signs
        columns = c(adjusted_excess_pipeline),
        decimals = 0,
        currency = "USD")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      cols_width(
        everything() ~ px(120))%>%
      cols_label(
        operating_unit = "Operating Unit",
        adjusted_excess_pipeline="Excess Pipeline",
        #pipeline_in_expired_awards="Expired Awards Pipeline"
      )%>%
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),) %>%
      
      tab_style(
        style = cell_borders(
          sides = "right",
          weight = px(1.5),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))%>%
      
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_align(
        align = "left",
        columns = tidyselect::contains("operating")
      )%>%
      tab_header(
        title = glue::glue(" COP21 USAID End of Fiscal Year Financial Performance Summary"),
        subtitle = "Top 5 OUs with Adjusted Excess Pipeline")%>%
      #gt::tab_source_note(
      # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | ref: {ref_id}"))
      )  %>%
      opt_table_outline()%>%
      
      opt_row_striping(row_striping = TRUE)%>%
      gtsave(.,path=OU,filename = "FY22_OU_excess.png")
    
    
    

# SPINDOWN ============================================================================
    target_location <- gs4_get(" https://docs.google.com/spreadsheets/d/1FSnWTGbxw9Xu2M9vBd8JAiK5RcpFJ5eHjDCinhhNnH4/edit#gid=0")
    sheet_write(df_ou_pipeline, target_location, sheet =  "OU pipeline_2022")
    # sheet_write(df_ou_pipeline_central, target_location, sheet =  "Central OU pipeline_2022")
