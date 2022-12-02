# PURPOSE: Munge and Analysis of Budget team data for EOFY Review
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2022-11-08
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
library(googledrive)
library(googlesheets4)
library(janitor)
library(readxl)
library(glue)



percent_clean <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}

ref_id <- "ba58b411"

# folder set up
# Path where you want to save your PDF outputs, log files, and temp files
save_dir <- "C:/Users/bkasdan/Documents/GitHub/i-need-a-dollar/Images"
# Add path to where folders should be placed
OU <- "bilateral"
OU <- glue("{save_dir}/{OU}")


# Build necessary directories if they are not present
dir.create(OU, showWarning=F)
# LOAD DATA ============================================================================  


budget<-list.files("~/Data/Budget Files/FY22Q4 EOFY/Bilateral",full.names = TRUE) #can try and automate via google folder
glimpse(budget)




#SA file for test
# library(readxl)
# df <- read_excel("C:/Users/Bkasdan/Downloads/South Africa_FY22 EOFY (Bilateral) for Visuals.xlsx", 
#                  sheet = "OUMechanisms", skip = 2)%>%
#   clean_names()%>%
#   select(c(operating_unit, last_active_cop,previous_cop_total_planned_funding,total_outlays_during_previous_fy,delta_between_plan_and_previous_fy_outlays ))
# 
# df<-df%>%
#   mutate(mechs_overspend=delta_between_plan_and_previous_fy_outlays)%>%
#   mutate(mechs_overspend = ifelse(delta_between_plan_and_previous_fy_outlays >=0, "0",
#                                   ifelse(delta_between_plan_and_previous_fy_outlays<0, "1",NA)))%>%
#   
#   mutate(mechs_overspend=as.numeric(mechs_overspend))%>%
#   group_by(operating_unit,last_active_cop)%>%
#   summarise_at(vars(previous_cop_total_planned_funding,total_outlays_during_previous_fy,delta_between_plan_and_previous_fy_outlays, mechs_overspend,), sum, na.rm = TRUE)
# df<-df%>%
#   mutate("unoutlayed"=previous_cop_total_planned_funding-total_outlays_during_previous_fy)%>%
#   ungroup()

# MUNGE ============================================================================
get_be_bilat<-function(df){
  df<-read_xlsx(df, 
                sheet = "OUMechanisms", skip = 2)%>%
    clean_names()%>%
    select(c(operating_unit, last_active_cop,previous_cop_total_planned_funding,total_outlays_during_previous_fy,delta_between_plan_and_previous_fy_outlays ))%>%
    mutate(funding_agency="USAID-bilateral")
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
#OU ESF
df1 <- purrr::map_dfr(.x = budget,
                      .f = ~ get_be_bilat(.x))
# %>%filter(last_active_cop=="2021 COP")

df3<-df1%>%
  # filter(last_active_cop=="2021 COP")%>%
  arrange(desc(unoutlayed))%>%
  slice_max(unoutlayed, n = 10)
  
  
#WCF ESF
data <- "~/Data/Budget Files/USAID_WCF-ESF.xlsx" #rename tab EOFY ESF
df2<-get_be_esf(data)%>%
  mutate(funding_agency="USAID-WCF")

df_top_ten<-df3%>%
  slice_max(unoutlayed,n=10)


df_bottom_20<-df1%>%
  slice_min(mechs_overspend, n = 20)
# charting ============================================================================

df3<-df3%>%
  gt()%>%
  cols_hide(
    columns = c(
      last_active_cop,delta_between_plan_and_previous_fy_outlays
    )
  )%>%
  fmt_currency( # add dolar signs
    columns = c(total_outlays_during_previous_fy, previous_cop_total_planned_funding,unoutlayed ),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(120))%>%
  cols_label(
    operating_unit = "Operating Unit",
    total_outlays_during_previous_fy = "Outlays",
    previous_cop_total_planned_funding = " Total Planned Funding",
    unoutlayed="Remaining Funds",
    mechs_overspend="Mechanisms Overspending (#)"
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
    subtitle = "OUs with highest remaining COP21 Funds")%>%
  #gt::tab_source_note(
  # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | Please reach out to your budget backstop for questions | ref: {ref_id}"))
  )  %>%
  # gtsave(.,"FY20_ESF.png") 
gtsave(.,path=OU,filename = glue::glue("COP21_bilat.png"))


df3<-df_bottom_20%>%
  gt()%>%
  fmt_percent(
    columns = c(budget_execution,),
    decimals = 0)%>%
  fmt_currency( # add dolar signs
    columns = c(total_outlays_during_previous_fy, previous_cop_total_planned_funding ),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(120))%>%
  cols_label(
    operating_unit = "Operating Unit",
    total_outlays_during_previous_fy = "Outlays",
    previous_cop_total_planned_funding = " COP20 Budget",
    budget_execution="Total Execution",
    mechs_overspend="Mechanisms Overspending (#)"
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
    title = glue::glue(" COP20 USAID End of Fiscal Year Financial Performance Summary"),
    subtitle = "OUs with fewest mechanisms that have over-outlayed their budget")%>%
  #gt::tab_source_note(
  # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | Please reach out to your budget backstop for questions |ref {ref_id} "))
  ) 

###Google sheet output====
target_location <- gs4_get(" https://docs.google.com/spreadsheets/d/1FSnWTGbxw9Xu2M9vBd8JAiK5RcpFJ5eHjDCinhhNnH4/edit#gid=0")
sheet_write(df1, target_location, sheet = "Bilateral OU outlays_2022")
sheet_write(df2, target_location, sheet = "ESF WCF OU outlays")
