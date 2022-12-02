# PURPOSE: Munge and Analysis of QROO data
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2022-12-02
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
OU <- "ESF"
OU <- glue("{save_dir}/{OU}")


# Build necessary directories if they are not present
dir.create(OU, showWarning=F)
   
    ref_id <- "c72bdf29"
    
  # Functions  
  
  

# LOAD DATA ============================================================================  
   df_qroo <- read_excel("Data/Budget Files/FY22Q4 EOFY/FY22 Q4 O&O USAID Submission_11.18.2022_V2 (2) (1).xlsx", sheet = "OGAC Deliverable", skip = 1)


# MUNGE ============================================================================
  
  #Get ESF funds
    df_esf<-df_qroo%>%
      filter(Fund=="ESF")%>%
      clean_names()%>%
      select(country_program,available, outlaid, pipeline_check)
    
    df_esf_top_10<-df_esf%>%
    adorn_totals("row",,,, -country_program)%>%
      arrange(desc(pipeline_check)) %>%
      slice_max(pipeline_check,n=12)
    
    #remove GFTA
    df_esf_top_10=df_esf_top_10[-2,]
    
    df_esf_top_10%>%
      gt()%>%
      fmt_currency( # add dolar signs
        columns = c(available, outlaid, pipeline_check),
        decimals = 0,
        currency = "USD")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      cols_width(
        everything() ~ px(120))%>%
      cols_label(
        country_program="OU",
        available="ESF Funds",
        outlaid="Outlays",
        pipeline_check="Remaining Funds"
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
        columns = tidyselect::contains("country_program")
      )%>%
      tab_header(
        title = glue::glue(" COP21 USAID End of Fiscal Year Financial Performance Summary: ESF Funds"),
        subtitle = "Top 10 OUs with highest remaining ESF funds")%>%
      #gt::tab_source_note(
      # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | ref: {ref_id}"))
      )  %>%
      tab_footnote(
        footnote = "Total Gobal of USAID/PEPFAR",
        locations = cells_body(
          columns = vars("country_program"),
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
      gtsave(.,path=OU,filename = "FY22_esf_bilateral.png")
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

