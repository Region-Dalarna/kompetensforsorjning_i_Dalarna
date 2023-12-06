
hamta_data_pendling_kommun <-function(region_vekt = "20", 
                                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                      spara_data = TRUE,
                                      returnera_data = FALSE,
                                      filnamn = "pendling_kommun.xlsx",
                                      ta_med_riket = TRUE,
                                      senaste_ar = FALSE,
                                      tid = 1900:2100){#välj ett högt avslutningsår (då kommer uppdateringar med automatiskt)

  ##### Hämtar data för att beräkna pendling #######
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,rKolada)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")
  
  if(ta_med_riket == TRUE){
    valda_kommuner = c("0000",hamtakommuner(region_vekt))
  }else valda_kommuner = c(hamtakommuner(region_vekt))
  
  if(senaste_ar == TRUE){
    tid = max(hamta_kolada_giltiga_ar("N00968"),region_vekt)
  }
  
  #### Dra hem variablerna från Kolada
  Indendling_df <- get_values(
    kpi = c("N00968"),
    municipality = valda_kommuner,
    period = tid
  )
  
  #### Dra hem variablerna från Kolada
  Utpendling_df <- get_values(
    kpi = c("N00920"),
    municipality = valda_kommuner,
    period = tid
  )
  
  # Inpendling
  # Väljer ut relevanta variabler och gör vissa justeringar
  Indendling_df <-Indendling_df %>% 
    select(year,gender,value,municipality) %>% 
      rename("Pendling_andel"=value) %>% 
        mutate(year = as.character(year),
               pendling_typ = "Andel inpendling")

  # Utpendling
  Utpendling_df <-Utpendling_df %>% 
    select(year,gender,value,municipality) %>%
      rename("Pendling_andel"=value)%>% 
        mutate(year = as.character(year),
               pendling_typ = "Andel utpendling")
 
  # Slår ihop de två
  pendling_df <- rbind(Indendling_df,Utpendling_df)
  
  if (spara_data == TRUE){
    openxlsx::write.xlsx(pendling_df,paste0(output_mapp,filnamn))
  }
  
  if(returnera_data == TRUE) return(pendling_df)
}

