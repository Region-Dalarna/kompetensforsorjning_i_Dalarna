diag_matchning_lan <- function(region_vekt = "20", # Region vi är intresserade av. Gäller diagrammet 
                               output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                               output_mapp_data = NA, # Här hamnar sparad data
                               filnamn_data = "matchning.xlsx",
                               valda_farger = diagramfarger("rus_sex"), # Vilka färger skall användas i diagram
                               spara_figur = TRUE, # Om true sparas figuren till output_mapp
                               kon_klartext = c("män", "kvinnor"), # Alternativet är "samtliga anställda" för att undvika könsuppdelning
                               diag_lan = TRUE, # Skapar ett diagram där län jämförs med riket
                               diag_kommun = TRUE, # Motsvarande diagram där kommuner jämförs med länet
                               diag_lan_antal = FALSE, # Antal för länet, uppdelat på kvinnor och män
                               returnera_figur = TRUE, # Skall figur returneras (i en lista)
                               returnera_data = FALSE){ # Skall data returneras (till R-studios globla miljö)
  
  # ========================================== Allmän info ============================================
  
  # 1: Skapar diagram för andelen förvärvsarbetande inom olika branscher, dels på länsnivå, dels på kommunnivå. Enbart senaste år och ingen uppdelning på kön
  # 1: Antal förvärvsarbetande senaste observation uppdelat på kön
  # Senast uppdaterad: Jon 2024-01-18
  # ========================================== Inställningar ============================================
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Det som står under diagrammet
  diagram_capt <- "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschens andel av totalt antal förvärvsarbetande"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c() 
  # Lista som används för att lägg till dataset till Excelfil (som sparas)
  list_data <- lst()
  
  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
  # =============================================== API-uttag ===============================================
  
  # Hämtar data
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_matchningsgrad_lan_FA.R")
  df = hamta_data_matchning(region = hamtaAllaLan(tamedriket = TRUE),
                            tid = "*",
                            kon_klartext = kon_klartext,
                            alder_fodelseland = c("Sverige","Norden/EU","Afrika","Asien","Övriga_världen","totalt"),
                            returnera_data = TRUE)
  
  # Summerar på region och sektor
  df <- df %>% 
    rename("fodelseland" = `ålder/födelselandgrupp`,
           matchningsgrad = `Matchningsgrad, procent `) %>% 
    mutate("fodelseland" = ifelse(fodelseland %in% c("födda i Europa utom Norden och EU samt Sydamerika, Nordamerika och Oceanien"),"övriga", fodelseland)) 

  if(diag_lan == TRUE | diag_kommun == TRUE){
    if(returnera_data == TRUE){
      assign("andel_forvarvsarbetande_bransch", df_sum, envir = .GlobalEnv)
    }
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Andel per bransch" = df_sum))
    }
    
  }
  
  if(diag_lan==TRUE){
    
    diagram_titel <- paste0("Matchningsgrad på arbetsmarknaden år ",max(df$år))
    diagramfil <- "matchning_jmf.png"
    objektnamn <- c(objektnamn,"matchning_jmf")
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, regionala matchningsindikatorer.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Med matchning avses matchad förvärvsgrad\ndvs. andelen anställda som har ett yrke som helt matchar deras utbildning (enligt SCB)."

    gg_obj <- SkapaStapelDiagram(skickad_df = df %>%
                                              filter(kön != "samtliga anställda",
                                                     fodelseland == "totalt",
                                                     år == max(år)),
                                            skickad_x_var = "region",
                                            skickad_y_var = "matchningsgrad",
                                            skickad_x_grupp = "kön",
                                            manual_x_axis_text_vjust=1,
                                            manual_x_axis_text_hjust=1,
                                            manual_color = diagramfarger("kon"),
                                            diagram_titel = diagram_titel,
                                            diagram_capt =  diagram_capt,
                                            y_axis_100proc = TRUE,
                                            x_axis_sort_value = TRUE,
                                            x_axis_lutning = 45,
                                            manual_y_axis_title = "procent",
                                            output_mapp = output_mapp_figur,
                                            filnamn_diagram = diagramfil,
                                            skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }

  
  if(diag_lan_antal==TRUE){
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, regionala matchningsindikatorer.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Med matchning avses matchad förvärvsgrad\ndvs. andelen anställda som har ett yrke som helt matchar deras utbildning (enligt SCB)."
    diagram_titel <- paste0("Matchning på arbetsmarknaden i" ,vald_region, " år ",max(df$år))
    diagramfil <- paste0("matchning_bakgrund.png")
    objektnamn <- c(objektnamn,"matchning_bakgrund")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =df %>%
                                               filter(kön != "samtliga anställda",
                                                      år == max(år),
                                                      region == vald_region,
                                                      fodelseland != "totalt"),
                                             skickad_x_var = "fodelseland",
                                             skickad_y_var = "matchningsgrad",
                                             skickad_x_grupp = "kön",
                                             manual_color = diagramfarger("kon"),
                                             diagram_titel = diagram_titel,
                                             diagram_capt =  diagram_capt,
                                             x_axis_sort_value = TRUE,
                                             x_axis_lutning =0,
                                             y_axis_100proc = TRUE,
                                             diagram_liggande = TRUE,
                                             manual_y_axis_title="procent",
                                             output_mapp = output_mapp_figur,
                                             filnamn_diagram = diagramfil,
                                             skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }
  
}