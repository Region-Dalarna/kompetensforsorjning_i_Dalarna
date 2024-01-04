#test = diagram_data_forvarvsarbetande_90(region_vekt = "20",spara_figur = FALSE,valda_ar = c("1990","2000","2010","2015"))
diagram_data_forvarvsarbetande_90 <- function(region_vekt = c("20"), # Vilken region vill man ha. Enbart 1 får väljas
                                              output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                              output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                              filnamn_data = "arbetsloshet_76.xlsx", # Filnamn på sparad data
                                              kon_klartext = c("män","kvinnor"), # män och kvinnor ger totalt. Det går även att välja ett av könen. Jämförelse mellan kön är inte möjlig.
                                              valda_ar = c("1990","2000","2010"), # Vilka år skall jämföras (max 1 mindre än antalet färger i vald_farg). Senaste år läggs till automatiskt
                                              vald_farg = diagramfarger("rus_sex"), # Val av diagramfärger
                                              filnamn_figur = "forvarvsarbetande_90.png", # Filnamn
                                              spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                              returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                                              returnera_data = FALSE){ # Tidsserie där kön jämförs. Går bara om en region valts i region_vekt
  
  
  # =================================================================================================================
  # Diagram för arbetslöshet från 1974 till senaste år (AKU - SCB). 
  # Finns för tillfället i två varianter, det ena visar en jämförelse mellan län (alternativt län och riket)
  # Det andra visar en jämförelse mellan kön inom ett län (eller riket).
  # Källa  https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # =================================================================================================================
  
  # Skript som behövs
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i <- 1 # Räknare
  objektnamn <- c() # Används för att namnge
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_forvarvsarbetande_bransch_1990_senastear_SCB.R")
 
  # Sourcar data för arbetslöshet
  df <-  hamta_data_sysselsatta_1990(region_vekt = region_vekt,
                                     kon_klartext = kon_klartext,
                                     returnera_data = TRUE) 
  
  if("kvinnor" %in% unique(df$kön) & "män" %in% unique(df$kön)) {
    variabellista = c("region","Näringsgren","år")
    diagram_titel <- paste0("Förvärvsarbetande (16+ år) i Dalarna")
    objektnamn <- "forvarvsarbetande_90_totalt"
  }else {
      variabellista = c("region","kön","Näringsgren","år")
      diagram_titel <- paste0("Förvärvsarbetande ",unique(df$kön) ," (16+ år) i Dalarna")
      objektnamn <- paste0("forvarvsarbetande_90_",unique(df$kön))
      }
  
  df_sum = df %>% 
    group_by(across(any_of(variabellista))) %>% 
      summarize(antal = sum(antal))
  
  if(returnera_data == TRUE){
    assign("forvarvsarbetande_90_senastear", df_sum, envir = .GlobalEnv)
  }
    
  # Gör om år till en faktorvariabel för utbildningsnivå_balans. Detta för att kunna bestämma vilken ordning staplarna för de olika åren kommer i diagrammet
  valda_ar <- c(valda_ar, max(df_sum$år))

  sysselsatta_90_df_alt <- df_sum %>% 
    mutate(Näringsgren = stringr::str_to_sentence(Näringsgren),
           Näringsgren = str_wrap(Näringsgren,40))
  

  diagram_capt <- "Källa: RAMS och BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschgruppering baserad på SNI2002 och SNI92.\nByte till bas från och med 2020."
  diagramfil <- paste0(objektnamn,".png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = sysselsatta_90_df_alt %>%
                                      filter(år%in%valda_ar),
                                    skickad_x_var = "Näringsgren",
                                    skickad_y_var = "antal",
                                    skickad_x_grupp = "år",
                                    manual_x_axis_text_vjust=1,
                                    manual_x_axis_text_hjust=1,
                                    manual_color = diagramfarger("rus_sex"),
                                    x_axis_sort_value = TRUE,
                                    vand_sortering = TRUE,
                                    stodlinjer_avrunda_fem = TRUE,
                                    x_axis_sort_grp = length(valda_ar),
                                    x_axis_lutning = 45,
                                    diagram_titel = diagram_titel,
                                    diagram_capt = diagram_capt,
                                    manual_y_axis_title = "",
                                    output_mapp = output_mapp_figur,
                                    filnamn_diagram = diagramfil,
                                    skriv_till_diagramfil = spara_figur)
  gg_list[[i]] <- gg_obj
  i=i+1

  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
}
