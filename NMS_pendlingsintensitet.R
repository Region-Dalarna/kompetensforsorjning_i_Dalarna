# Skript som beräknar andelen pendlare i olika län
pacman::p_load(tidyverse,openxlsx,here)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_pendlingsintensitet(skapa_fil=FALSE)
diag_pendlingsintensitet <- function(region_vekt="20",
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     skapa_fil=TRUE,
                                     diag_pendling_lan=TRUE,
                                     diag_pendling_kommun=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: NMS-databasen, Stativ (SCB).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Inpendling i förhållande till dagbefolkning (de som har arbetsställe i regionen)\noch utpendling i förhållande till nattbefolkning (de som är skrivna i regionen)."
  diagram_capt_kommun <- "Källa: NMS-databasen, Stativ (SCB).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Inpendling i förhållande till dagbefolkning (de som har arbetsställe i kommunen)\noch utpendling i förhållande till nattbefolkning (de som är skrivna i kommunen)."
  
  valda_farger <-c(rgb(169,208,142, maxColorValue = 255),
                   rgb(112,173,71, maxColorValue = 255))
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  objektnamn <- c()
  
  #==========================================================================================================  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # Läser in data från Excel (ursprung NMS)
  pendling_lan_df <- read.xlsx("G:/skript/projekt/kompetens_genomgang/Indata/30_sep_22_utdata_pendlingsintensitet.xlsx",sheet=1)
  pendling_kommun_df <- read.xlsx("G:/skript/projekt/kompetens_genomgang/Indata/30_sep_22_utdata_pendlingsintensitet.xlsx",sheet=2)
  
  # Kopplar län och kommunnummer till motsvarande namn.
  lan_namn <- hamtaregion_kod_namn(unique(pendling_lan_df$lan_nummer))
  kommun_namn <- hamtaregion_kod_namn(unique(pendling_kommun_df$kommun_nummer))
  
  # Lägger till län och kommunnamn i ursprunglig data
  pendling_lan_df <- left_join(pendling_lan_df,lan_namn,by=c("lan_nummer"="regionkod"))
  pendling_kommun_df <- left_join(pendling_kommun_df,kommun_namn,by=c("kommun_nummer"="regionkod"))
  
  # Tar bort län och s i vissa fall
  pendling_lan_df$region=skapa_kortnamn_lan(pendling_lan_df$region)
  
  # Gör om pendlingen till procent
  pendling_lan_df$andel_utpendling <- pendling_lan_df$andel_utpendling*100
  pendling_lan_df$andel_inpendling <- pendling_lan_df$andel_inpendling*100
  pendling_kommun_df$andel_utpendling <- pendling_kommun_df$andel_utpendling*100
  pendling_kommun_df$andel_inpendling <- pendling_kommun_df$andel_inpendling*100

  if(diag_pendling_lan==TRUE){
    diagramtitel <- paste0("Andel i arbetsför ålder (20-64 år) som pendlar över länsgräns i Sveriges regioner 2020")
    diagramfilnamn <- paste0("pendlingsintensitet.png")
    objektnamn <- paste0("pendlingsintensitet")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =pendling_lan_df %>%
                                   rename("Andel inpendling"=andel_inpendling, "Andel utpendling" = andel_utpendling) %>%
                                    pivot_longer(cols=c("Andel inpendling","Andel utpendling"),names_to = "variabel") , 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "value", 
                                 skickad_x_grupp = "variabel",
                                 manual_x_axis_text_hjust=1.1,
                                 manual_x_axis_text_vjust=1.2,
                                 manual_color = diagramfarger("gron_sex")[4:5],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title="procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_pendling_kommun==TRUE){
    diagramtitel <- paste0("Andel i arbetsför ålder (20-64 år) som pendlar över kommungräns i Dalarnas kommuner 2020")
    diagramfilnamn <- paste0("pendlingsintensitet_kommun.png")
    objektnamn <- c(objektnamn,paste0("pendlingsintensitet_kommun"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df =pendling_kommun_df %>%
                                   rename("Andel inpendling"=andel_inpendling, "Andel utpendling" = andel_utpendling) %>%
                                   pivot_longer(cols=c("Andel inpendling","Andel utpendling"),names_to = "variabel") , 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "value", 
                                 skickad_x_grupp = "variabel",
                                 manual_x_axis_text_vjust=1.2,
                                 manual_x_axis_text_hjust=1.1,
                                 manual_color = diagramfarger("gron_sex")[4:5],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt_kommun,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title="procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
}


