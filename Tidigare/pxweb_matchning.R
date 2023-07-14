# Matchning på arbetsmarknaden. Från reginonala matchningsindikatorer (val 24 under arbetsmarknad)
# Läser in nödvändiga bibliotek med pacman
pacman::p_load(tidyverse,httr,pxweb,here)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_matchning(skapa_fil=FALSE)
diag_matchning <- function(region_vekt="20",
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       skapa_fil=TRUE,
                                       matchning_lan=TRUE,
                                       matchning_bakgrund=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, regionala matchningsindikatorer.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Med matchning avses matchad förvärvsgrad\ndvs. andelen anställda som har ett yrke som helt matchar deras utbildning (enligt SCB)."
  
  valda_farger <-c(rgb(169,208,142, maxColorValue = 255),
                   rgb(112,173,71, maxColorValue = 255))
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  objektnamn <- c()
  
  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  pxweb_query_list <- 
    list("Region"=hamtaAllaLan(tamedriket = TRUE),
         "Kon"=c("1","2","SAMANST"),
         "AlderFodelselandgr"=c("Sverige","Norden/EU","Afrika","Asien","Övriga_världen","totalt"),
         "ContentsCode"=c("000005SF"),
         "Tid"=c("*"))
  
  # Download data 
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  # Döper om till 
  px_data_frame<-px_data_frame %>% 
    rename("fodelseland"="ålder/födelselandgrupp","matchningsgrad"="Matchningsgrad, procent ")
  
  px_data_frame$region=skapa_kortnamn_lan(px_data_frame$region)
 
  if(matchning_lan==TRUE){
    diagramtitel <- paste0("Matchning på arbetsmarknaden i Sveriges regioner ",max(px_data_frame$år))
    diagramfilnamn <- paste0("matchning_lan.png")
    objektnamn <- c(objektnamn,paste0("matchning_lan"))
    
    
    gg_obj <- SkapaStapelDiagram(skickad_df =px_data_frame %>%
                                              filter(kön!="samtliga anställda") %>% 
                                                filter(fodelseland=="totalt") %>% 
                                                  filter(år==max(px_data_frame$år)), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "matchningsgrad", 
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45, 
                                 diagram_liggande = FALSE,
                                 geom_position_stack = FALSE,
                                 manual_y_axis_title="procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(matchning_bakgrund==TRUE){
    
    px_data_frame[px_data_frame=="födda i Europa utom Norden och EU samt Sydamerika, Nordamerika och Oceanien"] <- "Övriga" 
    
    diagramtitel <- paste0("Matchning på arbetsmarknaden i ",ValdGeografi," ",max(px_data_frame$år)," per födelseregion")
    diagramfilnamn <- paste0("matchning_bakgrund.png")
    objektnamn <- c(objektnamn,paste0("matchning_bakgrund"))
    
    # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df =px_data_frame %>%
                                   filter(kön!="samtliga anställda") %>%
                                    filter(år==max(px_data_frame$år)) %>% 
                                      filter(region==skapa_kortnamn_lan(ValdGeografi)) %>% 
                                        filter(fodelseland!="totalt"),
                                 skickad_x_var = "fodelseland", 
                                 skickad_y_var = "matchningsgrad", 
                                 skickad_x_grupp = "kön",
                                 # manual_x_axis_text_vjust=1,
                                 # manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning =0, 
                                 diagram_liggande = TRUE,
                                 geom_position_stack = FALSE,
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
