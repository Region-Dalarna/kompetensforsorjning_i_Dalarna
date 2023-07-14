# Skript som beräknar andelen pendlare i olika län
pacman::p_load(pxweb,httr,tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_pendlingsintensitet(skapa_fil=FALSE,)
diag_pendlingsintensitet <- function(region_vekt="20",
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       skapa_fil=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Befolkningsregistret i SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Inpendling i förhållande till dagbefolkning och utpendling i förhållande till nattbefolkning (i arbetsför ålder, 20-64 år)"
  
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
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- c("/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906B/RegionInd19U2N")
  url3 <- paste0(url1,url2)
  
  varlista <- list("Region"=hamtaAllaLan(tamedriket=FALSE),
                   "Utbildngrupp"=c("00N","00I","0","00S"),
                   "Kon"=c("*"),
                   "ContentsCode"=c("000005SJ","000005SK","000005SL","000005SM","000005SN"),
                   "Tid"=c("*"))
  
  px_data <- pxweb_get(url = url3,query = varlista)
  
  pendling_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  # Filtrerar dataset och beräknar andelar pendlare
  pendling_df_sum <- pendling_df %>% 
    filter(utbildning=="samtliga utbildningsnivåer",kön=="totalt") %>% 
      filter(år==max(år)) %>% 
        mutate(`Andel inpendling`=(`Inpendlare över regiongräns`/`Dagbefolkning (förvärvsarbetande)`)*100,
               `Andel utpendling`=(`Utpendlare över regiongräns`/`Nattbefolkning (förvärvsarbetande)`)*100)
  
  # Tar bort län och s i vissa fall
  pendling_df_sum$region=skapa_kortnamn_lan(pendling_df_sum$region)
  
  # Skapar ett dataset som används för att skriva ut
  pendling_df_utskrift <- pendling_df_sum %>% 
    select(region,år,`Andel inpendling`,`Andel utpendling`)
  
  # Pivoterar dataset för att skrapa grupper av pendlingstyp
  pendling_df_utskrift <- pivot_longer(pendling_df_utskrift,cols=3:4,names_to = "Typ_pendling")
  
  # Skapar ett diagram för pendlingsintensitet
  diagramtitel <- paste0("Andel pendling i Sveriges regioner ",max(pendling_df_utskrift$år))
  diagramfilnamn <- paste0("pendlingsintensitet.png")
  objektnamn <- paste0("pendlingsintensitet")
  
  gg_obj <- SkapaStapelDiagram(skickad_df =pendling_df_utskrift, 
                               skickad_x_var = "region", 
                               skickad_y_var = "value", 
                               skickad_x_grupp = "Typ_pendling",
                               manual_x_axis_text_vjust=1.2,
                               manual_x_axis_text_hjust=1.1,
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
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
}


