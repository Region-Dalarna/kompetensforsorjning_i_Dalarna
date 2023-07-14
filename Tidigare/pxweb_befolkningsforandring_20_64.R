# Skript som beräknar befolkningsförändringen mellan 2010 och det senaste året

pacman::p_load(pxweb,httr,tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_befolkning_forandring(skapa_fil=FALSE,output_mapp= here("Diagram","/"))
diag_befolkning_forandring <- function(region_vekt="20",
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       skapa_fil=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Befolkningsregistret i SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ålderskomponent är den del av befolkningsförändringen som inte kan förklaras av inrikes och utrikes flyttnetto"
  
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
  url_folkmangd <- c("/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy")
  url_flyttningar <- c("/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97")
  
  # Hämtar det senate året i tabller
  senaste_ar_folkmangd <- hamta_senaste_tid_i_tabell(paste0(url1,url_folkmangd))
  senaste_ar_flyttningar <- hamta_senaste_tid_i_tabell(paste0(url1,url_flyttningar))
  
  varlista_folkmangd <- list("Region"=hamtaAllaLan(tamedriket=FALSE),
                             "Alder"=c(as.character(20:64)),
                             "Civilstand"=c("OG","G","SK","ÄNKL"),
                             "Kon"=c("1","2"),
                             "ContentsCode"=c("BE0101N1","BE0101N2"),
                             "Tid"=c(as.character(2010:senaste_ar_folkmangd)))
  
  varlista_flyttningar <-list("Region"=hamtaAllaLan(tamedriket=FALSE),
                            "Alder"=c(as.character(20:64)),
                            "Kon"=c("1","2"),
                            "ContentsCode"=c("BE0101AZ","BE0101A1","BE0101A4"),
                            "Tid"=c(as.character(2010:senaste_ar_flyttningar)))
 
  
  url2=c(url_folkmangd,url_flyttningar)
  
  varlista <- list(varlista_folkmangd,varlista_flyttningar)

  # Loop som används för att förbereda data för såväl befintlig data som prognos.
  for(k in 1:length(varlista)){
    url3 <- paste0(url1, url2[[k]])
    
    # Variabler som skall tas ut
    varlista_temp <-  varlista[[k]]
    
    # Uttag av data
    px_uttag <- pxweb_get(url = url3,query = varlista_temp)
    
    # Konverterar data till en Data Frame
    
    if(k==1) befolkning_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    else flyttningar_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  }
  # summerar flyttningar på regionnivå för åren 2010-2021
  flyttningar_df_sum <- flyttningar_df %>%
    filter(år %in% as.character(2010:senaste_ar_flyttningar)) %>% 
      group_by(region) %>% 
        summarize(flyttningsoverskott=sum(Flyttningsöverskott),
                  invandringsoverskott=sum(Invandringsöverskott),
                  inrikes_flyttningsoverskott=sum(`Inrikes flyttningsöverskott`))
  
  # Summerar den totala folkökningen
  befolkning_df_sum <- befolkning_df %>%
    filter(år %in% as.character(2010:senaste_ar_flyttningar)) %>% 
      group_by(region) %>% 
        summarize(folkokning_2010_2021=sum(Folkökning))
  
  # Tar ut befolkningen 2010 och slår ihop med folkökning (för att beräkna förändring)
  befolkning_df_2010 <- befolkning_df %>% 
    filter(år=="2010") %>% 
      group_by(region) %>% 
        summarize(folkmangd_2010=sum(Folkmängd))
  
  befolkning_df_sum <- merge(befolkning_df_sum,befolkning_df_2010)
  
  # Slår ihop befolkning och flyttningar
  slutgiltig_df <-merge(flyttningar_df_sum,befolkning_df_sum)
  
  # Beräknar procentuell förändring av befolkning baserat på olika kompontenter
  slutgiltig_df <- slutgiltig_df %>% 
    mutate(alderskomponent=folkokning_2010_2021-flyttningsoverskott) %>% 
      mutate("inrikes flyttnetto" =round((inrikes_flyttningsoverskott/folkmangd_2010)*100,2),
             "utrikes flyttnetto" =round((invandringsoverskott/folkmangd_2010)*100,2),
             "Demografisk förändring" =round((alderskomponent/folkmangd_2010)*100,2))
  
  # Väljer ut region och de tre komponenterna
  utskrift_df <- slutgiltig_df %>% 
    select(region,`inrikes flyttnetto`,`utrikes flyttnetto`,`Demografisk förändring`)
  
  # Pivoterar df för att skapa figur
  utskrift_df <- utskrift_df %>% 
    pivot_longer(!c(region),names_to="variabel",values_to="forandring")
  
  # Tar bort län och s i vissa fall
  utskrift_df$region=skapa_kortnamn_lan(utskrift_df$region)
  
    diagramtitel <- paste0("Befolkningsförändring i arbetsför ålder (20-64 år) per län ","2010","-",senaste_ar_flyttningar)
    diagramfilnamn <- paste0("befolkningsforandring_20_64.png")
    objektnamn <- paste0("befolkningsforandring_20_64")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =utskrift_df, 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "forandring", 
                                 skickad_x_grupp = "variabel",
                                 manual_x_axis_text_vjust=0.7,
                                 manual_color = diagramfarger("gron_fyra")[2:4],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 diagram_liggande = TRUE,
                                 geom_position_stack = TRUE,
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


