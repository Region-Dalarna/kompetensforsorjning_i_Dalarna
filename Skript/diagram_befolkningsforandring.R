diagram_kvalifikationskrav <- function(region_vekt = hamtaAllaLan(tamedriket = FALSE), # Vilka regioner/kommuner vill man titta på
                                       output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                       output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       tid = c("2010":"9999"), # Välj tidsintervall. Finns från 1968."*" ger alla. "9999" ger sista år
                                       spara_figur = TRUE,
                                       filnamn_data = "kvalifikationskrav.xlsx", # Filnamn på sparad data
                                       kon_klartext = c("män","kvinnor"), # män och kvinnor ger totalt. Det går även att välja ett av könen. Jämförelse mellan kön är inte möjlig.
                                       vald_farg = diagramfarger("rus_sex"), # Val av diagramfärger
                                       returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                                       returnera_data = FALSE){ # Skall data returneras
  
  
  # =================================================================================================================
  # Diagram som jämför kvalifikationskrav inom olika områden (kommun eller län) och bransch i valt län/kommun 
  # Funkar enbart för senaste år
  # Skapad av Jon 2024-01-17
  # Senast uppdaterad:
  # =================================================================================================================
  
  # Bibliotek som behövs
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge objekt i lista
  list_data <- lst() # Skapa tom lista som används för att spara till Excel.
  
  # Hämtar funktioner och laddar skript som hämtar data
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")
  
  region_vekt = hamtaAllaLan(tamedriket = FALSE)
  region_vekt = hamtakommuner("25",tamedlan = TRUE,tamedriket = FALSE)
  bef_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt, 
                                                 cont_klartext = c("Folkmängd","Folkökning"),
                                                 tid_koder = tid,
                                                 alder_koder = as.character(20:64)) %>% 
              pivot_wider(names_from = variabel,values_from = varde)
  
  flytt_df <- hamta_bef_flyttningar_region_alder_kon_scb(region_vekt = region_vekt, 
                                                         tid_koder = tid,
                                                         alder_koder = as.character(20:64)) %>% 
              pivot_wider(names_from = variabel,values_from = varde)
  
  #valt_lan = hamtaregion_kod_namn(region_vekt)[[2]]
  
  # summerar flyttningar på regionnivå för åren 2010-senaste år
  flytt_df_sum <- flytt_df %>%
    group_by(region) %>% 
    summarize(flyttningsoverskott = sum(Flyttningsöverskott),
              invandringsoverskott=sum(Invandringsöverskott),
              inrikes_flyttningsoverskott=sum(`Inrikes flyttningsöverskott`))
  
  # Summerar den totala folkökningen
  bef_df_sum <- bef_df %>%
    group_by(region) %>% 
      summarize(folkokning_period = sum(Folkökning))
  
  # Tar ut befolkningen 2010 och slår ihop med folkökning (för att beräkna förändring)
  bef_df_min <- bef_df %>% 
    filter(år == min (år)) %>% 
    group_by(region) %>% 
      summarize(folkmangd_forsta_ar = sum(Folkmängd))
  
  bef_df_sum <- merge(bef_df_sum,bef_df_min)
  
  # Slår ihop befolkning och flyttningar
  slutgiltig_df <-merge(flytt_df_sum,bef_df_sum)
  
  # Beräknar procentuell förändring av befolkning baserat på olika kompontenter
  slutgiltig_df <- slutgiltig_df %>% 
    mutate(alderskomponent = folkokning_period - flyttningsoverskott) %>% 
      mutate("Inrikes flyttnetto" = round((inrikes_flyttningsoverskott/folkmangd_forsta_ar)*100,2),
             "Utrikes flyttnetto" = round((invandringsoverskott/folkmangd_forsta_ar)*100,2),
             "Demografisk förändring" = round((alderskomponent/folkmangd_forsta_ar)*100,2),
             "Total förändring" = round((folkokning_period/folkmangd_forsta_ar)*100,2),
             "region" = skapa_kortnamn_lan(region)) %>% 
        select(region,`Inrikes flyttnetto`,`Utrikes flyttnetto`,`Demografisk förändring`,`Total förändring`) %>% 
          pivot_longer(!c(region),names_to="variabel",values_to="forandring")
          
  
  # # Väljer ut region och de tre komponenterna
  # utskrift_df <- slutgiltig_df %>% 
  #   select(region,`inrikes flyttnetto`,`utrikes flyttnetto`,`Demografisk förändring`)
  # 
  # # Pivoterar df för att skapa figur
  # utskrift_df <- utskrift_df %>% 
  #   pivot_longer(!c(region),names_to="variabel",values_to="forandring")
  
  # Tar bort län och s i vissa fall

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Jmf" = px_df_sum))
    }
    
    if(returnera_data == TRUE){
      assign("kvalifikationskrav_jmf", px_df_sum, envir = .GlobalEnv)
    }
    
  diagram_capt <- "Källa: Befolkningsregistret i SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

  diagramtitel <- paste0("Befolkningsförändring i arbetsför ålder(",min(bef_df$ålder)," - ",max(bef_df$ålder),") år ",min(bef_df$år),"-",max(bef_df$år))
  diagramfilnamn <- paste0("befolkningsforandring_20_64.png")
  
  bef_for_flytt <- SkapaStapelDiagram(skickad_df =slutgiltig_df %>% 
                                        filter(variabel!="Total förändring"),
                                      skickad_x_var = "region",
                                      skickad_y_var = "forandring",
                                      skickad_x_grupp = "variabel",
                                      manual_color = diagramfarger("rus_sex"),
                                      diagram_titel = diagramtitel,
                                      diagram_capt =  diagram_capt,
                                      diagram_facet = FALSE,
                                      x_axis_lutning = 0,
                                      x_axis_sort_value = TRUE,
                                      diagram_liggande = TRUE,
                                      stodlinjer_avrunda_fem = TRUE,
                                      geom_position_stack = TRUE,
                                      manual_y_axis_title="procent",
                                      berakna_index = FALSE,
                                      output_mapp = "",
                                      filnamn_diagram = "diagramfilnamn",
                                      skriv_till_diagramfil = FALSE)
    gg_list <- c(gg_list, list(gg_obj))
    

  
  if(diag_jmf_bransch == TRUE){
    if("kvinnor" %in% unique(px_df$kön) & "män" %in% unique(px_df$kön)) {
      variabellista = c("år","region","Branschgrupp","kompetensniva")
      diagram_titel <- paste0("Kvalifikationskrav för anställda (16-64 år) i ",valt_lan," ",max(px_df$år))
      objektnamn <- c(objektnamn,paste0("kvalifikationskrav_bransch_",valt_lan))
    }else {
      variabellista = c("år","kön","region","Branschgrupp","kompetensniva")
      diagram_titel <- paste0("Kvalifikationskrav för anställda ",unique(px_df$kön) ," (16-64 år) i ",valt_lan," ",max(px_df$år))
      objektnamn <- c(objektnamn,paste0("kvalifikationskrav_bransch_",unique(px_df$kön),"_",valt_lan))
    }
    
    diagram_capt <- "Källa: Yrkesregistret i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    
    px_df_sum <- px_df %>%
      filter(år == max(år),region == valt_lan) %>%
      group_by(across(any_of(variabellista))) %>%
      summarize(Antal = sum(`Anställda 16-64 år (dagbef)`)) %>%
      mutate(Andel=((Antal/sum(Antal))*100),
             Andel = ifelse(Andel<0.001,Andel,Andel-0.001))
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Bransch" = px_df_sum))
    }
    
    if(returnera_data == TRUE){
      assign("kvalifikationskrav_jmf_bransch", px_df_sum, envir = .GlobalEnv)
    }
    
    diagramfil <- "kompetenskrav_bransch.png"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_sum %>% 
                                   filter(Branschgrupp != "Okänd verksamhet") %>% 
                                   mutate(kompetensniva = factor(kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])), 
                                 skickad_x_var = "Branschgrupp",
                                 skickad_y_var = "Andel",
                                 skickad_x_grupp = "kompetensniva",
                                 manual_color = c(rgb(211,211,211, maxColorValue = 255),diagramfarger("rus_sex")),
                                 diagram_titel = diagram_titel,
                                 legend_vand_ordning = TRUE,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 diagram_liggande = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 6,
                                 vand_sortering = TRUE,
                                 geom_position_stack = TRUE,
                                 manual_y_axis_title = "procent",
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- c(objektnamn)
  
  if(returnera_figur == TRUE) return(gg_list)
  
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }
  
}
