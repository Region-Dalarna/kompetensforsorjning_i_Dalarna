#test = diagram_data_forvarvsarbetande_90(region_vekt = "20",spara_figur = FALSE,valda_ar = c("1990","2000","2010"),kon_klartext = c("män","kvinnor"))
diagram_data_forvarvsarbetande_90 <- function(region_vekt = "20", # Vilken region vill man ha. Enbart 1 får väljas
                                              output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                              output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                              spara_figur = TRUE,
                                              filnamn_data = "arbetsloshet_76.xlsx", # Filnamn på sparad data
                                              diag_antal = TRUE,
                                              diag_forandring = TRUE,
                                              kon_klartext = c("män","kvinnor"), # män och kvinnor ger totalt. Det går även att välja ett av könen. Jämförelse mellan kön är inte möjlig.
                                              valda_ar = c("1990","2000","2010"), # Vilka år skall jämföras (max 1 mindre än antalet färger i vald_farg). Senaste år läggs till automatiskt
                                              vald_farg = diagramfarger("rus_sex"), # Val av diagramfärger
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
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_forvarvsarbetande_bransch_1990_senastear_SCB.R")
 
  # Sourcar data för arbetslöshet
  df <-  hamta_data_sysselsatta_1990(region_vekt = region_vekt,
                                     kon_klartext = kon_klartext,
                                     returnera_data = TRUE) 
  
  if(diag_antal == TRUE){
    if("kvinnor" %in% unique(df$kön) & "män" %in% unique(df$kön)) {
      variabellista = c("region","Näringsgren","år")
      diagram_titel <- paste0("Förvärvsarbetande (16+ år) i ",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)[2]))
      objektnamn <- paste0("forvarvsarbetande_90_totalt_",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)[2]))
    }else {
        variabellista = c("region","kön","Näringsgren","år")
        diagram_titel <- paste0("Förvärvsarbetande ",unique(df$kön) ," (16+ år) i ",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)[2]))
        objektnamn <- paste0("forvarvsarbetande_90_",unique(df$kön),"_",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)[2]))
        }
    
    df_sum = df %>% 
      group_by(across(any_of(variabellista))) %>% 
        summarize(antal = sum(antal)) %>% 
          ungroup()
    
    # Om användaren vill returnera data görs detta här
    if(returnera_data == TRUE){
      assign("forvarvsarbetande_90_senastear", df_sum, envir = .GlobalEnv)
    }
    
    # Om användaren vill spara data görs detta här. Sker enbart om både outputmapp och filnamn har valts
    if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
      write.xlsx(df_sum,paste0(output_mapp,filnamn))
    }
      
    # Lägger till senaste år till de år som användaren valt
    valda_ar <- c(valda_ar, max(df_sum$år))
  
    # Branscher har för långa namn, vilket justeras här
    sysselsatta_90_df_alt <- df_sum %>% 
      mutate(Näringsgren = stringr::str_to_sentence(Näringsgren),
             Näringsgren = str_wrap(Näringsgren,40))
    
  
    diagram_capt <- "Källa: RAMS och BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschgruppering baserad på SNI2002 och SNI92.\nByte från RAMS till BAS som datakälla från och med 2020."
    diagramfil <- paste0(objektnamn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sysselsatta_90_df_alt %>%
                                        filter(år%in%valda_ar),
                                      skickad_x_var = "Näringsgren",
                                      skickad_y_var = "antal",
                                      skickad_x_grupp = "år",
                                      manual_x_axis_text_vjust=1,
                                      manual_x_axis_text_hjust=1,
                                      manual_color = vald_farg,
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
  
  }
  
  if(diag_forandring == TRUE){
    diagram_capt <- "Källa: RAMS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschgruppering baserad på SNI2002 och SNI92"
    
    # Beräknar förändring in antalet anställda från 1990 till senaste år
    df_for <- df %>%
      filter(år %in% c(min(år),max(år))) %>% 
      group_by(år,region,Näringsgren) %>% 
      summarize(antal = sum(antal)) %>% 
      mutate(skillnad = last(antal)-first(antal)) %>% 
      mutate(Näringsgren =case_when(
        Näringsgren == "byggindustri" ~ "Bygg",
        Näringsgren == "civila myndigheter, försvar; internat. organisationer" ~ "Myndigheter mm" ,
        Näringsgren == "energi- o vattenförsörjning, avfallshantering" ~ "Energi och miljö",
        Näringsgren == "enh för hälso- och sjukvård, socialtjänst; veterinärer" ~ "Hälso- och sjukvård mm" ,
        Näringsgren == "forskning o utveckling; utbildning" ~ "Utbildning",
        Näringsgren == "handel; transport, magasinering; kommunikation" ~ "Handel, transport mm" ,
        Näringsgren == "jordbruk, skogsbruk, jakt, fiske" ~ "Jordbruk och skogsbruk",
        Näringsgren == "kreditinstitut, fastighetsförvaltn, företagstjänster" ~ "Företagstjänster, finans mm",
        Näringsgren == "näringsgren okänd" ~ "Okänd verksamhet" ,
        Näringsgren == "personliga och kulturella tjänster" ~ "Kultur mm",
        Näringsgren == "utvinning av mineral, tillverkningsindustri" ~ "Tillverkning och utvinning"))
    
    diagram_titel <- paste0("Förändring av antalet förvärvsarbetande (16-74) år från år ", min(df_for$år), " till ", max(df_for$år))
    diagramfil <- "forvarvsarbetande_90_forandring.png"
    objektnamn <- c(objektnamn,"forvarvsarbetande_90_forandring")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df_for %>% 
                                      filter(år == max(år),Näringsgren != "Okänd verksamhet"), 
                                    skickad_x_var = "Näringsgren", 
                                    skickad_y_var = "skillnad",
                                    manual_color = vald_farg[1],
                                    diagram_titel = diagram_titel,
                                    x_axis_sort_value = TRUE,
                                    x_axis_lutning = 90,
                                    diagram_capt = diagram_capt,
                                    diagram_liggande = TRUE,
                                    stodlinjer_avrunda_fem = TRUE,
                                    geom_position_stack = TRUE,
                                    output_mapp = output_mapp_figur,
                                    filnamn_diagram = diagramfil,
                                    skriv_till_diagramfil = spara_figur)
    
    gg_list[[i]] <- gg_obj
    i=i+1
  }

  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
}
