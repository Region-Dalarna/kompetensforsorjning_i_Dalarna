#test = diagram_data_forvarvsarbetande_90(region_vekt = "20",spara_figur = FALSE,valda_ar = c("1990","2000","2010"),kon_klartext = c("män","kvinnor"))
diagram_kvalifikationskrav <- function(region_vekt = "20", # Vilken region vill man ha. Enbart 1 får väljas
                                              output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                              output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                              spara_figur = TRUE,
                                              filnamn_data = "forvarvsarbetande_bransch.xlsx", # Filnamn på sparad data
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
  objektnamn <- c() # Används för att namnge objekt i lista
  list_data <- lst() # Skapa tom lista som används för att spara till Excel.
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_yrke_bransch_SCB.R")
  
  # Sourcar data för arbetslöshet
  df <-  hamta_data_yrken_bransch(region_vekt = region_vekt,
                                  kon_klartext = kon_klartext,
                                  returnera_data = TRUE) 
  
  # Ändra namn på vissa branscher
  px_df$Branschgrupp <- case_when(
    px_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske" ~ "Jordbruk och skogsbruk",
    px_df$`näringsgren SNI 2007` == "tillverkning och utvinning" ~ "Tillverkning och utvinning",
    px_df$`näringsgren SNI 2007` == "energiförsörjning; miljöverksamhet" ~ "Energi och miljö",
    px_df$`näringsgren SNI 2007` == "byggverksamhet" ~ "Bygg",
    px_df$`näringsgren SNI 2007` == "handel" ~ "Handel",
    px_df$`näringsgren SNI 2007` == "transport och magasinering"~ "Transport",
    px_df$`näringsgren SNI 2007` == "hotell- och restaurangverksamhet" ~ "Hotell och restaurang",
    px_df$`näringsgren SNI 2007` == "information och kommunikation" ~ "IT och kommunikation",
    px_df$`näringsgren SNI 2007` == "finans- och försäkringsverksamhet" ~ "Finans och försäkring",
    px_df$`näringsgren SNI 2007` == "fastighetsverksamhet" ~ "Fastighet",
    px_df$`näringsgren SNI 2007` == "företagstjänster" ~ "Företagstjänster",
    px_df$`näringsgren SNI 2007` == "offentlig förvaltning och försvar"~ "Offentlig förvaltning",
    px_df$`näringsgren SNI 2007` == "utbildning " ~ "Utbildning",
    px_df$`näringsgren SNI 2007` == "vård och omsorg; sociala tjänster" ~ "Vård och omsorg",
    px_df$`näringsgren SNI 2007` == "kulturella och personliga tjänster m.m." ~ "Kultur m.m.",
    px_df$`näringsgren SNI 2007` == "okänd verksamhet"~ "Okänd verksamhet")
  
  # Klassificerar yrken utifrån hur avancerade de är
  px_df$kompetensniva <- case_when(
    substr(px_df$yrkeskod,1,1)=="1" ~ "Chefsyrken",
      between(as.integer(substr(px_df$yrkeskod,1,1)),2,2) ~ "Motsvarande fördjupad högskolekompetens",
        between(as.integer(substr(px_df$yrkeskod,1,1)),3,3) ~ "Motsvarande högskolekompetens",
         between(as.integer(substr(px_df$yrkeskod,1,1)),4,8) ~ "Motsvarande gymnasial kompetens",
          substr(px_df$yrkeskod,1,1) =="0" & px_df$yrkeskod !="0002" ~ "Motsvarande gymnasial kompetens",
             px_df$yrkeskod =="0002" ~ "Okänt",
              substr(px_df$yrkeskod,1,1) =="9" ~ "Enklare yrken")
  
  
  
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
      list_data <- c(list_data,list("Antal" = df_sum))
    }
    
    diagram_capt <- "Källa: Yrkesregistret i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    
    kompetens_lan <- kompetens_yrke_lan %>%
      filter(år == max(år)) %>% 
      group_by(år,region,kompetensniva) %>%
      summarize(Antal=sum(`Anställda.16-64.år.(dagbef)`)) %>%
      mutate(Andel=((Antal/sum(Antal))*100)-0.001)
    
    # Skapar en faktorvariabel för att få utbildningsnivå i "rätt" ordning i figuren
    kompetens_lan$kompetensniva <- factor(kompetens_lan$kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])
    
    diagram_titel <- paste0("Kvalifikationskrav för anställda (16-64 år) ",max(kompetens_lan$år))
    diagramfil <- "kompetenskrav_lan.png"
    
    kompetens_lan_fig <- SkapaStapelDiagram(skickad_df = kompetens_lan %>% 
                                              mutate(region = skapa_kortnamn_lan(region)),
                                            skickad_x_var = "region",
                                            skickad_y_var = "Andel",
                                            skickad_x_grupp = "kompetensniva",
                                            manual_color = c(rgb(211,211,211, maxColorValue = 255),diagramfarger("rus_sex")),
                                            diagram_titel = diagram_titel,
                                            x_axis_lutning = 0,
                                            diagram_capt = diagram_capt,
                                            legend_vand_ordning = TRUE,
                                            diagram_liggande = TRUE,
                                            geom_position_stack = TRUE,
                                            manual_y_axis_title = "procent",
                                            stodlinjer_avrunda_fem = TRUE,
                                            x_axis_sort_value = TRUE,
                                            x_axis_sort_grp = 6,
                                            vand_sortering = TRUE,
                                            output_mapp = "",
                                            filnamn_diagram = diagramfil,
                                            skriv_till_diagramfil = FALSE)
    gg_list[[i]] <- gg_obj
    i=i+1
    
  }
  
  if(diag_forandring == TRUE){
    diagram_capt <- "Källa: RAMS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschgruppering baserad på SNI2002 och SNI92"
    
    df_sum = df %>% 
      group_by(år,region,Näringsgren) %>% 
      summarize(antal = sum(antal)) %>% 
      ungroup()
    
    # Beräknar förändring in antalet anställda från 1990 till senaste år
    df_for <- df_sum %>%
      filter(år %in% c(min(år),max(år))) %>% 
      group_by(region,Näringsgren) %>%
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
    
    # Om användaren vill returnera data görs detta här
    if(returnera_data == TRUE){
      assign("forvarvsarbetande_90_forandring", df_for, envir = .GlobalEnv)
    }
    
    # Om användaren vill spara data görs detta här. Sker enbart om både outputmapp och filnamn har valts
    if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Andel" = df_sum))
    }
    
    diagram_titel <- paste0("Förändring av antalet förvärvsarbetande (16-74) år från år ", min(df_for$år), " till ", max(df_for$år))
    diagramfil <- "forvarvsarbetande_90_forandring.png"
    objektnamn <- c(objektnamn,paste0("forvarvsarbetande_90_forandring_",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)[2])))
    
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
  
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(df_sum,paste0(output_mapp,filnamn))
  }
  
}
