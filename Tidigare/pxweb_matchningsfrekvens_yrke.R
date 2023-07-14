# Skript som skriver ut matchningsfrekvensen för samtliga yrken samt valda yrken
pacman::p_load(tidyverse,pxweb,httr)
# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_diagramfunktioner.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_text.R", encoding = "utf-8", echo = FALSE)

#bygg=c("53B","53R","55A","55C","55H")
#testlist <- diag_matchad_forvarvsfrekvens(utb_grp=bygg)

diag_matchad_forvarvsfrekvens <- function(regionkod_vekt = "20",
                                          utb_grp = NA,
                                          kon_bakgr = "20-64 år",     # "20-39 år", "kvinnor", "män", "inrikes födda", "utrikes födda"
                                          output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                                          skapa_fil = TRUE,
                                          skickat_ar = NA,
                                          diag_utb_grp_alla = TRUE,
                                          diag_utb_grp_valda = TRUE){
  
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  
  # Mapp där diagrammet hamnar
  
  ValdGeografi <- skapa_kortnamn_lan(hamtaregion_kod_namn(regionkod_vekt)$region)             # hämta namnet på vald(a) region(er)
  
  gg_list <- list()  # skapa en tom lista att lägga ggplot-objekt i (om man skapar flera diagram).
  i=1
  objektnamn <- c() # Skapar en vektor med namn på ggplot-objekten
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  skickad_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19E3N1"
  
  # Variabler som skall tas ut
  varlista <-  list(Region = regionkod_vekt,
                    Utbildning = "*",
                    KonAlderFodelseland = "*",
                    ContentsCode = "000003WV",
                    Tid = "*")
  
  # Uttag av data
  px_uttag <- pxweb_get(url = skickad_url,query = varlista)
  
  # Konverterar data till en Data Frame - ta med region- och landskoder
  match_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(Region, Utbildning)) %>% 
    rename(regionkod = Region, utbildningskod = Utbildning) %>% 
    relocate(regionkod, .before = region) %>% 
    relocate(utbildningskod, .before = utbildning)
  
  if (diag_utb_grp_alla==TRUE){
    # om inget år skickats med, ta det senaste, har det skickats med kontrollera att det är korrekt
    akt_ar <- case_when(is.na(skickat_ar) ~ max(match_df$år),
                        skickat_ar > max(match_df$år) ~ max(match_df$år),
                        skickat_ar < min(match_df$år) ~ min(match_df$år))
    
    # filtrera ut den df vi sen använder för att skapa diagrammet
    chart_matchad_forvfrekv <- match_df %>% 
      select(år, region, utbildningskod, utbildning, `kön/ålder/födelseland`, 
              `Skillnad förvärvsgrad och matchad förvärvsgrad, procentenheter`) %>%
      mutate(`Andel i matchade yrken` = 100 - 
               `Skillnad förvärvsgrad och matchad förvärvsgrad, procentenheter`) %>% 
      filter(`kön/ålder/födelseland` == kon_bakgr) %>% 
      filter(år == max(år)) %>% 
      filter(!utbildningskod %in% c("00N", "00I")) %>% 
      filter(nchar(utbildningskod) > 2) %>% 
      filter(utbildning != "okänd utbildningsnivå") %>% 
      filter(!is.na(`Andel i matchade yrken`)) %>% 
      mutate(fokus = ifelse(utbildningskod %in% utb_grp, 1, 0))
    
    
    if (!is.na(utb_grp[1])) utb_grp_txt <- paste(utb_grp, collapse = "_")
    diagramtitel <-"hej"
    #diagramtitel <- paste0("Andel ", unique(chart_matchad_forvfrekv$`kön/ålder/födelseland`) ," av de förvärvsarbetande per utbildningsgrupp\nsom arbetar i matchade yrken i ", ValdGeografi, " år ", unique(chart_matchad_forvfrekv$år))
    diagramfilnamn <- paste0("Andel_matchade_yrken_",kon_bakgr, "_", ValdGeografi, ifelse(!is.na(utb_grp[1]), utb_grp_txt, "_allagrupper"), ".png")
    objektnamn <- c(objektnamn,paste0("Andel_matchade_yrken_",kon_bakgr, "_", ValdGeografi, ifelse(!is.na(utb_grp[1]), utb_grp_txt),"_allagrupper"))
    
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_matchad_forvfrekv, 
                                 skickad_x_var = "utbildning", 
                                 skickad_y_var = "Andel i matchade yrken", 
                                 skickad_x_grupp = "fokus", 
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.8,
                                 y_axis_storlek = 6,
                                 diagram_titel = diagramtitel,
                                 manual_color = diagramfarger("gron_tva_fokus"),
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 procent_0_100_10intervaller = TRUE,
                                 legend_tabort = TRUE,
                                 x_var_fokus = NA,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 0,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 berakna_index = FALSE,                         
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 diagramfil_hojd = 10,
                                 logga_scaling = 20,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <- gg_obj
    i=i+1
    
  }
  
  # Ser till att användaren måste ha valt en utbildningsgrupp 
  if(is.na(utb_grp)) diag_utb_grp_valda=FALSE
  
  # Ser till att minst en av koderna som användaren valt är korrekt
  k=0
  j=1
  while(j<=length(utb_grp)){
    if(utb_grp[j]%in%match_df$utbildningskod) k=1
    j=j+1
  }
  if(k==0){
    print("OBS!!! Korrekt utbildningskod saknas")
    diag_utb_grp_valda=FALSE
    } 
  
    
  if (diag_utb_grp_valda==TRUE){
    # om inget år skickats med, ta det senaste, har det skickats med kontrollera att det är korrekt
    akt_ar <- case_when(is.na(skickat_ar) ~ max(match_df$år),
                        skickat_ar > max(match_df$år) ~ max(match_df$år),
                        skickat_ar < min(match_df$år) ~ min(match_df$år))
    
    # filtrera ut den df vi sen använder för att skapa diagrammet
    chart_matchad_forvfrekv <- match_df %>% 
      select(år, region, utbildningskod, utbildning, `kön/ålder/födelseland`, 
              `Skillnad förvärvsgrad och matchad förvärvsgrad, procentenheter`) %>%
      mutate(`Andel i matchade yrken` = 100 - 
               `Skillnad förvärvsgrad och matchad förvärvsgrad, procentenheter`) %>% 
      filter(`kön/ålder/födelseland` == kon_bakgr) %>% 
      filter(år == max(år)) %>% 
      filter(!utbildningskod %in% c("00N", "00I")) %>% 
      filter(nchar(utbildningskod) > 2) %>% 
      filter(utbildningskod %in% utb_grp) %>% 
      filter(!is.na(`Andel i matchade yrken`))
    
    if (!is.na(utb_grp[1])) utb_grp_txt <- paste(utb_grp, collapse = "_")
      diagramtitel <- paste0("Andel ", unique(chart_matchad_forvfrekv$`kön/ålder/födelseland`) ," av de förvärvsarbetande per utbildningsgrupp\nsom arbetar i matchade yrken i ", ValdGeografi, " år ", unique(chart_matchad_forvfrekv$år))
      diagramfilnamn <- paste0("Andel_matchade_yrken_",kon_bakgr, "_", ValdGeografi, ifelse(!is.na(utb_grp[1]), utb_grp_txt, "_valdagrupper"), ".png")
      objektnamn <- c(objektnamn,paste0("Andel_matchade_yrken_",kon_bakgr, "_", ValdGeografi, ifelse(!is.na(utb_grp[1]), utb_grp_txt),"_valdagrupper"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = chart_matchad_forvfrekv, 
                   skickad_x_var = "utbildning", 
                   skickad_y_var = "Andel i matchade yrken",
                   manual_x_axis_text_vjust=0.8,
                   manual_x_axis_text_hjust=0.6,
                   #y_axis_storlek = 6,
                   diagram_titel = diagramtitel,
                   manual_color = diagramfarger("gron_sex")[6],
                   diagram_capt =  diagram_capt,
                   manual_y_axis_title = "procent",
                   procent_0_100_10intervaller = TRUE,
                   legend_tabort = TRUE,
                   x_var_fokus = NA,
                   x_axis_sort_value = TRUE,
                   x_axis_lutning = 0,
                   diagram_liggande = TRUE,
                   diagram_facet = FALSE,
                   berakna_index = FALSE,                         
                   output_mapp = output_mapp,
                   filnamn_diagram = diagramfilnamn,
                   #diagramfil_hojd = 10,
                   #logga_scaling = 20,
                   skriv_till_diagramfil = skapa_fil)
      
      gg_list[[i]] <- gg_obj
      i=i+1
  }
  
names(gg_list) <- c(objektnamn)
return(gg_list)
}
