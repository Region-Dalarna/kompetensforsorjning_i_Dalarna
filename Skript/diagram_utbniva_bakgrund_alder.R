#test_list=diag_50proc_lonesumma(spara_figur = TRUE,returnera_data = TRUE)
diag_utb_niva_bakgr_alder <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  spara_figur = TRUE,
                                  returnera_data = FALSE
){
  
  # ========================================== Allmän info ============================================
  # Två figurer: den första är ett facet-diagram som visar utbildningsnivå uppdelat på bakgrund (utrikes/inrikes) och utbildningsnivå (andel eller antal).
  #              den andra visar andel/antal i olika åldersgrupper som har en viss utbildningsnivå (som användaren får välja)
  # För tillfället enbart för Dalarna. Vill man ha annat län måste data hämtas via MONA. Skript finns på:
  # P1079_Gem/Jon/Kompetensförsörjning/utbildningsnivå_befolkning_bakgrund_2024
  # Data uppdaterades senast 9 feb 2024
  # ========================================== Inställningar ============================================
  
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  # Text till diagram
  diagram_capt_kommun <- c("Källa: SCB.\nBearbetning: Samhällsanalys, Region Dalarna.")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung NMS-databasen)
  df <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/9_feb_24_utdata_utbildningsniva_befolkning_bakgrund.xlsx",sheet=1)

  
  if(diag_utb_bakgrund){
    
    utb_niva_bakgrund <- df %>%
      group_by(Ar, Kon_namn,bakgrund, SUN_6kat) %>% 
        summarize(antal = sum(antal)) %>% 
          mutate(andel = round((antal/sum(antal))*100,2))
    
    if(returnera_data == TRUE){
      assign("utb_niva_bakgrund", utb_niva_bakgrund, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Befolkningens (20-64 år) utbildningsnivå i Dalarnas län ", unique(utb_niva_bakgrund$Ar))
    diagramfil <- "fodelseregion_utbildning.png"
    
    utb_bakgrund_fig <- SkapaStapelDiagram(skickad_df = utb_niva_bakgrund %>% 
                                             filter(SUN_6kat != "Okänd") %>% 
                                             mutate(SUN_6kat = factor(SUN_6kat,levels = c("Eftergymnasial utbildning 3 år eller längre","Eftergymnasial utbildning kortare än 3 år",
                                                                                          "Gymnsial utbildning 3 år","Gymnasial utbildning, högst 2-årig",
                                                                                          "Förgymnasial utbildning 9 år","Förgymnasial utbildning kortare än 9 år")[6:1])),
                                           skickad_x_var = "bakgrund", 
                                           skickad_y_var = "andel", 
                                           skickad_x_grupp = "SUN_6kat",
                                           manual_color = diagramfarger("rus_sex"),
                                           diagram_titel = diagram_titel,
                                           x_axis_sort_value = TRUE,
                                           manual_x_axis_text_vjust=1,
                                           manual_x_axis_text_hjust=1,
                                           diagram_capt = diagram_capt,
                                           diagram_facet = TRUE,
                                           facet_legend_bottom = TRUE,
                                           facet_grp = "Kon_namn",
                                           stodlinjer_avrunda_fem = TRUE,
                                           facet_scale = "fixed",
                                           manual_y_axis_title = "procent",
                                           output_mapp = output_mapp,
                                           filnamn_diagram = diagramfilnamn,
                                           skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  
  }
  
  if(diag_utb_alder){
    
    utb_niva_bakgrund <- df %>%
      group_by(Ar, Kon_namn,bakgrund, SUN_6kat) %>% 
      summarize(antal = sum(antal)) %>% 
      mutate(andel = round((antal/sum(antal))*100,2))
    
    if(returnera_data == TRUE){
      assign("utb_niva_bakgrund", utb_niva_bakgrund, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Befolkningens (20-64 år) utbildningsnivå i Dalarnas län ", unique(utb_niva_bakgrund$Ar))
    diagramfil <- "fodelseregion_utbildning.png"
    
    utb_bakgrund_fig <- SkapaStapelDiagram(skickad_df = utb_niva_bakgrund %>% 
                                             filter(SUN_6kat != "Okänd") %>% 
                                             mutate(SUN_6kat = factor(SUN_6kat,levels = c("Eftergymnasial utbildning 3 år eller längre","Eftergymnasial utbildning kortare än 3 år",
                                                                                          "Gymnsial utbildning 3 år","Gymnasial utbildning, högst 2-årig",
                                                                                          "Förgymnasial utbildning 9 år","Förgymnasial utbildning kortare än 9 år")[6:1])),
                                           skickad_x_var = "bakgrund", 
                                           skickad_y_var = "andel", 
                                           skickad_x_grupp = "SUN_6kat",
                                           manual_color = diagramfarger("rus_sex"),
                                           diagram_titel = diagram_titel,
                                           x_axis_sort_value = TRUE,
                                           manual_x_axis_text_vjust=1,
                                           manual_x_axis_text_hjust=1,
                                           diagram_capt = diagram_capt,
                                           diagram_facet = TRUE,
                                           facet_legend_bottom = TRUE,
                                           facet_grp = "Kon_namn",
                                           stodlinjer_avrunda_fem = TRUE,
                                           facet_scale = "fixed",
                                           manual_y_axis_title = "procent",
                                           output_mapp = output_mapp,
                                           filnamn_diagram = diagramfilnamn,
                                           skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  names(gg_list) <- "50_proc_lonesumma"
  return(gg_list)
}