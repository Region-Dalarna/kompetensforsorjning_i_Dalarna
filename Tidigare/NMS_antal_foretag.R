# Skapar en figur med utbildningsnivå kopplat till olika branscher.
# Skriptet finns på Mona under jon/kompetensförsörjning/utbildningsniva_bransch.R
pacman::p_load(tidyverse,openxlsx,here)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_antal_foretag(skapa_fil = FALSE)

diag_antal_foretag <-function(region_vekt = "20", 
                              output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                              skapa_fil = TRUE,
                              diag_foretag_bransch=TRUE,
                              diag_foretag_anstallda=TRUE){
  
  # ========================================== Inställningar ============================================
  
  diagram_capt <- "Källa: NMS-databasen (SCB), databasen Stativ\nBearbetning: Samhällsanalys, Region Dalarna"
  
  vald_region=hamtaregion_kod_namn(region_vekt)[[2]]
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare som används för att lägga till objekt i listan
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  
  # =====================================================================================================
  # Läser in data från Excel (ursprung NMS)
  antal_foretag_bransch_df <- read.xlsx("G:/skript/projekt/kompetens_genomgang/Indata/6_okt_22_antalforetag.xlsx",sheet=1)
  antal_foretag_storlek_df <- read.xlsx("G:/skript/projekt/kompetens_genomgang/Indata/6_okt_22_antalforetag.xlsx",sheet=2)
  
  # Gör om år till en character
  antal_foretag_bransch_df$Ar<-as.character(antal_foretag_bransch_df$Ar)
  antal_foretag_storlek_df$Ar<-as.character(antal_foretag_storlek_df$Ar)
  
  # Använder en faktorvariabel för att styra ordningen på företagsstorlekar
  antal_foretag_storlek_df$anstallda_grupp <- factor(antal_foretag_storlek_df$anstallda_grupp, levels = c("1-9","10-49","50-249","250-499","500-"))
  
  if(diag_foretag_bransch==TRUE){
    diagram_titel <- paste0("Antal företag per bransch i ",vald_region)
    diagram_typ <- "antal_ftg_bransch"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_foretag_bransch_df %>% 
                                   filter(SNI2007_Grupp_namn!="Okänt"), 
                                 skickad_x_var = "SNI2007_Grupp_namn", 
                                 skickad_y_var = "antal_ftg", 
                                 skickad_x_grupp = "Ar",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 x_axis_lutning = 45,
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 #x_axis_sort_grp = 4,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 legend_vand_ordning = FALSE,
                                 diagram_liggande =FALSE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = FALSE,
                                 manual_y_axis_title = "Antal företag",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_foretag_anstallda==TRUE){
    diagram_titel <- paste0("Antal företag per företagsstorlek i ",vald_region)
    diagram_typ <- "antal_ftg_storlek"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_foretag_storlek_df, 
                                 skickad_x_var = "anstallda_grupp", 
                                 skickad_y_var = "antal", 
                                 skickad_x_grupp = "Ar",
                                 # manual_x_axis_text_vjust=1,
                                 # manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 x_axis_lutning = 0,
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = FALSE,
                                 #x_axis_sort_grp = 4,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 legend_vand_ordning = FALSE,
                                 dataetiketter = TRUE,
                                 diagram_liggande = FALSE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = FALSE,
                                 manual_y_axis_title = "Antal företag",
                                 manual_x_axis_title = "företagsstorlek (i antal anställda)",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
}
