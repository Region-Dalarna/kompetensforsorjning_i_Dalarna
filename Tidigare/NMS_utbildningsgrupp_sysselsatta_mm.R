# Skapar en figur med utbildningsgrupper kopplat till olika variabler
# Skriptet finns på Mona under jon/kompetensförsörjning/utbildningsgrupp_sysselsattning_mm (de tre första) och
# jon/kompetensförsörjning/utbildningsgrupp_bransch_mm (flöden - den fjärde)
pacman::p_load(tidyverse,openxlsx,here)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_utbildningsgrupper(skapa_fil = FALSE)

diag_utbildningsgrupper <-function(region_vekt = "20", 
                                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         skapa_fil = TRUE,
                                         valda_utbildningar=c("53B","53R","55A","55C","55H"), 
                                         diag_utbildningsgrupper_sysselsattning=TRUE,
                                         diag_utbildningsgrupper_kon=TRUE,
                                         diag_utbildningsgrupper_alder=TRUE,
                                         diag_utbildningsgrupper_flode=TRUE){
  
  # ========================================== Inställningar ============================================

  diagram_capt <- "Källa: NMS-databasen (SCB), databasen Stativ\nBearbetning: Samhällsanalys, Region Dalarna"
  diagram_capt_kon <- "Källa: NMS-databasen (SCB), databasen Stativ\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Den skuggade ytan visar vad som normalt anses vara en jämn könsfördelning (40%-60%)"
  
  vald_region=hamtaregion_kod_namn(region_vekt)[[2]]
  bransch <- "bygg"
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare som används för att lägga till objekt i listan
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  
  # =====================================================================================================
  # Läser in data från Excel (ursprung NMS)
  utbildningsgrupp_syss_df <- read.xlsx(here("Indata","/","12_sep_22_utdata_utbildningsniva_sysselsattning_mm.xlsx"),sheet=1)
  utbildningsgrupp_kon_df <- read.xlsx(here("Indata","/","12_sep_22_utdata_utbildningsniva_sysselsattning_mm.xlsx"),sheet=2)
  utbildningsgrupp_alder_df <- read.xlsx(here("Indata","/","12_sep_22_utdata_utbildningsniva_sysselsattning_mm.xlsx"),sheet=3)
  utbildningsgrupp_flode_df <- read.xlsx(here("Indata","/","7_sep_22_utdata_utbildningsgrupp_flode_bransch.xlsx"),sheet=1)
  
  # Vill bara fokusera på vald bransch
  utbildningsgrupp_syss_df <- utbildningsgrupp_syss_df %>% 
    filter(utbildningsgrupp_syss_df$Sun2000Grp %in% valda_utbildningar)

  utbildningsgrupp_kon_df <- utbildningsgrupp_kon_df %>% 
    filter(utbildningsgrupp_kon_df$Sun2000Grp %in% valda_utbildningar)

  utbildningsgrupp_alder_df <- utbildningsgrupp_alder_df %>% 
    filter(utbildningsgrupp_alder_df$Sun2000Grp %in% valda_utbildningar)
  
  utbildningsgrupp_flode_df <- utbildningsgrupp_flode_df %>%
    filter(utbildningsgrupp_flode_df$Sun2000Grp %in% valda_utbildningar)
  
  # Skapar en variabel för sysselsättning som ej sysselsatta (dvs. antal-sysselsatta)
  utbildningsgrupp_syss_df <- utbildningsgrupp_syss_df %>% 
    mutate('Ej sysselsatta'=antal-antal_syss)
  
  # Byter namn på två variabler
  utbildningsgrupp_syss_df <- utbildningsgrupp_syss_df %>% 
    rename("Sysselsatta"=antal_syss)
  
  utbildningsgrupp_syss_df <- pivot_longer(utbildningsgrupp_syss_df,cols=c("Sysselsatta","antal","andel_syss","Ej sysselsatta"),names_to = "variabel")

  if(diag_utbildningsgrupper_sysselsattning==TRUE){
    diagram_titel <- paste0("Förvärvsarbetande (20-64 år) per utbildningsgrupp i ",utbildningsgrupp_syss_df$Lan_namn," ", utbildningsgrupp_syss_df$year)
    diagram_typ <- "_utbildningsgrupp_syss"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildningsgrupp_syss_df %>%
                                   filter(variabel%in%c("Sysselsatta","Ej sysselsatta")), 
                                 skickad_x_var = "Sun2000Grp_Namn", 
                                 skickad_y_var = "value", 
                                 skickad_x_grupp = "variabel",
                                 manual_x_axis_text_vjust=0.5,
                                 manual_x_axis_text_hjust=0.3,
                                 manual_y_axis_title="Antal",
                                 manual_color = diagramfarger("gron_sex")[5:6],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 90,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Använder faktorvariabler för att byta ordning på könen
  utbildningsgrupp_kon_df$Kon_namn <- factor(utbildningsgrupp_kon_df$Kon_namn, levels = c("man","kvinna"))
  
  if(diag_utbildningsgrupper_kon==TRUE){
    diagram_titel <- paste0("Könsfördelning (20-64 år) per utbildningsgrupp i  ",vald_region," ",max(utbildningsgrupp_kon_df$year))
    diagram_typ <- paste0("_konsfordelning_utb_grupp_",bransch)
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    # Ritar ut en delvis genomskinlig rektangel 
    rektangel=list(geom = "rect", ymin=40, ymax=60, xmin=0, xmax=Inf, alpha=0.2, fill="grey20")
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildningsgrupp_kon_df, 
                                 skickad_x_var = "Sun2000Grp_Namn", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "Kon_namn",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.5,
                                 manual_y_axis_title="procent",
                                 manual_color = diagramfarger("kon")[2:1],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp =1,
                                 x_axis_lutning = 0,
                                 diagram_capt = diagram_capt_kon,
                                 #procent_0_100_10intervaller = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 fokusera_varden=rektangel,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
    
  }
  
  # Använder faktorvariabler för att byta ordning åldersgrupperna
  utbildningsgrupp_alder_df$alder_grupper <- factor(utbildningsgrupp_alder_df$alder_grupper, levels = c("50-64 år","35-49 år","20-34 år"))
  
  if(diag_utbildningsgrupper_alder==TRUE){
    diagram_titel <- paste0("Åldersfördelningen per utbildningsgrupp i  ",vald_region," ",max(utbildningsgrupp_alder_df$year))
    diagram_typ <- paste0("_aldersniva_utb_grupp_",bransch)
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    #hej=c("rect","0","100","40","60","0.5","black")
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildningsgrupp_alder_df, 
                                 skickad_x_var = "Sun2000Grp_Namn", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "alder_grupper",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.3,
                                 manual_y_axis_title="procent",
                                 manual_color = diagramfarger("gron_sex")[6:4],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp =3,
                                 diagram_capt = diagram_capt,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 #fokusera_varden=hej,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_utbildningsgrupper_flode==TRUE){
    diagram_titel <- paste0("Dag/natt-befolkning (20-64 år) per utbildningsgrupp i Dalarnas län ", utbildningsgrupp_flode_df$year)
    diagram_typ <- "_utbildningsgrupp_flode"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildningsgrupp_flode_df %>% 
                                   rename(nattbefolkning=antal_natt, dagbefolkning = antal_dag) %>%
                                   pivot_longer(cols=c("nattbefolkning","dagbefolkning","Nettoflöde"),names_to = "variabel") %>% 
                                   filter(variabel%in%c("nattbefolkning","dagbefolkning")),
                                 skickad_x_var = "Sun2000Grp_Namn", 
                                 skickad_y_var = "value", 
                                 skickad_x_grupp = "variabel",
                                 manual_x_axis_text_vjust=0.5,
                                 manual_x_axis_text_hjust=0.3,
                                 manual_y_axis_title="Antal",
                                 manual_color = diagramfarger("gron_sex")[5:6],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 90,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = FALSE,
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
