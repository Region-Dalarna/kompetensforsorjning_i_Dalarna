# Skapar en figur med prognoser för utbud och efterfrågan inom olika utbildningsgrupper och utbildningsnivåer.
# Från trender och prognoser. Data mm. i "G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/
pacman::p_load(tidyverse,openxlsx,here)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_TP_Prognos_gym_hog(skapa_fil = FALSE)

diag_TP_Prognos_gym_hog <-function(region_vekt = "20", 
                           output_mapp = "G:/skript/jon/Slask/",
                           skapa_fil = TRUE,
                           diag_prognos_gym=TRUE,
                           diag_prognos_hogskola=TRUE,
                           diag_prognos_yh=TRUE){
  
  # ========================================== Inställningar ============================================
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  diagram_capt_1 <- "Källa: Regionala utbildnings och arbetsmarknadsprognoser, SCB\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: 2019 är verklig data, övriga prognoser"
  diagram_capt_2 <- "Källa: Regionala utbildnings och arbetsmarknadsprognoser, SCB\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: 2019 är verklig data, övriga prognoser\nEnbart program med minst 5 examinerade 2019"                  
  diagram_capt_3 <- "Källa: Regionala utbildnings och arbetsmarknadsprognoser, SCB\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: 2018 är verklig data, övriga prognoser\nEnbart program med minst 5 examinerade 2018\nNotera även att data för 2019 saknas"
  
  vald_region=hamtaregion_kod_namn(region_vekt)[[2]]
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare som används för att lägga till objekt i listan
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  
  # =====================================================================================================
  # Läser in data från Excel (ursprung Trender och prognoser)
  # Utbildningsgrupper
  prognos_df <- read.csv2("G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/02_Tabeller/CSV_filer/Tab1b_20.csv",sep='\t')
  prognos_df_hogskola <- read.csv2("G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/02_Tabeller/CSV_filer/Tab1k_20.csv",sep='\t')
  prognos_df_yh <- read.csv2("G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/02_Tabeller/CSV_filer/Tab1g_20.csv",sep='\t')
  
  #================================#
  #== Bearbetningar gymnasiedata ==#
  #================================#
  
  prognos_df_sum<-prognos_df %>% 
    group_by(pgm_ny,ar) %>% 
      summarize(antal=sum(ak1))
  
  prognos_df_utskrift<-prognos_df_sum %>% 
    filter(ar%in%c(2019,2025,2035))
  
  # Gör om år till en character, så att den är lättare att hantera i diagrammet.
  prognos_df_utskrift$ar<-as.character(prognos_df_utskrift$ar)
  
  # Skapar en faktorvariabel för att få utbildningsnivå i "rätt" ordning i figuren
  prognos_df_utskrift$ar <- factor(prognos_df_utskrift$ar, levels = c("2035","2025","2019"))
  
  #================================#
  #== Bearbetningar högskoledata ==#
  #================================#
  
  prognos_df_hogskola_sum <- prognos_df_hogskola %>% 
    group_by(Kortnamn_modell_alt,ar) %>% 
      summarize(antal=sum(exgen))
  
  # Plockar ut vilka utbildningar som har minst 5 examinerade 2019
  prognos_df_urval <- prognos_df_hogskola_sum %>% 
    filter(ar==2019,antal>5)
  
  # Skapar en variabel baserad på dessa 
  fokus_utbildningar <- unique(prognos_df_urval$Kortnamn_modell_alt)
  
  # Vill bara göra en pronos baserat på utbildningar med mer än 5 examinerade 2019 (sista året som är data)
  prognos_df_hogskola_utskrift <- prognos_df_hogskola_sum %>% 
    filter(ar%in%c(2019,2025,2035),Kortnamn_modell_alt%in%fokus_utbildningar)
  
  # Gör om år till en character, så att den är lättare att hantera i diagrammet.
  prognos_df_hogskola_utskrift$ar<-as.character(prognos_df_hogskola_utskrift$ar)
  
  # Skapar en faktorvariabel för att få utbildningsnivå i "rätt" ordning i figuren
  prognos_df_hogskola_utskrift$ar <- factor(prognos_df_hogskola_utskrift$ar, levels = c("2035","2025","2019"))
  
  #================================#
  #==   Bearbetningar yh-data    ==#
  #================================#
  prognos_df_yh_sum <- prognos_df_yh %>%
    filter(!(is.na(ex))) %>% 
      group_by(Kortnamn_modell_alt,ar) %>% 
        summarize(antal=sum(ex))
  
  prognos_df_urval_yh <- prognos_df_yh_sum %>% 
    filter(ar==2018,antal>5)
  
  # Skapar en variabel baserad på dessa 
  fokus_utbildningar_yh <- unique(prognos_df_urval_yh$Kortnamn_modell_alt)
  
  # Vill bara göra en pronos baserat på utbildningar med mer än 5 examinerade 2019 (sista året som är data)
  prognos_df_yh_utskrift <- prognos_df_yh_sum %>% 
    filter(ar%in%c(2018,2025,2035),Kortnamn_modell_alt%in%fokus_utbildningar_yh)
  
  # Gör om år till en character, så att den är lättare att hantera i diagrammet.
  prognos_df_yh_utskrift$ar<-as.character(prognos_df_yh_utskrift$ar)
  
  # Skapar en faktorvariabel för att få utbildningsnivå i "rätt" ordning i figuren
  prognos_df_yh_utskrift$ar <- factor(prognos_df_yh_utskrift$ar, levels = c("2035","2025","2018"))
  
  
  
  # Skapar ett diagram som jämför antagna till gymnasiet 2019,2025 och 2035
  if(diag_prognos_gym==TRUE){
    diagram_titel <- paste0("Antagna gymnasieelever i Dalarnas län per program")
    diagram_typ <- "gymnasieantagna_prognos"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)

    gg_obj <- SkapaStapelDiagram(skickad_df = prognos_df_utskrift %>% 
                                  filter(antal>10), 
                                 skickad_x_var = "pgm_ny", 
                                 skickad_y_var = "antal",
                                 skickad_x_grupp = "ar",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt_1,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 #manual_y_axis_title = "procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Skapar ett diagram som jämför examinerade från högskolan 2019,2025 och 2035
  if(diag_prognos_hogskola==TRUE){
    diagram_titel <- paste0("Antal examinerade från högskolan i Dalarnas län efter utbildningsgrupp")
    diagram_typ <- "hogskoleexamen_prognos"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)

    gg_obj <- SkapaStapelDiagram(skickad_df = prognos_df_hogskola_utskrift, 
                                 skickad_x_var = "Kortnamn_modell_alt", 
                                 skickad_y_var = "antal",
                                 skickad_x_grupp = "ar",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt_2,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 #manual_y_axis_title = "procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Skapar ett diagram som jämför examinerade från yh 2019,2025 och 2035
  if(diag_prognos_yh==TRUE){
    diagram_titel <- paste0("Antal examinerade från yrkeshögskolan i Dalarnas län efter utbildningsgrupp")
    diagram_typ <- "yrkeshogskoleexamen_prognos"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = prognos_df_yh_utskrift, 
                                 skickad_x_var = "Kortnamn_modell_alt", 
                                 skickad_y_var = "antal",
                                 skickad_x_grupp = "ar",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt_3,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 #manual_y_axis_title = "procent",
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
