# Skapar en figur med utbildningsnivå kopplat till utrikes och inrikes födda
# Skriptet finns på Mona under jon/kompetensförsörjning/utbildningsniva_befolkning_bakgrund.R
pacman::p_load(tidyverse,openxlsx,here)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_utbildning_bakgrund(skapa_fil = FALSE)

diag_utbildning_bakgrund <-function(region_vekt = "20", 
                                         output_mapp = "G:/skript/jon/Slask/",
                                         skapa_fil = TRUE,
                                         diag_bakgrund_tidsserie=TRUE,
                                         diag_bakgrund_utbildning=TRUE,
                                         diag_inriktning_kon=TRUE,
                                         diag_forgymnasial_kon=TRUE){
  
  # ========================================== Inställningar ============================================
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  diagram_capt <- "Källa: NMS-databasen (SCB), databasen Stativ\nBearbetning: Samhällsanalys, Region Dalarna"
  
  vald_region=hamtaregion_kod_namn(region_vekt)[[2]]
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare som används för att lägga till objekt i listan
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  
  # =====================================================================================================
  # Läser in data från Excel (ursprung NMS)
  utbildning_df <- read.xlsx(here("Indata","/","12_sep_22_utdata_utbildningsniva_befolkning_bakgrund.xlsx"),sheet=1)
  utbildning_df_tidsserie <- read.xlsx(here("Indata","/","12_sep_22_utdata_utbildningsniva_befolkning_bakgrund.xlsx"),sheet=2)
  
  # Hela befolkningen, tidsserie - diagram 1
  # Använder faktorvariabler för att byta ordning på utrikes och inrikes födda
  utbildning_df_tidsserie$bakgrund <- factor(utbildning_df_tidsserie$bakgrund, levels = c("Utrikes födda","Inrikes födda"))
  # Gör om antal till tusental
  utbildning_df_tidsserie$antal <- utbildning_df_tidsserie$antal/1000
  
  # # Döper om vissa av utbildningskategorierna
  utbildning_df$Sun2020Niva_namn[utbildning_df$Sun2020Niva_namn %in% c("Eftergymnasial utbildning, 2 år","Eftergymnasial utbildning, kortare än 2 år")]<-"Eftergymnasial utbildning <3år"
  utbildning_df$Sun2020Niva_namn[utbildning_df$Sun2020Niva_namn %in% c("Gymnasial utbildning, 2 år","Gymnasial utbildning, kortare än 2 år")]<-"Gymnasial utbildning högst 2-årig"
  utbildning_df$Sun2020Niva_namn[is.na(utbildning_df$Sun2020Niva_namn)] <- "Okänd"

  # Tittar på utbildning och bakgrund - diagram 2
  utbildning_df_bakgrund <- utbildning_df %>%
    group_by(year,Lan_namn,Kon_namn,bakgrund,Sun2020Niva_namn) %>% 
      summarize(antal=sum(antal)) %>%
        mutate(andel=round((antal/sum(antal))*100,2)) %>% 
          ungroup()

  # Filtrerar bort okänd utbildning
  utbildning_df_bakgrund <- utbildning_df_bakgrund %>% 
    filter(Sun2020Niva_namn!="Okänd")

  # Skapar en faktorvariabel för att få utbildningsnivå i "rätt" ordning i figuren
  utbildning_df_bakgrund$Sun2020Niva_namn <- factor(utbildning_df_bakgrund$Sun2020Niva_namn, levels = c("Eftergymnasial utbildning, 3 år eller längre","Eftergymnasial utbildning <3år",
                                                                                                        "Gymnasial utbildning, 3 år","Gymnasial utbildning högst 2-årig",
                                                                                                        "Förgymnasial utbildning, 9 (10) år","Förgymnasial utbildning kortare än 9 år")[6:1])
  
  # Tittar på utbildning och inriktning - diagram 3
  
  # Skapar tre kategorier av utbildningsnivå
  forgymnasial <- c("Förgymnasial utbildning kortare än 9 år","Förgymnasial utbildning, 9 (10) år")
  gymnasial <- c("Gymnasial utbildning högst 2-årig","Gymnasial utbildning, 3 år")
  eftergymnasial <- c("Eftergymnasial utbildning, 3 år eller längre","Eftergymnasial utbildning <3år")
  
  # Skapar en ny variabel som innehåller dessa kategorier
  utbildning_df$sun_grov <- ifelse(utbildning_df$Sun2020Niva_namn%in%forgymnasial,"Förgymnasial",NA)
  utbildning_df$sun_grov <- ifelse(utbildning_df$Sun2020Niva_namn%in%gymnasial,"Gymnasial",utbildning_df$sun_grov)
  utbildning_df$sun_grov <- ifelse(utbildning_df$Sun2020Niva_namn%in%eftergymnasial,"Eftergymnasial",utbildning_df$sun_grov)
  utbildning_df$sun_grov <- ifelse(utbildning_df$Sun2020Niva_namn=="Okänd","Okänd",utbildning_df$sun_grov)
  
  # Grupperar och beräknar antal
  utbildning_df_inriktning <- utbildning_df %>%
    group_by(year,Lan_namn,Kon_namn,Sun2020Inr_namn,sun_grov) %>%
      summarize(antal=sum(antal)) %>% 
          ungroup()
  
  # Gör om antal till tusental
  utbildning_df_inriktning$antal <- utbildning_df_inriktning$antal/1000
  
  # Använder faktorvariabler för att byta ordning på män och kvinnor
  utbildning_df_inriktning$Kon_namn <- factor(utbildning_df_inriktning$Kon_namn, levels = c("man","kvinna"))
  
  # Fokuserar enbart på förgymnasial utbildning -diagram 4
  utbildning_df_alder <- utbildning_df %>% 
    group_by(year,Lan_namn,Kon_namn,sun_grov,alder_grupper) %>% 
      summarize(antal=sum(antal)) 
  
  # Använder faktorvariabler för att byta ordning på män och kvinnor
  utbildning_df_alder$Kon_namn <- factor(utbildning_df_alder$Kon_namn, levels = c("man","kvinna"))
  
  if(diag_bakgrund_tidsserie==TRUE){
    diagram_titel <- paste0("Befolkning i arbetsför ålder (20-64 år) i ",vald_region, " efter födelseregion")
    diagram_typ <- "fodelseregion_tidsserie"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_tidsserie, 
                                 skickad_x_var = "year", 
                                 skickad_y_var = "antal", 
                                 skickad_x_grupp = "bakgrund",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[5:6],
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 manual_y_axis_title = "Antal personer i tusental",
                                 geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_bakgrund_utbildning==TRUE){
    diagram_titel <- paste0("Befolkningens (20-64 år) utbildningsnivå i ",vald_region," ", unique(utbildning_df_bakgrund$year), " efter kön och födelseregion")
    diagram_typ <- "fodelseregion_utbildning"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_bakgrund %>% 
                                   filter(Lan_namn==vald_region), 
                                 skickad_x_var = "bakgrund", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "Sun2020Niva_namn",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0,
                                 manual_color = diagramfarger("gron_sex")[6:1],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #diagram_liggande = TRUE,
                                 legend_vand_ordning = FALSE,
                                 dataetiketter = TRUE,
                                 dataetiketter_antal_dec = 1,
                                 diagram_facet = TRUE,
                                 facet_legend_bottom = TRUE,
                                 facet_grp = "Kon_namn",
                                 facet_scale = "fixed",
                                 manual_y_axis_title = "procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_inriktning_kon==TRUE){
    diagram_titel <- paste0("Befolkningens (20-64 år) utbildningsnivå i ",vald_region," ", unique(utbildning_df_bakgrund$year)," efter kön, utbildningsnivå och utbildningsgrupp")
    diagram_titel <-str_wrap(diagram_titel,width=80)
    diagram_typ <- "utbildning_inriktning_kon"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)

    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_inriktning %>%
                                   filter(Lan_namn==vald_region,sun_grov!="Förgymnasial",sun_grov!="Okänd"),
                                 skickad_x_var = "Sun2020Inr_namn",
                                 skickad_y_var = "antal",
                                 skickad_x_grupp = "Kon_namn",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon")[2:1],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 diagram_liggande = FALSE,
                                 diagram_facet = TRUE,
                                 facet_legend_bottom = TRUE,
                                 facet_grp = "sun_grov",
                                 facet_scale = "fixed",
                                 legend_vand_ordning = TRUE,
                                 geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 manual_y_axis_title = "Antal personer i tusental",
                                 manual_x_axis_title = "Utbildningsgrupper",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)

    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Använder faktorvariabler för att byta ordning på män och kvinnor
  utbildning_df_alder$alder_grupper <- factor(utbildning_df_alder$alder_grupper, levels = c("60-64 år","55-59 år","50-54 år","45-49 år","40-44 år","35-39 år","30-34 år","25-29 år","20-24 år")[9:1])
  
  
  if(diag_forgymnasial_kon==TRUE){
    diagram_titel <- paste0("Personer (20-64 år) med förgymnasial utbildning i ",vald_region," ",unique(utbildning_df_bakgrund$year), " efter ålder")
    diagram_typ <- "forgymnasial_alder_kon"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_alder %>%
                                   filter(Lan_namn==vald_region,sun_grov=="Förgymnasial"),
                                 skickad_x_var = "alder_grupper",
                                 skickad_y_var = "antal",
                                 skickad_x_grupp = "Kon_namn",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("kon")[2:1],
                                 diagram_titel = diagram_titel,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = FALSE,
                                 diagram_capt = diagram_capt,
                                 diagram_liggande = TRUE,
                                 legend_vand_ordning = TRUE,
                                 geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 manual_y_axis_title = "Antal personer",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
}
