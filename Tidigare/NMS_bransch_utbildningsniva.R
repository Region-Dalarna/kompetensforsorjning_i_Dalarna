# Skapar en figur med utbildningsnivå kopplat till olika branscher.
# Skriptet finns på Mona under jon/kompetensförsörjning/utbildningsniva_bransch.R
pacman::p_load(tidyverse,openxlsx,here)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_bransch_utbildning_alder(skapa_fil = FALSE)

diag_bransch_utbildning_alder <-function(region_vekt = "20", 
                                output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                skapa_fil = TRUE,
                                diag_bransch_utbildning=TRUE,
                                diag_bransch_alder=TRUE,
                                diag_bransch_alder_antal=TRUE,
                                diag_bransch_forandring=TRUE,
                                diag_bransch_forandring_ejsun=TRUE){

  # ========================================== Inställningar ============================================
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  diagram_capt <- "Källa: NMS-databasen (SCB), databasen Stativ\nBearbetning: Samhällsanalys, Region Dalarna"

  vald_region=hamtaregion_kod_namn(region_vekt)[[2]]
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare som används för att lägga till objekt i listan
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  
  # =====================================================================================================
  # Läser in data från Excel (ursprung NMS)
  utbildning_df <- read.xlsx(here("Indata","/","13_okt_22_utdata_utbildningsniva_bransch.xlsx"),sheet=1)
  
  # Ändrar från NA till uppgift saknas
  utbildning_df$Sun_namn[is.na(utbildning_df$Sun_namn)] <- "Uppgift saknas"
  
  utbildning_df[utbildning_df=="Eftergymnasial utbildning"]<-"eftergymnasial"
  utbildning_df[utbildning_df=="Förgymnasial utbildning"]<-"förgymnasial"
  utbildning_df[utbildning_df=="grundskola"]<-"förgymnasial"
  utbildning_df[utbildning_df=="Gymnasial utbildning"]<-"gymnasial"
  utbildning_df[utbildning_df=="Uppgift saknas"]<-"Okänd"
  
  # Grupperar på år, län, bransch och utbildning - används i diagram 1.
  #  Drar bort en obefintlig summa för att diagrammet skall gå till 100.
  utbildning_df_bransch_sum <- utbildning_df %>%
    group_by(year,AstLan_namn,SNI2007_Grupp_namn,Sun_namn) %>% 
      summarize(Antal=sum(antal)) %>% 
        mutate(Andel=(Antal/sum(Antal)*100)-0.001)
  
  # Skapar en faktorvariabel för att få utbildningsnivå i "rätt" ordning i figuren
  utbildning_df_bransch_sum$Sun_namn <- factor(utbildning_df_bransch_sum$Sun_namn, levels = c("Okänd","eftergymnasial","gymnasial","förgymnasial"))
  
  # Grupperar på år, län, ålder och bransch - används i diagram 2
  utbildning_df_alder_sum <- utbildning_df %>%
    group_by(year,AstLan_namn,SNI2007_Grupp_namn,alder_grupper) %>% 
      summarize(Antal=sum(antal)) %>% 
        mutate(Andel=(Antal/sum(Antal)*100))
  
  #utbildning_df_alder_sum$alder_grupper <- factor(utbildning_df_alder_sum$alder_grupper, levels = c("50-64 år","35-49 år","20-34 år"))
  utbildning_df_alder_sum$alder_grupper <- factor(utbildning_df_alder_sum$alder_grupper, levels = c("71-74 år","65-70 år","50-64 år","35-49 år","20-34 år","16-19 år"))
  
  # Diagram 3 - en specialare med några fler beräkningar
  # Tar bort okända brancher och filtrerar på Dalarna
  utbildning_df_fokus <- utbildning_df %>% 
    filter(AstLan_namn==vald_region,Sun_namn!="Okänd") %>% 
    group_by(year,SNI2007_Grupp_namn,Sun_namn) %>% 
    summarize(antal=sum(antal))
  
  # Skapar ett dataset för 2008 och ett för 2020
  utbildning_df_2008 <- utbildning_df_fokus %>% 
    filter(year==2008) %>% 
      group_by(SNI2007_Grupp_namn,Sun_namn) %>% 
        summarize(Antal_2008=sum(antal))
  
  utbildning_df_2020 <- utbildning_df_fokus %>% 
    filter(year==2020) %>% 
      group_by(SNI2007_Grupp_namn,Sun_namn) %>% 
        summarize(Antal_2020=sum(antal))

  # Slår ihop de två datasetten
  utbildning_df_total <- merge(utbildning_df_2008,utbildning_df_2020,by=c("SNI2007_Grupp_namn","Sun_namn"))

  # Beräknar förändring av anställda i en bransch fördelat på utbildningsnivå
  utbildning_df_total$forandring <- utbildning_df_total$Antal_2020-utbildning_df_total$Antal_2008
  
  utbildning_df_total$Sun_namn <- factor(utbildning_df_total$Sun_namn, levels = c( "eftergymnasial","gymnasial","förgymnasial")[3:1])
  
  # Diagram 4 - Motsvarande diagram 3 men utan uppdelning på branscher
  # Skapar ett dataset för 2008 och ett för 2020 - som ovan men utan gruppering på sun-kod
  utbildning_df_2008 <- utbildning_df_fokus %>% 
    filter(year==2008) %>% 
      group_by(SNI2007_Grupp_namn) %>% 
        summarize(Antal_2008=sum(antal))
  
  utbildning_df_2020 <- utbildning_df_fokus %>% 
    filter(year==2020) %>% 
      group_by(SNI2007_Grupp_namn) %>% 
        summarize(Antal_2020=sum(antal))
  
  # Slår ihop de två datasetten
  utbildning_df_total_ejsun <- merge(utbildning_df_2008,utbildning_df_2020,by=c("SNI2007_Grupp_namn"))
  
  # Beräknar förändring av anställda i en bransch fördelat på utbildningsnivå
  utbildning_df_total_ejsun$forandring <- utbildning_df_total_ejsun$Antal_2020-utbildning_df_total_ejsun$Antal_2008
  
  
  if(diag_bransch_utbildning==TRUE){
    diagram_titel <- paste0("Utbildningsnivå för förvärvsarbetande 16-74 år per bransch i ",vald_region," ",max(utbildning_df_bransch_sum$year))
    diagram_typ <- "_utbildningsniva_bransch"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    # Skapar en separat färgkod, så att okänd blir ljusgrå
    farger_gra<-c(rgb(211,211,211, maxColorValue = 255),diagramfarger("gron_sex")[4:6])
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_bransch_sum %>% 
                                                filter(AstLan_namn==vald_region,SNI2007_Grupp_namn!="Okänt") %>% 
                                                  filter(year==max(utbildning_df_bransch_sum$year)), 
                                 skickad_x_var = "SNI2007_Grupp_namn", 
                                 skickad_y_var = "Andel", 
                                 skickad_x_grupp = "Sun_namn",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.3,
                                 manual_color = farger_gra,
                                 x_axis_lutning = 0,
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 4,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 manual_y_axis_title = "procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Diagram för ålder, andelar
  if(diag_bransch_alder==TRUE){
    diagram_titel <- paste0("Åldersfördelning för förvärvsarbetande 16-74 år per bransch i ",vald_region," ",max(utbildning_df_alder_sum$year))
    diagram_typ <- "_aldersniva_bransch"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_alder_sum %>% 
                                               filter(AstLan_namn==vald_region,SNI2007_Grupp_namn!="Okänt") %>% 
                                                filter(year==max(utbildning_df_alder_sum$year)), 
                                 skickad_x_var = "SNI2007_Grupp_namn", 
                                 skickad_y_var = "Andel", 
                                 skickad_x_grupp = "alder_grupper",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.3,
                                 manual_color = diagramfarger("gron_sex"),
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_grp = 6,
                                 diagram_capt = diagram_capt,
                                 diagram_liggande = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 manual_y_axis_title = "procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Diagram för ålder, antal
  if(diag_bransch_alder_antal==TRUE){
    diagram_titel <- paste0("Åldersfördelning för förvärvsarbetande 16-74 år per bransch i ",vald_region," ",max(utbildning_df_alder_sum$year))
    diagram_typ <- "_aldersniva_bransch_antal"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_alder_sum %>% 
                                   filter(AstLan_namn==vald_region,SNI2007_Grupp_namn!="Okänt") %>% 
                                   filter(year==max(utbildning_df_alder_sum$year)), 
                                 skickad_x_var = "SNI2007_Grupp_namn", 
                                 skickad_y_var = "Antal", 
                                 skickad_x_grupp = "alder_grupper",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex"),
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 #x_axis_sort_grp = 6,
                                 diagram_capt = diagram_capt,
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 #manual_y_axis_title = "procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  if(diag_bransch_forandring==TRUE){
    
    diagram_titel <- paste0("Förändring av antalet förvärvsarbetande 16-74 år per bransch från år ", min(utbildning_df$year), " till ", max(utbildning_df$year))
    diagram_typ <- "antal_sysselsatta_proc_min_max_ar"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn<-c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_total %>% 
                                   rename("Förändring"=forandring), 
                                 skickad_x_var = "SNI2007_Grupp_namn", 
                                 skickad_y_var = "Förändring", 
                                 skickad_x_grupp = "Sun_namn",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.8,
                                 manual_color = diagramfarger("gron_sex")[6:4],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 90,
                                 diagram_capt = diagram_capt,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 #manual_y_axis_title = "procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_bransch_forandring_ejsun){
    diagram_titel <- paste0("Förändring av antalet förvärvsarbetande 16-74 år per bransch från år ", min(utbildning_df$year), " till ", max(utbildning_df$year))
    diagram_typ <- "antal_sysselsatta_proc_min_max_ar_ejsun"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn<-c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = utbildning_df_total_ejsun %>% 
                                   rename("Förändring"=forandring), 
                                 skickad_x_var = "SNI2007_Grupp_namn", 
                                 skickad_y_var = "Förändring", 
                                 #skickad_x_grupp = "Sun_namn",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.8,
                                 manual_color = diagramfarger("gron_sex")[6],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 90,
                                 diagram_capt = diagram_capt,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 #manual_y_axis_title = "procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
    
names(gg_list)<-c(objektnamn)
return(gg_list)
}
