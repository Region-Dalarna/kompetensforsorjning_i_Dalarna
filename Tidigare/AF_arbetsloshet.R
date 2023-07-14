# Arbetslöshet från 2008 och framåt
# https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik
pacman::p_load(openxlsx,here,tidyverse)

# Skript som behövs
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_antal_varslade(skapa_fil=FALSE)
diag_arbetsloshet_2008_senaste <- function(region_vekt="20",
                                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                           skapa_fil=TRUE,
                                           diag_arbetsloshet_totalt=TRUE,
                                           diag_arbetsloshet_kon=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Arbetsförmedlingen.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Månadsdata. Diagrammet visar medelvärdet för året"
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()
  i=1 # Räknare som används för att lägga till objekt i listan
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen). Skall användas i flera projekt varför hela sökvägen läses in (blir fel annars)
  arbetslosa_Dalarna_df <- read.xlsx("G:/skript/projekt/kompetens_genomgang/Indata/Arbetslöhet_2008_senastear.xlsx",sheet="Dalarna",startRow=6)
  arbetslosa_Riket_df <- read.xlsx("G:/skript/projekt/kompetens_genomgang/Indata/Arbetslöhet_2008_senastear.xlsx",sheet="Riket",startRow=6)
  
  # Lägger till en variabel region
  arbetslosa_Dalarna_df$Region<-"Dalarna"
  arbetslosa_Riket_df$Region<-"Riket"
  
  # Slår ihop de två datasetten
  arbetslosa_totalt<-rbind(arbetslosa_Dalarna_df,arbetslosa_Riket_df)
  # =========== Årsdata =======================
  # Döper om variabel utan namn till år
  arbetslosa_totalt<- arbetslosa_totalt %>% 
    rename("Ar"="Radetiketter")
  
  # Separerar års och månadsdata
  arbetslosa_totalt= arbetslosa_totalt %>% 
    separate(Ar,c("Ar","Manad"),"-")
  
  # Väljer ut de kategorier vi är intresserade av (Totalt, kön och inrikes/utrikes)
  arbetslosa_totalt<- arbetslosa_totalt %>% 
    select(c(Ar,Manad,Region,Totalt,Kvinnor,Män,Inrikesfödda,Inrikes.kvinnor,Inrikes.män,Utrikesfödda,Utrikes.kvinnor,Utrikes.män))
  
  # Pivoterar så att länsnamnen hamnar som grupper snarare än kolumner
  arbetslosa_totalt<-pivot_longer(arbetslosa_totalt,4:12,names_to = "Grupp",values_to = "Arbetslöshet")
  
  # Grupperar på år och grupper och beräknar årsdata som genomsnitt av månadsdata
  arbetslosa_totalt_utskrift<-arbetslosa_totalt %>% 
    group_by(Ar,Region,Grupp) %>% 
      summarize(Arbetslöshet=mean(Arbetslöshet)*100)

  
  # Diagram 1 - arbetslöshet utan uppdelning
  if(diag_arbetsloshet_totalt==TRUE){
    diagramtitel <- paste0("Arbetslöshet (16-64 år) i Dalarna och Riket per år")
    diagramfilnamn <- paste0("arbetsloshet_Dalarna_tidsserie.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =arbetslosa_totalt_utskrift %>% 
                                   filter(Grupp=="Totalt"),
                                 skickad_x_var = "Ar", 
                                 skickad_y_var = "Arbetslöshet", 
                                 skickad_x_grupp = "Region",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=0.5,
                                 manual_color = diagramfarger("gron_sex")[5:6],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 0,
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning=FALSE,
                                 manual_y_axis_title = "procent",
                                 geom_position_stack = FALSE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # I nästa steg vill vi bara fokusera på inrikes/utrikes (kopplat till kön). Eftersom data är lite konstigt upplagt blir detta en lite annorlunda lösning
  # Män
  arbetslosa_bakgrund<-arbetslosa_totalt %>% 
    filter(Grupp%in%c("Inrikes.kvinnor","Inrikes.män","Utrikes.kvinnor","Utrikes.män"))
  
  # Separerar kön från bakgrund
  arbetslosa_bakgrund= arbetslosa_bakgrund %>% 
    separate(Grupp,c("Grupp","Kon"),sep = "\\.")
  
  # Ändrar namn
  #arbetslosa_bakgrund$Kon<-ifelse(arbetslosa_bakgrund$Kon=="kvinnor","kvinna","man")
  arbetslosa_bakgrund$Grupp<-ifelse(arbetslosa_bakgrund$Grupp=="Utrikes","utrikes född","inrikes född")
  
  # Grupperar på år och grupper och beräknar årsdata som genomsnitt av månadsdata
  arbetslosa_bakgrund_utskrift<-arbetslosa_bakgrund %>% 
    group_by(Ar,Region,Grupp,Kon) %>% 
      summarize(Arbetslöshet=mean(Arbetslöshet)*100)
  
  # Diagram 1 - arbetslöshet uppdelat på kön och bakgrund
  if(diag_arbetsloshet_kon==TRUE){
    diagramtitel <- paste0("Arbetslöshet (16-64 år) i Dalarna per år, kön och bakgrund")
    diagramfilnamn <- paste0("arbetsloshet_Dalarna_tidsserie_bakgrund.png")
    objektnamn <- c(objektnamn,diagramtitel)
    
    gg_obj <- SkapaStapelDiagram(skickad_df =arbetslosa_bakgrund_utskrift %>% 
                                   filter(Region=="Dalarna"),
                                 skickad_x_var = "Ar", 
                                 skickad_y_var = "Arbetslöshet", 
                                 skickad_x_grupp = "Kon",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=0.5,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = TRUE,
                                 facet_legend_bottom = TRUE,
                                 facet_grp = "Grupp",
                                 facet_scale = "fixed",
                                 x_axis_lutning = 0,
                                 diagram_liggande = FALSE,
                                 legend_vand_ordning=FALSE,
                                 manual_y_axis_title = "procent",
                                 geom_position_stack = FALSE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  
  names(gg_list)<-objektnamn
  return(gg_list)
}