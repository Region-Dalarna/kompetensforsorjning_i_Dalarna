# Skapar en figur branscher tillbaka till 97
# Skriptet finns på Mona under jon/kompetensförsörjning/antal_anstallda_bransch_97.R
pacman::p_load(tidyverse,openxlsx,here)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_branch_fran_97(skapa_fil =FALSE)

diag_branch_fran_97 <-function(region_vekt = "20", 
                                         output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                         skapa_fil = TRUE,
                                         diag_bransch_97=TRUE,
                                         diag_bransch_97_forandring=TRUE){
  
  # ========================================== Inställningar ============================================

  diagram_capt <- "Källa: NMS-databasen (SCB), databasen Stativ\nBearbetning: Samhällsanalys, Region Dalarna"

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare som används för att lägga till objekt i listan
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  
  # =====================================================================================================
  # Läser in data från Excel (ursprung NMS)
  branscher_df <- read.xlsx(here("Indata","/","28_okt_22_forv_branscher_97-.xlsx"),sheet=1)
  branscher_df$Ar<-as.character(branscher_df$Ar)
  
  min_ar<-min(branscher_df$Ar)
  max_ar<-max(branscher_df$Ar)
  
  # För figur 2 skapas två dataset och skillnad3en tas mellan dessa. Vill skapa ett diagram som visar förändringar
  branscher_df_forsta<-branscher_df %>% 
    filter(Ar==min_ar)
  
  branscher_df_Sista<-branscher_df %>% 
    filter(Ar==max_ar)
  
  # Slår ihop de två
  branscher_df_forandring <- merge(branscher_df_forsta,branscher_df_Sista,by=c("SNI2007_Grupp_namn"))
  
  # Beräknar förändring av anställda i en bransch fördelat på utbildningsnivå
  branscher_df_forandring$forandring <- branscher_df_forandring$antal.y-branscher_df_forandring$antal.x
  
  # Gör om år till en faktorvariabel. Detta för att kunna bestämma vilken ordning staplarna för de olika åren kommer i diagrammet (används i det första diagrammet)
  branscher_df$Ar <- factor(branscher_df$Ar, levels = c(max_ar,"2015","2005","1997"))
  
  if(diag_bransch_97==TRUE){
    diagram_titel <- paste0("Förvärvsarbetande 16-74 år i Dalarnas län per bransch")
    diagram_typ <- "forv_bransch_97_"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = branscher_df %>% 
                                   filter(Ar%in%c("1997","2005","2015", max_ar)), 
                                 skickad_x_var = "SNI2007_Grupp_namn", 
                                 skickad_y_var = "antal", 
                                 skickad_x_grupp = "Ar",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=0.5,
                                 manual_color = diagramfarger("gron_sex")[3:6],
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 0,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 facet_grp="Ar",
                                 legend_vand_ordning=TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_bransch_97_forandring==TRUE){
    
    diagram_titel <- paste0("Förändring av antalet förvärvsarbetande 16-74 år per bransch från år ", min_ar, " till ", max_ar)
    diagram_typ <- "forv_bransch_97_forandring"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn<-c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = branscher_df_forandring %>% 
                                   rename("Förändring"=forandring), 
                                 skickad_x_var = "SNI2007_Grupp_namn", 
                                 skickad_y_var = "Förändring",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.5,
                                 manual_color = diagramfarger("gron_sex")[6],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 0,
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
