# Gymnasieantagningen i Dalarnas skolor. Data från en Excelfil som Peter fått från elevantagningen
library(readxl)
library(tidyr)
library(RColorBrewer)
starttid <- Sys.time()

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test_lista <-diag_gymnasieantagning(skapa_fil = FALSE)

diag_gymnasieantagning <-function(region_vekt = "20", 
                                  output_mapp = "G:/skript/jon/Slask/",
                                  fokus_program = c("Bygg- och anläggningsprogrammet","El- och energiprogrammet","VVS- och fastighetsprogrammet"),
                                  skapa_fil=TRUE,
                                  diag_alla=TRUE,
                                  diag_fokus=TRUE){
  
  # ============================ Inställningar ====================================
  
  inlasningsmapp <- "G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Gymnasieantagning/"
  inlasningsfil <- "Statistik gymnasiet sökande och antagna.xlsm"
  output_mapp <- output_mapp
  #skriv_diagramfiler <- FALSE             # om TRUE skrivs filer i output-mapp - SÄTTER MAN DEN TILL FALSE FÅR MAN ETT MÄRKLIGT FELMEDDELANDE NÄR MAN FÖRSÖKER SKRIVA UT PPT
  skapa_fil=skapa_fil 
  diagram_capt <- "Källa: Elevantagningen, Region Dalarna\nBearbetning:Samhällsanalys, Region Dalarna"
  
  # Skapar en vektor för objektnamn, en lista som skall hålla ggplot-objekt och en räknare för dessa objekt
  objektnamn <-c()
  gg_list <-list()
  i=1
  # ========================== Läs in data ======================================
  
  # Läs in datafil med företagsuppgifter
  gymnasieantagning_df <- readxl::read_xlsx(paste0(inlasningsmapp, inlasningsfil), sheet ="Data")
  
  gymnasieantagning_df_sum <- gymnasieantagning_df %>% 
    group_by(År,ProgramNamn) %>% 
    summarize(Antagna=sum(Antagna),
              Antagna_man=sum(`Antagna, män`),
              Antagna_kvinnor=sum(`Antagna, kv`))
  
  gymnasieantagning_df_sum_test <- gymnasieantagning_df %>% 
    group_by(År) %>% 
    summarize(Antagna=sum(Antagna),
              Antagna_man=sum(`Antagna, män`),
              Antagna_kvinnor=sum(`Antagna, kv`))
  
  gymnasieantagning_df_sum[gymnasieantagning_df_sum=="Flygteknikutbildningen, Marinteknikutbildningen, Sjöfartsutbildningen, Tågteknikutbildningen, Utbildningen samiska näringar eller Yrkesdansarutbildningen"] <- "Flygteknik mm"

  # Program att fokusera på (diagram 2, fokus)
  fokus_program <- fokus_program 
  
  # Gör om år till en character
  gymnasieantagning_df_sum$År <- as.character(gymnasieantagning_df_sum$År)
  
  if(diag_alla==TRUE){
    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna ", max(gymnasieantagning_df_sum$År)," per program")
    diagram_typ <- "gymnasieantagning_alla"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)

    gg_obj <- SkapaStapelDiagram(skickad_df = gymnasieantagning_df_sum%>% 
                                   filter(År==max(gymnasieantagning_df_sum$År),!is.na(ProgramNamn)),
                                 skickad_x_var = "ProgramNamn", 
                                 skickad_y_var = "Antagna", 
                                 #skickad_x_grupp = "ProgramNamn",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[6],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 #legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 manual_y_axis_title = "Antagna elever",
                                 #geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1 
  }
  
  if(diag_fokus==TRUE){
    diagram_titel <- paste0("Antagna gymnasieelever på byggrelaterade program i Dalarna")
    diagram_typ <- "gymnasieantagning_fokus"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = gymnasieantagning_df_sum %>% 
                                   filter(ProgramNamn%in%fokus_program),
                                 skickad_x_var = "År", 
                                 skickad_y_var = "Antagna", 
                                 skickad_x_grupp = "ProgramNamn",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 #legend_vand_ordning = TRUE,
                                 #diagram_liggande = TRUE,
                                 manual_y_axis_title = "Antagna elever",
                                 #geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }

  names(gg_list) <- c(objektnamn)
  return(gg_list)
}
