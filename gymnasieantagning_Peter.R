# Skript som skapar diagram kopplade till gymnasieantagning. Data från Peter
pacman::p_load(tidyverse,pdftools)

source("G:/skript/func/func_gymnasieantagningen.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_lista <-diag_gymnasieantagning_Peter(skapa_fil = FALSE)

diag_gymnasieantagning_Peter <-function(region_vekt = "20", 
                                        output_mapp = "G:/skript/jon/Slask/",
                                        fokus_program = c("Bygg- och anläggningsprogrammet","El- och energiprogrammet","VVS- och fastighetsprogrammet"),
                                        skapa_fil=TRUE,
                                        diag_alla=TRUE,
                                        diag_alla_kon=TRUE,
                                        diag_alla_fleraar=TRUE){
  
  # ============================ Inställningar ====================================
   
  diagram_capt <- "Källa: Källan är Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Antagna i början av september"
  diagram_capt_fleraar<-"Källa: Källan är Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Antagna i början av september\nEnbart program som existerat samtliga år"
  # Skapar en vektor för objektnamn, en lista som skall hålla ggplot-objekt och en räknare för dessa objekt
  objektnamn <-c()
  gg_list <-list()
  i=1
  
  # =========================== läs in gymnasiedata från Gymnasieantagningen =========================
  gymnant_df <- las_in_data_gymnasieantagningen()
  
  gymnant_sum_Dal <- gymnant_df %>% 
    group_by(ar,program) %>% 
      summarize(Män=sum(Ant_Män),
                Kvinnor=sum(Ant_Kv))
  
  # Pivoterar data för att enkelt kunna dela upp på kvinnor och män
  
  gymnant_sum_Dal<-pivot_longer(gymnant_sum_Dal,3:4,names_to="Kon",values_to = "Antagna")
  
  if(diag_alla==TRUE){
    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna ", max(gymnant_sum_Dal$ar)," per program")
    diagram_typ <- "gymnasieantagning_alla_Peter"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = gymnant_sum_Dal%>% 
                                   filter(ar==max(gymnant_sum_Dal$ar)),
                                 skickad_x_var = "program", 
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
  
  
  if(diag_alla_kon==TRUE){
    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna ", max(gymnant_sum_Dal$ar)," per program")
    diagram_typ <- "gymnasieantagning_kon_Peter"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = gymnant_sum_Dal%>% 
                                   filter(ar==max(gymnant_sum_Dal$ar)),
                                 skickad_x_var = "program", 
                                 skickad_y_var = "Antagna", 
                                 skickad_x_grupp = "Kon",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("kon"),
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

  # Tar fram det år som ligger mellan min och max i dataset
  medel_ar=round(((max(as.integer(gymnant_sum_Dal$ar))-min(as.integer(gymnant_sum_Dal$ar)))/2)+min(as.integer(gymnant_sum_Dal$ar)))
  medel_ar<-as.character(medel_ar)
  
  # Välj ut de utbildningar som finns under respektive år. Vill bara ha med utbildningar som finns under alla år
  utb_sista<-unique(gymnant_sum_Dal$program[gymnant_sum_Dal$ar==max(gymnant_sum_Dal$ar)])
  utb_mellan<-unique(gymnant_sum_Dal$program[gymnant_sum_Dal$ar==medel_ar])
  utb_forsta<-unique(gymnant_sum_Dal$program[gymnant_sum_Dal$ar==min(gymnant_sum_Dal$ar)])
  
  # Filtrerar ut de år vi vill fokusera på. Går inte att göra efter att år förvandlats till en faktor
  gymnant_sum_Dal_fleraar<-gymnant_sum_Dal %>% 
    filter(ar%in%c(min(gymnant_sum_Dal$ar),medel_ar,max(gymnant_sum_Dal$ar)))
  
  # Skapar en faktorvariabel för att ändra ordningen på åren
  min<-min(gymnant_sum_Dal_fleraar$ar)
  max=max(gymnant_sum_Dal_fleraar$ar)
  gymnant_sum_Dal_fleraar$ar <- factor(gymnant_sum_Dal_fleraar$ar, levels = c(max,medel_ar,min))
  
  if(diag_alla_fleraar==TRUE){
    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna per program")
    diagram_typ <- "gymnasieantagning_fleraar_Peter"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = gymnant_sum_Dal_fleraar%>% 
                                    filter(program%in%c(utb_forsta)) %>% 
                                      filter(program%in%c(utb_mellan)) %>% 
                                        filter(program%in%c(utb_sista)),
                                 skickad_x_var = "program", 
                                 skickad_y_var = "Antagna", 
                                 skickad_x_grupp = "ar",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt_fleraar,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 legend_vand_ordning = TRUE,
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
  
  names(gg_list) <- c(objektnamn)
  return(gg_list)
}
