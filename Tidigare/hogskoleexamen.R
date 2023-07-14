# Data för högskoleexamen. Från Mona, Jon/Kompetensförsörjning/hogskoleexamen_ny
library(readxl)
library(tidyr)
library(RColorBrewer)
starttid <- Sys.time()

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test_lista <-diag_hogskoleexamen(skapa_fil = FALSE)

diag_hogskoleexamen <-function(region_vekt = "20", 
                                  output_mapp = "G:/skript/jon/Slask/",
                                  skapa_fil=TRUE,
                                  diag_hogskola_bygg=TRUE,
                                  diag_hogskola_alla=TRUE){
  
  # ============================ Inställningar ====================================
  output_mapp <- output_mapp
  #skriv_diagramfiler <- FALSE             # om TRUE skrivs filer i output-mapp - SÄTTER MAN DEN TILL FALSE FÅR MAN ETT MÄRKLIGT FELMEDDELANDE NÄR MAN FÖRSÖKER SKRIVA UT PPT
  skapa_fil=skapa_fil 
  diagram_capt <- "Källa: NMS-databasen (SCB), högskoleregistret\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Enbart program/områden med minst 5 examinerade"
  
  # Skapar en vektor för objektnamn, en lista som skall hålla ggplot-objekt och en räknare för dessa objekt
  objektnamn <-c()
  gg_list <-list()
  i=1
  # ========================== Läs in data ======================================
  
  # Läs in datafil med högskoleexamen
  hogskoleexamen_df_bygg <- readxl::read_xlsx("G:/skript/projekt/kompetens_genomgang/Indata/7_jul_22_Dalarna_hogskoleexamen.xlsx", sheet =1)
  hogskoleexamen_df_alla <- readxl::read_xlsx("G:/skript/projekt/kompetens_genomgang/Indata/7_jul_22_Dalarna_hogskoleexamen.xlsx", sheet =2)
  # Mall för att få klartext (utbildningsnamn)
  mall_sun <- readxl::read_xlsx("G:/skript/projekt/kompetens_genomgang/Indata/mall_sun2020inr.xlsx", sheet =1)
  hogskoleexamen_df_bygg <-merge(hogskoleexamen_df_bygg,mall_sun)
  hogskoleexamen_df_alla <-merge(hogskoleexamen_df_alla,mall_sun)
  
  # Alla utbildningar
  # Plockar ut de två första siffrorna i sun-koden (för att matcha mot en bredare mall)
  hogskoleexamen_df_alla <- hogskoleexamen_df_alla %>% 
    mutate("SUN2020Inr_2siffer"=substr(SUN2020Inr,1,2))
  
  # Läser in en mall för 2-sifffer och slår ihop den med data för alla utbildning
  mall_sun_2siffer<-readxl::read_xlsx("G:/skript/projekt/kompetens_genomgang/Indata/mall_sun2020inr.xlsx", sheet =2)
  hogskoleexamen_df_alla <-merge(hogskoleexamen_df_alla,mall_sun_2siffer)
  
  # Grupperar alla utbildningar på kategorier(2-siffer)
  hogskoleexamen_df_alla_sum <- hogskoleexamen_df_alla %>% 
    group_by(Lar,SUN2020Inr_2siffer_namn) %>% 
      summarize(antal=sum(antal))
    
  if(diag_hogskola_bygg==TRUE){
    diagram_titel <- paste0("Examen från Högskolan Dalarna i byggrelaterade program")
    diagram_typ <- "hogskoleexamen_bygg"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = hogskoleexamen_df_bygg%>% 
                                   filter(Lar>2011),
                                 skickad_x_var = "Lar", 
                                 skickad_y_var = "antal", 
                                 skickad_x_grupp = "SUN2020Inr_namn",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[5:6],
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 #legend_vand_ordning = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_y_axis_title = "Antal examinerade",
                                 manual_x_axis_title = "Examensår",
                                 #geom_position_stack = TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1 
  }
  
  if(diag_hogskola_alla==TRUE){
    diagram_titel <- paste0("Examen från Högskolan Dalarna ",max(hogskoleexamen_df_alla_sum$Lar)," per program")
    diagram_typ <- "hogskoleexamen_alla"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = hogskoleexamen_df_alla_sum%>% 
                                   filter(antal>4),
                                 skickad_x_var = "SUN2020Inr_2siffer_namn", 
                                 skickad_y_var = "antal", 
                                 #skickad_x_grupp = "SUN2020Inr_namn",
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
                                 manual_y_axis_title = "Antal examinerade",
                                 manual_x_axis_title = "Utbildningsområde",
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
