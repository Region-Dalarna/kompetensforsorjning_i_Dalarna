# Data för högskoleexamen. Från Mona, P1079gem/Jon/kompetensförsörjning/hogskoleexamen_korrekt.R
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

options(dplyr.summarise.inform = FALSE)

#test_lista <-diag_hogskoleexamen(skapa_fil = FALSE)

diag_hogskoleexamen <-function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               spara_data = TRUE,
                               filnamn = "hogskoleexamen.xlsx"){
  

  diagram_capt <- "Källa: NMS-databasen (SCB), högskoleregistret\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Enbart program/områden med minst 5 examinerade"

  # ========================== Läs in data ======================================
  
  # Läs in datafil med högskoleexamen
  hogskoleexamen_df <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/12_jul_23_Dalarna_hogskoleexamen.xlsx")
  # Mall för att få klartext (utbildningsnamn)
  mall_sun <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/mall_sun2020inr.xlsx", sheet = 1)
  mall_sun_2siffer <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/mall_sun2020inr.xlsx", sheet = 2) %>% 
    mutate("SUN2020Inr_2siffer" = as.character(SUN2020Inr_2siffer))

  # Alla utbildningar
  # Plockar ut de två första siffrorna i sun-koden (för att matcha mot en bredare mall)
  hogskoleexamen_df <- hogskoleexamen_df %>% 
    mutate("SUN2020Inr_2siffer"= substr(SUN2020Inr,1,2)) %>% 
      left_join(mall_sun,by = "SUN2020Inr") %>% 
        left_join(mall_sun_2siffer, by = "SUN2020Inr_2siffer") %>% 
          relocate(antal, .after = SUN2020Inr_2siffer_namn)

  # Grupperar på år och den bredare utbildningsnivån (2 siffror). Dessutom fokuserar vi enbart på senaste år
  hogskoleexamen_df_2siffer <- hogskoleexamen_df %>% 
    group_by(Lar,SUN2020Inr_2siffer_namn) %>% 
      summarize(antal=sum(antal))
  
  
  
  flik_lista <- lst("2-siffer" = hogskoleexamen_df_2siffer,"3-siffer" = hogskoleexamen_df %>% select(Lar,SUN2020Inr,SUN2020Inr_namn,antal))
  
  if (spara_data==TRUE){
    write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
  # 
  # if(diag_hogskola_bygg==TRUE){
  #   diagram_titel <- paste0("Examen från Högskolan Dalarna i byggrelaterade program")
  #   diagram_typ <- "hogskoleexamen_bygg"
  #   diagramfil <- paste0(diagram_typ, ".png")
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df = hogskoleexamen_df_bygg%>% 
  #                                  filter(Lar>2011),
  #                                skickad_x_var = "Lar", 
  #                                skickad_y_var = "antal", 
  #                                skickad_x_grupp = "SUN2020Inr_namn",
  #                                manual_x_axis_text_vjust=0,
  #                                manual_x_axis_text_hjust=0.6,
  #                                manual_color = diagramfarger("gron_sex")[5:6],
  #                                diagram_titel = diagram_titel,
  #                                #x_axis_sort_value = TRUE,
  #                                diagram_capt = diagram_capt,
  #                                #procent_0_100_10intervaller = TRUE,
  #                                x_axis_lutning = 0,
  #                                #legend_vand_ordning = TRUE,
  #                                diagram_liggande = FALSE,
  #                                manual_y_axis_title = "Antal examinerade",
  #                                manual_x_axis_title = "Examensår",
  #                                #geom_position_stack = TRUE,
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfil,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1 
  # }
  # 
  # if(diag_hogskola_alla==TRUE){
  #   diagram_titel <- paste0("Examen från Högskolan Dalarna ",max(hogskoleexamen_df_alla_sum$Lar)," per program")
  #   diagram_typ <- "hogskoleexamen_alla"
  #   diagramfil <- paste0(diagram_typ, ".png")
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df = hogskoleexamen_df_alla_sum%>% 
  #                                  filter(antal>4),
  #                                skickad_x_var = "SUN2020Inr_2siffer_namn", 
  #                                skickad_y_var = "antal", 
  #                                #skickad_x_grupp = "SUN2020Inr_namn",
  #                                manual_x_axis_text_vjust=0,
  #                                manual_x_axis_text_hjust=0.6,
  #                                manual_color = diagramfarger("gron_sex")[6],
  #                                diagram_titel = diagram_titel,
  #                                x_axis_sort_value = TRUE,
  #                                diagram_capt = diagram_capt,
  #                                #procent_0_100_10intervaller = TRUE,
  #                                x_axis_lutning = 0,
  #                                #legend_vand_ordning = TRUE,
  #                                diagram_liggande = TRUE,
  #                                manual_y_axis_title = "Antal examinerade",
  #                                manual_x_axis_title = "Utbildningsområde",
  #                                #geom_position_stack = TRUE,
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfil,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1 
  # }
  # 
  # names(gg_list) <- c(objektnamn)
  # return(gg_list)
}
