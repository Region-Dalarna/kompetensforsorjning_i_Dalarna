# Data för examinerade från yrkeshögskolan. 
# Skriptet finns på Mona under P1079gem/peter/YH_uppfoljning_bransch_yrke
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       openxlsx)

# Funktioner som behövs
#source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_lista <-diag_yh_examen(skapa_fil = FALSE)

hamta_data_yh_examen <-function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                spara_data = TRUE,
                                filnamn = "forvarvsarbetande_prognos.xlsx"){
  

  # ========================== Läs in data ======================================
  
  # Läs in datafil med högskoleexamen
  folkhogskola_df <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/13_okt_22_Dalarna_yrkeshogskoleexamen.xlsx", sheet =1)
  
  folkhogskola_df_bransch <- folkhogskola_df %>% 
    group_by(AstSNI2007_namn) %>% 
      summarize(antal=sum(antal)) %>% 
        ungroup()
  
  folkhogskola_df_inriktning <- folkhogskola_df %>% 
    group_by(Sun2000Inr_namn) %>% 
      summarize(antal=sum(antal)) %>% 
        ungroup()
  
  # Väljer de 10 största branscherna totalt 
  folkhogskola_df_bransch_urval <- folkhogskola_df_bransch %>%
    slice_max(antal,n=10)
  
  # Väljer de 10 största inriktningarna totalt 
  folkhogskola_df_inriktning_urval <- folkhogskola_df_inriktning %>%
    slice_max(antal,n=10)
  
  # Grupperar ytterligare en gång men vill ha med båda könen denna gång. Filtrerar även så att vi bara får branscherna i topp 10
  folkhogskola_df_bransch_utskrift <-folkhogskola_df %>% 
    filter(AstSNI2007_namn %in% unique(folkhogskola_df_bransch_urval$AstSNI2007_namn)) %>% 
      group_by(AstSNI2007_namn,Kon_namn) %>% 
        summarize(antal=sum(antal)) %>% 
          ungroup()
  
  folkhogskola_df_inriktning_utskrift <- folkhogskola_df %>%
    filter(Sun2000Inr_namn %in% unique(folkhogskola_df_inriktning_urval$Sun2000Inr_namn)) %>% 
      group_by(Sun2000Inr_namn,Kon_namn) %>% 
        summarize(antal=sum(antal)) %>% 
          ungroup()
  
  if (spara_data==TRUE){
    write.xlsx(prognos_df_sum,paste0(output_mapp,filnamn))
  }
  
  
  # if(diag_yh_examen_bransch==TRUE){
  #   diagram_titel <- paste0("De tio vanligaste branscherna för förvärvsarbetande (16-74 år) i Dalarnas län för YH-utbildade ",max(folkhogskola_df$Ar))
  #   # Rubriken blir för lång. Använder string_wrap för skriva den i två rader
  #   diagram_titel <- str_wrap(diagram_titel)
  #   diagram_typ <- "yh_bransch"
  #   diagramfil <- paste0(diagram_typ, ".png")
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df = folkhogskola_df_bransch_utskrift,
  #                                skickad_x_var = "AstSNI2007_namn", 
  #                                skickad_y_var = "antal", 
  #                                #skickad_x_grupp = "Kon_namn",
  #                                manual_x_axis_text_vjust=0,
  #                                manual_x_axis_text_hjust=0.6,
  #                                manual_color = diagramfarger("gron_fyra")[6],
  #                                diagram_titel = diagram_titel,
  #                                x_axis_sort_value = TRUE,
  #                                diagram_capt = diagram_capt,
  #                                #procent_0_100_10intervaller = TRUE,
  #                                x_axis_lutning = 0,
  #                                #legend_vand_ordning = TRUE,
  #                                diagram_liggande = TRUE,
  #                                #manual_y_axis_title = "Antal examinerade",
  #                                #manual_x_axis_title = "Examensår",
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
  # if(diag_yh_examen_inriktning==TRUE){
  #   diagram_titel <- paste0("De tio vanligaste YH-inriktningarna för förvärvsarbetande (16-74 år) i Dalarnas län  ",max(folkhogskola_df$Ar))
  #   diagram_typ <- "yh_inriktning"
  #   diagramfil <- paste0(diagram_typ, ".png")
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df = folkhogskola_df_inriktning_utskrift,
  #                                skickad_x_var = "Sun2000Inr_namn", 
  #                                skickad_y_var = "antal", 
  #                                #skickad_x_grupp = "Kon_namn",
  #                                manual_x_axis_text_vjust=0,
  #                                manual_x_axis_text_hjust=0.6,
  #                                manual_color = diagramfarger("gron_fyra")[6],
  #                                diagram_titel = diagram_titel,
  #                                x_axis_sort_value = TRUE,
  #                                diagram_capt = diagram_capt,
  #                                #procent_0_100_10intervaller = TRUE,
  #                                x_axis_lutning = 0,
  #                                #legend_vand_ordning = TRUE,
  #                                diagram_liggande = TRUE,
  #                                #manual_y_axis_title = "Antal examinerade",
  #                                #manual_x_axis_title = "Examensår",
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
