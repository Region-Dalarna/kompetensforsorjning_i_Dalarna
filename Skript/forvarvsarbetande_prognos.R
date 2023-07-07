# Skapar en figur med prognoser för utbud och efterfrågan inom olika utbildningsgrupper.
# Från trender och prognoser. Data mm. i "G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_TP_Prognos_bransch(skapa_fil = FALSE)

TP_Prognos_bransch <-function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   spara_data = TRUE,
                                   filnamn = "forvarvsarbetande_prognos.xlsx"){

    # =====================================================================================================
  # Läser in data från Excel (ursprung NMS)
  prognos_df <- read.csv2("G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/02_Tabeller/CSV_filer/Tab5_20.csv",sep='\t')
  
  bransch_nyckel <- read.xlsx("G:/skript/jon/Nycklar/TP_bred_branch.xlsx")
  
  prognos_df <- prognos_df %>% 
    left_join(bransch_nyckel,by=c("ToP_bransch"="Bransch")) %>% 
      mutate(Ar = as.character(Ar))

  # Grupperar på den bredare branschgruppering
  prognos_df_sum <- prognos_df %>% 
    group_by(Ar,SNI2007_Grupp_namn) %>% 
      summarize(sysselsatta=sum(Syss))
  
  # # Skapar ett diagram som jämför efterfrågan och utbud 2035
  # 
  # diagram_titel <- paste0("Prognos för antalet förvärvsarbetande 16-74 år per bransch i Dalarnas län")
  # diagram_typ <- "prognos_branscher"
  # diagramfil <- paste0(diagram_typ, ".png")
  # objektnamn <- c(objektnamn,diagram_typ)
  
  if (spara_data==TRUE){
    write.xlsx(prognos_df_sum,paste0(output_mapp,filnamn))
  }
  
  # gg_obj <- SkapaStapelDiagram(skickad_df = prognos_df_sum %>% 
  #                                filter(Ar%in%c("2018","2025","2035"),SNI2007_Grupp_namn!="Okänt"), 
  #                              skickad_x_var = "SNI2007_Grupp_namn", 
  #                              skickad_y_var = "sysselsatta",
  #                              skickad_x_grupp = "Ar",
  #                              manual_x_axis_text_vjust=1,
  #                              manual_x_axis_text_hjust=1,
  #                              manual_color = diagramfarger("gron_sex")[4:6],
  #                              diagram_titel = diagram_titel,
  #                              #x_axis_sort_value = TRUE,
  #                              diagram_capt = diagram_capt,
  #                              #procent_0_100_10intervaller = TRUE,
  #                              x_axis_lutning = 45,
  #                              x_axis_sort_value = TRUE,
  #                              legend_vand_ordning = FALSE,
  #                              diagram_liggande = FALSE,
  #                              #manual_y_axis_title = "procent",
  #                              berakna_index = FALSE,
  #                              output_mapp = output_mapp,
  #                              filnamn_diagram = diagramfil,
  #                              skriv_till_diagramfil = skapa_fil)
  # 
  # gg_list[[i]] <-gg_obj
  # i=i+1
  # 
  # names(gg_list)<-c(objektnamn)
  # return(gg_list)
}
