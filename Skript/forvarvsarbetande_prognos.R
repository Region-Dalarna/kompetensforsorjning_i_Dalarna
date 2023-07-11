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
  # Läser in data från Excel
  prognos_df <- read.csv2("G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/02_Tabeller/CSV_filer/Tab5_20.csv",sep='\t')
  
  bransch_nyckel <- read.xlsx("G:/skript/jon/Nycklar/TP_bred_branch.xlsx")
  
  prognos_df <- prognos_df %>% 
    left_join(bransch_nyckel,by=c("ToP_bransch"="Bransch")) %>% 
      mutate(Ar = as.character(Ar))

  # Grupperar på den bredare branschgruppering
  prognos_df_sum <- prognos_df %>% 
    group_by(Ar,SNI2007_Grupp_namn) %>% 
      summarize(sysselsatta=sum(Syss))

  if (spara_data==TRUE){
    write.xlsx(prognos_df_sum,paste0(output_mapp,filnamn))
  }

}
