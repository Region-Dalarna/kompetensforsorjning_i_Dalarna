# Utbildningsgrupper för samtliga län senaste år
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list=diag_utbniva_85(skapa_fil=FALSE)
hamta_data_utbniva <- function(region_vekt = "20", 
                                output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                spara_data = TRUE,
                                filnamn = "utbildningsniva_senastear.xlsx"){

    # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  
  pxweb_query_list <- 
    list("Region"=hamtaAllaLan(tamedriket = TRUE),
         "Kon"=c("*"),
         "Alder"=c(as.character(25:64)),
         "UtbildningsNiva"=c("*"),
         "ContentsCode"=c("UF0506A1"),
         "Tid"=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  # Download data 
  px_df <-
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning",
              query = pxweb_query_list) %>% 
      as.data.frame(column.name.type = "text", variable.value.type = "text")
  
  px_df$utb_niva<- case_when(
    px_df$utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
    px_df$utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
    px_df$utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
    px_df$utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
    px_df$utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
    px_df$utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
    px_df$utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
    px_df$utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas")
  
  if (spara_data==TRUE){
    openxlsx::write.xlsx(px_df,paste0(output_mapp,filnamn))
  }

  
}
