# Befolkning uppdelat på utrikes/inrikes födda.
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list=diag_utbniva_85(skapa_fil=FALSE)
hamta_data_bef_utr <- function(region_vekt = "20", 
                               output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               spara_data = TRUE,
                               filnamn = "befolkning_utr_inr.xlsx"){
  
  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101Q/UtlSvBakgFin"

  pxweb_query_list <- 
    list("Region"=region_vekt,
         'UtlBakgrund' = c("*"),
         "Kon"=c("*"),
         "Alder"=c(as.character(20:64)),
         "ContentsCode"=c("000001NT"),
         "Tid"=c("*"))
  
  # Download data 
  px_df <-
    pxweb_get(url = url,
              query = pxweb_query_list) %>% 
      as.data.frame(column.name.type = "text", variable.value.type = "text") 
  
  px_df <- px_df %>% 
    mutate(bakgrund = ifelse(`utländsk/svensk bakgrund` == "utrikes födda","Utrikes födda","Inrikes födda")) %>% 
      group_by(region,år,bakgrund) %>% 
        summarize(antal = sum(`Antal personer`)) %>% 
          mutate(aldersgrupp = "20-64 år")

  if (spara_data==TRUE){
    openxlsx::write.xlsx(px_df,paste0(output_mapp,filnamn))
  }
  
}
