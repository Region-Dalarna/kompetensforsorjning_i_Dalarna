# Skript som beräknar andelen pendlare på länsnivå
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM/
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list=diag_pendlingsintensitet(skapa_fil=FALSE,)
hamta_data_pendling_lan <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                    spara_data = TRUE,
                                    filnamn = "pendling_lan.xlsx"){
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906B/RegionInd19U2N"

  varlista <- list("Region"=hamtaAllaLan(tamedriket=FALSE),
                   "Utbildngrupp"=c("00N","00I","0","00S"),
                   "Kon"=c("*"),
                   "ContentsCode"=c("000005SJ","000005SK","000005SL","000005SM","000005SN"),
                   "Tid"=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  px_data <- pxweb_get(url = url,query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text")
  
  # Filtrerar dataset och beräknar andelar pendlare
  pendling_df <- px_data %>% 
    filter(utbildning=="samtliga utbildningsnivåer",kön=="totalt") %>%
        mutate(`Andel inpendling`=(`Inpendlare över regiongräns`/`Dagbefolkning (förvärvsarbetande)`)*100,
               `Andel utpendling`=(`Utpendlare över regiongräns`/`Nattbefolkning (förvärvsarbetande)`)*100)
  
  # Tar bort län och s i vissa fall
  pendling_df$region=skapa_kortnamn_lan(pendling_df$region)
  
  if (spara_data==TRUE){
    openxlsx::write.xlsx(pendling_df,paste0(output_mapp,filnamn))
  }

}


