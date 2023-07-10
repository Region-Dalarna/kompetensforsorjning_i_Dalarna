# Skript som beräknar befolkningsförändringen mellan 2010 och det senaste året
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,
               tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list=diag_befolkning_forandring(skapa_fil=FALSE,output_mapp= here("Diagram","/"))
hamta_data_bef_for <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               spara_data = TRUE,
                               filnamn = "befolkningsforandring_20_64.xlsx"){
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url_folkmangd <- c("/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy")
  url_flyttningar <- c("/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97")
  
  # Hämtar det senaste året i tabeller
  senaste_ar <- max(hamta_giltiga_varden_fran_tabell(paste0(url1,url_flyttningar), "tid"))
  
  varlista_folkmangd <- list("Region"=hamtaAllaLan(tamedriket=FALSE),
                             "Alder"=c(as.character(20:64)),
                             "Civilstand"=c("OG","G","SK","ÄNKL"),
                             "Kon"=c("1","2"),
                             "ContentsCode"=c("BE0101N1","BE0101N2"),
                             "Tid"=c(as.character(2010:senaste_ar)))
  
  varlista_flyttningar <-list("Region"=hamtaAllaLan(tamedriket=FALSE),
                              "Alder"=c(as.character(20:64)),
                              "Kon"=c("1","2"),
                              "ContentsCode"=c("BE0101AZ","BE0101A1","BE0101A4"),
                              "Tid"=c(as.character(2010:senaste_ar)))
  
  
  url2=c(url_folkmangd,url_flyttningar)
  
  varlista <- list(varlista_folkmangd,varlista_flyttningar)
  
  # Loop som används för att förbereda data för såväl befintlig data som prognos.
  for(k in 1:length(varlista)){
    url3 <- paste0(url1, url2[[k]])
    
    # Variabler som skall tas ut
    varlista_temp <-  varlista[[k]]
    
    # Uttag av data
    px_uttag <- pxweb_get(url = url3,query = varlista_temp)
    
    # Konverterar data till en Data Frame
    if(k==1) befolkning_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    else flyttningar_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  }
  # summerar flyttningar på regionnivå för åren 2010-senaste år
  flyttningar_df_sum <- flyttningar_df %>%
    filter(år %in% as.character(2010:senaste_ar)) %>% 
      group_by(region) %>% 
        summarize(flyttningsoverskott=sum(Flyttningsöverskott),
                  invandringsoverskott=sum(Invandringsöverskott),
                  inrikes_flyttningsoverskott=sum(`Inrikes flyttningsöverskott`))
  
  # Summerar den totala folkökningen
  befolkning_df_sum <- befolkning_df %>%
    filter(år %in% as.character(2010:senaste_ar)) %>% 
      group_by(region) %>% 
        summarize(folkokning_2010_2021=sum(Folkökning))
  
  # Tar ut befolkningen 2010 och slår ihop med folkökning (för att beräkna förändring)
  befolkning_df_2010 <- befolkning_df %>% 
    filter(år=="2010") %>% 
      group_by(region) %>% 
        summarize(folkmangd_2010=sum(Folkmängd))
  
  befolkning_df_sum <- merge(befolkning_df_sum,befolkning_df_2010)
  
  # Slår ihop befolkning och flyttningar
  slutgiltig_df <-merge(flyttningar_df_sum,befolkning_df_sum)
  
  # Beräknar procentuell förändring av befolkning baserat på olika kompontenter
  slutgiltig_df <- slutgiltig_df %>% 
    mutate(alderskomponent=folkokning_2010_2021-flyttningsoverskott) %>% 
      mutate("inrikes flyttnetto" =round((inrikes_flyttningsoverskott/folkmangd_2010)*100,2),
             "utrikes flyttnetto" =round((invandringsoverskott/folkmangd_2010)*100,2),
             "Demografisk förändring" =round((alderskomponent/folkmangd_2010)*100,2))
  
  # # Väljer ut region och de tre komponenterna
  # utskrift_df <- slutgiltig_df %>% 
  #   select(region,`inrikes flyttnetto`,`utrikes flyttnetto`,`Demografisk förändring`)
  # 
  # # Pivoterar df för att skapa figur
  # utskrift_df <- utskrift_df %>% 
  #   pivot_longer(!c(region),names_to="variabel",values_to="forandring")
  
  # Tar bort län och s i vissa fall
  slutgiltig_df$region=skapa_kortnamn_lan(slutgiltig_df$region,byt_ut_riket_mot_sverige = TRUE)
  
  # Lägger till senaste år
  slutgiltig_df$år <- senaste_ar
  
  if (spara_data==TRUE){
    openxlsx::write.xlsx(slutgiltig_df,paste0(output_mapp,filnamn))
  }

}


