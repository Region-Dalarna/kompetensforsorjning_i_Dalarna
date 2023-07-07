# Arbetslöshet från 2008 och framåt
# https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik
# Välj Inskrivna arbetslösa, som andel av registerbaserad arbetskraft 2008 - maj 2023
# Excelfilen bör laddas ned. Om man istället kopierar och klistrar in blir det något märkligt med datumvariabeln.
# Därav finns en nedladdad fil för Dalarna (i projektet kvinnor och män) och en nedladdad fil för riket (i detta projekt)
pacman::p_load(openxlsx,
               here,
               tidyverse)

# Funktioner som behövs (hämtas från Git-Hub)
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list=diag_antal_varslade(skapa_fil=FALSE)
hamta_data_arbetsloshet <- function(region_vekt = "20",
                                    output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                    filnamn = "arbetslöshet_08_senastear_bearbetad.xlsx",
                                    spara_data = TRUE){
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen). Den ena datafilen finns i detta projekt, den andra i kvinnor och män
  arbetslosa_Sverige_df <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/Arbetslöshet_08_senastear.xlsx",sheet = "Andel",startRow = 9)
  arbetslosa_Dalarna_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/Arbetslöshet_2008_senastear.xlsx",sheet = "Andel",startRow = 9)
  
  # Lägger till en variabel region för respektive dataset
  arbetslosa_Sverige_df$Region <- "Sverige"
  arbetslosa_Dalarna_df$Region <- "Dalarna"
  
  arbetslosa_df <- rbind(arbetslosa_Sverige_df,arbetslosa_Dalarna_df)
  
  # Bearbetar data - Tar bara ut för inrikes och utrikes födda
  arbetslosa_utskrift_df <- arbetslosa_df %>% 
    separate(PERIOD,c("Ar","Manad"),"-") %>%
      select(c(Ar,Manad,Region,Totalt)) %>% 
        pivot_longer(4,names_to = "Grupp",values_to = "Arbetslöshet") %>% 
          group_by(Ar,Region,Grupp) %>% 
            summarize(Arbetslöshet = mean(Arbetslöshet)*100)
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Arbetslöshet"= arbetslosa_utskrift_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}