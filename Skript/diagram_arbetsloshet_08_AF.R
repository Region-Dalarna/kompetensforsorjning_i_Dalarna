#test_list=diag_antal_varslade(skapa_fil=FALSE)
hamta_data_arbetsloshet <- function(region_vekt = "20",
                                    output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                    filnamn = "arbetslöshet_08_senastear_bearbetad.xlsx",
                                    spara_data = TRUE){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse,
                 readxl)
  
  # ========================================== Läser in data ============================================
  
    # Arbetslöshet från 2008 och 2023
  # https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik
  # Välj Inskrivna arbetslösa, som andel av registerbaserad arbetskraft 2008 - maj 2023 - Denna data uppdateras inte längre
  # Excelfilen bör laddas ned. Om man istället kopierar och klistrar in blir det något märkligt med datumvariabeln.
  # Därav finns en nedladdad fil för Dalarna (i projektet kvinnor och män) och en nedladdad fil för riket (i detta projekt)
  
  # Från 2024 och framåt används istället data från AF:s verktyg
  # Filtrera på Dalarna/Sverige och sedan alla år från 2024 och framåt
  # https://arbetsformedlingen.se/statistik/statistikverktyget/andel-inskrivna-arbetslosa-manad
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen). Den ena datafilen finns i detta projekt, den andra i kvinnor och män
  # Uppdateras inte längre
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
    summarize(Arbetslöshet = mean(Arbetslöshet)*100) %>% 
    ungroup()
  
  # Läser in data för de senaste åren
  
  path <- "G:/skript/projekt/data/kompetensforsorjning/arbetsformedlingen_arbetsloshet_2025_09_29.xlsx"
  
  arbetslosa_dalarna_24_df <- read_excel(
    path,
    col_types  = c("date", "text", "numeric", "numeric", "numeric")
  )
  
  arbetslosa_dalarna_24_df$Period <- format(as.Date(arbetslosa_dalarna_24_df$Period), "%Y-%m")
  
  arbetslosa_dalarna_24_df <- arbetslosa_dalarna_24_df %>% 
    rename(Region = Län,
           Arbetslöshet = `Inskrivna arbetslösa`) %>% 
      select(1:3) %>% 
        mutate(Grupp = "Totalt",
               Region = skapa_kortnamn_lan(Region),
               Arbetslöshet = Arbetslöshet*100,
               Ar = substr(Period,1,4)) %>% 
          group_by(Ar,Region,Grupp) %>% 
          summarize(Arbetslöshet = mean(Arbetslöshet)) %>% 
          ungroup()

  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Arbetslöshet"= arbetslosa_utskrift_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}