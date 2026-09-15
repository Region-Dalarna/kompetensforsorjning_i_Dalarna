diag_arbetsloshet_08 <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 filnamn = "arbetslöshet_08_senastear_bearbetad.xlsx",
                                 returnera_data = FALSE,
                                 spara_data = FALSE,
                                 spara_figur = FALSE){
  
  
  
  # ==================================================================================================
  
  # Arbetslöshet från 2008 och 2023
  # https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik
  # Välj Inskrivna arbetslösa, som andel av registerbaserad arbetskraft 2008 - maj 2023 - Denna data uppdateras inte längre
  # Excelfilen bör laddas ned. Om man istället kopierar och klistrar in blir det något märkligt med datumvariabeln.
  # Därav finns en nedladdad fil för Dalarna (i projektet kvinnor och män) och en nedladdad fil för riket (i detta projekt)
  
  # Från 2024 och framåt används istället data från AF:s verktyg
  # Filtrera på Dalarna/ingen filtrering (ger Sverige) och sedan alla år från 2024 och framåt
  # https://arbetsformedlingen.se/statistik/statistikverktyget/andel-inskrivna-arbetslosa-manad
  # Fil sparas sedan av oklar anledning som xlsb, spara istället som xlsx (och ta bort info längst ned i dokument). Dokumentnamn skall sedan innehålla "arbetsformedlingen" och dalarna/Sverige
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse,
                 readxl,
                 glue)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
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
  
  folder_path <- "G:/skript/projekt/data/kompetensforsorjning/"
  
  # List all files (recursively) and drop directories
  files <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
  files <- files[!file.info(files)$isdir]
  
  # Dalarna
  # Keep files whose *names* contain both words (case-insensitive)
  nm <- basename(files)
  keep <- grepl("arbetsformedlingen", nm, ignore.case = TRUE) &
    grepl("dalarna",        nm, ignore.case = TRUE)
  candidates <- files[keep]
  
  if (!length(candidates)) stop("Hittade inga filer med både 'arbetsformedlingen' och 'Dalarna' i namnet.")
  
  # Pick the most recently modified among the matches
  latest_file <- candidates[which.max(file.info(candidates)$mtime)]
  
  # Dalarna
  #path <- "G:/skript/projekt/data/kompetensforsorjning/arbetsformedlingen_arbetsloshet_Dalarna_2025_09_29.xlsx"
  
  arbetslosa_dalarna_24_df <- read_excel(
    latest_file,
    col_types  = c("date", "text", "numeric", "numeric", "numeric")
  )
  
  ar_manad_capt <- last(manader_bearbeta_scbtabeller(arbetslosa_dalarna_24_df,"Period"))
  
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
  
  # Sverige
  nm <- basename(files)
  keep <- grepl("arbetsformedlingen", nm, ignore.case = TRUE) &
    grepl("Sverige",        nm, ignore.case = TRUE)
  candidates <- files[keep]
  
  if (!length(candidates)) stop("Hittade inga filer med både 'arbetsformedlingen' och 'Dalarna' i namnet.")
  
  # Pick the most recently modified among the matches
  latest_file <- candidates[which.max(file.info(candidates)$mtime)]
  
  # Dalarna
  #path <- "G:/skript/projekt/data/kompetensforsorjning/arbetsformedlingen_arbetsloshet_Dalarna_2025_09_29.xlsx"
  
  arbetslosa_sverige_24_df <- read_excel(
    latest_file,
    col_types  = c("date", "numeric", "numeric", "numeric")
  )
  
  arbetslosa_sverige_24_df$Period <- format(as.Date(arbetslosa_sverige_24_df$Period), "%Y-%m")
  
  arbetslosa_sverige_24_df <- arbetslosa_sverige_24_df %>% 
    mutate(Region = "Sverige") %>% 
    rename(Arbetslöshet = `Inskrivna arbetslösa`) %>% 
    select(1,2,5) %>% 
    mutate(Grupp = "Totalt",
           Arbetslöshet = Arbetslöshet*100,
           Ar = substr(Period,1,4)) %>% 
    group_by(Ar,Region,Grupp) %>% 
    summarize(Arbetslöshet = mean(Arbetslöshet)) %>% 
    ungroup()
  
  # Lägger till data till tidigare
  arbetslosa_utskrift_df <- rbind(arbetslosa_utskrift_df,arbetslosa_dalarna_24_df,arbetslosa_sverige_24_df)
  
  
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Arbetslöshet"= arbetslosa_utskrift_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
  # Returnerar data till global enviroment
  if(returnera_data == TRUE){
    assign("arbetslosa_utskrift_df", arbetslosa_utskrift_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- glue("Källa: Arbetsförmedlingen.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Månadsdata. Diagrammet visar medelvärdet för året.\nData för {ar_manad_capt$år} till och med {ar_manad_capt$månad}")
  
  diagramtitel <- paste0("Arbetslöshet i Dalarna och Sverige")
  diagramfilnamn <- paste0("arbetsloshet_08_senastear.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = arbetslosa_utskrift_df ,
                               skickad_x_var = "Ar",
                               skickad_y_var = "Arbetslöshet",
                               skickad_x_grupp = "Region",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("rus_sex"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               x_axis_lutning = 45,
                               manual_y_axis_title = "procent",
                               stodlinjer_avrunda_fem = TRUE,
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)
  
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
}
