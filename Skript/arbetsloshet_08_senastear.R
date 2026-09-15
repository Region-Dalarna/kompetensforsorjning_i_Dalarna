diag_arbetsloshet_08 <- function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 spara_figur = FALSE,
                                 returnera_data = FALSE){
  
  # ========================================== Info ============================================
  # Arbetslöshet från 2008 och framåt, Dalarna och Sverige, totalt (ej uppdelat på kön/bakgrund).
  # Hämtas numera automatiskt från Arbetsförmedlingens statistiksida - manuell nedladdning behövs inte längre.
  # Källa: https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik-tidsserier
  # ========================================== Info ============================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse,
                 rvest,
                 glue)
  
  # Funktioner som behövs (hämtas från Git-Hub)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/kvinnor_man_i_Dalarna/refs/heads/main/Skript/func_hamta_data_kvinnor_man.R")
  
  gg_list <- list()
  
  # ========================================== Läser in data ============================================
  # URL:en till Arbetsförmedlingens statistiksida
  af_url <- "https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik-tidsserier"
  
  af_url_lankar <- read_html(af_url) %>%
    html_elements("a") %>%
    html_attr("href") %>%
    purrr::discard(is.na)
  
  avkodade_lankar <- af_url_lankar %>%
    URLdecode()
  
  # Väljer vilka länkar vi skall hämta data från
  af_index <- which(
    (str_detect(avkodade_lankar, "web-inskrivna-arbetslosa-andel-av-reg") |
       str_detect(avkodade_lankar, "web-inskrivna-arbetslosa-andel-av-bas")) &
      !str_detect(avkodade_lankar, "(?i)arsgenomsnitt")
  )
  
  inlasfil <- af_url_lankar[af_index] %>%
    paste0("https://arbetsformedlingen.se", .)
  
  # En äldre källa (t.o.m. 2022-12) och en nyare (fr.o.m. 2023-01)
  url_old <- inlasfil[2]
  url_new <- inlasfil[1]
  
  old_df <- process_url(url_old)
  new_df <- process_url(url_new)
  
  cutoff_datum <- "2022-12"
  cutoff_ar <- str_sub(cutoff_datum, 1, 4)
  
  # Riksnivån byggs numera upp explicit i get_andel_data() (func_hamta_data_kvinnor_man.R)
  # som en egen totalrad med LAN = KOM = "Sverige", på samma sätt som länstotalerna.
  
  regioner_df <- combine_old_new(old_df, new_df, cutoff = cutoff_datum) %>%
    filter(KOM %in% c("Dalarnas län", "Sverige")) %>%
    separate(PERIOD, c("Ar", "Manad"), "-") %>%
    mutate(Region = ifelse(KOM == "Sverige", "Sverige", "Dalarna"))
  
  # Tar reda på vilka månader som faktiskt finns med för det senaste (ofta ofullständiga)
  # året, så att diagramtexten uppdateras automatiskt utan att behöva ändras för hand.
  svenska_manadsnamn <- c("januari", "februari", "mars", "april", "maj", "juni",
                          "juli", "augusti", "september", "oktober", "november", "december")
  
  senaste_ar <- max(regioner_df$Ar)
  senaste_manad_nr <- regioner_df %>%
    filter(Ar == senaste_ar) %>%
    dplyr::pull(Manad) %>%
    as.numeric() %>%
    max()
  senaste_manad_namn <- svenska_manadsnamn[senaste_manad_nr]
  
  # Plockar ut åldersintervallen (t.ex. "16-64 år"/"16-65 år") ur rubrikraderna som
  # lästes in i process_url(), istället för att återge hela rubriktexten ordagrant.
  alla_rubriker <- c(attr(old_df, "rubrik"), attr(new_df, "rubrik"))
  alder_forekomster <- str_extract(alla_rubriker, "\\d{2}-\\d{2} år") %>%
    unique() %>%
    sort()
  alder_forekomster <- alder_forekomster[!is.na(alder_forekomster)]
  
  alder_capt <- if (length(alder_forekomster) >= 2) {
    glue("Inskrivna arbetslösa i procent av arbetskraften. Till och med {cutoff_ar}, {alder_forekomster[1]}, därefter {alder_forekomster[2]}.")
  } else if (length(alder_forekomster) == 1) {
    glue("Inskrivna arbetslösa i procent av arbetskraften ({alder_forekomster[1]}).")
  } else {
    "Inskrivna arbetslösa i procent av arbetskraften."
  }
  
  arbetslosa_utskrift_df <- regioner_df %>%
    select(c(Ar, Manad, Region, Totalt)) %>%
    rename(Arbetslöshet = Totalt) %>%
    group_by(Ar, Region) %>%
    summarize(Arbetslöshet = mean(Arbetslöshet) * 100, .groups = "drop") %>%
    mutate(Grupp = "Totalt")
  
  rm(old_df, new_df)
  
  # Returnerar data till global environment
  if (returnera_data == TRUE) {
    assign("arbetslosa_utskrift_df", arbetslosa_utskrift_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- paste(c(
    "Källa: Arbetsförmedlingen.",
    "Bearbetning: Samhällsanalys, Region Dalarna.",
    "Diagramförklaring: Månadsdata. Diagrammet visar medelvärdet för året.",
    alder_capt,
    glue("Data för {senaste_ar} till och med {senaste_manad_namn}")
  ), collapse = "\n")
  
  diagramtitel <- paste0("Arbetslöshet i Dalarna och Sverige")
  diagramfilnamn <- paste0("arbetsloshet_08_senastear.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = arbetslosa_utskrift_df,
                               skickad_x_var = "Ar",
                               skickad_y_var = "Arbetslöshet",
                               skickad_x_grupp = "Region",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = diagramfarger("rus_sex"),
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
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
