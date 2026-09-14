# Skript som hämtar data och skapar figurer/variabler som används för att skapa markdown-rapporten. Det finns två alternativ för skriptet:

# 1: Kör skriptet utan att uppdatera data - sätt variabeln uppdatera_data till FALSE. Då läses den senast sparade versionen av R-studio global environment in.
# Detta är ett bra alternativ om man enbart vill ändra text eller liknande, men inte uppdatera data.

# 2: Uppdatera data - sätt variabeln uppdatera_data till FALSE. Då uppdateras data, alla figurer skapas på nytt och en ny enviroment sparas.
# Tar längre tid (ett par minuter) och medför en risk att text inte längre är aktuell då figurer har ändrats.

# ====================================================================
# TRIMMAD VERSION - innehåller enbart de ggplot-objekt (och de
# textvariabler som faktiskt används i brödtexten) som förekommer i
# den slimmade rapportversionen. Borttagna block finns kvar i
# originalfilen om de behöver återinföras.
# ====================================================================

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here)

# Skall data uppdateras? Annars läses data in från en sparad global environment-fil.
uppdatera_data = FALSE

if(uppdatera_data == TRUE){
  
  cat("Hämtning av data påbörjad")
  start_time <- Sys.time()
  
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  # ska sättas till FALSE när skriptet är i produktion men smidigare att felsöka fel som inte handlar om rcurl om det är TRUE
  hoppa_over_forsok_igen <- FALSE
  
  Output_mapp_figur <- here("Diagram","/")
  spara_diagram_som_bildfiler <- FALSE
  
  # Diagram - demografisk försörjningskvot
  source(here("Skript","demografisk_forsorjningskvot.R"), encoding="UTF-8")
  gg_demo_forsorjning <- funktion_upprepa_forsok_om_fel( function() {
    diagram_demo_forsorjningkvot_tid_region(region_vekt = c("20","00"),
                                            output_mapp_figur = Output_mapp_figur,
                                            tid_koder = "*",
                                            spara_figur = spara_diagram_som_bildfiler,
                                            returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  forsorjningskov_min_ar <- min(demografisk_forsorjningskvot_df$år)
  forsorjningskov_max_ar <- max(demografisk_forsorjningskvot_df$år)
  
  forsorjningskov_min_ar_dalarna_varde <- demografisk_forsorjningskvot_df %>% filter(region=="Dalarnas län") %>% filter(år == min(år)) %>% .$varde
  forsorjningskov_max_ar_dalarna_varde <- demografisk_forsorjningskvot_df %>% filter(region=="Dalarnas län") %>% filter(år == max(år)) %>% .$varde
  
  
  # Antal utrikes/inrikes födda i arbetsför ålder (20-64 år)
  source("https://raw.githubusercontent.com/Region-Dalarna/integrationen_i_dalarna/refs/heads/master/skript/andel_utrikes_inrikes_tidsserie.R")
  gg_antal_utrikes_inrikes <- funktion_upprepa_forsok_om_fel( function() {
    diag_bef_inr_utr_tid(output_mapp = "Output_mapp_figur",
                         diag_andel = FALSE, # Andel inrikes/utrikes födda i arbetsför ålder
                         diag_antal = TRUE, # Antal "-"
                         stodlinjer_avrunda_fem = FALSE,
                         skriv_diagrambildfil = spara_diagram_som_bildfiler,
                         returnera_data_rmarkdown= TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  antal_utrikes_inrikes_min_ar <- min(antal_utrikes_inrikes_bakgr_df$år)
  antal_utrikes_inrikes_max_ar <- max(antal_utrikes_inrikes_bakgr_df$år)
  
  inrikes_antal_min_ar <- format(plyr::round_any(antal_utrikes_inrikes_bakgr_df %>% filter(år==min(år),födelseregion == "Inrikes född") %>% .$Antal,1000),big.mark = " ")
  inrikes_antal_max_ar <- format(plyr::round_any(antal_utrikes_inrikes_bakgr_df %>% filter(år==max(år),födelseregion == "Inrikes född") %>% .$Antal,1000),big.mark = " ")
  
  # Lediga jobb E1 - NY 7/10
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_lediga_jobb_E1.R")
  gg_lediga_jobb <- funktion_upprepa_forsok_om_fel( function() {
    diagram_lediga_jobb_tid_sektor_E1(region_vekt = "20",
                                      sektor_klartext = c("offentlig sektor", "privat sektor"),
                                      kon_klartext = "totalt",
                                      cont_klartext = "Lediga jobb",           
                                      kvartal_klartext = "9999",
                                      spara_figur=spara_diagram_som_bildfiler,
                                      returnera_data = TRUE,
                                      tid_koder = "*",
                                      output_mapp_figur = Output_mapp_figur)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  lediga_jobb_senaste_ar <- max(lediga_jobb_E1_df$ar)
  antal_lediga_jobb <- sum(lediga_jobb_E1_df %>% filter(ar==max(ar)) %>% .$varde)
  andel_privat_lediga_jobb <- round((lediga_jobb_E1_df %>% filter(ar==max(ar),sektor == "privat sektor") %>% .$varde/antal_lediga_jobb)*100,0)
  antal_lediga_jobb <- format(antal_lediga_jobb,big.mark = " ")
  
  # Kompetensnivå för län och bransch
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_kvalifikationskrav_SCB.R", encoding="UTF-8")
  gg_kvalifikation <- funktion_upprepa_forsok_om_fel( function() {
    diagram_kvalifikationskrav(output_mapp_figur = Output_mapp_figur,
                               spara_figur = spara_diagram_som_bildfiler,
                               stodlinjer_avrunda_fem = FALSE,
                               returnera_figur = TRUE,
                               returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Utbildningsnivå och ålder för län och bransch (antal) - NMS: UPPDATATERAS FÖR HAND. EJ GJORT 2025-09-23
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_bransch_utb_alder_NMS.R", encoding="UTF-8")
  gg_bransch_utb_alder_antal <- funktion_upprepa_forsok_om_fel( function() {
    diag_bransch_utb_alder(output_mapp_figur = Output_mapp_figur,
                           spara_figur = spara_diagram_som_bildfiler,
                           returnera_figur = TRUE,
                           returnera_data = TRUE,
                           andel = FALSE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # skapa df med bransch som har högst antal personer i åldersgruppen 60-74 år
  bransch_aldst_antal <- bransch_alder %>%
    filter(alder %in% c("60-64 år","65-69 år","70-74 år")) %>%
    summarise(antal = sum(antal, na.rm = TRUE), .by = c(ar, lan, bransch)) %>%
    slice_max(antal, n = 1, by = c(ar, lan), with_ties = TRUE)
  
  
  # Arbetskraftsdeltagande - NY 7/10
  source(here("Skript","diagram_arbetskraftsdeltagande.R"), encoding="UTF-8")
  gg_arbetskraftsdeltagande <- funktion_upprepa_forsok_om_fel( function() {
    diagram_arbetskraftsdeltagande_tid_region(region_vekt = "20",
                                              spara_figur=spara_diagram_som_bildfiler,
                                              returnera_data = TRUE,
                                              tid_koder = "*",
                                              kon_klartext = c("kvinnor","män"),
                                              output_mapp_figur = Output_mapp_figur)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  
  arbetskraftsdeltagande_senaste_ar <- max(arbetskraftsdeltagande_df$år)
  arbetskraftsdeltagande_senaste_ar_man <- round(arbetskraftsdeltagande_df %>% filter(kön=="män",år==max(år)) %>% .$arbetskraftsdeltagande,0)
  arbetskraftsdeltagande_senaste_ar_kvinna <- round(arbetskraftsdeltagande_df %>% filter(kön=="kvinnor",år==max(år)) %>% .$arbetskraftsdeltagande,0)
  
  # Förvärvsarbetande senaste observation (uppdelat på bransch) 
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_andel_forvarvsarbetande_bransch.R")
  gg_forv_senastear <- funktion_upprepa_forsok_om_fel( function() {
    diag_sysselsatta_andel(region_vekt = c("20"),
                           output_mapp_figur = Output_mapp_figur,
                           returnera_data = TRUE,
                           spara_figur = spara_diagram_som_bildfiler,
                           returnera_figur = TRUE,
                           diag_lan = FALSE,
                           diag_kommun = FALSE,
                           diag_lan_antal = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Förvärvsarbetande från 1990 till senaste år. Både antal och förändring (från första till sista)
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_forvarvsarbetande_90_senastear_SCB.R")
  gg_forv_90 <- funktion_upprepa_forsok_om_fel( function() {
    diagram_forvarvsarbetande_90(output_mapp_figur = Output_mapp_figur,
                                 spara_figur = spara_diagram_som_bildfiler,
                                 diag_antal = TRUE,
                                 diag_forandring = TRUE,
                                 returnera_figur = TRUE,
                                 returnera_data = TRUE,
                                 vald_farg = diagramfarger("rus_sex"))
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Befolkningsförändring uppdelat på komponent (län)
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_befolkningsforandring.R", encoding="UTF-8")
  gg_bef_for <- funktion_upprepa_forsok_om_fel( function() {
    diagram_befolkningsforandring(output_mapp_figur = Output_mapp_figur,
                                  spara_figur = spara_diagram_som_bildfiler,
                                  tid = c("2010":"2024"),
                                  returnera_figur = TRUE,
                                  returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Befolkningsförändring uppdelat på åldersgrupper inklusive prognos
  source(here("Skript","diagram_befolkningsgrupper_prognos.R"), encoding="UTF-8")
  gg_bef_for_alder <- funktion_upprepa_forsok_om_fel( function() {
    diag_befolkning_aldersgrupper_prognos(output_mapp_figur = Output_mapp_figur,
                                          spara_figur = spara_diagram_som_bildfiler,
                                          returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Utbildningsnivå (bakgrund)
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_inr_utr_fodda_lan_scb.R")
  gg_utbniva_bakgrund <- funktion_upprepa_forsok_om_fel( function() {
    diag_utbniva_inr_utr_fodda_kon_lan(skriv_diagramfil = spara_diagram_som_bildfiler,
                                       output_mapp = Output_mapp_figur,
                                       returnera_df_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Utbildningsnivå från 85 och framåt uppdelat på kön.
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")
  gg_utbniva_85 <- funktion_upprepa_forsok_om_fel( function() {
    diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("20"),
                                       output_mapp = Output_mapp_figur,
                                       diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                       skapa_fil = spara_diagram_som_bildfiler,
                                       diag_hogutb_over_tid = TRUE,
                                       diag_lagutb_over_tid = TRUE,
                                       diag_andel_alla_utbnivaer = TRUE,
                                       vald_utb_niva = "hogutb")
  }, hoppa_over = hoppa_over_forsok_igen)
  
  andel_hogutb_forsta_ar <- min(gg_utbniva_85[[names(gg_utbniva_85)[1]]]$data$år)
  andel_hogutb_sista_ar <- max(gg_utbniva_85[[names(gg_utbniva_85)[1]]]$data$år)
  
  andel_hogutb_kvinnor_min_ar <- round(gg_utbniva_85[[names(gg_utbniva_85)[1]]]$data %>% filter(år == min(år),kön == "kvinnor") %>% .$total,0)
  andel_hogutb_kvinnor_max_ar <- round(gg_utbniva_85[[names(gg_utbniva_85)[1]]]$data %>% filter(år == max(år),kön == "kvinnor") %>% .$total,0)
  
  andel_hogutb_man_min_ar <- round(gg_utbniva_85[[names(gg_utbniva_85)[1]]]$data %>% filter(år == min(år),kön == "män") %>% .$total,0)
  andel_hogutb_man_max_ar <- round(gg_utbniva_85[[names(gg_utbniva_85)[1]]]$data %>% filter(år == max(år),kön == "män") %>% .$total,0)
  
  # Gymnasieantagning, könsuppdelat
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_gymnasiantagning_antal_kon.R")
  gg_gymnasiet_kon <- funktion_upprepa_forsok_om_fel( function() {
    diag_gymnasieantagna_antal(output_mapp_figur = Output_mapp_figur,
                               spara_figur = spara_diagram_som_bildfiler,
                               returnera_figur = TRUE,
                               returnera_data = FALSE,
                               konsuppdelat = TRUE,
                               diag_antal_fleraar = FALSE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Sysselsättningsgrad, tidserie
  source(here("Skript","diagram_sysselsattningsgrad_93.R"), encoding="UTF-8")
  gg_sysselsattningsgrad_93 <- funktion_upprepa_forsok_om_fel( function() {
    diagram_sysselsattningsgrad_93(region_vekt = "20",
                                   spara_figur = spara_diagram_som_bildfiler,
                                   returnera_data = TRUE,
                                   tid_koder = "*",
                                   kon_klartext = c("kvinnor","män"),
                                   output_mapp_figur = Output_mapp_figur)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  syssgrad_93_forsta_ar <- min(forvarvsintensitet_93_df$år)
  syssgrad_93_senaste_ar <- max(forvarvsintensitet_93_df$år)
  
  syssgrad_93_senaste_ar_man_varde <- round(forvarvsintensitet_93_df %>% filter(år == max(år),kön == "män") %>% .$sysselsättningsgrad,0)
  syssgrad_93_senaste_ar_kvinna_varde <- round(forvarvsintensitet_93_df %>% filter(år == max(år),kön == "kvinnor") %>% .$sysselsättningsgrad,0)
  
  # Arbetslöshet tidsserie
  source(here("Skript","diagram_arbetsloshet_08_AF.R"), encoding="UTF-8")
  gg_arbetsloshet_08 <- funktion_upprepa_forsok_om_fel( function() {
    diag_arbetsloshet_08(output_mapp_figur = Output_mapp_figur,
                         returnera_data = TRUE,
                         spara_figur = spara_diagram_som_bildfiler)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Matchning (län och bakgrund)
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_matchning_lan_bakgrund.R", encoding="UTF-8")
  gg_matchning <- funktion_upprepa_forsok_om_fel( function() {
    diag_matchning_lan(output_mapp_figur = Output_mapp_figur,
                       spara_figur = spara_diagram_som_bildfiler,
                       returnera_figur = TRUE,
                       returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  # Kompetensbrist
  source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_kompetensbrist_lan_TVV.R",encoding="UTF-8")
  gg_kompetensbrist <- funktion_upprepa_forsok_om_fel( function() {
    diag_kompetensbrist(output_mapp_figur = Output_mapp_figur,
                        skapa_fil = spara_diagram_som_bildfiler,
                        returnera_figur = TRUE,
                        returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)
  
  kompetensbrist_Dalarna <- round(kompetensbrist %>% filter(År == max(År)) %>% filter(Region == "Dalarna") %>% .$Andel,0)
  kompetensbrist_Dalarna_forandring_2020 <- round(kompetensbrist %>% filter(År == max(År)) %>% filter(Region == "Dalarna") %>% .$Andel - kompetensbrist %>% filter(År == "2020") %>% filter(Region == "Dalarna") %>% .$Andel ,0)
  
  
  save.image(file = "G:/skript/projekt/environments/kompetensforsorjning_i_Dalarna.RData")
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat(sprintf("Hämtning av data klar: Det tog %.2f sekunder.", elapsed_time))
  cat("\n\n")
  
  
}else{
  load("G:/skript/projekt/environments/kompetensforsorjning_i_Dalarna.RData")
} 

# 2. om man vill knitta rapporten
#source(paste0(here("Skript","/"), "2_knitta_rapport.R"))

# 3. om man vill kopiera den till docs, för publicering på webben med Github Pages
#source(paste0(here("Skript","/"), "3_kopiera_till_docs_for_publicera_pa_webben.R"))

# 4. skjut upp hela repositoryt till Github
#source(paste0(here("Skript","/"), "4_push_av_hela_repo_till_github.R"))