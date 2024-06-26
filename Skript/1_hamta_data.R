# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

Output_mapp="G:/skript/projekt/data/kompetensforsorjning/"
Output_mapp_figur <- here("Diagram","/")

# Befolknings uppdelat på åldersgrupp
# source(here("Skript","befolkning_aldersgrupp_prognos.R"), encoding="UTF-8")
# diag_befolkning(output_mapp = Output_mapp,
#                 spara_data = TRUE)

# Förvärvsarbetande från 1990 till senaste år. Både antal och förändring (från första till sista)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_forvarvsarbetande_90_senastear_SCB.R")
gg_forv_90 <- diagram_forvarvsarbetande_90(output_mapp_figur = Output_mapp_figur,
                                           spara_figur = TRUE,
                                           diag_antal = TRUE,
                                           diag_forandring = TRUE,
                                           returnera_figur = TRUE,
                                           returnera_data = TRUE,
                                           vald_farg = diagramfarger("rus_sex"))

# Utbildningsnivå från 85 och framåt uppdelat på kön. Data hämtas i detta fall från GGplot-objektet (när data används i markdown) FEL
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")
gg_utbniva_85 <- diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("20"),
                                                    output_mapp = Output_mapp_figur,
                                                    diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                                    skapa_fil = TRUE,
                                                    diag_hogutb_over_tid = TRUE,
                                                    diag_lagutb_over_tid = TRUE,
                                                    diag_andel_alla_utbnivaer = TRUE,
                                                    vald_utb_niva = "hogutb")

# source("G:/skript/diagram/diag_utbniva_over_tid_och_andel_specifikt_ar.R")
# gg_utbniva_85 <- diag_utbniva_lang_tidserie(region_vekt = c("20"),
#                                             output_mapp = Output_mapp_figur,
#                                             diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
#                                             skapa_fil = TRUE,
#                                             diag_hogutb_over_tid = TRUE,
#                                             diag_lagutb_over_tid = FALSE,
#                                             diag_andel_alla_utbnivaer = FALSE,
#                                             diag_andel_eftergymn_jmfr_lan = FALSE)

# Utbildningsnivå senaste år FEL
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")
gg_utbniva_senastear <- diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("20"),
                                                           output_mapp = Output_mapp_figur,
                                                           diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                                           skapa_fil = TRUE,
                                                           diag_hogutb_over_tid = FALSE,
                                                           diag_lagutb_over_tid = FALSE,
                                                           diag_andel_alla_utbnivaer = FALSE,
                                                           diag_andel_utbniva_jmfr_lan = TRUE,
                                                           vald_utb_niva = "hogutb")

# source("G:/skript/diagram/diag_utbniva_over_tid_och_andel_specifikt_ar.R")
# gg_utbniva_senastear <- diag_utbniva_lang_tidserie(region_vekt = c("20"),
#                                                    output_mapp = Output_mapp_figur,
#                                                    diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
#                                                    skapa_fil = TRUE,
#                                                    diag_hogutb_over_tid = FALSE,
#                                                    diag_lagutb_over_tid = FALSE,
#                                                    diag_andel_alla_utbnivaer = FALSE,
#                                                    diag_andel_eftergymn_jmfr_lan = TRUE,
#                                                    minst_3_ar = TRUE)

# Gymnasieantagning, senaste och flera år
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_gymnasiantagning_antal_kon.R")
# Inte könsuppdelat
gg_gymnasiet <- diag_gymnasieantagna_antal(output_mapp_figur = Output_mapp_figur,
                                          returnera_figur = TRUE,
                                          returnera_data = TRUE)

# Könsuppdelat
gg_gymnasiet_kon <- diag_gymnasieantagna_antal(output_mapp_figur = Output_mapp_figur,
                                              returnera_figur = TRUE,
                                              returnera_data = FALSE,
                                              konsuppdelat = TRUE,
                                              diag_antal_fleraar = FALSE)

# Högskoleexamen
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_examen_hogskolan_NMS.R")
gg_hogskoleexamen <- diagram_examen_hogskolan_NMS (output_mapp_figur = Output_mapp_figur,
                                                   returnera_figur = TRUE,
                                                   returnera_data = TRUE,
                                                   spara_figur = TRUE)

# YH-utbildning
source(here("Skript","diagram_examen_yh_NMS.R"), encoding="UTF-8")
#source("C:/Users/frkjon/Projekt/kompetensforsorjning_i_Dalarna/Skript/diagram_examen_yh_NMS.R")
gg_yh <- diagram_examen_yh_NMS(output_mapp_figur = Output_mapp_figur,
                               returnera_figur = TRUE,
                               returnera_data = TRUE,
                               spara_figur = TRUE)

# Arbetslöshet 08-senaste år. Excel, Arbetsförmedlingen - KVAR
# source(here("Skript","arbetsloshet_08_senastear.R"), encoding="UTF-8")
# hamta_data_arbetsloshet(output_mapp = Output_mapp,
#                         spara_data = TRUE)

# Förvärvsarbetande senaste observation (uppdelat på kön) 
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_andel_forvarvsarbetande_bransch.R")
gg_forv_senastear <- diag_sysselsatta_andel(region_vekt = c("20"),
                                            output_mapp_figur = Output_mapp_figur,
                                            returnera_data = TRUE,
                                            spara_figur = TRUE,
                                            returnera_figur = TRUE,
                                            diag_lan = FALSE,
                                            diag_kommun = FALSE,
                                            diag_lan_antal = TRUE)

# source(here("Skript","diagram_forvarvsarbetande_90_senastear_SCB.R"), encoding="UTF-8")
# gg_forv_forandring <- diagram_data_forvarvsarbetande_90(output_mapp_figur = Output_mapp_figur,
#                                                         spara_figur = TRUE,
#                                                         returnera_figur = TRUE,
#                                                         returnera_data = TRUE,
#                                                         diag_antal = FALSE,
#                                                         diag_forandring =TRUE,
#                                                         vald_farg = diagramfarger("rus_sex"))

# # Förvärvsarbetande prognos - Excel - Trender och prognoser
# source(here("Skript","forvarvsarbetande_prognos.R"), encoding="UTF-8")
# TP_Prognos_bransch(spara_data = TRUE,
#                    output_mapp = Output_mapp)

# Kompetensnivå för län och bransch
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_kvalifikationskrav_SCB.R", encoding="UTF-8")
gg_kvalifikation <- diagram_kvalifikationskrav(output_mapp_figur = Output_mapp_figur,
                                               returnera_figur = TRUE,
                                               returnera_data = TRUE)

# Utbildningsnivå och ålder för län och bransch. Andel och antal
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_bransch_utb_alder_NMS.R", encoding="UTF-8")
gg_bransch_utb_alder <- diag_bransch_utb_alder(output_mapp_figur = Output_mapp_figur,
                                               returnera_figur = TRUE,
                                               returnera_data = TRUE,
                                               andel = TRUE)

gg_bransch_utb_alder_antal = diag_bransch_utb_alder(output_mapp_figur = Output_mapp_figur,
                                                    returnera_figur = TRUE,
                                                    returnera_data = TRUE,
                                                    andel = FALSE)

# Befolkningsförändring uppdelat på komponent (län)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_befolkningsforandring.R", encoding="UTF-8")
gg_bef_for <- diagram_befolkningsforandring(output_mapp_figur = Output_mapp_figur,
                                            returnera_figur = TRUE,
                                            returnera_data = TRUE)

# # Befolkningsförändring uppdelat på komponent (län)
# source(here("Skript","befolkning_utr_inr.R"), encoding="UTF-8")
# hamta_data_bef_utr(spara_data = TRUE,
#                    output_mapp = Output_mapp)

# Pendling
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_pendlare_over_kommungrans.R", encoding="UTF-8")
gg_pendling_kommun <- diag_pendling_over_kommungrans(output_mapp_figur = Output_mapp_figur,
                                                     enbart_in_ut = TRUE,
                                                     diagramfarg_vektor = diagramfarger("rus_sex"),
                                                     diag_absoluta_tal = FALSE,
                                                     returnera_figur = TRUE,
                                                     returnera_data = TRUE)

# Utbildningsnivå (bakgrund och åldersgrupper)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_utb_bakgr_alder_NMS.R", encoding="UTF-8")
gg_utbniva_bakgrund_alder <- diag_utb_niva_bakgr_alder(output_mapp_figur = Output_mapp_figur,
                                                       diag_utb_bakgrund = TRUE,
                                                       diag_utb_alder = TRUE,
                                                       returnera_figur = TRUE,
                                                       returnera_data = TRUE)

# Matchning (län och bakgrund)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_matchning_lan_bakgrund.R", encoding="UTF-8")
gg_matchning <- diag_matchning_lan(output_mapp_figur = Output_mapp_figur,
                                   returnera_figur = TRUE,
                                   returnera_data = TRUE)
# Kompetensbrist
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_kompetensbrist_lan_TVV.R",encoding="UTF-8")
gg_kompetensbrist <- diag_kompetensbrist(output_mapp_figur = Output_mapp_figur,
                                         returnera_figur = TRUE,
                                         returnera_data = TRUE)

# # Befolkningsförändring uppdelat på komponent (län)
# source(here("Skript","pendling_kommun.R"), encoding="UTF-8")
# hamta_data_pendling_kommun(spara_data = TRUE,
#                            output_mapp = Output_mapp)

# # Högskoleexamen - från NMS-databasen/MONA. Uppdateras inte automatisk.
# # Skript för att uppdatera data finns på P1079gem/Jon/kompetensförsörjning/hogskoleexamen_korrekt.R. Välj senast tillgängliga år i MONA.
# source(here("Skript","hogskoleexamen.R"), encoding="UTF-8")
# diag_hogskoleexamen(spara_data = TRUE,
#                     output_mapp = Output_mapp)

# Sysselsättningsgrad, kommun
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_senastear.R", encoding="UTF-8")
gg_arbetsmarknadsstatus_kommun <- diagram_arbetsmarknadsstatus(output_mapp_figur = Output_mapp_figur,
                                                               diag_arbetskraftsdeltagande = FALSE,
                                                               diag_arbetslosthet = FALSE,
                                                               valda_farger = diagramfarger("kon"),
                                                               kon_klartext = c("kvinnor","män"),
                                                               fodelseregion_klartext_vekt =  c("inrikes född", "utrikes född"),
                                                               spara_figur = TRUE,
                                                               returnera_figur = TRUE,
                                                               returnera_data = TRUE)

# Sysselsättningsgrad, län
gg_arbetsmarknadsstatus_lan <- diagram_arbetsmarknadsstatus(region_vekt = hamtaAllaLan(),
                                                            output_mapp_figur = Output_mapp_figur,
                                                            diag_arbetskraftsdeltagande = FALSE,
                                                            diag_arbetslosthet = FALSE,
                                                            valda_farger = diagramfarger("kon"),
                                                            kon_klartext = c("kvinnor","män"),
                                                            fodelseregion_klartext_vekt =  c("inrikes född", "utrikes född"),
                                                            spara_figur = TRUE,
                                                            returnera_figur = TRUE,
                                                            returnera_data = TRUE,
                                                            data_namm = "arbetsmarknadsstatus_lan")



# # Matchning på arbetsmarknaden, län och bakgrund - från projektet "Kvinnor och män i Dalarna"
# source("C:/Users/frkjon/Projekt/kvinnor_man_i_Dalarna/Skript/matchning.R", encoding="UTF-8")
# diag_matchning(spara_data = TRUE,
#                output_mapp = Output_mapp)

rmarkdown::render(
  input = 'kompetens_markdown.Rmd',
  output_file = 'kompetens_markdown_ny.html',
  envir = parent.frame()
)

