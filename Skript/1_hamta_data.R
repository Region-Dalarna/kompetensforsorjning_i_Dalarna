# Uppdaterar data som används i rapporten "Läget i Dalarna"

system.time({
if (!require("pacman")) install.packages("pacman")
p_load(here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# ska sättas till FALSE när skriptet är i produktion men smidigare att felsöka fel som inte handlar om rcurl om det är TRUE
hoppa_over_forsok_igen <- FALSE

Output_mapp="G:/skript/projekt/data/kompetensforsorjning/"
Output_mapp_figur <- here("Diagram","/")
spara_diagram_som_bildfiler <- FALSE

# Befolknings uppdelat på åldersgrupp
# source(here("Skript","befolkning_aldersgrupp_prognos.R"), encoding="UTF-8")
# diag_befolkning(output_mapp = Output_mapp,
#                 spara_data = TRUE)
# Diagram - demografisk försörjningskvot
source("")
gg_demo_forsorjning <- funktion_upprepa_forsok_om_fel( function() {
  diagram_demo_forsorjningkvot_tid_region(region_vekt = c("20","00"),			# Val av region. Finns: "00", "FA00", "FA01", "FA02", "FA03", "FA04", "FA05", "FA06", "FA07", "FA08", "FA09", "FA10", "0114", "0115", "0117", "FA11", "0120", "0123", "0125", "0126", "0127", "0128", "FA12", "0136", "0138", "0139", "FA13", "0140", "FA14", "FA15", "0160", "0162", "0163", "FA16", "FA17", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "FA18", "0191", "0192", "FA19", "FA20", "FA21", "FA22", "FA23", "FA24", "FA25", "FA26", "FA27", "FA28", "FA29", "0305", "FA30", "0319", "FA31", "FA32", "0330", "0331", "FA33", "FA34", "FA35", "0360", "FA36", "FA37", "0380", "0381", "0382", "FA38", "FA39", "FA40", "FA41", "0428", "FA42", "FA43", "FA44", "FA45", "0461", "FA46", "FA47", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "FA48", "FA49", "0509", "FA50", "0512", "0513", "FA51", "FA52", "FA53", "FA54", "FA55", "0560", "0561", "0562", "0563", "FA56", "FA57", "0580", "0581", "0582", "0583", "0584", "0586", "FA58", "FA59", "0604", "FA60", "0617", "01", "03", "0642", "0643", "04", "05", "0662", "0665", "06", "07", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "08", "09", "10", "12", "13", "14", "17", "18", "0760", "0761", "0763", "0764", "0765", "0767", "19", "20", "0780", "0781", "21", "22", "23", "24", "0821", "25", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "0980", "1060", "1080", "1081", "1082", "1083", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "1315", "1380", "1381", "1382", "1383", "1384", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584" 
                                                              output_mapp_figur = Output_mapp_figur,
                                                              tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
                                                              spara_figur = spara_diagram_som_bildfiler, # Skall diagrammet sparas
                                                              returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

# Antal utrikes/inrikes födda i arbetsför ålder (20-64 år)
source("https://raw.githubusercontent.com/Region-Dalarna/integrationen_i_dalarna/refs/heads/master/skript/andel_utrikes_inrikes_tidsserie.R")
gg_antal_utrikes_inrikes <- funktion_upprepa_forsok_om_fel( function() {
  diag_bef_inr_utr_tid(output_mapp = "Output_mapp_figur",
                                                 diag_andel = FALSE, # Andel inrikes/utrikes födda i arbetsför ålder
                                                 diag_antal = TRUE, # Antal "-"
                                                 skriv_diagrambildfil = spara_diagram_som_bildfiler,
                                                 returnera_data_rmarkdown= TRUE)
}, hoppa_over = hoppa_over_forsok_igen)

antal_utrikes_inrikes_min_ar <- min(antal_utrikes_inrikes_bakgr_df$år)
antal_utrikes_inrikes_max_ar <- max(antal_utrikes_inrikes_bakgr_df$år)

inrikes_forandring_antal <- format(abs(antal_utrikes_inrikes_bakgr_df %>% filter(år==max(år),födelseregion == "Inrikes född") %>% .$Antal - antal_utrikes_inrikes_bakgr_df %>% filter(år==min(år),födelseregion == "Inrikes född") %>% .$Antal),big.mark = " ")
utrikes_forandring_antal <- format(abs(antal_utrikes_inrikes_bakgr_df %>% filter(år==max(år),födelseregion == "Utrikes född") %>% .$Antal - antal_utrikes_inrikes_bakgr_df %>% filter(år==min(år),födelseregion == "Utrikes född") %>% .$Antal),big.mark = " ")

# # Diagram  befolkningsförändring
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_befolkningsforandring_region_kon_ar_SCB.R", encoding="UTF-8")
gg_befolkning <- funktion_upprepa_forsok_om_fel( function() {
  diagram_befolkningsforandring_ar(region_vekt = "20",
                                                 spara_figur=spara_diagram_som_bildfiler,
                                                 diag_folkmangd = FALSE,
                                                 returnera_data = TRUE,
                                                 avrunda_fem = FALSE,
                                                 output_mapp_figur = Output_mapp_figur)
  }, hoppa_over = hoppa_over_forsok_igen)

bef_forandring_max_ar <- max(befolkning_df$år)

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
antal_lediga_jobb <- sum(lediga_jobb_E1_df %>% filter(ar==max(ar)) %>% .$`Lediga jobb`) 
andel_privat_lediga_jobb <- round((lediga_jobb_E1_df %>% filter(ar==max(ar),sektor == "privat sektor") %>% .$`Lediga jobb`/antal_lediga_jobb)*100,0)

# Jobbinflöde - NY 7/10
source(here("Skript","jobbinflode_procent_region_ny.R"), encoding="UTF-8")
gg_jobbinflode <- funktion_upprepa_forsok_om_fel( function() {
  diagram_jobbinflode_tid_region(region_vekt = "20",
                                                 spara_figur=spara_diagram_som_bildfiler,
                                                 returnera_data = TRUE,
                                                 tid_koder = "*",
                                                 output_mapp_figur = Output_mapp_figur)
  }, hoppa_over = hoppa_over_forsok_igen)

jobbinflode_senaste_ar <- max(jobbinflode_df$årsintervall)
jobbinflode_senaste_ar_varde <- sum(jobbinflode_df %>% filter(årsintervall == jobbinflode_senaste_ar) %>% .$varde)

# Kompetensnivå för län och bransch
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_kvalifikationskrav_SCB.R", encoding="UTF-8")
gg_kvalifikation <- funktion_upprepa_forsok_om_fel( function() {
  diagram_kvalifikationskrav(output_mapp_figur = Output_mapp_figur,
                             spara_figur = spara_diagram_som_bildfiler,
                             returnera_figur = TRUE,
                             returnera_data = TRUE)
}, hoppa_over = hoppa_over_forsok_igen)

enkla_yrken_lan_hogst <- skapa_kortnamn_lan(kvalifikationskrav_jmf %>% filter(kompetensniva=="Enklare yrken") %>% filter(Andel == max(.$Andel)) %>% .$region)
enkla_yrken_lan_hogst_andel <- round(kvalifikationskrav_jmf %>% filter(kompetensniva=="Enklare yrken") %>% filter(Andel == max(.$Andel)) %>% .$Andel,0)

gymnasie_yrken_lan_hogst <- skapa_kortnamn_lan(kvalifikationskrav_jmf %>% filter(kompetensniva=="Motsvarande gymnasial kompetens") %>% filter(Andel == max(.$Andel)) %>% .$region)
gymnasie_yrken_lan_hogst_varde <- round(kvalifikationskrav_jmf %>% filter(kompetensniva=="Motsvarande gymnasial kompetens") %>% filter(Andel == max(.$Andel)) %>% .$Andel,0)

gymnasie_yrken_lan_lagst <- skapa_kortnamn_lan(kvalifikationskrav_jmf %>% filter(kompetensniva=="Motsvarande gymnasial kompetens") %>% filter(Andel == min(.$Andel)) %>% .$region)
gymnasie_yrken_lan_lagst_varde <- round(kvalifikationskrav_jmf %>% filter(kompetensniva=="Motsvarande gymnasial kompetens") %>% filter(Andel == min(.$Andel)) %>% .$Andel,0)

hogskola_yrken_lan_hogst <- skapa_kortnamn_lan(kvalifikationskrav_jmf %>% filter(kompetensniva=="Motsvarande fördjupad högskolekompetens") %>% filter(Andel == max(.$Andel)) %>% .$region)
hogskola_yrken_lan_hogst_varde <- round(kvalifikationskrav_jmf %>% filter(kompetensniva=="Motsvarande fördjupad högskolekompetens") %>% filter(Andel == max(.$Andel)) %>% .$Andel,0)

hogskola_yrken_lan_lagst <- skapa_kortnamn_lan(kvalifikationskrav_jmf %>% filter(kompetensniva=="Motsvarande fördjupad högskolekompetens") %>% filter(Andel == min(.$Andel)) %>% .$region)
hogskola_yrken_lan_lagst_varde <- round(kvalifikationskrav_jmf %>% filter(kompetensniva=="Motsvarande fördjupad högskolekompetens") %>% filter(Andel == min(.$Andel)) %>% .$Andel,0)

# Utbildningsnivå och ålder för län och bransch. Andel och antal - NMS: UPPDATATERAS FÖR HAND. EJ GJORT 2025-09-23
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_bransch_utb_alder_NMS.R", encoding="UTF-8")
gg_bransch_utb_alder <- funktion_upprepa_forsok_om_fel( function() {
  diag_bransch_utb_alder(output_mapp_figur = Output_mapp_figur,
                         spara_figur = spara_diagram_som_bildfiler,
                         returnera_figur = TRUE,
                         returnera_data = TRUE,
                         andel = TRUE)
}, hoppa_over = hoppa_over_forsok_igen)

gg_bransch_utb_alder_antal <- funktion_upprepa_forsok_om_fel( function() {
  diag_bransch_utb_alder(output_mapp_figur = Output_mapp_figur,
                         spara_figur = spara_diagram_som_bildfiler,
                         returnera_figur = TRUE,
                         returnera_data = TRUE,
                         andel = FALSE)
}, hoppa_over = hoppa_over_forsok_igen)

unga_andel_bransch_hogst <- bransch_alder %>% filter(alder == "16-19 år",bransch != "Okänt") %>%filter(andel == max(.$andel)) %>% .$bransch
unga_andel_bransch_hogst_varde <-round(bransch_alder %>% filter(alder == "16-19 år") %>%filter(andel == max(.$andel)) %>% .$andel,0)
  
# skapa df med bransch som har högst andel personer i åldersgruppen 60-74 år
bransch_aldst_andel <- bransch_alder %>%
  filter(alder %in% c("60-64 år","65-69 år","70-74 år")) %>%
  summarise(andel = sum(andel, na.rm = TRUE), .by = c(ar, lan, bransch)) %>%
  slice_max(andel, n = 1, by = c(ar, lan), with_ties = TRUE)

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

# Utbildningsnivå från 85 och framåt uppdelat på kön. Data hämtas i detta fall från GGplot-objektet (när data används i markdown) FEL
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


# Utbildningsnivå senaste år FEL
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")
gg_utbniva_senastear <- funktion_upprepa_forsok_om_fel( function() {
  diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("20"),
                                                           output_mapp = Output_mapp_figur,
                                                           diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                                           skapa_fil = spara_diagram_som_bildfiler,
                                                           diag_hogutb_over_tid = FALSE,
                                                           diag_lagutb_over_tid = FALSE,
                                                           diag_andel_alla_utbnivaer = FALSE,
                                                           diag_andel_utbniva_jmfr_lan = TRUE,
                                                           vald_utb_niva = "hogutb")
  }, hoppa_over = hoppa_over_forsok_igen)

# Gymnasieantagning, senaste och flera år
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_gymnasiantagning_antal_kon.R")
# Inte könsuppdelat
gg_gymnasiet <- funktion_upprepa_forsok_om_fel( function() {
  diag_gymnasieantagna_antal(output_mapp_figur = Output_mapp_figur,
                                           spara_figur = spara_diagram_som_bildfiler,
                                          returnera_figur = TRUE,
                                          returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

# Könsuppdelat
gg_gymnasiet_kon <- funktion_upprepa_forsok_om_fel( function() {
  diag_gymnasieantagna_antal(output_mapp_figur = Output_mapp_figur,
                                              spara_figur = spara_diagram_som_bildfiler,
                                              returnera_figur = TRUE,
                                              returnera_data = FALSE,
                                              konsuppdelat = TRUE,
                                              diag_antal_fleraar = FALSE)
  }, hoppa_over = hoppa_over_forsok_igen)

# Högskoleexamen
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_examen_hogskolan_NMS.R")
gg_hogskoleexamen <- funktion_upprepa_forsok_om_fel( function() {
  diagram_examen_hogskolan_NMS (output_mapp_figur = Output_mapp_figur,
                                                   returnera_figur = TRUE,
                                                   returnera_data = TRUE,
                                                   spara_figur = spara_diagram_som_bildfiler)
  }, hoppa_over = hoppa_over_forsok_igen)

# YH-utbildning
source(here("Skript","diagram_examen_yh_NMS.R"), encoding="UTF-8")
#source("C:/Users/frkjon/Projekt/kompetensforsorjning_i_Dalarna/Skript/diagram_examen_yh_NMS.R")
gg_yh <- funktion_upprepa_forsok_om_fel( function() {
  diagram_examen_yh_NMS(output_mapp_figur = Output_mapp_figur,
                               returnera_figur = TRUE,
                               returnera_data = TRUE,
                               spara_figur = spara_diagram_som_bildfiler)
  }, hoppa_over = hoppa_over_forsok_igen)

# Yh - Antagna som påbörjat utbildning
#source(here("Skript","antagna_yh_tid.R"), encoding="UTF-8")
source(here("Skript","antagna_yh_tid.R"), encoding="UTF-8")
gg_antagna_yh <- funktion_upprepa_forsok_om_fel( function() {
  diagram_antagna_yh_tid_region(region_vekt = "20",
                                              spara_figur=spara_diagram_som_bildfiler,
                                              returnera_data = TRUE,
                                              tid_koder = "*",
                                              output_mapp_figur = Output_mapp_figur)
  }, hoppa_over = hoppa_over_forsok_igen)



# Arbetslöshet 08-senaste år. Excel, Arbetsförmedlingen - KVAR
# source(here("Skript","arbetsloshet_08_senastear.R"), encoding="UTF-8")
# hamta_data_arbetsloshet(output_mapp = Output_mapp,
#                         spara_data = TRUE)

# Förvärvsarbetande senaste observation (uppdelat på kön) 
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



# Utbildningsnivå och ålder för län och bransch. Andel och antal
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_bransch_utb_alder_NMS.R", encoding="UTF-8")
gg_bransch_utb_alder <- funktion_upprepa_forsok_om_fel( function() {
  diag_bransch_utb_alder(output_mapp_figur = Output_mapp_figur,
                                               spara_figur = spara_diagram_som_bildfiler,
                                               returnera_figur = TRUE,
                                               returnera_data = TRUE,
                                               andel = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

gg_bransch_utb_alder_antal <- funktion_upprepa_forsok_om_fel( function() {
  diag_bransch_utb_alder(output_mapp_figur = Output_mapp_figur,
                                                    spara_figur = spara_diagram_som_bildfiler,
                                                    returnera_figur = TRUE,
                                                    returnera_data = TRUE,
                                                    andel = FALSE)
  }, hoppa_over = hoppa_over_forsok_igen)

# skapa df med bransch som har högst andel personer i åldersgruppen 60-74 år
bransch_aldst_andel <- bransch_alder %>% 
  filter(alder %in% c("60-64 år","65-69 år","70-74 år")) %>% 
  group_by(ar, lan, bransch) %>% 
  summarise(andel = sum(andel, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(andel)) %>% 
  filter(andel == max(andel)) 

# skapa df med bransch som har högst antal personer i åldersgruppen 60-74 år
bransch_aldst_antal <- bransch_alder %>% 
  filter(alder %in% c("60-64 år","65-69 år","70-74 år")) %>% 
  group_by(ar, lan, bransch) %>% 
  summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(antal)) %>% 
  filter(antal == max(antal)) 


# Befolkningsförändring uppdelat på komponent (län)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_befolkningsforandring.R", encoding="UTF-8")
gg_bef_for <- funktion_upprepa_forsok_om_fel( function() {
  diagram_befolkningsforandring(output_mapp_figur = Output_mapp_figur,
                                            spara_figur = spara_diagram_som_bildfiler,
                                            returnera_figur = TRUE,
                                            returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

# # Befolkningsförändring uppdelat på komponent (län)
# source(here("Skript","befolkning_utr_inr.R"), encoding="UTF-8")
# hamta_data_bef_utr(spara_data = TRUE,
#                    output_mapp = Output_mapp)

# Pendling
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_pendling_over_kommungrans.R", encoding="UTF-8")
gg_pendling_kommun <- funktion_upprepa_forsok_om_fel( function() {
  diag_pendling_over_kommungrans(output_mapp_figur = Output_mapp_figur,
                                                     enbart_in_ut = TRUE,
                                                     diagramfarg_vektor = diagramfarger("rus_sex"),
                                                     diag_absoluta_tal = FALSE,
                                                     skapa_fil = spara_diagram_som_bildfiler,
                                                     returnera_figur = TRUE,
                                                     returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

utpendling_storst_kommun <- andel_pendlare_kommun_df %>% 
  filter(variabel == "Andel utpendling") %>% 
  arrange(desc(andel)) %>%
  slice(1)

inpendling_storst_kommun <- andel_pendlare_kommun_df %>% 
  filter(variabel == "Andel inpendling") %>% 
  arrange(desc(andel)) %>%
  slice(1)


source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_pendling_over_lans_fa_grans.R")
gg_pendling_lan <- funktion_upprepa_forsok_om_fel( function() {
  diag_pendling_over_lans_fa_grans(output_mapp_figur = Output_mapp_figur,
                                                  enbart_in_ut = TRUE,
                                                  diag_absoluta_tal = FALSE,
                                                  diagramfarg_vektor = diagramfarger("rus_sex"),
                                                  skapa_fil = spara_diagram_som_bildfiler,
                                                  returnera_figur = TRUE,
                                                  returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

utpendling_uppsala <- andel_pendlare_lan_df %>% 
  filter(regionkod == "03",
         variabel == "Andel utpendling")

utpendling_halland <- andel_pendlare_lan_df %>% 
  filter(regionkod == "13",
         variabel == "Andel utpendling")

inpendling_storst_lan <- andel_pendlare_lan_df %>% 
  filter(variabel == "Andel inpendling") %>% 
  arrange(desc(andel)) %>%
  slice(1)
  

# Utbildningsnivå (bakgrund och åldersgrupper)
#source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_utb_bakgr_alder_NMS.R", encoding="UTF-8")
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_inr_utr_fodda_lan_scb.R")
gg_utbniva_bakgrund <- funktion_upprepa_forsok_om_fel( function() {
  diag_utbniva_inr_utr_fodda_kon_lan(skriv_diagramfil = spara_diagram_som_bildfiler,
                                                                output_mapp = Output_mapp_figur,
                                                                returnera_df_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_antal_utbniva_alder_kon_scb.R")
gg_lagutbildade_alder <- funktion_upprepa_forsok_om_fel( function() {
  diag_antal_utbniva_alder_kon(skriv_diagramfil = spara_diagram_som_bildfiler,
                                                      output_mapp = Output_mapp_figur,
                                                      returnera_df_rmarkdown = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

# ta ut alla åldrar som numeriska värden
alder_num <- str_extract_all(unique(utbniva_kon_alder_df$ålder), "\\d+") %>% 
  unlist() %>% 
  as.numeric() 

# skapa en textsträng med minsta och största ålder i datasetet som vi lägger 
alder_txt <- paste0(min(alder_num), "-", max(alder_num), " år")

# skapa en dataframe med andelar för utbildningsnivåer för åldersgruppen 20-64 år
utb_niva_20_64_ar <- utbniva_kon_alder_df %>% 
  mutate(ålder = alder_txt) %>% 
  group_by(år, regionkod, region, ålder, kön, utbildningsnivå) %>% 
  summarise(varde = sum(varde, na.rm = TRUE)) %>% 
  mutate(andel = varde / sum(varde, na.rm = TRUE) * 100) %>% 
  ungroup()

# skapa en variabel för andelen kvinnor som är eftergymnasialt utbildade
kv_eftergymn_andel <- utb_niva_20_64_ar %>% 
  filter(utbildningsnivå == "eftergymnasial utbildning",
         kön == "kvinnor") %>% 
  dplyr::pull(andel) %>% round()

# skapa en variabel för andelen män som är eftergymnasialt utbildade
man_eftergymn_andel <- utb_niva_20_64_ar %>% 
  filter(utbildningsnivå == "eftergymnasial utbildning",
         kön == "män") %>% 
  dplyr::pull(andel) %>% round()

man_inr_forgymn_andel <- utbniva_bakgr_kon_df %>% 
  filter(utbildning == "förgymnasial utbildning",
         bakgrund == "Födda i Sverige",
         kön == "män") %>% 
  dplyr::pull(varde) %>% round()

kv_inr_forgymn_andel <- utbniva_bakgr_kon_df %>% 
  filter(utbildning == "förgymnasial utbildning",
         bakgrund == "Födda i Sverige",
         kön == "kvinnor") %>% 
  dplyr::pull(varde) %>% round()

man_utr_forgymn_andel <- utbniva_bakgr_kon_df %>% 
  filter(utbildning == "förgymnasial utbildning",
         bakgrund == "Utrikes födda",
         kön == "män") %>% 
  dplyr::pull(varde) %>% round()

kv_utr_forgymn_andel <- utbniva_bakgr_kon_df %>% 
  filter(utbildning == "förgymnasial utbildning",
         bakgrund == "Utrikes födda",
         kön == "kvinnor") %>% 
  dplyr::pull(varde) %>% round()

# Matchning (län och bakgrund)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_matchning_lan_bakgrund.R", encoding="UTF-8")
gg_matchning <- funktion_upprepa_forsok_om_fel( function() {
  diag_matchning_lan(output_mapp_figur = Output_mapp_figur,
                                   spara_figur = spara_diagram_som_bildfiler,
                                   returnera_figur = TRUE,
                                   returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

# Lediga jobb M1 - NY 7/10
source(here("Skript","diagram_lediga_jobb_arbetslosa_M1.R"), encoding="UTF-8")
gg_lediga_jobb_M1 <- funktion_upprepa_forsok_om_fel( function() {
  diagram_lediga_jobb_arbetslosa_M1(region_vekt = "20",
                                                   spara_figur = spara_diagram_som_bildfiler,
                                                   returnera_data = TRUE,
                                                   tid_koder = "*",
                                                   output_mapp_figur = Output_mapp_figur)
  }, hoppa_over = hoppa_over_forsok_igen)

# Kompetensbrist
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_kompetensbrist_lan_TVV.R",encoding="UTF-8")
gg_kompetensbrist <- funktion_upprepa_forsok_om_fel( function() {
  diag_kompetensbrist(output_mapp_figur = Output_mapp_figur,
                                         skapa_fil = spara_diagram_som_bildfiler,
                                         returnera_figur = TRUE,
                                         returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

# # Befolkningsförändring uppdelat på komponent (län)
# source(here("Skript","pendling_kommun.R"), encoding="UTF-8")
# hamta_data_pendling_kommun(spara_data = TRUE,
#                            output_mapp = Output_mapp)

# # Högskoleexamen - från NMS-databasen/MONA. Uppdateras inte automatisk.
# # Skript för att uppdatera data finns på P1079gem/Jon/kompetensförsörjning/hogskoleexamen_korrekt.R. Välj senast tillgängliga år i MONA.
# source(here("Skript","hogskoleexamen.R"), encoding="UTF-8")
# diag_hogskoleexamen(spara_data = TRUE,
#                     output_mapp = Output_mapp)

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

# Sysselsättningsgrad, kommun
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_senastear.R", encoding="UTF-8")
gg_arbetsmarknadsstatus_kommun <- funktion_upprepa_forsok_om_fel( function() {
  diagram_arbetsmarknadsstatus(output_mapp_figur = Output_mapp_figur,
                                                               diag_arbetskraftsdeltagande = FALSE,
                                                               diag_arbetslosthet = FALSE,
                                                               valda_farger = diagramfarger("kon"),
                                                               kon_klartext = c("kvinnor","män"),
                                                               fodelseregion_klartext_vekt =  c("inrikes född", "utrikes född"),
                                                               spara_figur = spara_diagram_som_bildfiler,
                                                               returnera_figur = TRUE,
                                                               returnera_data = TRUE)
  }, hoppa_over = hoppa_over_forsok_igen)

# Sysselsättningsgrad, län
gg_arbetsmarknadsstatus_lan <- funktion_upprepa_forsok_om_fel( function() {
  diagram_arbetsmarknadsstatus(region_vekt = hamtaAllaLan(),
                                                            output_mapp_figur = Output_mapp_figur,
                                                            diag_arbetskraftsdeltagande = FALSE,
                                                            diag_arbetslosthet = FALSE,
                                                            valda_farger = diagramfarger("kon"),
                                                            kon_klartext = c("kvinnor","män"),
                                                            fodelseregion_klartext_vekt =  c("inrikes född", "utrikes född"),
                                                            spara_figur = spara_diagram_som_bildfiler,
                                                            returnera_figur = TRUE,
                                                            returnera_data = TRUE,
                                                            data_namm = "arbetsmarknadsstatus_lan")
  }, hoppa_over = hoppa_over_forsok_igen) 


source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_region_alder_kon_fodelseregion_tid_InrUtrFoddaRegAlKon_scb.R")
befolkning_utr_inr_df <- hamta_bef_region_alder_kon_fodelseregion_tid_scb(region_vekt = "20",
                                                                          alder_koder = c(20:64) %>% as.character()) %>% 
  group_by(år, regionkod, region, bakgrund = födelseregion) %>% 
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") %>% 
  mutate(aldersgrupp = "20-64 år",
         bakgrund = ifelse(bakgrund == "Utrikes född", "Utrikes födda", "Inrikes födda"),
         region = region %>% skapa_kortnamn_lan())




}) # slut system.time för att ladda data

# 2. om man vill knitta rapporten
#source(paste0(here("Skript","/"), "2_knitta_rapport.R"))

# 3. om man vill kopiera den till docs, för publicering på webben med Github Pages
#source(paste0(here("Skript","/"), "3_kopiera_till_docs_for_publicera_pa_webben.R"))

# 4. skjut upp hela repositoryt till Github
#source(paste0(here("Skript","/"), "4_push_av_hela_repo_till_github.R"))


