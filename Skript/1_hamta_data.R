# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

Output_mapp="G:/skript/projekt/data/kompetensforsorjning/"
Output_mapp_figur <- here("Diagram","/")

# Befolknings uppdelat på åldersgrupp
source(here("Skript","befolkning_aldersgrupp_prognos.R"), encoding="UTF-8")
diag_befolkning(output_mapp = Output_mapp,
                spara_data = TRUE)

# Förvärvsarbetande från 1990 till senaste år
source(here("Skript","forvarvsarbetande_90_senastear.R"), encoding="UTF-8")
hamta_data_sysselsatta_1990(output_mapp = Output_mapp,
                            spara_data = TRUE)

# Utbildningsnivå från 85 och framåt uppdelat på kön
source("G:/skript/hamta_data/utbildningsniva_85.R")
data_utbniva_85(spara_data = TRUE,
                output_mapp = Output_mapp)

# Utbildningsnivå senaste år
source(here("Skript","utbildningsniva_senastear.R"), encoding="UTF-8")
hamta_data_utbniva(output_mapp = Output_mapp,
                   spara_data = TRUE)

# Arbetslöshet 08-senaste år. Excel, Arbetsförmedlingen
source(here("Skript","arbetsloshet_08_senastear.R"), encoding="UTF-8")
hamta_data_arbetsloshet(output_mapp = Output_mapp,
                        spara_data = TRUE)

# Förvärvsarbetande, uppdelat på kön - Från projektet kvinnor och män
source("C:/Users/frkjon/Projekt/kvinnor_man_i_Dalarna/Skript/forvarvsarbetande_bransch.R", encoding="UTF-8")
data_forvarvsarbetande_bransch(spara_data = TRUE,
                               output_mapp = Output_mapp)

# Förvärvsarbetande prognos - Excel - Trender och prognoser
source(here("Skript","forvarvsarbetande_prognos.R"), encoding="UTF-8")
TP_Prognos_bransch(spara_data = TRUE,
                   output_mapp = Output_mapp)

# Kompetensnivå för län och bransch
source(here("Skript","kompetensnivå_bransch.R"), encoding="UTF-8")
yrken_kompetens(spara_data = TRUE,
                   output_mapp = Output_mapp)
