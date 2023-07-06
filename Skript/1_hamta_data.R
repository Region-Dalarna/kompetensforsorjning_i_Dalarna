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
