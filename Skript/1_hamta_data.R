# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

Output_mapp="G:/skript/projekt/data/kompetensforsorjning/"
Output_mapp_figur <- here("Diagram","/")

# Befolknings uppdelat på åldersgrupp
source(here("Skript","befolkning_aldersgrupp_prognos.R"), encoding="UTF-8")
diag_befolkning(output_mapp = Output_mapp,
                spara_data = TRUE)
