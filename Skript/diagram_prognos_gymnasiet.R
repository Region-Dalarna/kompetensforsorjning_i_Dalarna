#test <- diagram_demo_forsorjningkvot_tid_region(region_vekt = c("20"), spara_figur = FALSE)
diagram_gymnasiet_prognos <- function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                      spara_figur = TRUE, # Skall diagrammet sparas
                                      returnera_data = FALSE, # Skall data returneras
                                      returnera_figur = TRUE){
  
  # ====================================================================================================
  #
  # Prognos för gymnasieskolans olika program. Trender och prognoser
  # För tillfället från 2019
  # ====================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue,
         openxlsx,
         here)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  # Hämtar data för prognos från trender och prognoser
  gymnasium_prognos_df <- read.csv2("G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/02_Tabeller/CSV_filer/Tab1b_20.csv",sep='\t',encoding="latin1") %>%
    group_by(pgm_ny,ar) %>% 
    summarize(antal=sum(ak1)) 
  
  # Hämtar data för gymnsasieantagningen
  source("G:/skript/hamta_data/func_gymnasieantagningen.R", encoding = "utf-8", echo = FALSE)
  df <- las_in_data_gymnasieantagningen()%>%
    group_by(ar,program) %>% 
    summarize(Män=sum(Ant_Män),
              Kvinnor=sum(Ant_Kv)) %>%
    pivot_longer(.,3:4,names_to="Kon",values_to = "Antagna") 
  
  # Slår ihop flera mindre program till en större kategori
  df[df == "Flygteknikutbildningen, Marinteknikutbildningen, Sjöfartsutbildningen, Tågteknikutbildningen, Utbildningen samiska näringar eller Yrkesdansarutbildningen"] <- "Flygteknikutbildningen mfl."
  
  df_senaste <- df %>% filter(ar == max(df$ar))
  
  # Funktion för att hantera matchning av namn. TP avslutar inte med programmet och kan i vissa fall ha helt olika namn (se custom nedan). Vissa program saknas dessutom
  source(here("Skript","funktion_matcha_gym_prognos.R"), encoding="UTF-8")
  
  # Notera att vissa program saknas och därför explicit måste tas bort. Dessutom har vissa program för olika namn. 
  map_prog <- build_program_map(
    from = unique(gymnasium_prognos_df$pgm_ny), # Namn som skall ändras
    to   = unique(df_senaste$program), # Namn som vi skall ändra till
    max_dist = 30,
    custom = c("Handels- och administration" = "Försäljnings- och serviceprogrammet",
               "Riksrekryterande utbildningar" = NA_character_,   # explicitly drop
               "Hantverk" = NA_character_)                        # explicitly drop
  )
  
  # Use the returned mapping in your pipe (rename + drop in one go)
  df_std <- gymnasium_prognos_df %>%
    filter(ar==max(ar)) %>%
    rename(program = pgm_ny) %>%
    mutate(
      program = as.character(program),
      program = dplyr::coalesce(unname(map_prog[program]), program)
    ) %>%
    dplyr::filter(program %in% df_senaste$program) %>%
    droplevels()
  
  df_totalt <- df %>% 
    ungroup() %>% 
    summarise(Antagna = sum(Antagna, na.rm = TRUE), .by = c(ar, program)) %>% 
    filter(ar == max(ar))
  
  df_plus_prognos <- rbind(df_totalt,df_std %>%  rename(Antagna = antal))
  
  if(returnera_data == TRUE){
    assign("gym_historik_prognos_df", df_plus_prognos, envir = .GlobalEnv)
  }
  
  diagram_capt <- glue("Källa: Regionala utbildnings- och arbetsmarknadsprognoser, SCB\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: {min(df_plus_prognos$ar)} är verklig data, övriga prognoser")
  
  max_ar = max(gymnasium_prognos_df$ar)
  
  diagram_titel <- "Antagna gymnasieelever i Dalarnas län per program"
  diagramfil <- "gymnasieantagna_prognos.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = df_plus_prognos, 
                                        skickad_x_var = "program", 
                                        skickad_y_var = "Antagna",
                                        skickad_x_grupp = "ar",
                                        manual_x_axis_text_vjust=0,
                                        manual_x_axis_text_hjust=0.6,
                                        manual_color = diagramfarger("rus_sex"),
                                        diagram_titel = diagram_titel,
                                        diagram_capt = diagram_capt,
                                        x_axis_lutning = 0,
                                        x_axis_sort_value = TRUE,
                                        x_axis_sort_grp = 1,
                                        vand_sortering = TRUE,
                                        legend_vand_ordning = TRUE,
                                        diagram_liggande = TRUE,
                                        output_mapp = "output_mapp_figur",
                                        filnamn_diagram = "diagramfil",
                                        skriv_till_diagramfil = FALSE)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  return(gg_list)
}


# Mappar program i trender och prognoser mot de som finns 2025 (Chat GPT) 
# map_prog <- c(
#   "Barn- och fritid"            = "Barn- och fritidsprogrammet",
#   "Bygg- och anläggning"        = "Bygg- och anläggningsprogrammet",
#   "Ekonomi"                     = "Ekonomiprogrammet",
#   "El- och energi"              = "El- och energiprogrammet",
#   "Estetiska"                   = "Estetiska programmet",
#   "Fordons- och transport"      = "Fordons- och transportprogrammet",
#   "Handels- och administration" = "Försäljnings- och serviceprogrammet",
#   "Hotell- och turism"          = "Hotell- och turismprogrammet",
#   "Humanistiska"                = "Humanistiska programmet",
#   "Industritekniska"            = "Industritekniska programmet",
#   "Naturbruk"                   = "Naturbruksprogrammet",
#   "Naturvetenskap"              = "Naturvetenskapsprogrammet",
#   "Restaurang- och livsmedel"   = "Restaurang- och livsmedelsprogrammet",
#   "Samhällsvetenskap"           = "Samhällsvetenskapsprogrammet",
#   "Teknik"                      = "Teknikprogrammet",
#   "VVS- och fastighet"          = "VVS- och fastighetsprogrammet",
#   "Vård- och omsorg"            = "Vård- och omsorgsprogrammet"
#   # "International Baccalaurate" and "Introduktionsprogram" already match your data vector
#   # "Riksrekryterande utbildningar" & "Hantverk" are not in your data -> will be dropped
#)

# prognos_std <- tibble(program = gymnasium_prognos_df$pgm_ny) %>%
#   mutate(program = coalesce(unname(map_prog[program]), program)) %>%  # rename where mapped
#   filter(program %in% df_senaste$program) %>%                                        # drop not in first vector
#   distinct(program) %>%                                                # optional: dedupe
#   pull(program)
# 
# df_std <- gymnasium_prognos_df %>%
#   filter(ar==max(ar)) %>% 
#   rename(program = pgm_ny) %>% 
# mutate(
#   program = as.character(program),                           # in case it's a factor
#   program = coalesce(unname(map_prog[program]), program)     # rename via lookup
# ) %>%
#   filter(program %in% prognos_std) %>%                          # drop non-matching
#   droplevels()   

# df_std <- gymnasium_prognos_df %>%
#   filter(ar==max(ar)) %>%
#   rename(program = pgm_ny) %>%
#   mutate(
#     program = as.character(program),
#     # rename short → canonical where a mapping exists; otherwise keep original
#     program = coalesce(unname(map_prog[program]), program)
#   ) %>%
#   # keep only programs that exist in your canonical list
#   filter(program %in% df_senaste$program) %>%
#   droplevels()