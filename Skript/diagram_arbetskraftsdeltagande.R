diagram_arbetskraftsdeltagande_tid_region <- function(region_vekt = c("20"),			# Val av region. Finns: "00", "FA00"-"FA60", samtliga län (denna tabell saknar kommuner)
                                                      output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                                      tid_koder = "*",			 # "*" = alla år, "9999" = senaste, finns 2019-2024 (se kommentar i hamta_data-funktionen - äldre år är inte längre tillgängliga hos SCB)
                                                      kon_klartext = "totalt", # Finns: "män", "kvinnor", "totalt"
                                                      spara_figur = TRUE, # Skall diagrammet sparas
                                                      returnera_data = FALSE, # Skall data returneras
                                                      returnera_figur = TRUE){

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse). Anropas med fullt
  # namespace (dplyr::filter() osv.) i stället för library(). hamta_data-funktionen sourcas
  # fortfarande direkt (samma mönster som övriga migrerade skript) - den är själv omskriven till
  # pxweb2r (v1-tabellen AM9906O/RegionInd19U1b som tidigare kombinerades med denna är helt borttagen
  # ur SCB:s API, se kommentar i den filen).
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_arbetskraftsdeltagande_region_utbildngrupp_kon_tid_RegionInd19U1b_19U1bN1_scb.R")

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel av befolkningen 20-64 år som antingen är förvärvsarbetande eller inskrivna på arbetsförmedlingen "
  gg_list <- list()

  # Bugfix (confirmed genom kodgranskning): region_vekt hämtades tidigare aldrig från funktionens egen
  # parameter (som dokumentationen och anropssignaturen lovar) - "20" stod hårdkodat direkt i anropet
  # nedan, så funktionens region_vekt-argument gjorde ingenting alls oavsett vad man skickade med.
  #
  # Bugfix-konsekvens av att hamta_data-funktionen migrerats till pxweb2r (se den filens egen
  # kommentar): cont_klartext = "*" ger numera korrekt long-format (en rad per innehållsvariabel) i
  # stället för att - som i den gamla v1-baserade versionen - råka bli wide-format på grund av en
  # egenhet i hur SCB:s v1-API hanterade den bokstavliga strängen "*". Ber därför uttryckligen om
  # long_format = FALSE för att få den wide-form (en kolumn per innehållsvariabel: "I arbetskraften"/
  # "Inte i arbetskraften"/"Totalt antal personer") som beräkningen några rader ned förutsätter.
  arbetskraftsdeltagande_df <- hamta_arbetskraftsdeltagande_region_utbildngrupp_kon_tid_scb(
    region_vekt = region_vekt,
    utbildngrupp_klartext = "samtliga utbildningsnivåer",
    kon_klartext = kon_klartext,
    cont_klartext = "*",
    tid_koder = tid_koder,
    long_format = FALSE,
    output_mapp = NA,
    returnera_df = TRUE
  )
  # Beräkna andelen av befolkningen som är i arbetskraften
  arbetskraftsdeltagande_df$arbetskraftsdeltagande <- arbetskraftsdeltagande_df$`I arbetskraften` / arbetskraftsdeltagande_df$`Totalt antal personer` * 100

  if (returnera_data == TRUE) {
    assign("arbetskraftsdeltagande_df", arbetskraftsdeltagande_df, envir = .GlobalEnv)
  }

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(arbetskraftsdeltagande_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(arbetskraftsdeltagande_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(arbetskraftsdeltagande_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) paste0(unique(arbetskraftsdeltagande_df$regionkod), collapse = "_") else region_txt

  diagramtitel <- glue::glue("Arbetskraftsdeltagande hos befolkningen 20-64 år{region_txt}")
  diagramfil <- stringr::str_replace_all(glue::glue("arbetskraftsdeltagande_{regionfil_txt}.png"), "__", "_")

  har_konsuppdelning <- "kön" %in% names(arbetskraftsdeltagande_df) && length(unique(arbetskraftsdeltagande_df$kön)) > 1

  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = arbetskraftsdeltagande_df,
                               skickad_x_var = "år",
                               skickad_y_var = "arbetskraftsdeltagande",
                               skickad_x_grupp = if (har_konsuppdelning) "kön" else NA,
                               x_axis_sort_value = FALSE,
                               diagram_titel = diagramtitel,
                               skriv_till_diagramfil = spara_figur,
                               diagram_capt = diagram_capt,
                               procent_0_100_10intervaller = TRUE,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               manual_y_axis_title = "procent",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = if (har_konsuppdelning) rddiagram::diagramfarger("kon") else rddiagram::diagramfarger("rus_sex")[1],
                               output_mapp = output_mapp_figur,
                               facet_grp = NULL,
                               facet_scale = "free"
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")
  return(gg_list)
}
