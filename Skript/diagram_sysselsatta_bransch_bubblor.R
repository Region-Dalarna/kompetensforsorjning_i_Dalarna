diag_sysselsatta_bransch_bubblor <- function(region_vekt = "20",                # Län vars kommuner ska ingå
                                             output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                             output_mapp_data = NA,            # Här hamnar sparad data
                                             filnamn_data = "sysselsatta_bransch_kommun.xlsx",
                                             kon_klartext = "totalt",          # "totalt", "kvinnor" eller "män". Ange bara ett värde, annars summeras de i bubblorna
                                             spara_figur = TRUE,               # Om TRUE sparas figuren till output_mapp_figur
                                             skal_bubblor = 5,                 # Antal referensbubblor i storleksförklaringen (0 = ingen)
                                             layout_tabell = NULL,             # Kommunernas placering (kolumner: grupp, gx, gy). NULL = paketets egen dalarna_layout
                                             branschtabell = NULL,             # Branschnyckel (BrKod, Br15kod, Bransch). NULL = hämtas från Region Dalarnas depot på GitHub
                                             bransch_nyckel_url = "https://raw.githubusercontent.com/Region-Dalarna/depot/main/Bransch_Gxx_farger.xlsx",
                                             returnera_figur = TRUE,           # Skall figur returneras (i en lista)
                                             returnera_data = FALSE,           # Skall data returneras (till R-studios globala miljö)
                                             ...                               # Övriga argument skickas vidare till rddiagram::skapa_packed_circles(), t.ex. skala_styrka eller skal_varden_manuell
) {
  
  # ========================================== Allmän info ============================================
  # Skapar ett bubbeldiagram över antal sysselsatta per bransch och kommun (en bubbelklunga per kommun,
  # placerad geografiskt). Enbart senaste månad. Sysselsatta efter arbetsställets belägenhet (dagbefolkning).
  #
  # Tabell: TAB3784 (SCB, via pxweb2r)
  # ========================================== Inställningar ============================================
  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("curl", quietly = TRUE)) install.packages("curl")
  
  gg_list <- list()  # skapa en tom lista att lägga ggplot-objekt i
  
  vald_region <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region) |>
    paste(collapse = ", ")
  
  # Branschnyckeln kopplar SNI-koderna (branschkod) till branschnamn. Samma fil som paketet själv använder för färgerna.
  if (is.null(branschtabell)) {
    tmp_fil <- tempfile(fileext = ".xlsx")
    curl::curl_download(bransch_nyckel_url, tmp_fil, quiet = TRUE)
    branschtabell <- readxl::read_excel(tmp_fil) |>
      # I Excel-filen är koden för okänd verksamhet lagrad som talet 0 och läses in som "0", inte "00"
      dplyr::mutate(Br15kod = ifelse(Br15kod == "0", "00", Br15kod))
  }
  
  # # Kommunernas placering på kartan. Om inget skickas in används paketets egen tabell.
  # if (is.null(layout_tabell)) {
  #   layout_tabell <- tryCatch(rddiagram::dalarna_layout, error = function(e) NULL)
  # }
  
  # =============================================== API-uttag ===============================================
  
  # Sysselsatta efter arbetsställets belägenhet, bransch (SNI 2007) och kommun. Senaste månad.
  df <- pxweb2r::pxweb2_get_data(
    table = "TAB3784",
    query = list(
      Region = rdverktyg::hamtakommuner(region_vekt, tamedlan = FALSE, tamedriket = FALSE),
      Kon = kon_klartext,
      SNI2007 = "*",
      Fodelseregion = "totalt",
      ContentsCode = "sysselsatta efter arbetsställets belägenhet",
      Tid = "9999"
    ), quiet = TRUE) |>
    dplyr::mutate(`näringsgren sni 2007_kod` = ifelse(`näringsgren sni 2007_kod` == "US", "00", `näringsgren sni 2007_kod`)) |>
    dplyr::filter(`näringsgren sni 2007_kod` != "A-U+US") |>      # tar bort raden för alla branscher (skulle annars dubbelräknas)
    dplyr::rename(branschkod = `näringsgren sni 2007_kod`,
                  regionkod = region_kod,
                  `sysselsatta efter arbetsställets belägenhet` = value) |>
    dplyr::left_join(dplyr::select(branschtabell, BrKod, Br15kod, bransch = Bransch), by = c("branschkod" = "Br15kod")) |>
    # Om koden "00" (okänd verksamhet) inte finns i branschnyckelns Br15kod-kolumn fylls den i här,
    # annars blir bransch NA och raderna försvinner tyst ur diagrammet. G99 är nyckelns BrKod för okänt.
    dplyr::mutate(bransch = ifelse(branschkod == "00" & is.na(bransch), "Okänt", bransch),
                  BrKod = ifelse(branschkod == "00" & is.na(BrKod), "G99", BrKod)) |>
    dplyr::select(-`näringsgren SNI 2007`, -tabellinnehåll, -födelseregion) |>
    dplyr::relocate(branschkod, .after = region) |>
    dplyr::relocate(bransch, .after = branschkod) |>
    rdverktyg::manader_bearbeta_scbtabeller()
  
  if (returnera_data == TRUE) {
    assign("sysselsatta_bransch_df", df, envir = .GlobalEnv)
  }
  
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)) {
    openxlsx::write.xlsx(df, paste0(output_mapp_data, filnamn_data))
  }
  
  # =============================================== Diagram ===============================================
  
  senaste_manad <- unique(df$månad_år)[1]    # t.ex. "juli 2026"
  
  diagram_titel <- paste0("Sysselsatta per bransch i ", vald_region, ", ", senaste_manad)
  diagram_capt <- paste0("Källa: BAS i SCB:s öppna statistikdatabas.\n",
                         "Bearbetning: Samhällsanalys, Region Dalarna.\n",
                         "Diagramförklaring: Cirklarnas storlek visar antal sysselsatta (efter arbetsställets belägenhet) i respektive bransch.")
  diagramfil <- "sysselsatta_bransch_bubblor.png"
  
  diagram_capt <-rddiagram::lagg_till_ckm_notering(diagram_capt = diagram_capt,
                                                  har_ckm_data = TRUE,
                                                  fran_ar = NULL,
                                                  ckm_text = NULL
                                                )
  
  gg_obj <- rddiagram::skapa_packed_circles(
    data = df,
    grupp_kol = "region",
    layout_tabell = layout_tabell,
    skal_bubblor = skal_bubblor,
    titel = diagram_titel,
    diagram_caption = diagram_capt,
    spara_bildfil = spara_figur,
    filnamn = diagramfil,
    mapp = output_mapp_figur,
    ...
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list) <- "sysselsatta_bransch_bubblor"
  
  if (returnera_figur == TRUE) return(gg_list)
}
