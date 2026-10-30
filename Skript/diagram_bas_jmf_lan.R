diag_bas_arblosa_jmf_lan_manad <- function(
    region_vekt = c("00","20"),     # Går att jämföra flera regioner
    alder_vekt = "20–64",   # finns : 15-19 år, 16-19 år, 20-24 år, 25-29 år, 30-34 år, 35-39 år, 40-44 år, 45-49 år, 50-54 år, 55-59 år, 60-64 år, 65-69 år, 70-74 år, 15-74 år, 16-64 år, 16-65 år, 20-64 år, 20-65 år
    cont_klartext = "arbetslöshet",  # finns: arbetslöshet, antal sysselsatta, antal arbetslösa, antal sysselsatta och arbetslösa (arbetskraften), antal studerande,
    skriv_diagram = FALSE,
    x_axis_visa_var_xe_etikett = NA,   # möjlighet att bara visa var x:e etikett. X bestäms av det värde man skickar med (kan vara ex. 3 för att visa var tredje etikett på x-axeln)
    inkludera_sista_vardet_var_xe_etikett = TRUE,              # om man vill ha med sista värdet när man kör x_axis_visa_var_xe_etikett
    returnera_dataframe_global_environment = FALSE, # om TRUE returneras dataframen som skapats i global environment
    output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna"
) {
  # ===========================================================================================================  #
  # Skript där man kan jämföra arbetslöshet i valda kommuner/län.
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse)
  
  source("G:/skript/hamta_data/hamta_bas_arbmarknstatus_manad.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()

  if (length(cont_klartext) > 1) {
    cont_klartext <- cont_klartext[1]
    print("Endast en innehållsvariabel kan skickas med i funktionen, därför andvänds bara den första innehållsvariabeln som skickats med.")
  }
  
  # hämta klartext för den region vi valt, lägg i en lista om vi har fler kommuner/län valda
  vald_region_txt <- hamtaregion_kod_namn(region_vekt)$region %>%
    skapa_kortnamn_lan() %>%
    list_komma_och()

  # =============================================== API-uttag ===============================================
  arblosa_bakgr <- hamta_bas_arbmarknstatus(region_vekt = region_vekt,
                                            alder_vekt = alder_vekt,
                                            kon_klartext_vekt ="totalt",
                                            fodelseregion_klartext_vekt = "totalt",
                                            cont_klartext_vekt = cont_klartext) %>%
    mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE))
  

  # Returnerar data
  if(returnera_dataframe_global_environment == TRUE){
    assign("arbetsmarknadsstatus_tidsserie_jmf_lan", arblosa_bakgr, envir = .GlobalEnv)
  }
  
  # skapa strängar till diagramtitel och filnamn från innehållsvariabeln
  cont_txt <- if (cont_klartext == "arbetslöshet") "Andel arbetslösa" else str_to_sentence(cont_klartext)
  cont_filnamn <- cont_txt %>% tolower() %>% str_replace(" ", "_") %>% str_replace("ö", "o") %>% str_replace("å", "a") %>% str_replace("ä", "a")
  
  # är variablen en antalsvariabel
  
  antal_var <- if (str_detect(cont_klartext, "antal")) TRUE else FALSE
  

  alder_txt <- arblosa_bakgr$ålder %>% unique() %>% list_komma_och()
  alder_filnamn <- arblosa_bakgr$ålder %>% unique() %>% str_remove(" år") %>% paste0(., "ar") %>% paste0(collapse = "_")
  

    # ==================================== tidsserie för länet ==================================================================
    
    diagram_titel <- paste0(cont_txt, " ", alder_txt )
    diagramfil <- paste0(cont_filnamn, "_tid_", alder_filnamn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = arblosa_bakgr %>%
                                   filter(regionkod %in% region_vekt),
                                 skickad_x_var = "månad_år",
                                 skickad_y_var = cont_klartext,
                                 skickad_x_grupp = "region",
                                 x_axis_storlek = 8,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_y_axis_title = if (antal_var) "antal" else "procent",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagramfarger("rd_gron")[c(1,4)] ,
                                 x_axis_visa_var_xe_etikett = x_axis_visa_var_xe_etikett,
                                 inkludera_sista_vardet_var_xe_etikett = inkludera_sista_vardet_var_xe_etikett,
                                 skriv_till_diagramfil = skriv_diagram,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- diagramfil %>% str_remove(".png")
  
    return(gg_list)
} # slut funktion
