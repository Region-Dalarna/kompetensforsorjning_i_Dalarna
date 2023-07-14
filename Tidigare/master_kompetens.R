library(here)
# Skapar listan

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
#test=hamta_figurer(skapa_fil=FALSE)
hamta_figurer <- function(skapa_ppt=FALSE,skapa_fil=TRUE,Output_mapp= here("Diagram","/")){
  master_lista <-list()
  vald_region="20"
  
  #==========================================
  # Bakgrund och teori
  #==========================================
  # Pendlingsintensitet - pxweb, 2 figurer (pendlingsintensitet på läns respektive kommunnivå) - OBS - ligger ej längre under denna rubrik i HTML-dokument
  source(here("NMS_pendlingsintensitet.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_pendlingsintensitet(skapa_fil = skapa_fil,
                                                          output_mapp = Output_mapp,
                                                          region_vekt=vald_region,
                                                          diag_pendling_lan=TRUE,
                                                          diag_pendling_kommun=TRUE))
  
  # Befolkning, åldersgrupper - pxweb, 1 figur (Dalarnas befolkning uppdelat på år (inkl. prognosår) och åldersgrupper (0-19, 20-64 etc.))
  source(here("pxweb_befolkning_aldersgrupper.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_befolkning(skapa_fil = skapa_fil,
                                                 output_mapp = Output_mapp,
                                                 region_vekt=vald_region,
                                                 diag_befolkning_aldersgrupper=TRUE))
  
  # Förvärvsarbetande 1990- - pxweb, 1 figur (Antal förvärvsarbetande per bransch från 1990 till senaste år)
  source(here("pxweb_syss_bransch_1990_2020.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_sysselsatta_1990(skapa_fil = skapa_fil,
                                                       output_mapp = Output_mapp,
                                                       region_vekt=vald_region,
                                                       diag_forvarvsarbetande=TRUE))
  
  # Arbetslöshet 2008 till senaste år - AF (Excel), 1 figur (visar arbetslöshet utan uppdelning från 2008 till senaste år)
  source(here("AF_arbetsloshet.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_arbetsloshet_2008_senaste(skapa_fil = skapa_fil,
                                                               output_mapp = Output_mapp,
                                                               region_vekt=vald_region,
                                                               diag_arbetsloshet_totalt=TRUE,
                                                               diag_arbetsloshet_kon=FALSE))
  
  # Förvärvsarbetande 1999- - NMS, 2 figur - UTGÅR!! Ersätts av ovanstående
  # source(here("NMS_forvarvsarbetande_branch_97.R"), encoding = "utf-8", echo = FALSE)
  # master_lista <- c(master_lista,diag_branch_fran_97(skapa_fil = skapa_fil,
  #                                                    output_mapp = Output_mapp,
  #                                                    region_vekt=vald_region,
  #                                                    diag_bransch_97=TRUE,
  #                                                    diag_bransch_97_forandring=TRUE))
  
  # Utbildningsgrupper 1985-2021 - pxweb, 2 figurer (Andel med minst 3 årig eftergymnasial utbildning från 85 till senaste år och samma för senaste år - går även att dela upp på kön)
  source(here("pxweb_utbniva_85_21.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_utbniva_85(skapa_fil = skapa_fil,
                                                 output_mapp = Output_mapp,
                                                 region_vekt=vald_region,
                                                 diag_utb_85=TRUE,
                                                 diag_utb_85_kon=FALSE,
                                                 diag_utb_lan=TRUE,
                                                 diag_utb_lan_kon=FALSE))
  
  
  #==========================================
  # Arbetsmarknadsbehoven i Dalarnas län
  #==========================================
  # Kompetensnivå för län och branscher - pxweb 2 figurer
  source(here("pxweb_yrke_kompetensniva.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_yrken_kompetens(skapa_fil = skapa_fil,
                                                      output_mapp = Output_mapp,
                                                      region_vekt=vald_region,
                                                      diag_kompetens_lan=TRUE,
                                                      diag_kompetens_bransch=TRUE))
  
  # Förvärvsarbetande per bransch och kön - pxweb 1 figur
  source(here("pxweb_syss_bransch.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_sysselsatta(skapa_fil = skapa_fil,
                                                  output_mapp = Output_mapp,
                                                  region_vekt=vald_region))
  
  # Utbildningsniva och åldersfördelning per bransch - NMS, fem figurer 
  source(here("NMS_bransch_utbildningsniva.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_bransch_utbildning_alder(skapa_fil = skapa_fil,
                                                               output_mapp = Output_mapp,
                                                               region_vekt=vald_region,
                                                               diag_bransch_utbildning=TRUE,
                                                               diag_bransch_alder=TRUE,
                                                               diag_bransch_alder_antal=TRUE,
                                                               diag_bransch_forandring=TRUE,
                                                               diag_bransch_forandring_ejsun=TRUE))
  
  # Prognos, förvärvsarbetande per bransch - Trender och prognoser 1 figur
  source(here("TP_branscher_prognos.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_TP_Prognos_bransch(skapa_fil = skapa_fil,
                                                  output_mapp = Output_mapp,
                                                  region_vekt=vald_region))
  
  Valda_utbildningar_alt <- c("53B","53R","55BG","55H-L") 
  # Prognoser för utbildningsgrupper - 2 figurer, Trender och prognoser
  source(here("TP_utbgrupper_prognos.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_TP_Prognos(skapa_fil = skapa_fil,
                                                 output_mapp = Output_mapp,
                                                 region_vekt=vald_region,
                                                 valda_utbildningar=Valda_utbildningar_alt,
                                                 diag_prognos_2035=FALSE,
                                                 diag_prognos_fleraar=FALSE,
                                                 diag_prognos_balans_utbgrupp=TRUE,
                                                 diag_prognos_fleraar_utbgrupp=TRUE))
  
  
  #==========================================
  # Utbudet i Dalarnas län
  #==========================================
  
  # Befolkningsförändring 20-64 år - pxweb 1 figur 
  source(here("pxweb_befolkningsforandring_20_64.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_befolkning_forandring(skapa_fil = skapa_fil,
                                                            output_mapp = Output_mapp,
                                                            region_vekt=vald_region))

  # # Befolkningsprognosen - 2 figurer, läns och kommunnivå.
  # source("G:/skript/jon/Presentationer/Byggforetagen/PXWeb_Befolkningsprognos.R", encoding = "utf-8", echo = FALSE)
  # master_lista <- c(master_lista, diag_befolkningsprognos(region_vekt = vald_region,
  #                                                         skapa_fil = skapa_fil,
  #                                                         output_mapp = Output_mapp,
  #                                                         manual_farg_lan=diagramfarger("gron_sex")[4:6],
  #                                                         manual_farg_kommun = diagramfarger("gron_sex")[4:6]),
  #                                                         bef_lan=TRUE,
  #                                                         bef_kommun=FALSE)
  
  # Befolkning kopplat till utbildningsnivå, kön och bakgrund - NMS, 4a figurer
  source(here("NMS_utbildningsniva_utlsv.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_utbildning_bakgrund(region_vekt = "20", 
                                                          skapa_fil = skapa_fil,
                                                          output_mapp = Output_mapp,
                                                          diag_bakgrund_tidsserie=TRUE,
                                                          diag_bakgrund_utbildning=TRUE,
                                                          diag_inriktning_kon=TRUE,
                                                          diag_forgymnasial_kon=TRUE))
  
  
  # Skript används inte längre (Peter har nyare data). Låter vara kvar i master
  # # Antagna gymnasieelever i Dalarnas län - 1 figur för alla program i dalarna (diag_alla) och 1 där man fokuserar på en specifik bransch
  # source(here("gymnasieantagning.R"), encoding = "utf-8", echo = FALSE)
  # master_lista <- c(master_lista,diag_gymnasieantagning(region_vekt = "20", 
  #                                                       skapa_fil = skapa_fil,
  #                                                       output_mapp = Output_mapp,
  #                                                       fokus_program = c("Bygg- och anläggningsprogrammet","El- och energiprogrammet","VVS- och fastighetsprogrammet"),
  #                                                       diag_alla=TRUE,
  #                                                       diag_fokus=FALSE))
  
  # Antagna gymnasieelever i Dalarnas län Gymnasieantagningen - 3 figurer
  source(here("gymnasieantagning_peter.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_gymnasieantagning_Peter(region_vekt = "20", 
                                                              skapa_fil = skapa_fil,
                                                              output_mapp = Output_mapp,
                                                              #fokus_program = c("Bygg- och anläggningsprogrammet","El- och energiprogrammet","VVS- och fastighetsprogrammet"),
                                                              diag_alla=TRUE,
                                                              diag_alla_kon=TRUE,
                                                              diag_alla_fleraar=TRUE))
  # Högskoleexamen 1 figur - NMS
  source(here("hogskoleexamen.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_hogskoleexamen(region_vekt = "20", 
                                                     skapa_fil = skapa_fil,
                                                     output_mapp = Output_mapp,
                                                     diag_hogskola_bygg=FALSE,
                                                     diag_hogskola_alla=TRUE))
  
  # Prognos högskole, gymnasieexamen och yh - 3 figurer - Trender och prognoser
  source(here("TP_gymhog_prognos.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_TP_Prognos_gym_hog(region_vekt = "20", 
                                                     skapa_fil = skapa_fil,
                                                     output_mapp = Output_mapp,
                                                     diag_prognos_gym=TRUE,
                                                     diag_prognos_hogskola=TRUE,
                                                     diag_prognos_yh=TRUE))
  
  # Examen från yrkeshögskolan - 2 figurer - NMS
  source(here("yrkeshogskoleexamen.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_yh_examen(region_vekt = "20", 
                                                     skapa_fil = skapa_fil,
                                                     output_mapp = Output_mapp,
                                                     diag_yh_examen_bransch=TRUE,
                                                     diag_yh_examen_inriktning=TRUE))
  
  #==========================================
  # Matchning i Dalarnas län
  #==========================================
  # Arbetsmarknadsstatus (arbetslöshet mm) uppdelat på län,kommun, kön och bakgrund - Pxweb, SCB 6 figurer
  # Från projektet kvinnor och män i Dalarna
  source("G:/skript/projekt/kvinnor_man/Projekt_kvinnor_man/pxweb_arbetsmarknadsstatus.R", encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_arbetsloshet(region_vekt = "20",
                                                  skapa_fil = skapa_fil,
                                                  output_mapp = Output_mapp,
                                                  diag_lan = TRUE,
                                                  diag_kommun = TRUE,
                                                  diag_arbetslosthet = FALSE,
                                                  diag_arbetskraftsdeltagande = FALSE,
                                                  diag_sysselsattningsgrad = TRUE))
  
  # Matchningsindikatorer - Peters skript. 1 diagram
  source("G:/skript/diagram/diag_matchningsindikatorerna.R", encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_matchad_forvarvsfrekvens(regionkod_vekt = "20",
                                                               utb_grp = "53B",
                                                               skapa_fil = skapa_fil,
                                                               output_mapp = Output_mapp))
  
  # Allmän matchning på arbetsmarknaden - pxweb, 2 diagram
  source(here("pxweb_matchning.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_matchning(region_vekt = "20", 
                                                     skapa_fil = skapa_fil,
                                                     output_mapp = Output_mapp,
                                                     matchning_lan=TRUE,
                                                     matchning_bakgrund=TRUE))
  
  # Upplevd kompetensbrist - Tillväxtverket (Excel)
  source(here("TVV_kompetensbrist.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_kompetensbrist(region_vekt = "20", 
                                                     skapa_fil = skapa_fil,
                                                     output_mapp = Output_mapp))
  
  #==========================================
  # Fokus på valda branscher i Dalarnas län
  #==========================================
  Valda_utbildningar <- c("53B","53R","55A","55C","55H") 
  # Matchningsfrekvens för utbildningsgrupper - pxweb, 2 figurer (per bransch)
  source(here("pxweb_matchningsfrekvens_yrke.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_matchad_forvarvsfrekvens(skapa_fil = skapa_fil,
                                                               output_mapp = Output_mapp,
                                                               regionkod_vekt=vald_region,
                                                               utb_grp = Valda_utbildningar,
                                                               kon_bakgr = "20-64 år",     # "20-39 år", "kvinnor", "män", "inrikes födda", "utrikes födda"
                                                               skickat_ar = NA,
                                                               diag_utb_grp_alla = TRUE,
                                                               diag_utb_grp_valda = TRUE))
  
  # Antal sysselsatta, kön, ålder och natt/dag-befolkning kopplat till utbildningsgrupper - NMS, 4 figurer
  source(here("NMS_utbildningsgrupp_sysselsatta_mm.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_utbildningsgrupper(skapa_fil = skapa_fil,
                                                         output_mapp = Output_mapp,
                                                         region_vekt=vald_region,
                                                         valda_utbildningar=Valda_utbildningar,
                                                         diag_utbildningsgrupper_sysselsattning=TRUE,
                                                         diag_utbildningsgrupper_kon=TRUE,
                                                         diag_utbildningsgrupper_alder=TRUE,
                                                         diag_utbildningsgrupper_flode=TRUE))
  
  # Det finns ett färre antal valda utbildningar i Trender och prognoser. Byter ut specifika ingenjörsutbildningar
  # mot allmänna
  Valda_utbildningar_alt <- c("53B","53R","55BG","55H-L") 
  # Prognoser för utbildningsgrupper - 2 figurer, Trender och prognoser
  source(here("TP_utbgrupper_prognos.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_TP_Prognos(skapa_fil = skapa_fil,
                                                 output_mapp = Output_mapp,
                                                 region_vekt=vald_region,
                                                 valda_utbildningar=Valda_utbildningar_alt,
                                                 diag_prognos_2035=TRUE,
                                                 diag_prognos_fleraar=TRUE,
                                                 diag_prognos_balans_utbgrupp=FALSE,
                                                 diag_prognos_fleraar_utbgrupp=FALSE))
 
  ##########################################################################################
  #######################           Export till Powerpoint           ####################### 
  ##########################################################################################
  if (skapa_ppt==TRUE){
    
    dag <- paste0(gsub("^0", "", format(Sys.Date(), "%d")), "_")
    man_ar <- format(Sys.Date(), "%b_%Y")
    datum <- paste0(dag, man_ar)
    filnamn_datum=paste0("Kompetensforsorjning_",datum,".pptx")
    output <- here("Output","/")
    
    source("G:/skript/jon/PPT/PPT_jon_slutgiltig.R", encoding = "utf-8", echo = FALSE)
    skapa_ppt_fran_lista(pptlista=master_lista,"Kompetensförsörjning i Dalarnas län",filnamn=filnamn_datum,output_mapp=output)
  }
  return(master_lista)

}

