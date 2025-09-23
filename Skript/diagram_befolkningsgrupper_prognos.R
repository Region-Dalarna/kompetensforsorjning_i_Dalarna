diag_befolkning_aldersgrupper_prognos <-function(region_vekt = "20", 
                                                 output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                                 jmf_ar = c("1968","2000","2010"), # År att jämföra med (förutom senaste år och 10 år framåt i tiden)
                                                 spara_figur = FALSE, # Skall diagrammet sparas
                                                 returnera_data = FALSE # Skall data returneras
                                                 ){
  
  # =========================================================================================================
  # Skapar ett diagram där historisk data och prognos över befolkningen slås ihop (uppdelat på åldersgrupper)
  # Prognos finns här: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0401__BE0401A/BefProgOsiktRegN/
  # =============================================== API-uttag ===============================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx,
         glue)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  gg_list <- list()
  
  url <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
           "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy")
  
  lista=list()
  
  varlista_prognos <- list(
    Region = region_vekt,
    Kon = '*',
    Alder = '*',
    ContentsCode = "*",
    Tid = '*')
  
  varlista_folkmangd <- list(
    Region = region_vekt,
    Civilstand=c("*"),
    Kon = '*',
    Alder = '*',
    ContentsCode = "BE0101N1",
    Tid = '*')
  
  varlista_lista=list(varlista_prognos,varlista_folkmangd)
  
  i=1
  while(i<(length(url)+1)){
    px_uttag <- pxweb_get(url = url[i],
                          query = varlista_lista[[i]]
    ) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    
    lista[[i]] <- as.data.frame(px_uttag) %>% 
      cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
              select(Region)) %>% 
      rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    
    # I de fall där det finns en kategori total ålder skall dessa tas bort - blir dubbelräkning annars
    if("totalt ålder" %in% unique(lista[[i]]$ålder)) lista[[i]]<- lista[[i]][lista[[i]]$ålder!="totalt ålder",]
    
    # Ändrar ålder på alla 100+ till 100. För att word-funktionen nedan skall funka
    if("100+ år" %in% unique(lista[[i]]$ålder)) lista[[i]][lista[[i]]=="100+ år"]<-"100"
    
    # Mellanslag och år tas bort
    lista[[i]]$ålder <- as.integer(word(lista[[i]]$ålder))
    
    i=i+1
  }
  
  names(lista) <- c("Prognos","Historik_folkmangd")
  
  # Folkmängd 
  px_df_folkmangd_prognos <- lista$Prognos %>%
    summarise(`Folkmängd`= sum(`Folkmängd`, na.rm = TRUE), .by = c(region,år,ålder))
  
  px_df_folkmangd_historik <- lista$Historik_folkmangd %>%
    summarise(`Folkmängd`= sum(`Folkmängd`, na.rm = TRUE), .by = c(region,år,ålder))
  
  # SCB släpper data för variablerna innan en ny prognos görs, vilket skapar en överlapp. I dessa fall tas det 
  # första året i prognosen bort och ersätts med data.
  if(last(px_df_folkmangd_historik$år)==first(px_df_folkmangd_prognos$år)){
    px_df_folkmangd_prognos <- px_df_folkmangd_prognos[px_df_folkmangd_prognos$år!=min(px_df_folkmangd_prognos$år),]
    
  } 
  # Binder ihop historik och prognos
  px_df_folkmangd <- rbind(px_df_folkmangd_historik,px_df_folkmangd_prognos) %>%
    mutate(
      aldersgrupper = cut(
        ålder,
        breaks  = c(-Inf, 19, 64, 79, Inf),           # (-Inf,19], (19,64], (64,79], (79,Inf]
        labels  = c("0-19 år","20-64 år","65-79 år","80+ år"),
        right   = TRUE,
        ordered_result = TRUE
      )
    )

  px_df_folkmangd_utskrift <- px_df_folkmangd %>%
    filter(år%in%c(jmf_ar,max(px_df_folkmangd_historik$år),as.character((as.integer(max(px_df_folkmangd_historik$år))+10)))) %>% 
      summarise(`Folkmängd`= sum(`Folkmängd`, na.rm = TRUE), .by = c(region,år,aldersgrupper)) %>% 
       mutate(region = skapa_kortnamn_lan(region))
  
  if(returnera_data == TRUE){
    assign("folkmangd_aldersgrupper_df", px_df_folkmangd_utskrift, envir = .GlobalEnv)
  }
  
  diagram_capt <- glue("Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: {max(px_df_folkmangd_utskrift$år)} är prognos, övriga historisk data.")
  diagram_titel <- glue("Befolkningen i {unique(px_df_folkmangd_utskrift$region)}  uppdelat på åldersgrupper")
  diagramfil <- paste0("befolkning_forandring_prognos_",unique(px_df_folkmangd_utskrift$region),".png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = px_df_folkmangd_utskrift %>% 
                                 mutate(aldersgrupper = factor(aldersgrupper, levels = c("0-19 år","20-64 år","65-79 år","80+ år"))),
                               skickad_x_var = "aldersgrupper",
                               skickad_y_var = "Folkmängd",
                               skickad_x_grupp = "år",
                               manual_x_axis_text_vjust = 0.7,
                               manual_x_axis_text_hjust = 0.3,
                               manual_color = diagramfarger("rus_sex"),
                               x_axis_lutning = 0,
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               manual_x_axis_title = "Åldersgrupper",
                               stodlinjer_avrunda_fem = TRUE,
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfil,
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  return(gg_list)
  
  }
  


