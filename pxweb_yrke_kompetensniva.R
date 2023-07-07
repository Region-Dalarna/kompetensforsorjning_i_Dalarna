# Yrke kopplat till kompetensnivå.
# Data från yrkesregistret i SCB öppna databas (val 26 under arbetsmarknad, yrkesregistret med yrkesstatistik)
pacman::p_load(pxweb,httr,tidyverse)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_yrken_kompetens(skapa_fil = FALSE)

diag_yrken_kompetens <-function(region_vekt = "20", 
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            skapa_fil = TRUE,
                            diag_kompetens_lan=TRUE,
                            diag_kompetens_bransch=TRUE){
  
  # ========================================== Inställningar ============================================
  
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"

  url_list <- "/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N"
  vald_region=hamtaregion_kod_namn(region_vekt)[[2]]
  diagram_capt <- "Källa: Yrkesregistret i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare som används för att lägga till objekt i listan
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  
  # =============================================== API-uttag ===============================================
  
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N"
  url3 <- paste0(url1, url2)
  
  varlista <- list(
    Region = c(hamtaAllaLan(FALSE)),
    Yrke2012='*',
    SNI2007 = '*',
    Kon = c("1","2"),
    ContentsCode = c("000003T3"),
    Tid = c('*'))
    
  px_uttag <- pxweb_get(url = url3,
                          query = varlista) 
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(Yrke2012_kod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Yrke2012)) %>% 
      rename(yrkeskod = Yrke2012) %>% 
        relocate(yrkeskod, .before = 'Yrke (SSYK 2012)')
  names(px_df)[ncol(px_df)] <- "Anställda 16-64 år (dagbef)"
  
  # Ändra namn på vissa branscher
  px_df$Branschgrupp <- case_when(
    px_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske" ~ "Jordbruk och skogsbruk",
    px_df$`näringsgren SNI 2007` == "tillverkning och utvinning" ~ "Tillverkning och utvinning",
    px_df$`näringsgren SNI 2007` == "energiförsörjning; miljöverksamhet" ~ "Energi och miljö",
    px_df$`näringsgren SNI 2007` == "byggverksamhet" ~ "Bygg",
    px_df$`näringsgren SNI 2007` == "handel" ~ "Handel",
    px_df$`näringsgren SNI 2007` == "transport och magasinering"~ "Transport",
    px_df$`näringsgren SNI 2007` == "hotell- och restaurangverksamhet" ~ "Hotell och restaurang",
    px_df$`näringsgren SNI 2007` == "information och kommunikation" ~ "IT och kommunikation",
    px_df$`näringsgren SNI 2007` == "finans- och försäkringsverksamhet" ~ "Finans och försäkring",
    px_df$`näringsgren SNI 2007` == "fastighetsverksamhet" ~ "Fastighet",
    px_df$`näringsgren SNI 2007` == "företagstjänster" ~ "Företagstjänster",
    px_df$`näringsgren SNI 2007` == "offentlig förvaltning och försvar"~ "Offentlig förvaltning",
    px_df$`näringsgren SNI 2007` == "utbildning " ~ "Utbildning",
    px_df$`näringsgren SNI 2007` == "vård och omsorg; sociala tjänster" ~ "Vård och omsorg",
    px_df$`näringsgren SNI 2007` == "kulturella och personliga tjänster m.m." ~ "Kultur m.m.",
    px_df$`näringsgren SNI 2007` == "okänd verksamhet"~ "Okänd verksamhet")
  
  # Klassificerar yrken utifrån hur avancerade de är
  px_df$kompetensniva <- ifelse(substr(px_df$yrkeskod,1,1)=="1","Chefsyrken",NA)
  px_df$kompetensniva <- ifelse(between(as.integer(substr(px_df$yrkeskod,1,1)),2,2),"Motsvarande fördjupad högskolekompetens",px_df$kompetensniva)
  px_df$kompetensniva <- ifelse(between(as.integer(substr(px_df$yrkeskod,1,1)),3,3),"Motsvarande högskolekompetens",px_df$kompetensniva)
  px_df$kompetensniva <- ifelse(between(as.integer(substr(px_df$yrkeskod,1,1)),4,8),"Motsvarande gymnasial kompetens",px_df$kompetensniva)
  px_df$kompetensniva <- ifelse(substr(px_df$yrkeskod,1,1)=="0" & px_df$yrkeskod !="0002","Motsvarande gymnasial kompetens",px_df$kompetensniva)
  px_df$kompetensniva <- ifelse((px_df$yrkeskod)=="0002","Okänt",px_df$kompetensniva)
  px_df$kompetensniva <- ifelse(substr(px_df$yrkeskod,1,1)=="9","Enklare yrken",px_df$kompetensniva)
  
  px_df$kompetensniva <- factor(px_df$kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])
  
  px_df$kompetensniva <- factor(px_df$kompetensniva)
  # Grupperar på läns och kompetensnivå
  px_df_lan_sum <- px_df %>%
    filter(år==max(år)) %>% 
      group_by(region,kompetensniva) %>% 
        summarize(Antal=sum(`Anställda 16-64 år (dagbef)`)) %>% 
          mutate(Andel=(Antal/sum(Antal))*100)
  
  # Skapar en faktorvariabel för att få utbildningsnivå i "rätt" ordning i figuren
  px_df_lan_sum$kompetensniva <- factor(px_df_lan_sum$kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])

  # Fokuserar istället på valt län
  px_df_bransch_sum <- px_df %>%
    filter(år==max(år),region==vald_region) %>% 
      group_by(Branschgrupp,kompetensniva) %>% 
        summarize(Antal=sum(`Anställda 16-64 år (dagbef)`)) %>% 
          mutate(Andel=(Antal/sum(Antal))*100)
  
  #Tar bort s och sista bokstav
  px_df_lan_sum <- px_df_lan_sum %>%
    mutate(region=skapa_kortnamn_lan(region))
  
  # Alternativt sätt utan att använda Dplyr
  #px_df_lan_sum$region <- skapa_kortnamn_lan(px_df_lan_sum$region)
  
  px_df_bransch_sum$kompetensniva <- factor(px_df_bransch_sum$kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])
  
  # Vill att färgen ljusgrå skall motsvara okänt
  farger_gra<-c(rgb(211,211,211, maxColorValue = 255),diagramfarger("gron_sex")[2:6])
  
  if(diag_kompetens_lan==TRUE){
    
    diagram_titel <- paste0("Kvalifikationskrav för anställda 16-64 år per län ",max(px_df$år))
    diagram_typ <- "_kompetenskrav_lan"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_lan_sum, 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "Andel", 
                                 skickad_x_grupp = "kompetensniva",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.3,
                                 manual_color = farger_gra,
                                 diagram_titel = diagram_titel,
                                 x_axis_lutning = 0,
                                 diagram_capt = diagram_capt,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 geom_position_stack = TRUE,
                                 manual_y_axis_title = "procent",
                                 x_axis_sort_value = TRUE,         
                                 x_axis_sort_grp = 6,
                                 vand_sortering = TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diag_kompetens_bransch==TRUE){
    
    diagram_titel <- paste0("Kvalifikationskrav för anställda 16-64 år per bransch ",max(px_df$år)," i ",vald_region)
    diagram_typ <- "_kompetenskrav_bransch"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_bransch_sum[px_df_bransch_sum$Branschgrupp!="Okänd verksamhet",], 
                                 skickad_x_var = "Branschgrupp", 
                                 skickad_y_var = "Andel", 
                                 skickad_x_grupp = "kompetensniva",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.3,
                                 manual_color = farger_gra,
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,         
                                 x_axis_sort_grp = 6,
                                 vand_sortering = TRUE,
                                 geom_position_stack = TRUE,
                                 manual_y_axis_title = "procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  names(gg_list) <-c(objektnamn)
  return(gg_list)
  
}
