# Skapar en figur med antal anställda i olika branscher.
# Från RAMS i SCBs öppna statistikdatabas (val 25 under arbetsmaknad)
library(pxweb)
library(httr)
library(askpass)
library(writexl)
library(dplyr)
library(tidyr)
library(openxlsx)


source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test_list <- diag_sysselsatta(skapa_fil = FALSE)

diag_sysselsatta <-function(region_vekt = "20", 
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  # url_list <- c("/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207K/DagSNI07KonK", 
  #               "/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSni07KonKN")
  
  url_list <- "/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSni07KonKN"

  SummeringsVar <- c("Förvärvsarbetande 16+ år (dagbef)")
  
  # Hämtar data för regionkoder
  region_mall <-read.xlsx("G:/skript/naringsliv/mall/regionnummer.xlsx",sheet=1)
  
  # Kopplar ihop kommuner och den region de tillhör
  j=1
  region_uttag=c()
  while(j<=length(region_mall$regionnummer)){
    if (substr(region_mall$regionnummer[j],0,2)==region_vekt){ 
      region_uttag <- c(region_uttag,region_mall$regionnummer[j])
      j=j+1}
    else j=j+1
  }
  
  varlista <- list(
    Region = c("00",region_uttag),
    SNI2007 = '*',
    Kon = '*',
    ContentsCode = "*",
    Tid = c('*')
  )
  
  BaraEttLän <- region_vekt
  ValdGeografi <- c(hamtaregion_kod_namn(region_vekt)$region,"Riket")
  #SummeringsVar <- "BRP, löpande priser, mnkr"
  diagram_capt <- "Källa: RAMS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  
  # =============================================== API-uttag ===============================================
  
  px_df <- NULL
  url1 <- "https://api.scb.se"
  
  for (url_tab in 1:length(url_list)){
    
    url3 <- paste0(url1, url_list[url_tab])
    px_uttag <- pxweb_get(url = url3,
                          query = varlista
    ) 
    
    filnamn <- paste0(unlist(strsplit(px_uttag$metadata[[1]][[3]], " "))[1], "_API.xlsx")
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    px_df_temp <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region))  
    px_df_temp <- px_df_temp %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    names(px_df_temp)[ncol(px_df_temp)] <- "Förvärvsarbetande 16+ år (dagbef)"
    
    px_df <- rbind(px_df, px_df_temp)
    
  }
  
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
  
  pre_titel <- gsub(",.*", "", SummeringsVar)

  diagram_titel <- paste0("Förvärvsarbetande 16-74 år i ", ValdGeografi[1], " per bransch ",max(px_df$år))
  diagram_typ <- "_per_bransch_kon"
  diagramfil <- paste0(pre_titel, diagram_typ, ".png")
  objektnamn <- paste0(pre_titel, diagram_typ)
  
  gg_obj <- SkapaStapelDiagram(skickad_df = px_df %>%
                                              filter(år==max(år)
                                                      ,region=="Dalarnas län"), 
                               skickad_x_var = "Branschgrupp", 
                               skickad_y_var = SummeringsVar, 
                               skickad_x_grupp = "kön",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("kon"),
                               x_axis_sort_value = TRUE,
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               diagram_facet = FALSE,
                               facet_grp="år",
                               berakna_index = FALSE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfil,
                               skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  i=i+1
  names(gg_list) <-c(objektnamn)
  return(gg_list)
  
}
