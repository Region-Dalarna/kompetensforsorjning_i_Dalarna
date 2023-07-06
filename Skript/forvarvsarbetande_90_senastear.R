# Hämtar data för antalet anställda tillbaka till 1990.
# Från RAMS i SCBs öppna statistikdatabas (val 25 under arbetsmaknad)
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       openxlsx)

# source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_sysselsatta_1990(skapa_fil = FALSE)

hamta_data_sysselsatta_1990 <-function(region_vekt = "20", 
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       spara_data = TRUE,
                                       filnamn = "forvarvsarbetande_90.xlsx"){
  
  # ========================================== Inställningar ============================================
  # Förvärvsarbetande under olika tidsperioder kommer från olika källor
  url_1990_2003 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207C/AMPAK3"
  url_2004_2007 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207K/DagSNIKonK"
  url_2008_2018 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207K/DagSNI07KonK"
  url_2019_2020 <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0207/AM0207Z/DagSni07KonKN"
  
  url<-c(url_1990_2003,url_2004_2007,url_2008_2018,url_2019_2020)
  lista=list()
  
  varlista_1990_2003 <- list(
    Region = c(region_vekt),
    SNI92 = '*',
    Kon = '*',
    ContentsCode = "*",
    Tid = c('*')
  )
  
  varlista_2004_2007 <- list(
    Region = c(region_vekt),
    SNI2002 = '*',
    Kon = '*',
    ContentsCode = "*",
    Tid = c('*')
  )
  # Samma varlista för 08-18 och 19-20
  varlista_2008_2018 <- list(
    Region = c(region_vekt),
    SNI2007 = '*',
    Kon = '*',
    ContentsCode = "*",
    Tid = c('*')
  )
  
  varlista_lista=list(varlista_1990_2003,varlista_2004_2007,varlista_2008_2018,varlista_2008_2018)

  diagram_capt <- "Källa: RAMS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschgruppering baserad på SNI2002 och SNI92"

  # =============================================== API-uttag ===============================================
  i=1
  
  while(i<(length(url)+1)){
    px_uttag <- pxweb_get(url = url[i],
                          query = varlista_lista[[i]]
    ) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    
    lista[[i]] <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region))  
    lista[[i]] <- lista[[i]] %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    names(lista[[i]])[ncol(lista[[i]])] <- "Förvärvsarbetande 16+ år (dagbef)"
    
    i=i+1
  }
  
  names(lista) <- c("1990-2003","2004-2007","2008-2018","2019-2020")
  
  
  # Branschgruppering ändras när nya SNI-koder införs 2007. Den mest aggregerade grupperingen (innan 2008) används
  k=3
  
  while(k<5){
    lista[[k]]$Branschgrupp <- case_when(
      lista[[k]]$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske" ~ "jordbruk, skogsbruk, jakt, fiske",
      lista[[k]]$`näringsgren SNI 2007` == "tillverkning och utvinning" ~ "utvinning av mineral, tillverkningsindustri",
      lista[[k]]$`näringsgren SNI 2007` == "energiförsörjning; miljöverksamhet" ~ "energi- o vattenförsörjning, avfallshantering",
      lista[[k]]$`näringsgren SNI 2007` == "byggverksamhet" ~ "byggindustri",
      lista[[k]]$`näringsgren SNI 2007` == "handel" ~ "handel; transport, magasinering; kommunikation",
      lista[[k]]$`näringsgren SNI 2007` == "transport och magasinering"~ "handel; transport, magasinering; kommunikation",
      lista[[k]]$`näringsgren SNI 2007` == "hotell- och restaurangverksamhet" ~ "personliga och kulturella tjänster",
      lista[[k]]$`näringsgren SNI 2007` == "information och kommunikation" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      lista[[k]]$`näringsgren SNI 2007` == "finans- och försäkringsverksamhet" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      lista[[k]]$`näringsgren SNI 2007` == "fastighetsverksamhet" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      lista[[k]]$`näringsgren SNI 2007` == "företagstjänster" ~ "kreditinstitut, fastighetsförvaltn, företagstjänster",
      lista[[k]]$`näringsgren SNI 2007` == "offentlig förvaltning och försvar"~ "civila myndigheter, försvar; internat. organisationer",
      lista[[k]]$`näringsgren SNI 2007` == "utbildning " ~ "forskning o utveckling; utbildning",
      lista[[k]]$`näringsgren SNI 2007` == "vård och omsorg; sociala tjänster" ~ "enh för hälso- och sjukvård, socialtjänst; veterinärer",
      lista[[k]]$`näringsgren SNI 2007` == "kulturella och personliga tjänster m.m." ~ "personliga och kulturella tjänster",
      lista[[k]]$`näringsgren SNI 2007` == "okänd verksamhet"~ "näringsgren okänd")
    k=k+1
  }
  
  # Döper om variabler för att sedan slå ihop de dataset
  # 1990-2003
  lista[[1]]<-lista[[1]] %>% 
    rename("Näringsgren"="näringsgren SNI92")
  # 2004-2007
  lista[[2]]<-lista[[2]] %>% 
    rename("Näringsgren"="näringsgren SNI 2002")
  # 2008-2018
  lista[[3]]<-lista[[3]] %>% 
    select(-c("näringsgren SNI 2007")) %>% 
    rename("Näringsgren"="Branschgrupp") 
  lista[[4]]<-lista[[4]] %>% 
    select(-c("näringsgren SNI 2007")) %>% 
    rename("Näringsgren"="Branschgrupp")     
  
  df_utskrift<-rbind(lista[[1]],lista[[2]],lista[[3]],lista[[4]])
  
  # Grupperar på näringsgren och år
  df_utskrift<-df_utskrift %>% 
    group_by(region,Näringsgren,år) %>% 
      summarize(antal=sum(`Förvärvsarbetande 16+ år (dagbef)`))
  
  if (spara_data==TRUE){
    write.xlsx(df_utskrift,paste0(output_mapp,filnamn))
  }
  
  
  # max_ar=max(df_utskrift$år)
  # 
  # # Gör om år till en faktorvariabel för utbildningsnivå_balans. Detta för att kunna bestämma vilken ordning staplarna för de olika åren kommer i diagrammet
  # df_utskrift$år <- factor(df_utskrift$år, levels = c(max_ar,"2010","2000","1990"))
  # 
  # df_utskrift$Näringsgren <- stringr::str_to_sentence(df_utskrift$Näringsgren)
  # 
  # if(diag_forvarvsarbetande==TRUE){
  #   
  #   diagram_titel <- paste0("Förvärvsarbetande 16+ år i ", ValdGeografi[1], " per bransch ")
  #   diagram_typ <- "per_bransch_tidsserie"
  #   diagramfil <- paste0("per_bransch_tidsserie", ".png")
  #   objektnamn <- paste0(diagram_titel)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df = df_utskrift %>%
  #                                  filter(år%in%c("1990","2000","2010",max_ar)), 
  #                                skickad_x_var = "Näringsgren", 
  #                                skickad_y_var = "antal", 
  #                                skickad_x_grupp = "år",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=0.5,
  #                                manual_color = diagramfarger("gron_sex")[3:6],
  #                                x_axis_sort_value = TRUE,
  #                                x_axis_lutning = 0,
  #                                diagram_titel = diagram_titel,
  #                                diagram_capt = diagram_capt,
  #                                diagram_liggande = TRUE,
  #                                diagram_facet = FALSE,
  #                                facet_grp="år",
  #                                legend_vand_ordning=TRUE,
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfil,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[j]] <-gg_obj
  #   i=i+1
  # }
  # names(gg_list) <-c(objektnamn)
  # return(gg_list)
  
}
