# Historisk data och prognos för befolkningen i Dalarna
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/func/func_text.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_befolkning(skapa_fil=FALSE)

diag_befolkning <-function(region_vekt = "20", 
                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                           spara_data = TRUE,
                           filnamn = "befolkning_prognos.xlsx"){

  # =============================================== API-uttag ===============================================
  url_framskrivning <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
  url_folkmangd <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  
  url<-c(url_framskrivning,url_folkmangd)
  
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
    group_by(år,ålder) %>% 
      summarize(`Folkmängd`=sum(`Folkmängd`))
  
  px_df_folkmangd_historik <- lista$Historik_folkmangd %>%
    group_by(år,ålder) %>% 
      summarize(`Folkmängd`=sum(`Folkmängd`))
  
  # SCB släpper data för variablerna innan en ny prognos görs, vilket skapar en överlapp. I dessa fall tas det 
  # första året i prognosen bort och ersätts med data.
  
  if(last(px_df_folkmangd_historik$år)==first(px_df_folkmangd_prognos$år)){
    px_df_folkmangd_prognos <- px_df_folkmangd_prognos[px_df_folkmangd_prognos$år!=min(px_df_folkmangd_prognos$år),]
    
  } 
  # Binder ihop historik och prognos
  px_df_folkmangd <- rbind(px_df_folkmangd_historik,px_df_folkmangd_prognos)
  
  # Skapar åldersgrupper
  px_df_folkmangd <- px_df_folkmangd %>% 
    mutate(aldersgrupper = ifelse(between(ålder,0,19),"0-19 år",
                                  ifelse(between(ålder,20,64),"20-64 år",
                                         ifelse(between(px_df_folkmangd$ålder,65,79),"65-79 år",
                                                ifelse(ålder>79,"80+ år",ålder)))))
  
  # Väljer ut år och filtrerar på dessa
  #fokus_ar <- c("1968","2000",max(px_df_folkmangd_historik$år),"2031")
  
  px_df_folkmangd_utskrift <- px_df_folkmangd %>%
    filter(år%in%c("1968","2000","2010",max(px_df_folkmangd_historik$år),as.character((as.integer(max(px_df_folkmangd_historik$år))+10))))
  
  
  if (spara_data==TRUE){
    openxlsx::write.xlsx(px_df_folkmangd_utskrift,paste0(output_mapp,filnamn))
  }
  
}

