# Historisk data och prognos för folkmängden i Dalarna
pacman::p_load(pxweb,tidyverse,openxlsx)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_text.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_befolkning(skapa_fil=FALSE)

diag_befolkning <-function(region_vekt = "20", 
                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                           skapa_fil = TRUE,
                           valda_farger=diagramfarger("gron_sex")[3:6],
                           diag_befolkning_aldersgrupper=TRUE){

  # ========================================== Inställningar ============================================
  
  Region <- region_vekt

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: 2031 är prognos, övriga historisk data."
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  j=1
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer

  # =============================================== API-uttag ===============================================
  url_framskrivning <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
  url_folkmangd <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  
  url<-c(url_framskrivning,url_folkmangd)
  
  lista=list()
  
  varlista_prognos <- list(
    Region = Region,
    Kon = '*',
    Alder = '*',
    ContentsCode = "*",
    Tid = '*')

  varlista_folkmangd <- list(
    Region = Region,
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
    lista[[i]]$ålder<-as.integer(word(lista[[i]]$ålder))

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
  
  # SCB släpper data för variablerina innan en ny prognos görs, vilket skapar en överlapp. I dessa fall tas det 
  # första året i prognosen bort och ersätts med data.
  
  if(last(px_df_folkmangd_historik$år)==first(px_df_folkmangd_prognos$år)){
    px_df_folkmangd_prognos <- px_df_folkmangd_prognos[px_df_folkmangd_prognos$år!=min(px_df_folkmangd_prognos$år),]
      
  } 
  px_df_folkmangd <- rbind(px_df_folkmangd_historik,px_df_folkmangd_prognos)
  
  # Binder ihop historik och prognos
  px_df_folkmangd <- rbind(px_df_folkmangd_historik,px_df_folkmangd_prognos)
  
  # Skapar åldersgrupper
  px_df_folkmangd$aldersgrupper<-ifelse(between(px_df_folkmangd$ålder,0,19),"0-19 år",NA)
  px_df_folkmangd$aldersgrupper<-ifelse(between(px_df_folkmangd$ålder,20,64),"20-64 år",px_df_folkmangd$aldersgrupper)
  px_df_folkmangd$aldersgrupper<-ifelse(between(px_df_folkmangd$ålder,65,79),"65-79 år",px_df_folkmangd$aldersgrupper)
  px_df_folkmangd$aldersgrupper<-ifelse(px_df_folkmangd$ålder>79,"80+ år",px_df_folkmangd$aldersgrupper)
  
  # Det första datasettet - innehåller åldersgrupper men inte totalt för alla
  # Summerar på år och aldersgrupper
  px_df_folkmangd_sum<-px_df_folkmangd %>% 
    group_by(år,aldersgrupper) %>% 
      summarize(`Folkmängd`=sum(`Folkmängd`))
  
  # Väljer ut år och filtrerar på dessa
  fokus_ar <- c("1968","2000",max(px_df_folkmangd_historik$år),"2031")
  px_df_folkmangd_utskrift <- px_df_folkmangd_sum %>% 
    filter(år%in%fokus_ar)
  
  # Det andra datasettet, skapar en kategori totalt
  px_df_folkmangd_totalt<-px_df_folkmangd_utskrift %>% 
    group_by(år) %>% 
      summarize(`Folkmängd`=sum(`Folkmängd`)) %>% 
        mutate(aldersgrupper="totalt")
  
  # Sätter ihop dessa två dataset
  px_df_folkmangd_utskrift <- rbind(px_df_folkmangd_utskrift,px_df_folkmangd_totalt)
  
  # Använder en faktorvariabel för att styra ordningen på åldersgrupper
  px_df_folkmangd_utskrift$aldersgrupper <- factor(px_df_folkmangd_utskrift$aldersgrupper, levels = c("totalt","0-19 år","20-64 år","65-79 år","80+ år"))
  
  if(diag_befolkning_aldersgrupper==TRUE){
    diagram_titel <- paste0("Dalarnas befolkning uppdelat på åldersgrupper")
    diagram_typ <- "befolkning_forandring"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_folkmangd_utskrift %>% 
                                   filter(aldersgrupper!="totalt"), 
                                 skickad_x_var = "aldersgrupper", 
                                 skickad_y_var = "Folkmängd", 
                                 skickad_x_grupp = "år",
                                 manual_x_axis_text_vjust=0.7,
                                 manual_x_axis_text_hjust=0.3,
                                 manual_color = valda_farger,
                                 x_axis_lutning = 0,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 manual_x_axis_title = "Åldersgrupper",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[j]] <-gg_obj
    j=j+1
  }
  
names(gg_list)<-c(objektnamn)
return(gg_list)

}

