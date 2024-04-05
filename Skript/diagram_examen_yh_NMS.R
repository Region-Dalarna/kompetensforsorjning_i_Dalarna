diagram_examen_yh_NMS <- function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                         valda_farger = diagramfarger("rus_sex"), # Vilka färger skall användas i diagram
                                         caption = "Källa: NMS-databasen (SCB), utbildningsregistret\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Examensår efter 2011.",
                                         diag_bransch = TRUE, # Skall diagram över vanligaste branscher för yh-utbildade skapas
                                         diag_inriktning = TRUE, # Skall diagram över vanligaste inriktningar för yh-utbildade skapas
                                         spara_figur = TRUE, # Om true sparas figuren till output_mapp
                                         returnera_figur = TRUE, # Skall figur returneras (i en lista)
                                         returnera_data = FALSE){ # Skall data returneras (till R-studios globla miljö)
  
  # ========================================== Allmän info ============================================
  
  # Två figurer för att visa de vanligaste branscherna och inriktningarna för förvärvsarbetande i Dalarnas län som har en YH-utbildning
  
  # Skriptet som skapar nedanstående data finns på Mona: P1079gem/peter/YH_uppfoljning_bransch_yrke
  # Data hämtades senaste 2024-03-08 (data för år 2021)
  # Skapat av Jon Frank 2024-04-03
  # ========================================== Inställningar ============================================
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # skapa en tom vektor att lägga namn på ggplot-obj
  
  # =============================================== Hämta data ===============================================
  
  
  # Läs in datafil med högskoleexamen
  folkhogskola_df <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/8_mar_24_Dalarna_yrkeshogskoleexamen.xlsx", sheet =1)

  # =============================================== Diagram ===============================================
  
  if(diag_bransch == TRUE){
    diagram_titel <- paste0("De tio vanligaste branscherna för förvärvsarbetande (16-74 år) i Dalarnas län för YH-utbildade ",max(folkhogskola_df$Ar))
    # Rubriken blir för lång. Använder string_wrap för skriva den i två rader
    diagram_titel <- str_wrap(diagram_titel)
    diagram_typ <- "yh_bransch"
    diagramfil <- paste0(diagram_typ,".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    folkhogskola_bransch <- folkhogskola_df %>% 
      group_by(Ar,AstSNI2007_namn) %>% 
        summarize(antal=sum(antal)) %>% 
          slice_max(antal,n=10) %>% 
            ungroup()
    
    # Returnerar data till R globala miljö
    if(returnera_data == TRUE){
      assign("folkhogskola_bransch_df", folkhogskola_bransch, envir = .GlobalEnv)
    }
    
    #folkhogskola_df_bransch_urval$AstSNI2007_namn <- str_wrap(folkhogskola_df_bransch_urval$AstSNI2007_namn,50)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = folkhogskola_bransch %>% 
                                   mutate(AstSNI2007_namn = str_wrap(AstSNI2007_namn,50)),
                                         skickad_x_var = "AstSNI2007_namn",
                                         skickad_y_var = "antal",
                                         manual_x_axis_text_vjust=0,
                                         manual_x_axis_text_hjust=0.6,
                                         manual_color = valda_farger,
                                         diagram_titel = diagram_titel,
                                         x_axis_sort_value = TRUE,
                                         diagram_capt = caption,
                                         x_axis_lutning = 0,
                                         diagram_liggande = TRUE,
                                         output_mapp = output_mapp_figur,
                                         filnamn_diagram = diagramfil,
                                         skriv_till_diagramfil = spara_figur)
    
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if(diag_inriktning == TRUE){
    
    folkhogskola_inriktning <- folkhogskola_df %>% 
      group_by(Ar,Sun2000Inr_namn) %>% 
        summarize(antal=sum(antal)) %>%
          slice_max(antal,n=10) %>% 
            ungroup()
    
    # Returnerar data till R globala miljö
    if(returnera_data == TRUE){
      assign("folkhogskola_inriktning_df", folkhogskola_inriktning, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("De tio vanligaste YH-inriktningarna för förvärvsarbetande (16-74 år) i Dalarnas län  ",max(folkhogskola_df$Ar))
    diagram_titel <- str_wrap(diagram_titel)
    diagram_typ <- "yh_inriktning"
    diagramfil <- paste0(diagram_typ,".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    #folkhogskola_df_inriktning_urval$Sun2000Inr_namn <- str_wrap(folkhogskola_df_inriktning_urval$Sun2000Inr_namn,50)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = folkhogskola_inriktning %>% 
                                              mutate(Sun2000Inr_namn = str_wrap(Sun2000Inr_namn,50)),
                                            skickad_x_var = "Sun2000Inr_namn",
                                            skickad_y_var = "antal",
                                            manual_color = valda_farger,
                                            diagram_titel = diagram_titel,
                                            x_axis_sort_value = TRUE,
                                            diagram_capt = caption,
                                            x_axis_lutning = 0,
                                            diagram_liggande = TRUE,
                                            output_mapp = output_mapp_figur,
                                            filnamn_diagram = diagramfil,
                                            skriv_till_diagramfil = spara_figur)
    
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  names(gg_list) <- objektnamn
  if(returnera_figur == TRUE) return(gg_list)

}
