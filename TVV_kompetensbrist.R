# Data från Tillväxtverket för företag som upplever problem att rekrytera: 
# https://tillvaxtverket.se/statistik/foretagande/hinder-for-tillvaxt.html
pacman::p_load(tidyverse,openxlsx,here)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_lista <-diag_kompetensbrist(skapa_fil = FALSE)

diag_kompetensbrist <-function(region_vekt = "20", 
                               output_mapp = "G:/skript/jon/Slask/",
                               skapa_fil=TRUE){
  
  # ============================ Inställningar ====================================
  output_mapp <- output_mapp
  #skriv_diagramfiler <- FALSE             # om TRUE skrivs filer i output-mapp - SÄTTER MAN DEN TILL FALSE FÅR MAN ETT MÄRKLIGT FELMEDDELANDE NÄR MAN FÖRSÖKER SKRIVA UT PPT
  skapa_fil=skapa_fil 
  diagram_capt <- "Källa: Tillväxtverket: Företagens villkor och verklighet.\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel småföretag som anser att tillgång till lämplig arbetskraft är ett stort hinder för tillväxt"
  
  # Skapar en vektor för objektnamn, en lista som skall hålla ggplot-objekt och en räknare för dessa objekt
  objektnamn <-c()
  gg_list <-list()
  i=1
  # ========================== Läs in data ======================================
  
  # Läs in datafil med högskoleexamen
  kompetensbrist_df <- read.xlsx(here("Indata","/","Tillgång till lämplig arbetskraft.xlsx"),sheet=8)
  
  # Snyggar till data lite. Fokuserar enbart på det senaste året, 2020
  kompetensbrist_df_utskrift <- kompetensbrist_df %>% 
    select(Län,"2020") %>% 
      rename("Andel"="2020") %>% 
        mutate(andel_procent=round(Andel*100,2)) %>% 
          select(Län,andel_procent)
  
  # Tar bort län i länsnamnen
  kompetensbrist_df_utskrift$Län <- skapa_kortnamn_lan(kompetensbrist_df_utskrift$Län)
  
  # Byter från totalt till Riket
  kompetensbrist_df_utskrift[kompetensbrist_df_utskrift=="Totalt"]<-"Riket"
  
  kompetensbrist_df_utskrift_fokus <- kompetensbrist_df_utskrift
  # skapa fokusvariabel för att fokusera på valt län och riket
  kompetensbrist_df_utskrift_fokus$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  kompetensbrist_df_utskrift_fokus$fokus <- 0
  kompetensbrist_df_utskrift_fokus$fokus[kompetensbrist_df_utskrift_fokus$Län =="Riket"] <- 2
  
  diagram_titel <- paste0("Upplevd kompetensbrist i Sveriges regioner ","2020")
  diagram_typ <- "kompetensbrist"
  diagramfil <- paste0(diagram_typ, ".png")
  objektnamn <- c(objektnamn,diagram_typ)
  
  gg_obj <- SkapaStapelDiagram(skickad_df = kompetensbrist_df_utskrift_fokus,
                               skickad_x_var = "Län", 
                               skickad_y_var = "andel_procent",
                               manual_x_axis_text_vjust=0,
                               manual_x_axis_text_hjust=0.4,
                               manual_color = diagramfarger("gron_tva_fokus"),
                               diagram_titel = diagram_titel,
                               x_axis_sort_value = TRUE,
                               diagram_capt = diagram_capt,
                               #procent_0_100_10intervaller = TRUE,
                               x_axis_lutning = 0,
                               #legend_vand_ordning = TRUE,
                               diagram_liggande = TRUE,
                               manual_y_axis_title = "procent",
                               #manual_x_axis_title = "Examensår",
                               #geom_position_stack = TRUE,
                               berakna_index = FALSE,
                               x_var_fokus = "fokus",
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfil,
                               skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  i=i+1 
  
  names(gg_list) <- c(objektnamn)
  return(gg_list)
}
