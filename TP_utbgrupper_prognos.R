# Skapar en figur med prognoser för utbud och efterfrågan inom olika utbildningsgrupper och utbildningsnivåer.
# Från trender och prognoser. Data mm. i "G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/
pacman::p_load(tidyverse,openxlsx,here)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_TP_Prognos(skapa_fil = FALSE)

diag_TP_Prognos <-function(region_vekt = "20", 
                           output_mapp = "G:/skript/jon/Slask/",
                           skapa_fil = TRUE,
                           valda_utbildningar = c("53B","53R","55BG","55H-L"),
                           diag_prognos_2035=TRUE,
                           diag_prognos_fleraar=TRUE,
                           diag_prognos_balans_utbgrupp=TRUE,
                           diag_prognos_fleraar_utbgrupp=TRUE){
  
  # ========================================== Inställningar ============================================
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  diagram_capt_1 <- "Källa: Regionala utbildnings och arbetsmarknadsprognoser, SCB\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring. Tillgång delat med efterfrågan. En siffra över 100 indikerar att tillgången överstiger efterfrågan."
  diagram_capt_2 <- "Källa: Regionala utbildnings och arbetsmarknadsprognoser, SCB\nBearbetning: Samhällsanalys, Region Dalarna."                  
  
  vald_region=hamtaregion_kod_namn(region_vekt)[[2]]
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i<-1 # Räknare som används för att lägga till objekt i listan
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  
  # =====================================================================================================
  # Läser in data från Excel (ursprung Trender och prognoser)
  # Utbildningsgrupper
  prognos_df <- read.csv2("G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/02_Tabeller/CSV_filer/Tab0_20.csv",sep='\t',encoding="latin1")
  
  prognos_df_utbniva <- read.csv2("G:/Samhällsanalys/Projekt och uppdrag/Kompetensförsörjning/Trender och prognoser/02_Tabeller/CSV_filer/Tab0A_20.csv",sep='\t')
  
  # Gör om årtalen till en character då dessa är lättare att arbeta med
  prognos_df$Ar <- as.character(prognos_df$Ar)
  prognos_df_utbniva$Ar <- as.character(prognos_df_utbniva$Ar)
  
  #====================================================================================
  # Data till figur där tillgång och efterfrågan jämförs (för en specifik bransch) 2035
  #===================================================================================
  
  # För specifika utbildningar kopplat till en bransch. 
  prognos_df_2035 <- prognos_df %>% 
    filter(Ar=="2035",sun2000grp_reg_alt%in%valda_utbildningar) %>% 
      mutate("balans"=(Till_Dag_Prog/Eftr_Prog)*100)
  
  #===========================================================================
  # Data till figurer där tillgång och efterfrågan jämförs 2018, 2025 och 2035
  #===========================================================================
  # Väljer år och tar bort den grupp där utbildning saknas. Används för att beräkna tillgång delat på efterfrågan för utbildningsgrupper
  prognos_df_utbniva_balans <- prognos_df_utbniva %>% 
    filter(Ar%in%c("2018","2025","2035"),Aggregat!="Saknar uppgift om utbildning samt ospecificerad utbildning")
  
  # Branschspecifikt dataset (motsvarar prognos_df_2035 men med flera år)
  prognos_df_fleraar <- prognos_df %>% 
    filter(Ar%in%c("2018","2025","2035"),sun2000grp_reg_alt%in%valda_utbildningar)
  
  # Motsvarande som ovan men för utbildningsgrupper istället för utbildning
  prognos_df_utbniva_fleraar <- prognos_df_utbniva %>% 
    filter(Ar%in%c("2018","2025","2035"),Aggregat!="Saknar uppgift om utbildning samt ospecificerad utbildning")
  
  # Lägger ihop data och prognos i samma kolumn för tillgång respektive efterfrågan
  # Tillgång
  prognos_df_fleraar$tillgång<-ifelse(is.na(prognos_df_fleraar$Till_Dag_Prog),prognos_df_fleraar$Till_Dag,prognos_df_fleraar$Till_Dag_Prog)
  # Efterfrågan
  prognos_df_fleraar$efterfrågan<-ifelse(is.na(prognos_df_fleraar$Eftr_Prog),prognos_df_fleraar$Eftr,prognos_df_fleraar$Eftr_Prog)
  # Tillgång
  prognos_df_utbniva_fleraar$tillgång<-ifelse(is.na(prognos_df_utbniva_fleraar$Till_Dag_Prog),prognos_df_utbniva_fleraar$Till_Dag,prognos_df_utbniva_fleraar$Till_Dag_Prog)
  # Efterfrågan
  prognos_df_utbniva_fleraar$efterfrågan<-ifelse(is.na(prognos_df_utbniva_fleraar$Eftr_Prog),prognos_df_utbniva_fleraar$Eftr,prognos_df_utbniva_fleraar$Eftr_Prog)
  # Tillgång
  prognos_df_utbniva_balans$tillgång<-ifelse(is.na(prognos_df_utbniva_balans$Till_Dag_Prog),prognos_df_utbniva_balans$Till_Dag,prognos_df_utbniva_balans$Till_Dag_Prog)
  # Efterfrågan
  prognos_df_utbniva_balans$efterfrågan<-ifelse(is.na(prognos_df_utbniva_balans$Eftr_Prog),prognos_df_utbniva_balans$Eftr,prognos_df_utbniva_balans$Eftr_Prog)
  
  # För utbildningsnivå beräknas tillgång genom efterfrågan för 18, 25 och 35. 
  prognos_df_utbniva_balans<-prognos_df_utbniva_balans %>% 
    mutate("balans"=(tillgång/efterfrågan)*100)
  
  # Gör om år till en faktorvariabel. Detta för att kunna bestämma vilken ordning staplarna för de olika åren kommer i diagrammet
  prognos_df_utbniva_balans$Ar <- factor(prognos_df_utbniva_balans$Ar, levels = c("2035","2025","2018"))
  
  # Väljer ut de variabler vi är intresserade av i dataset prognos_df_fleraar (utbildningar)
  prognos_df_fleraar <- prognos_df_fleraar %>% 
    select(kortnamn_modell_alt,Ar,tillgång,efterfrågan)
  # Pivoterar data för ovanstående dataset
  prognos_df_fleraar_longer <- pivot_longer(prognos_df_fleraar,cols=3:4)
  
  # Gör motsvarande för utbildningsgrupper
  prognos_df_utbniva_fleraar <- prognos_df_utbniva_fleraar %>% 
    select(Aggregat,Ar,tillgång,efterfrågan)
  # Pivoterar data
  prognos_df_utbniva_fleraar_longer <- pivot_longer(prognos_df_utbniva_fleraar,cols=3:4)
  
  # Skapar ett diagram som jämför efterfrågan och utbud 2035 för en utvald bransch (exemplifierat med bransch)
  if(diag_prognos_2035==TRUE){
    diagram_titel <- paste0("Tillgång delat på efterfrågan inom bygg i Dalarnas län 2035 per utbildning (16-74 år)")
    diagram_typ <- "balans_tillg_eft_prognos"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    # Skapar en svart linje vid 100 (dvs. tillgång=efterfrågan)
    rektangel=list(geom = "rect", ymin=99.9, ymax=101.1, xmin=0, xmax=Inf, alpha=1, fill="grey20")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = prognos_df_2035, 
                                 skickad_x_var = "kortnamn_modell_alt", 
                                 skickad_y_var = "balans",
                                 #skickad_x_grupp = "Ar",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[6],
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt_1,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 #manual_y_axis_title = "procent",
                                 geom_position_stack = TRUE,
                                 fokusera_varden=rektangel,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Gör om år till en faktorvariabel för utbildningsnivå_balans. Detta för att kunna bestämma vilken ordning staplarna för de olika åren kommer i diagrammet
  #prognos_df_utbniva_balans$Ar <- factor(prognos_df_utbniva_balans$Ar, levels = c("2035","2025","2018"))
  
  # Gör om utbildningsgrupper till en faktorvariabel för utbildningsnivå_balans. Detta för att kunna bestämma vilken ordning staplarna för de olika åren kommer i diagrammet
  prognos_df_utbniva_balans$Aggregat <- factor(prognos_df_utbniva_balans$Aggregat, levels = c("Folk– och grundskoleutbildning","Gymnasieutbildning – högskoleförb. (inkl. teknisk utb.)","Gymnasieutbildning – yrkesförberedande","Eftergymnasialt utbildade – utan examen","Eftergymnasialt utbildade – med examen"))
  
  # Skapar ett diagram som visar kvonten mellan utbud och efterfrågan för utbildningsgrupper (tre utvalda år)
  if(diag_prognos_balans_utbgrupp==TRUE){
    diagram_titel <- paste0("Tillgång delat på efterfrågan i Dalarnas län efter utbildningsgrupp (16-74 år)")
    diagram_typ <- "balans_tillg_eft_utbniva_prognos"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    # Skapar ett svart streck vid 100
    rektangel=list(geom = "rect", ymin=99.9, ymax=101.1, xmin=0, xmax=Inf, alpha=1, fill="grey20")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = prognos_df_utbniva_balans, 
                                 skickad_x_var = "Aggregat", 
                                 skickad_y_var = "balans",
                                 skickad_x_grupp = "Ar",
                                 manual_x_axis_text_vjust=0,
                                 manual_x_axis_text_hjust=0.6,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 diagram_titel = diagram_titel,
                                 #x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt_1,
                                 #procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 #x_axis_sort_value = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = TRUE,
                                 #manual_y_axis_title = "procent",
                                 geom_position_stack = FALSE,
                                 fokusera_varden=rektangel,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  
  # Skapar ett facet-diagram som jämför efterfrågan och utbud 2018,2025 och 2035 (ej kvot) - branschspecifikt
  if(diag_prognos_fleraar==TRUE){
    diagram_titel <- paste0("Tillgång och efterfrågan inom bygg i Dalarnas län (16-74 år)")
    diagram_typ <- "tillg_eft_prognos"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = prognos_df_fleraar_longer, 
                                 skickad_x_var = "Ar", 
                                 skickad_y_var = "value",
                                 skickad_x_grupp = "name",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex")[5:6],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = FALSE,
                                 diagram_capt = diagram_capt_2,
                                 #diagram_liggande = TRUE,
                                 legend_vand_ordning = FALSE,
                                 diagram_facet = TRUE,
                                 facet_legend_bottom = TRUE,
                                 facet_grp = "kortnamn_modell_alt",
                                 facet_scale = "free",
                                 #geom_position_stack = TRUE,
                                 dataetiketter = FALSE,
                                 manual_y_axis_title = "Antal",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  # En av rubrikerna i diagrammet blir för lång. Kör därför en stringwrap på den
  prognos_df_utbniva_fleraar_longer[prognos_df_utbniva_fleraar_longer=="Gymnasieutbildning – högskoleförb. (inkl. teknisk utb.)"]<-str_wrap("Gymnasieutbildning – högskoleförb. (inkl. teknisk utb.)",40)
  
  # Gör om utbildningsgrupper till en faktorvariabel för utbildningsnivå_balans. Detta för att kunna bestämma vilken ordning staplarna för de olika åren kommer i diagrammet
  prognos_df_utbniva_fleraar_longer$Aggregat <- factor(prognos_df_utbniva_fleraar_longer$Aggregat, levels = c("Folk– och grundskoleutbildning",str_wrap("Gymnasieutbildning – högskoleförb. (inkl. teknisk utb.)",40),"Gymnasieutbildning – yrkesförberedande","Eftergymnasialt utbildade – utan examen","Eftergymnasialt utbildade – med examen")[5:1])
  
  # Skapar ett facet-diagram som jämför efterfrågan och utbud 2018,2025 och 2035
  if(diag_prognos_fleraar_utbgrupp==TRUE){
    diagram_titel <- paste0("Tillgång och efterfrågan i Dalarnas län efter utbildningsgrupp (16-74 år)")
    diagram_typ <- "tillg_eft_prognos_utbniva"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaStapelDiagram(skickad_df = prognos_df_utbniva_fleraar_longer, 
                                 skickad_x_var = "Ar", 
                                 skickad_y_var = "value",
                                 skickad_x_grupp = "name",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("gron_sex")[4:6],
                                 diagram_titel = diagram_titel,
                                 x_axis_sort_value = FALSE,
                                 diagram_capt = diagram_capt_2,
                                 #diagram_liggande = TRUE,
                                 legend_vand_ordning = FALSE,
                                 diagram_facet = TRUE,
                                 facet_legend_bottom = TRUE,
                                 facet_grp = "Aggregat",
                                 facet_scale = "free",
                                 #geom_position_stack = TRUE,
                                 dataetiketter = FALSE,
                                 manual_y_axis_title = "Antal",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  names(gg_list)<-c(objektnamn)
  return(gg_list)
}
