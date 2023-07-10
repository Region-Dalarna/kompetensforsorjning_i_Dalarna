##### Hämtar data för att beräkna pendling #######
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,rKolada)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_pendling(skapa_fil = FALSE,region_vekt ="20")

hamta_data_pendling_kommun <-function(region_vekt = "20", 
                                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                      spara_data = TRUE,
                                      filnamn = "pendling_kommun.xlsx"){

  # =============================================== API-uttag ===============================================
  
  #### Dra hem variablerna från Kolada
  Indendling_df <- get_values(
    kpi = c("N00968"),
    municipality = c("0000",hamtakommuner(region_vekt)),
    period = 1900:2100
  )
  
  #### Dra hem variablerna från Kolada
  Utpendling_df <- get_values(
    kpi = c("N00920"),
    municipality = c("0000",hamtakommuner(region_vekt)),
    period = 1900:2100
  )
  
  # Inpendling
  # Väljer ut relevanta variabler och döper om value
  
  Indendling_df <-Indendling_df %>% 
    select(year,gender,value,municipality) %>% 
    rename("Pendling_andel"=value)
  
  # Gör om år till en character
  Indendling_df$year<-as.character(Indendling_df$year)
  
  # Lägger till en variabel som markerar om det är in eller utpendling
  Indendling_df$pendling_typ<-"Andel inpendling"
  
  # Utpendling
  
  Utpendling_df <-Utpendling_df %>% 
    select(year,gender,value,municipality) %>%
    rename("Pendling_andel"=value)
  
  # Gör om år till en character
  Utpendling_df$year<-as.character(Utpendling_df$year)
  
  # Lägger till en variabel som markerar om det är in eller utpendling
  Utpendling_df$pendling_typ<-"Andel utpendling"
  
  # Slår ihop de två
  pendling_df<-rbind(Indendling_df,Utpendling_df)
  
  if (spara_data==TRUE){
    openxlsx::write.xlsx(pendling_df,paste0(output_mapp,filnamn))
  }
  
  # 
  # if(diag_pendling_andel==TRUE){
  #   diagram_titel <- paste0("Andel av förvärvsarbetande som pendlar över kommungräns år ",max(pendling_df$year))
  #   diagram_titel<-str_wrap(diagram_titel,50)
  #   diagram_typ <- "andel_pendling"
  #   diagramfil <- "andel_pendling_kommun.png"
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df = pendling_df %>% 
  #                                  filter(year%in%c(max(pendling_df$year))) %>% 
  #                                  filter(gender=="T") %>% 
  #                                  mutate(municipality=ifelse(municipality=="Riket", "Sverige",municipality)),
  #                                skickad_x_var = "municipality", 
  #                                skickad_y_var = "Pendling_andel", 
  #                                skickad_x_grupp = "pendling_typ",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=1,
  #                                manual_color = diagramfarger("rus_sex"),
  #                                x_axis_sort_value = TRUE,
  #                                x_axis_sort_grp = 2,
  #                                vand_sortering=TRUE,
  #                                dataetiketter = FALSE,
  #                                dataetiketter_antal_dec = 0,
  #                                manual_y_axis_title="procent",
  #                                diagram_titel = diagram_titel,
  #                                diagram_capt = diagram_capt,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfil,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # 
  # 
  # names(gg_list) <-c(objektnamn)
  # return(gg_list)
  
}

