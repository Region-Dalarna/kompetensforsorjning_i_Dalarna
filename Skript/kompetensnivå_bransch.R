# Yrke kopplat till kompetensnivå.
# Data från yrkesregistret i SCB öppna databas (val 26 under arbetsmarknad, yrkesregistret med yrkesstatistik)
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       tidyverse,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_yrken_kompetens(skapa_fil = FALSE)

yrken_kompetens <-function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                          spara_data = TRUE,
                          filnamn = "kompetens_lan_bransch.xlsx"){

  # =============================================== API-uttag ===============================================
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208D/YREG56N"
  
  varlista <- list(
    Region = c(hamtaAllaLan(FALSE)),
    Yrke2012='*',
    SNI2007 = '*',
    Kon = c("1","2"),
    ContentsCode = c("000003T3"),
    Tid = max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  px_uttag <- pxweb_get(url = url,
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
  px_df$kompetensniva <- case_when(
    substr(px_df$yrkeskod,1,1)=="1" ~ "Chefsyrken",
    between(as.integer(substr(px_df$yrkeskod,1,1)),2,2) ~ "Motsvarande fördjupad högskolekompetens",
    between(as.integer(substr(px_df$yrkeskod,1,1)),3,3) ~ "Motsvarande högskolekompetens",
    between(as.integer(substr(px_df$yrkeskod,1,1)),4,8) ~ "Motsvarande gymnasial kompetens",
    substr(px_df$yrkeskod,1,1) =="0" & px_df$yrkeskod !="0002" ~ "Motsvarande gymnasial kompetens",
    px_df$yrkeskod =="0002" ~ "Okänt",
    substr(px_df$yrkeskod,1,1) =="9" ~ "Enklare yrken")
  
  if (spara_data==TRUE){
    openxlsx::write.xlsx(px_df,paste0(output_mapp,filnamn))
  }
}
