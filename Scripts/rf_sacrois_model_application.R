library(data.table)

SACROIS=data.table()



library(data.table)


setwd("C:/Users/cdestrem/Documents/Data/SACROIS")
SACROIS=data.table()
setwd("C:/Users/cdestrem/Documents/Data/SACROIS")

# Loop over the years 2007 to 2023

for (i in 2007:2023) {  
  
  # Set working directory for the specific year
  setwd(paste0('C:/Users/cdestrem/Documents/Data/SACROIS/', i))
  
  # Read the data file and bind to SACROIS
  SACROIS <- rbind(SACROIS, fread(paste0("NAVIRES-MOIS-MAREES-JOUR-IFR_", i, ".txt"), dec=","), fill=TRUE)
  
  # Remove vessels without id
  SACROIS <- SACROIS[NAVS_COD != ""]
  
  # Produce a unique ID per vessel and per year
  SACROIS[, NAVS_COD_YEAR := paste(NAVS_COD, substr(DATE_SEQ, 7, 10), sep = "_")]
  
  combined_select <- tapply(SACROIS[, SECT_COD_SACROIS_NIV3], SACROIS[, NAVS_COD_YEAR], 
                          function(x) any(x %in% c("27.7.e", "27.7.d")) |
                            any(SACROIS[NAVS_COD_YEAR == x, SECT_COD_SACROIS_NIV4] %in% c("27E3", "27E4", "26E4", "26E3", "25E3", "25E4")))
  
  SACROIS <- SACROIS[NAVS_COD_YEAR %in% combined_select]
  
  # Remove the extra column to be able to bind data table
  SACROIS[, NAVS_COD_YEAR := NULL]
}

# Produce a unique ID per vessel and per year again at the end
SACROIS[, NAVS_COD_YEAR := paste(NAVS_COD, substr(DATE_SEQ, 7, 10), sep = "_")]

# Print the resulting data table (optional)
print(SACROIS)







# Set working directory
setwd("~/Data/SACROIS")
# Load FPC data

FPC=data.table()

for (i in 2007:2023) {
  
  setwd(paste0("C:/Users/cdestrem/Documents/Data/SACROIS/",i));
  
  FPC<-rbind(fread(dir(getwd(), pattern="ISIH-504549-vueAnnuelleFpc"), dec=",", encoding="Latin-1", select= c("NAVS_COD", "DATE_REF", "FLOTTILLE_IFREMER_LIB", "S_FLOTTILLE_IFREMER_LIB", "DCR_FLOTTILLE_LIB", "DCR_S_FLOTTILLE_LIB", "DCR_S_S_FLOTTILLE_LIB", "NAVLC8_COD", "NAVP_LONGUEUR_HT","GRA_SYN_COD", "NAVP_PUISSANCE_AD")), FPC)
}



### Unique vessel ID based on year
FPC$NAVS_COD_YEAR=paste(FPC[,NAVS_COD], str_sub(FPC[,DATE_REF],7,10), sep="_")
FPC[,c("DATE_REF", "NAVS_COD"):=NULL]

setDT(FPC)



SACROIS <- SACROIS %>%
  left_join(FPC, by = c("NAVS_COD_YEAR" = "NAVS_COD_YEAR"))



setwd("~/Data")
Code_FAO <- fread("ISIH-504549-espece_fao-20230320105303.txt")

########################################################




library(data.table)
library(randomForest)

# Remove duplicated ID_CAPTURE rows (MESURES.csv file will add an ungodly amount of duplicated rows due to the nature of what is measured in the file (e.g., weight of capture, length of individual, comments, etc.), resulting in a lot of redundancy within the dataset)

#data <- BYC[!duplicated(ID_CAPTURE),] 

# Grabbing the necessary columns, most of these ended up not being used for analysis

data_sacrois <- SACROIS %>%
  select(MAREE_ID,DCR_FLOTTILLE_LIB,ESP_COD_FAO, MAILLAGE, NAVP_LONGUEUR_HT, TP_NAVIRE_SACROIS, QUANT_POIDS_VIF_SACROIS, NAVP_PUISSANCE_AD, SEQ_ID,TP_NAVIRE_MOYENNE)

# Grabbing total weight per marées

data_sacrois <- data_sacrois %>% 
  group_by(MAREE_ID) %>%
  mutate(Poids_tot_MAREE = sum(as.numeric(QUANT_POIDS_VIF_SACROIS),na.rm = T)) %>% 
  ungroup()


# Adjust here type of Flottille analysis will be conducted on 

pelagique <- data_sacrois %>% filter (DCR_FLOTTILLE_LIB == "Chalutiers pélagiques")



#pelagique_ptmbss <- pelagique_ptmbss %>% 
#  group_by(ID_MAREE) %>%
#  mutate(Poids_tot_MAREE = sum(as.numeric(POIDS_REF),na.rm = T)) %>% 
#  ungroup()

# Periodically check for NAs

colSums(is.na(pelagique))


pelagique$QUANT_POIDS_VIF_SACROIS <- as.numeric(pelagique$QUANT_POIDS_VIF_SACROIS)


# Add a bew column with absence (0) or presence (1) of bycatch, if NA then we assign a O 

#pelagique_ptmbss <- pelagique_ptmbss %>%
#  group_by(MAREE_ID) %>%
#  mutate(Bycatch = if_else(any(SPP_COD_FAO == "Delphinus delphis"), 1, 0, missing = 0))


# periodically check of NAs
colSums(is.na(pelagique))
#table(pelagique_ptmbss$Bycatch)


library(tidyr)

espf_parent_summary <- Code_FAO %>%
  filter(!is.na(ESPF_PARENT_COD), ESPF_PARENT_COD != "", !is.na(FAMILLE)) %>%  # Remove NA values
  group_by(FAMILLE) %>%
  summarise(
    ESPF_COD_list = list(unique(ESPF_PARENT_COD))  # Collect unique ESPF_CODs per FAMILY_COD_FAO
  ) %>%
  ungroup()

espf_summary <- Code_FAO %>%
  filter(!is.na(ESPF_COD), ESPF_PARENT_COD != "", !is.na(FAMILLE)) %>%  # Remove NA values
  group_by(FAMILLE) %>%
  summarise(
    ESPF_COD_list = list(unique(ESPF_COD))  # Collect unique ESPF_CODs per FAMILY_COD_FAO
  ) %>%
  ungroup()


espf_parent_summary <- espf_parent_summary %>%
  unnest(cols = ESPF_COD_list) %>%
  filter(!is.na(FAMILLE) & FAMILLE != "") %>%
  distinct(ESPF_COD_list, .keep_all = TRUE)

espf_summary <- espf_summary %>%
  unnest(cols = ESPF_COD_list) %>%
  filter(!is.na(FAMILLE) & FAMILLE != "") %>%
  distinct(ESPF_COD_list, .keep_all = TRUE)

combined_espf_summary <- bind_rows(espf_parent_summary, espf_summary) %>%
  group_by(FAMILLE) %>%
  summarise(
    ESPF_COD_list = list(unique(unlist(ESPF_COD_list)))
  ) %>%
  ungroup()


combined_espf_summary <- combined_espf_summary %>%
  unnest(cols = ESPF_COD_list) %>%
  filter(!is.na(FAMILLE) & FAMILLE != "") %>%
  distinct(ESPF_COD_list, .keep_all = TRUE)



pelagique_sacrois <- pelagique %>%
  left_join(combined_espf_summary, by = c("ESP_COD_FAO" = "ESPF_COD_list")) %>%
  mutate (across(where(is.integer), as.numeric))%>%
  filter(!is.na(FAMILLE) & FAMILLE != "")

Trachurus_spp <- c("JAA", "JJM" ,"TUD", "TUJ", "TUZ", "RSC", "PJM", "HMC", "HMG" ,"HMM", "HMZ" ,"HOM", "CJM", "JAX")
Scomber_spp <- c("MAC", "MAA", "MAS", "MAZ")
Dicentrarchus_spp <- c("BSS", "SPU", "BSE")
Anchois_spp <- c("ANA", "ANC", "ANE", "VET", "NPA", "JAN","ENR")
# Summary of species fished of interest

sp_p_w_sacrois <- pelagique_sacrois %>%
  filter(!is.na(ESP_COD_FAO)) %>%
  group_by(MAREE_ID) %>%
  reframe(
    Trachurus_weight = sum(if_else(ESP_COD_FAO %in%Trachurus_spp, QUANT_POIDS_VIF_SACROIS, 0), na.rm = T),
    Trachurus_p = Trachurus_weight / Poids_tot_MAREE,
    Gadidae_weight = sum(if_else(FAMILLE ==  "Gadidae", QUANT_POIDS_VIF_SACROIS, 0),na.rm = T),
    Gadidae_p = Gadidae_weight / Poids_tot_MAREE,
    Scomber_weight = sum(if_else(ESP_COD_FAO %in%Scomber_spp, QUANT_POIDS_VIF_SACROIS, 0),na.rm = T),
    Scomber_p = Scomber_weight / Poids_tot_MAREE,
    #Merluccius_weight = sum(if_else(ESP_COD_FAO == "HKE", QUANT_POIDS_VIF_SACROIS, 0),na.rm = T),
    #Merluccius_p = sum(if_else(ESP_COD_FAO == "HKE", QUANT_POIDS_VIF_SACROIS, 0),na.rm = T) / Poids_tot_MAREE,
    Dicentrarchus_weight = sum(if_else(ESP_COD_FAO %in%Dicentrarchus_spp, QUANT_POIDS_VIF_SACROIS, 0),na.rm = T),
    Dicentrarchus_p = Dicentrarchus_weight / Poids_tot_MAREE,
    Sprat_weight = sum(if_else(ESP_COD_FAO == "SPF", QUANT_POIDS_VIF_SACROIS, 0),na.rm = T),
    Sprat_p = Sprat_weight / Poids_tot_MAREE,
    Anchois_weight = sum(if_else(ESP_COD_FAO %in%Anchois_spp, QUANT_POIDS_VIF_SACROIS, 0),na.rm = T),
    Anchois_p = Anchois_weight / Poids_tot_MAREE,
    Sardine_weight = sum(if_else(ESP_COD_FAO == "PIL", QUANT_POIDS_VIF_SACROIS, 0),na.rm = T),
    Sardine_p = sum(if_else(ESP_COD_FAO == "PIL", QUANT_POIDS_VIF_SACROIS, 0),na.rm = T) / Poids_tot_MAREE,
    
    
  )%>%
  distinct(MAREE_ID, .keep_all = TRUE)



# Check NAs
colSums(is.na(pelagique))

# Assigning respective family to each sp 

library(tidyr)

# Summary of maree characteristics
# We join this table with the previous one



seq <- pelagique_sacrois%>%
  filter(!is.na(ESP_COD_FAO)) %>%
  filter(!duplicated(SEQ_ID)) %>%
  group_by(MAREE_ID) %>%
  summarise(av_mail = mean(as.numeric(MAILLAGE),na.rm = T),
            av_peche = mean(as.numeric(TP_NAVIRE_SACROIS),  na.rm = T) / 24) 

pelagique_sacrois <- pelagique_sacrois %>%
  left_join(seq, by = "MAREE_ID")


maree_summary_sacrois <- pelagique_sacrois %>%
  filter(!is.na(ESP_COD_FAO)) %>%
  group_by(MAREE_ID) %>%
  summarise(n_especes = n_distinct(ESP_COD_FAO),
            av_mail = mean(av_mail,na.rm = T),
            av_peche = mean(av_peche,  na.rm = T), 
            Longueur = unique(NAVP_LONGUEUR_HT) / 100,
            av_puissance = unique(NAVP_PUISSANCE_AD),
            poids_tot = round(sum(as.numeric(Poids_tot_MAREE), na.rm = TRUE)),
            #first_sp_fished = names(sort(tapply(POIDS_REF, ESPF_COD, sum, na.rm = TRUE), decreasing = TRUE))[1],
            poids_total_sp_plus_peche = max(tapply(QUANT_POIDS_VIF_SACROIS, ESP_COD_FAO, sum, na.rm = TRUE), na.rm = TRUE),
            #stat_rectangle = names(sort(tapply(POIDS_REF, RECTANGLE, sum, na.rm = TRUE), decreasing = TRUE))[1],
            #month = names(sort(tapply(POIDS_REF, Month, sum, na.rm = TRUE), decreasing = TRUE))[1],
            fam_plus_peche = names(sort(tapply(QUANT_POIDS_VIF_SACROIS, FAMILLE, sum, na.rm = TRUE), decreasing = TRUE))[1],
            #total_w_fam_fished = max(tapply(POIDS_REF, FAMILY_COD_FAO, sum, na.rm = TRUE), na.rm = TRUE),
            #Months = paste(Month, collapse = ", "),
            #Etat_mer = paste(ETAT_MER, collapse = ", "),
            #av_prof_fond = round(mean(as.numeric(PROF_FOND, na.rm = T))),
            #av_prof_engin = round(mean(as.numeric(PROF_ENGIN, na.rm = T))),
  ) %>%
  ungroup() %>%
  left_join(sp_p_w_sacrois, by = "MAREE_ID")


# Put qualitative variables as.factor() for analysis 
maree_summary_sacrois$n_especes <- as.numeric(maree_summary_sacrois$n_especes)
#maree_summary$first_fam_fished <- as.factor(maree_summary$first_fam_fished)



# Check NAs 
#sum(is.na(maree_summary$Bycatch[maree_summary$Bycatch == 1]))

# Get rid of unecessary data
maree_summary_sacrois <- maree_summary_sacrois %>% select (-MAREE_ID, -Trachurus_weight, -Gadidae_weight, -Scomber_weight, -Dicentrarchus_weight, -Sprat_weight, -Anchois_weight, -Sardine_weight)


# Check NAs
colSums(is.na(maree_summary_sacrois))
maree_summary_sacrois <- na.omit(maree_summary_sacrois)
colSums(is.na(maree_summary_sacrois))

# Put whatever is not in correct format in the correct format

maree_summary_sacrois$n_especes <- as.numeric(maree_summary_sacrois$n_especes)
maree_summary_sacrois$Longueur <- as.numeric(maree_summary_sacrois$Longueur)
#maree_summary$sp_plus_peche <- as.factor(maree_summary$sp_plus_peche)
maree_summary_sacrois$fam_plus_peche <- as.factor(maree_summary_sacrois$fam_plus_peche)

# One last check for NA for good measure

colSums(is.na(maree_summary_sacrois))

#maree_summary_sacrois$Bycatch <- NA  # Or any default value
#maree_summary_sacrois$Bycatch <- as.factor(maree_summary_sacrois$Bycatch)
maree_summary$fam_plus_peche <- as.character(maree_summary$fam_plus_peche)
maree_summary_sacrois$fam_plus_peche <- as.character(maree_summary_sacrois$fam_plus_peche)


##############################################################

# RandomForest (classification)

bycatch_events <- predict(rf_byc, newdata = maree_summary_sacrois)

maree_summary_sacrois$bycatch_prediction <- bycatch_events
# 246 events predicted


# RandomForest (regression)
## Must re-run the maree_summary_sacrois chunk before running the RF

bycatch_numbers <- predict(rf_byc_nb, newdata = maree_summary_sacrois)

maree_summary_sacrois$bycatch_nb_predictions <- bycatch_numbers


sum(maree_summary_sacrois$bycatch_nb_predictions)
#2018.994 dolphins

###################################################################

# Scaled up method 

sum(maree_summary_sacrois$av_peche)

# 1173.414 days

# Total fishing effort (in days) of ObsMer is equal to 34.40146

BPUE <- (69/34.40146) * 1173.414

# 2353.55 dolphins captured by pelagic trawlers between 2007 and 2023 

# which gives us about 147ind/year 




