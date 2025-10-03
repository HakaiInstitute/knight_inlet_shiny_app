# -------------------------------------------------------------------------

# INITIALIZE DATA FOR SHINY
# CREATE FULL SPECIES LIST
# SPECIES LIST AT EACH SITE-DEPTH

# -------------------------------------------------------------------------

#### load libraries ####
library(tidyverse)
library(reshape2)
library(stringr)
library(ape)
library(phyloseq)
library(data.table)
library(viridis)
library(qualpalr)
library(ggplot2)
library(ranacapa)
library(vegan)
library(ggVennDiagram)
library(microViz)
library(eulerr)
library(taxize)
library(tibble)
library(stringr)

# Save all cleaned datasets as RDS
# setwd("~/Hakai/hakai-knight/knight_inlet_shiny_app/data")

#site_coords <- read_csv("site-coords.csv")
#### 2023 data -----------------------------------------------------------------

## load the meta data
metadata_2023 <- read_excel("2023/hoeya_metadata_2023.xlsx")
metadata_2023$date <- as.Date(metadata_2023$date)
metadata_2023$year <- as.numeric(format(metadata_2023$date, "%Y"))

site_coords <- select(metadata_2023, c(year,site,survey,area,5,6,10,12,16)) 
colnames(site_coords)[5] <- "lat"
colnames(site_coords)[6] <- "long"

## load fill list
hoeya_dive_2023_fill_list <- read_csv("2023/hoeya_dive_2023 - fill_list.csv")
hoeya_dive_2023_fill_list <- hoeya_dive_2023_fill_list %>%
  rename(species_scientific = taxon) %>%  
  rename(corrected_comnam = common_name) %>%
  mutate(corrected_comnam = str_to_title(corrected_comnam)) %>% # capitalize every word in common name
  mutate(species_scientific = sprintf("<i>%s</i>", species_scientific)) %>% # surround scientific names with italic symbol
  mutate(group = case_when(
    species_scientific == "<i>Pandalas platyceros</i>" ~ "ARTHROPODA",
    species_scientific == "<i>Hydrolagus colliei</i>" ~ "FISH",
    species_scientific == "<i>hydroid spp.</i>" ~ "CNIDARIA",
    species_scientific == "<i>Eumastia sitiens</i>" ~ "PORIFERA",
    species_scientific == "<i>Cribrinopsis fernaldi</i>" ~ "CNIDARIA",
    species_scientific == "<i>barnacle spp.</i>" ~ "ARTHROPODA",
    TRUE ~ group)) %>%
  #filter(!is.na(group)) %>%
  unique() #%>%
  #mutate(species = str_c(species_scientific.y, corrected_comnam, sep = " - ")) #%>%
  #select(c(1,2))
  

## load species data
hoeya_dive_2023 <- read_excel("2023/hoeya_dive_2023.xlsx", 
                              sheet = "biodiversity")

# remove extra rows brought in from google sheet 
hoeya_dive_2023 <- hoeya_dive_2023 %>%
  filter(!grepl("#N/A", species_scientific, ignore.case = TRUE)) %>%  # remove all rows that contain the string #N/A 
  filter(!grepl("na", species_common, ignore.case = TRUE)) %>%
  filter(!is.na(date))  # remove all NAs in the date column to ensure we only remove rows with no data

# extract rows scinam and comnam rows: this shows us which entries need to be combined (redundant names) and which need to be changed (syntax error/non-standardized name)
hoeya_dive_2023_sci_com <- hoeya_dive_2023 %>%
  select(c(4,5)) %>%
  unique() %>%
  mutate(corrected_comnam = species_common)

# fix common and scientific names
hoeya_dive_2023_fixed_names <- hoeya_dive_2023_sci_com %>%
  mutate(corrected_comnam = case_when(
    species_common == "California lamp shell" ~ "California lampshell",
    species_common == "crimson anemone" ~ "crimson anenome",
    species_common == "crustone corallines" ~ "crustose corallines",
    species_common == "cucmaria" ~ "cucumaria",
    species_common == "green urchins" ~ "green sea urchin",
    species_common == "green urchin" ~ "green sea urchin",
    species_common == "hildenbrandia" ~ "rusty rock",
    species_common == "hydroid spp" ~ "hydroid sp.",
    species_common == "lampshells" ~ "lampshell",
    species_common == "orange crust bryozoan" ~ "orange encrusting bryozoan",
    species_common == "rat fish" ~ "spotted ratfish",
    species_common == "solaster spp" ~ "solaster sp.",
    species_common == "stick bryo" ~ "stick bryozoan",
    species_common == "stylaster" ~ "stylaster sp.",
    species_common == "tiny grey mussel" ~ "pacific blue mussel",
    species_common == "vieled chiton" ~ "veiled chiton",
    species_common == "california cucumber" ~ "california sea cucumber",
    species_common == "ceratostoma" ~ "leafy hornmouth",
    species_common == "brachiopods" ~ "lampshell",
    species_common == "sea perch" ~ "sea peach",
    species_common == "plumose anenome" ~ "giant plumose anemone",
    TRUE ~ corrected_comnam)) %>%
  mutate(species_scientific = case_when(
    corrected_comnam == "California lampshell" ~ "Laqueus californicus",
    corrected_comnam == "creeping pedal sea cucumber" ~ "Psolus chitonoides",
    corrected_comnam == "crimson anenome" ~ "Cribrinopsis fernaldi",
    corrected_comnam == "cucumaria" ~ "Cucumaria sp.",
    corrected_comnam == "dusky rockfish" ~ "Sebastes ciliatus",
    corrected_comnam == "rusty rock" ~ "Hildenbrandia sp.",
    corrected_comnam == "heart crab" ~ "Phyllolithodes papillosus",
    corrected_comnam == "Amphisbetia sp." ~ "hydroid sp.",
    corrected_comnam == "painted anenome" ~ "Urticina crassicornis",
    corrected_comnam == "puget sound king crab" ~ "Lopholithodes mandtii",
    corrected_comnam == "spotted ratfish" ~ "Hydrolagus colliei",
    corrected_comnam == "solaster sp." ~ "Solaster sp.",
    corrected_comnam == "stylaster sp." ~ "Stylaster sp.",
    corrected_comnam == "tiny grey mussel" ~ "Mytilus trossulus",
    corrected_comnam == "veiled chiton" ~ "Placiphorella velata",
    corrected_comnam == "lampshell" ~ "Laqueus sp.",
    corrected_comnam == "calcareous tube worm" ~ "Serpula columbiana",
    corrected_comnam == "california sea cucumber" ~ "Parastichopus californicus",
    corrected_comnam == "leafy hornmouth" ~ "Ceratostoma foliatum",
    corrected_comnam == "dungeness crab" ~ "Metacarcinus magister",
    corrected_comnam == "pacific blue mussel" ~ "Mytilus trossulus",
    corrected_comnam == "sea peach" ~ "Halocynthia aurantium",
    corrected_comnam == "green sea urchin" ~ "Strongylocentrotus droebachiensis",
    corrected_comnam == "crustose corallines" ~ "Clathromorphum sp.",
    corrected_comnam == "giant plumose anemone" ~ "Metridium farcimen",
    corrected_comnam == "stick bryozoan" ~ "Microporina borealis",
    corrected_comnam == "stylaster coral" ~ "Stylaster verrillii",
    corrected_comnam == "orange encrusting bryozoan" ~ "Schizoporella japonica",
    TRUE ~ species_scientific)) %>%
  mutate(corrected_comnam = case_when(
    species_scientific == "Alaria marginata" ~ "broad winged-kelp",
    TRUE ~ corrected_comnam)) %>%
  mutate(species_scientific = gsub("spp.", "sp.", species_scientific)) %>% # fix sp names
  mutate(corrected_comnam = str_to_title(corrected_comnam)) %>% # capitalize every word in common name
  mutate(species_scientific = sprintf("<i>%s</i>", species_scientific)) %>%  # surround scientific names with italic symbol
  left_join(hoeya_dive_2023_fill_list) %>%
  unique() %>%
  filter(species_scientific != '<i>NA</i>')
  
  
# just keep common and fixed common columns
hoeya_dive_2023_comnam_fixed <- select(hoeya_dive_2023_fixed_names, c(2,3)) %>%
  unique()

# just keep fixed common and scientific name
hoeya_dive_2023_scinam_fixed <- select(hoeya_dive_2023_fixed_names, c(3,1)) %>%
  unique()


# make date a date
hoeya_dive_2023_clean$date <- as.Date(hoeya_dive_2023_clean$date)

# incorporate hoeya_dive_2023_fixed_names into the full dataset
hoeya_dive_2023_clean <- hoeya_dive_2023 %>%
  left_join(hoeya_dive_2023_comnam_fixed, by = "species_common") %>%
  left_join(hoeya_dive_2023_scinam_fixed, by = "corrected_comnam") %>%
  mutate(species = str_c(corrected_comnam, species_scientific.y, sep = " - ")) %>%
  select(-c(4,5,8:12)) %>%
  filter(!is.na(species))

# add a year column 
hoeya_dive_2023_clean$year <- as.numeric(format(hoeya_dive_2023_clean$date, "%Y"))


#### create species_list
species_list <- unique(hoeya_dive_2023_clean$species) 
species_list <- sort(species_list)

saveRDS(species_list, file = "knight-species-list.rds")



#### 2021 data -----------------------------------------------------------------





#### create large list object for shiny ####
# copying `calvert-site-spp-tables`: sites are of type "list" which include, species, group and numerous trials

# Splitting the dataframe into a list by site_id
sorted_hoeya_dive_2023_clean <- hoeya_dive_2023_clean %>%
  arrange(species)

`knight-site-spp-tables` <- split(sorted_hoeya_dive_2023_clean$species, sorted_hoeya_dive_2023_clean$site)  # may need to change combine site and year to create site_id when other years are incorporated
saveRDS(`knight-site-spp-tables`, file = "knight-site-spp-tables.rds")

#### species richness ####

# remove non species columns
trimmed_hoeya_dive_2023_clean <- dplyr::select(hoeya_dive_2023_clean, site, species)

# summarize data to get species occurrence
hoeya_dive_2023_oc <- trimmed_hoeya_dive_2023_clean %>%
  group_by(site,species) %>%
  summarise(abundance = n(), .groups = "drop")

# pivot wider
hoeya_dive_2023_matrix <- hoeya_dive_2023_oc %>% 
  pivot_wider(names_from = species, values_from = abundance, values_fill = list(abundance = 0)) %>%
  na.omit()

hoeya_dive_2023_matrix <- as.data.frame(hoeya_dive_2023_matrix)

# finalize species matrix
rownames(hoeya_dive_2023_matrix) <- hoeya_dive_2023_matrix$site
hoeya_dive_2023_matrix_final <- hoeya_dive_2023_matrix[, -1]

# get species richness
sppr <- specnumber(hoeya_dive_2023_matrix_final)

sppr_df <- sppr %>% 
  enframe() %>%
  rename(site = name) %>%
  rename(sppr = value)

site_coords <- left_join(site_coords, sppr_df)

# final formatting of site_coords
site_coords <- site_coords %>%
  mutate(survey = str_to_title(survey)) %>% # capitalize every word in common name
  mutate(area = str_to_title(area)) %>%
  mutate(objective = str_to_title(objective)) %>%
  mutate(survey = case_when(
    survey == "Rov" ~ "ROV",
    TRUE ~ survey))

# KNI26 and KNI28 overlap, so stagger them
site_coords$long[13] <- "-125.9990"  # changed from -125.9989
site_coords$long <- as.numeric(site_coords$long)

write.csv(site_coords, "C:/Users/alex.schmill/Documents/knight inlet eDNA/shinyapp/App-1/data/site-coords.csv")


