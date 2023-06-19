library(readxl)
library(tidyverse)
library(wbstats)
library(WDI)
library(dplyr)
library(sf)
library(raster)

overfish.raw <- read_excel("../input/pecentages1974_2019.xlsx")
overfish <- select(overfish.raw, year = Year, fully = FullyFished, over = Overfished, under = Underfished)
write.csv(overfish, file = "../output/overfishing.csv", row.names = FALSE)

prod.raw <- read.csv("../input/Fishstatj FAO Global Fishery and Aquaculture Production Statistics.csv")
fish.raw <- WDI(indicator=c('ER.FSH.CAPT.MT'), extra=TRUE)
pop.raw <- WDI(indicator = "SP.POP.TOTL", extra=TRUE)
pop <- filter(pop.raw, year == 2020) %>%
  select(iso3c, SP.POP.TOTL)
fish <- filter(fish.raw, year == 2020, region != "Aggregates") %>%
  select(iso3c, ER.FSH.CAPT.MT) %>%
  filter(!is.na(ER.FSH.CAPT.MT))

world.total <- sum(fish$ER.FSH.CAPT.MT)
world <- data.frame("WLD", world.total)
names(world) <- c("iso3c", "ER.FSH.CAPT.MT")

fish <- rbind(fish, world)

popfish <- left_join(pop, fish, by = "iso3c") %>%
  mutate(percapita_catch = round(ER.FSH.CAPT.MT*1000/SP.POP.TOTL, 1)) %>%
  filter(!is.na(percapita_catch)) %>%
  select(iso3c, percapita_catch)
write.csv(popfish, file = "../output/percapita_catch.csv", row.names = FALSE)

# Catch breakdown
catch.raw <- read_excel('../input/fish, crustaceans and molluscs (2021).xlsx')
catch.raw <- rename(catch.raw,
                    iso3c = Country,
                    countryname = `Country (Name)`,
                    source = Series,
                    asfisspeciesname = `ASFIS species (Name)`,
                    faomajorfishingareaname = `FAO major fishing area (Name)`)

catch <- filter(catch.raw, !grepl('Inland waters', faomajorfishingareaname)) %>%
  filter(source == "1. Capture") %>%
  dplyr::select(-countryname, -source) %>%
  pivot_longer(cols = 4:75, names_to = "year", values_to = "catch") %>%
  mutate(year = as.numeric(gsub("Data_", "", year))) %>%
  mutate(area = case_when(
    grepl("Inland waters", faomajorfishingareaname, fixed=TRUE) ~ "inland",
    grepl("Atlantic", faomajorfishingareaname, fixed=TRUE) ~ "atlantic",
    grepl("Indian Ocean", faomajorfishingareaname, fixed=TRUE) ~ "indian",
    grepl("Pacific", faomajorfishingareaname, fixed=TRUE) ~ "pacific",
    grepl("Arctic", faomajorfishingareaname, fixed=TRUE) ~ "arctic",
    grepl("Mediterranean", faomajorfishingareaname, fixed=TRUE) ~ "medblack",
  ))

# By production
catch.byproduction <- dplyr::select(catch.raw, -iso3c, -countryname, -asfisspeciesname, -faomajorfishingareaname) %>%
  pivot_longer(cols = 2:73, names_to = "year", values_to = "catch") %>%
  mutate(year = as.numeric(gsub("Data_", "", year))) %>%
  group_by(year, source) %>%
  summarise(catch = sum(catch)) %>%
  mutate(source = case_when(
    source == "2. Aquaculture" ~ "aquaculture",
    source == "1. Capture" ~ "capture"
  )) %>%
  pivot_wider(names_from = source, values_from = catch)

catch.byproduction.sea <- filter(catch.raw, !grepl('Inland waters', faomajorfishingareaname)) %>%
  dplyr::select(-iso3c, -countryname, -asfisspeciesname, -faomajorfishingareaname) %>%
  pivot_longer(cols = 2:73, names_to = "year", values_to = "catch") %>%
  mutate(year = as.numeric(gsub("Data_", "", year))) %>%
  group_by(year, source) %>%
  summarise(catch = sum(catch)) %>%
  mutate(source = case_when(
    source == "2. Aquaculture" ~ "aquaculture_sea",
    source == "1. Capture" ~ "capture_sea"
  )) %>%
  pivot_wider(names_from = source, values_from = catch)

# Single out inland capture and aquaculture
catch.byproduction.inland <- dplyr::select(catch.raw, -iso3c, -countryname, -asfisspeciesname) %>%
  filter(grepl('Inland waters', faomajorfishingareaname)) %>%
  pivot_longer(cols = 3:74, names_to = "year", values_to = "catch") %>%
  mutate(year = as.numeric(gsub("Data_", "", year))) %>%
  group_by(year, source) %>%
  summarise(catch = sum(catch)) %>%
  mutate(source = case_when(
    source == "2. Aquaculture" ~ "aquaculture_inland",
    source == "1. Capture" ~ "capture_inland"
  )) %>%
  pivot_wider(names_from = source, values_from = catch)

check <- left_join(catch.byproduction.inland, catch.byproduction.sea, by = "year") %>%
  left_join(catch.byproduction, by = "year") %>%
  mutate(check.aquaculture = aquaculture - aquaculture_inland - aquaculture_sea) %>%
  mutate(check.capture = capture - capture_inland - capture_sea)
View(check)

# Areas
catch.byarea <- group_by(catch, area, year) %>%
  summarise(catch = sum(catch)) %>%
  pivot_wider(names_from = area, values_from = catch)

# Species
top10species <- filter(catch, year == 2021) %>%
  group_by(asfisspeciesname) %>%
  summarise(catch = sum(catch)) %>%
  top_n(12, catch) %>%
  filter(!asfisspeciesname %in% c("Freshwater fishes nei", "Marine fishes nei", "Scads nei"))

catch.byspecies <- mutate(catch, species = ifelse(asfisspeciesname %in% top10species$asfisspeciesname, asfisspeciesname, "other_species")) %>%
  group_by(species, year) %>%
  summarise(catch = sum(catch), .groups = "drop") %>%
  mutate(species = case_when(
    species == "Alaska pollock(=Walleye poll.)" ~ "pollock",
    species == "Anchoveta(=Peruvian anchovy)" ~ "anchovy",
    species == "Atlantic cod" ~ "cod",
    species == "Atlantic herring" ~ "herring",
    species == "Blue whiting(=Poutassou)" ~ "whiting",
    species == "European pilchard(=Sardine)" ~ "sardine",
    species == "Pacific sardine" ~ "pacific_sardine",
    species == "Pacific chub mackerel" ~"mackerel",
    species == "Skipjack tuna" ~ "skipjack",
    species == "Yellowfin tuna" ~ "yellowfin",
    TRUE ~ species
  )) %>%
  pivot_wider(names_from = species, values_from = catch)

# Countries
top10countries <- filter(catch, year == 2021) %>%
  group_by(iso3c) %>%
  summarise(catch = sum(catch)) %>%
  top_n(10, catch)

catch.bycountry <- mutate(catch, iso3c = ifelse(iso3c %in% top10countries$iso3c, iso3c, "other_country")) %>%
  group_by(iso3c, year) %>%
  summarise(catch = sum(catch)) %>%
  pivot_wider(names_from = iso3c, values_from = catch)

catch.ok <- left_join(catch.byproduction, catch.byproduction.inland, by = "year") %>%
  left_join(catch.byproduction.sea, by = "year") %>%
  left_join(catch.byarea, by = "year") %>%
  left_join(catch.bycountry, by = "year") %>%
  left_join(catch.byspecies, by = "year")
write.csv(catch.ok, file = "../output/capture_aquaculture.csv", row.names = FALSE)

# Vessels
vessels.raw <- read.csv("../input/CapacityCountryLevel_Detailed.csv")
vessels <- select(vessels.raw, year = Year, vessels = NV, iso3c = Country) %>%
  group_by(year, iso3c) %>%
  summarise(vessels = sum(vessels))
countries <- wbstats::wb_countries()
countries.regions <- select(countries, iso3c, region_iso3c)

vessels.4sheets <- left_join(vessels, countries.regions, by = "iso3c") %>%
  arrange(region_iso3c)

vessels.regions <- group_by(vessels.4sheets, year, region_iso3c) %>%
  summarise(vessels = sum(vessels)) %>%
  filter(!is.na(region_iso3c)) %>%
  pivot_wider(names_from = region_iso3c, values_from = vessels)

china <- filter(vessels, iso3c == "CHN") %>%
  select(year, CHN = vessels)

vessels.regions.china <- left_join(vessels.regions, china, by = "year")
write.csv(vessels.regions.china, file = "../output/vessels.csv", row.names = FALSE)

# Global Fishing Watch fishing hours
devtools::install_github("GlobalFishingWatch/gfwr")
library(gfwr)
key <- gfw_auth()

world <- '{"geojson":{"type":"Polygon","coordinates":[[[-180, 90], [180, 90], [180, -90], [-180, -90],[-180, 90]]]}}'

fish.hours <- get_raster(spatial_resolution = 'low',
                   temporal_resolution = 'yearly',
                   group_by = 'flag',
                   date_range = '2021-01-01,2021-12-31',
                   region = world,
                   region_source = 'user_json',
                   key = key)
# Save to not having to generate the data again
#write.csv(fish.hours, file = "apparent_fishing_hours.csv", row.names = FALSE)
#fish.hours <- read.csv("apparent_fishing_hours.csv")
fish.hours.aggr <- group_by(fish.hours, Lat, Lon) %>%
  summarise(apparent_fishing_hours = sum(`Apparent Fishing hours`))

countries <- wbstats::wb_countries()
countries.income <- dplyr::select(countries, iso3c, income_level_iso3c)
fishing.income <- left_join(fish.hours, countries.income, by = c("flag" = "iso3c")) %>%
  group_by(Lat, Lon, income_level_iso3c) %>%
  summarise(apparent_fishing_hours = sum(`Apparent Fishing hours`)) %>%
  filter(income_level_iso3c != c("INX")) %>%
  filter(!is.na(income_level_iso3c)) %>%
  mutate(income_level_iso3c = factor(income_level_iso3c, levels = c("HIC", "UMC", "LMC", "LIC")))

# Convert to tif
fishing.income.log <- mutate(fishing.income, apparent_fishing_hours_log = log(apparent_fishing_hours))
fishing.geopoints <- st_as_sf(filter(fishing.income.log, income_level_iso3c == "LMC"), coords=c("Lon","Lat"), crs=4326)
rast <- raster(resolution = 0.1, xmn=-179.95, xmx=179.95, ymn=-77.85, ymx=82.65, crs=4326)
fishing.raster <- rasterize(x=fishing.geopoints, y=rast, field = "apparent_fishing_hours_log")
writeRaster(fishing.raster, "../output/fishinghours_lmc_log.tif")

# Fishing zones and centroids
centroids <- read.csv("../input/fishingzones_centroids_2019.csv")
colnames(centroids) <- c("lat", "lon", "name", "share_overfished_stocks")
centroids <- mutate(centroids, lat = round(lat, 1), lon = round(lon, 1)) %>%
  mutate(zone = case_when(name == "Pacific, Western Central" ~ "pacificwc", 
                          name == "Atlantic, Northwest" ~ "atlanticnw", 
                          name == "Atlantic, Antarctic" ~ "atlanticant", 
                          name == "Atlantic, Northeast" ~ "atlanticne", 
                          name == "Atlantic, Southeast" ~ "atlanticse", 
                          name == "Atlantic, Eastern Central" ~ "atlanticec",
                          name == "Atlantic, Western Central" ~ "atlanticwc",
                          name == "Mediterranean and Black Sea" ~ "medblack", 
                          name == "Pacific, Northeast" ~ "pacificne", 
                          name == "Atlantic, Southwest" ~ "atlanticsw", 
                          name == "Indian Ocean, Eastern" ~ "indiane", 
                          name == "Atlantic, Western Central" ~ "atlanticwc", 
                          name == "Pacific, Antarctic" ~ "pacificant", 
                          name == "Indian Ocean, Antarctic" ~ "indianant", 
                          name == "Pacific, Southeast" ~ "pacificse", 
                          name == "Pacific, Northwest" ~ "pacificnw", 
                          name == "Pacific, Eastern Central" ~ "pacificec", 
                          name == "Indian Ocean, Western" ~ "indianw", 
                          name == "Arctic Sea" ~ "arctic", 
                          name == "Pacific, Southwest" ~ "pacificsw"
  )) %>%
  mutate(lon = ifelse(name == "Pacific, Southwest", -150, lon)) %>%
  mutate(lon = ifelse(name == "Pacific, Western Central", lon + 20, lon)) %>%
  mutate(lon = ifelse(name == "Pacific, Northwest", lon + 30, lon)) %>%
  mutate(lon = ifelse(name == "Pacific, Antarctic", lon - 50, lon)) %>%
  mutate(lat = ifelse(name == "Atlantic, Southwest", lat - 5, lat)) %>%
  mutate(lat = ifelse(name == "Atlantic, Southeast", lat + 5, lat))
write.csv(centroids, file = "../output/fishingzones.csv", row.names = FALSE)

# Share of protected marine areas
pma.raw <- read_excel("../input/export_jan10.xlsx")
pma <- dplyr::select(pma.raw, iso3c = countrycode, totalmarinearea, PAmarinecover) %>%
  mutate(PAmarinecover = round(PAmarinecover, 1)) %>%
  mutate(totalmarinearea = round(totalmarinearea, 0)) %>%
  filter(!is.na(totalmarinearea)) %>%
  filter(totalmarinearea != 0)
write.csv(pma, file = "../output/marine_protected_areas.csv", row.names = FALSE)

# Kobe plot
kobe <- read_excel('../input/Stock_Condition_data.xlsx')
colnames(kobe) <- c("stock", "year", "B_Bmsy", "F_Fmsy")
kobe <- filter(kobe, !grepl('Complex', stock)) %>%
  separate(stock, sep=" - ", into = c("species", "area")) %>%
  mutate(area = gsub(' / ', '/', area)) %>%
  mutate(species_code = gsub(' ', '_', species)) %>%
  mutate(area_code = gsub(' ', '_', area))