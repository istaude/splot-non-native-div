# This script aggregates sPlot, GloNAF, and GRIIS databases,
# and groups plots within 32km2 grid cells



# preamble ----------------------------------------------------------------
source("00-preamble.R")

# load splot data ---------------------------------------------------------
load("Data/sPlotOpen.RData")


# load glonaf data --------------------------------------------------------
# regions
glonaf_regions <-
  read_csv("Data/GLONAF/Region_GloNAF_vanKleunenetal2018Ecology.csv")

# species
glonaf_species <-
  read_delim(
    "Data/GLONAF/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology.csv",
    delim = "\t",
    escape_double = FALSE,
    locale = locale(encoding = "UTF-16"),
    trim_ws = TRUE
  )

# join
glonaf <- full_join(glonaf_species, glonaf_regions)


# regional matching of splot with glonaf ----------------------------------
# glonaf regional polygons
sf_use_s2(FALSE)
glonaf_shape <- read_sf("Data/GLONAF/regions2.shp")

# splot coords
splot_coordinates <- header.oa %>% 
  dplyr::select(PlotObservationID, Longitude, Latitude) %>% 
  st_as_sf(coords=c("Longitude", "Latitude"),crs=4326)

# join
# arg largest: if TRUE, 
# return x features augmented with the fields of y that have the largest overlap
# with each of the features of x  
splot_glonafregions <- st_join(splot_coordinates, glonaf_shape, largest=TRUE) %>% 
  drop_na() 

# write 
write.csv(splot_glonafregions %>% 
            dplyr::select(-geometry), 
          "Data/splot_glonafregions_world.csv",
          row.names=FALSE)

# read
splot_glonafregions <- read_csv("Data/splot_glonafregions_world.csv")

# data exploration
length(unique(splot_glonafregions$GeodAREA)) # 319 regions
length(unique(splot_glonafregions$PlotObservationID)) #88,892 plots


# join splot and glonaf ---------------------------------------------------
splotdt_glonaf <- splot_glonafregions %>%
  # add splot species
  left_join(DT2.oa %>%
              dplyr::select("Species", "PlotObservationID"),
            by = c("PlotObservationID")) %>% 
  # add glonaf species status
  left_join(glonaf %>% 
              dplyr::select(standardized_name, status, OBJIDsic), 
            by=c("Species"="standardized_name","OBJIDsic")) %>% 
  distinct()

head(splotdt_glonaf)
nrow(splotdt_glonaf) # 1,825,997

# write
write.csv(splotdt_glonaf, 
          "Data/splotdt_glonaf.csv", 
          row.names=FALSE)

# read
splot_glonafregions <- read_csv("Data/splotdt_glonaf.csv")


# griis -------------------------------------------------------------------
griis <- read_csv("Data/GRIIS - Country Compendium V1_0.csv")
unique(griis$isInvasive)
griis <- griis %>% filter(isInvasive == "INVASIVE")

splotdt_glonaf_griis <- splotdt_glonaf %>% 
  left_join(glonaf_regions %>%
              dplyr::select(OBJIDsic, countryCode_alpha3 = country_ISO) %>% 
              distinct()) %>%
  left_join(griis %>% 
              dplyr::select(species, countryCode_alpha3, isInvasive), 
            by = c("Species" = "species", "countryCode_alpha3"))


# clean -------------------------------------------------------------------

d <- splotdt_glonaf_griis %>% 
  mutate(status = ifelse(is.na(status) == T, "native", "non-native")) %>% 
  mutate(isInvasive = ifelse(is.na(isInvasive) == T, "non-invasive", "invasive")) %>% 
  mutate(species_status = ifelse(isInvasive == "invasive", "invasive", status))

d %>% count(species_status)


# create grid cells and pair plots ----------------------------------------
# resolution 13, cell size c. 32km2 
dggs <- dgconstruct(res = 13, metric = TRUE, resround = 'down') 

header.oa$cell <- dgGEO_to_SEQNUM(dggs, 
                                  header.oa$Longitude, 
                                  header.oa$Latitude)$seqnum


d <- d %>% 
  left_join(header.oa %>% dplyr::select("PlotObservationID","cell"),
            by="PlotObservationID")

View(d)
nrow(d)

# write
write.csv(d,
          "Data/dt.csv",
          row.names=FALSE)

write.csv(header.oa,
          "Data/splotheader_glonaf_13.csv",
          row.names=FALSE)
