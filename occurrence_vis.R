library(ggplot2)
library(maps)
library(mapproj)
library(tidyverse)
library(leaflet)

# set working directory
setwd("/nfs/insectinvasions-data")

# world map
world_data <- map_data('world')
world_data <- world_data %>% filter(region != "Antarctica")
world_data <- fortify(world_data)

p1 <- ggplot() + 
      geom_map(data=world_data, map=world_data, aes(x=long, y=lat, group=group, map_id=region),
               fill="white", colour="#7f7f7f", size=0.5) +
      coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))


occ <- read.csv("./data/clean_data/occurrence_table.csv", stringsAsFactors = FALSE)

occ_c <- occ %>% 
         dplyr::rename(continent = region, region = country) %>% 
         mutate_if(is.factor, as.character) %>% # convert to character vectors
         mutate(region = ifelse(is.na(region), continent, region)) %>% 
         mutate(region = replace(region, region %in% c("Us", "Us, may not actually be adventive", "Hawaii"), "USA"),
                region = replace(region, region %in% c("Ogasawara", "Okinawa"), "Japan"),
                region = replace(region, region == "United Kingdom", "UK"),
                region = replace(region, region %in% c("Corse", "Sicily", "Sardinia"), "Italy"))

occ2 <- occ_c %>% 
        select(country, region, year, genus_species) %>% 
        group_by(region) %>% 
        count() %>% 
        ungroup() %>% 
        full_join(world_data, by = "region") %>% 
        dplyr::filter(!is.na(n), !is.na(region), !is.na(long))

p2 <- p1 + 
      geom_map(data=occ2, map=world_data, aes(map_id=region, fill=n)) +
      scale_fill_gradient(low="lightgreen", high="darkgreen")

p2

# leaflet

wd2 <- world_data %>% 
       select(-group, -order, -subregion) %>% 
       group_by(region) %>% 
       summarize_all(median) %>% 
       ungroup()

occ3 <- occ_c %>%  
        group_by(region) %>% 
        add_count(region) %>% 
        ungroup() %>% 
        select(-present_status, -year, -origin, -ecozone, -continent, -taxon_id, -user_supplied_name) %>% 
        left_join(wd2, by = "region") %>% 
        dplyr::filter(!is.na(n), !is.na(region))

markers <- aggregate(genus_species ~ lat + long, occ3, paste, collapse = "<br/>")


L1 <- leaflet(options = leafletOptions(minZoom = 1.5)) %>%
      addTiles() %>%
      addMarkers(data = markers, popup = markers$genus_species)

L1
 