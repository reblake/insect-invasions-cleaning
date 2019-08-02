library(ggplot2) ; library(ggthemes) ; library(maps) ; 
library(mapproj) ; library(tidyverse) ; library(leaflet)
library(gganimate) ; library(viridis)

# set working directory
setwd("/nfs/insectinvasions-data")

# world map
world_data <- map_data('world')
world_data <- world_data %>% filter(region != "Antarctica")
world_data <- fortify(world_data)

p1 <- ggplot() + 
      geom_map(data=world_data, map=world_data, aes(x=long, y=lat, group=group, map_id=region),
               fill="white", colour="#7f7f7f", size=0.5) +
      coord_map("mollweide", xlim=c(-180,180), ylim=c(-60, 90))

# occurrence data
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
        select(region, year, genus_species) %>% 
        group_by(region) %>% 
        count() %>% 
        ungroup() %>% 
        full_join(world_data, by = "region") %>% 
        dplyr::filter(!is.na(n), !is.na(region), !is.na(long))

occ3 <- occ_c %>% 
        select(region, year, genus_species) %>% 
        group_by(region, year) %>% 
        count() %>% 
        ungroup() %>% 
        full_join(world_data, by = "region") %>% 
        mutate_at("year", as.numeric) %>% 
        dplyr::filter(complete.cases(.))

p2 <- p1 + 
      geom_map(data=occ2, map=world_data, aes(map_id=region, fill=n)) +
      scale_fill_gradient(low="lightgreen", high="darkgreen") +
      theme(plot.margin = margin(0,0,0,0, "mm"),
            plot.background = element_blank()) 

# gganimate
yr_colors <- c("white", viridis(208))
p3 <- ggplot() + borders("world", colour = "black", fill = "white") +
      geom_map(data=occ3, map=world_data, aes(map_id=region, fill=year), alpha = 0.3) +
      scale_fill_gradientn(colors = yr_colors) +
      transition_reveal(year) +
      ease_aes("linear")
# make the animation
animate(p3, nframes = 100, fps = 10, height = 604, width = 1000)

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

markers <- aggregate(genus_species ~ lat + long + region, occ3, paste, collapse = "<br/>")


L1 <- leaflet(options = leafletOptions(minZoom = 1.5)) %>%
      addTiles(options=providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = markers, popup = markers$genus_species,  
                 label = markers$region, group = "hover") %>% 
      setView(lat = 30, lng = 0, zoom = 1.75)

#L1
 