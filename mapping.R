

library(tmap)
library(sf)
library(tidygeocoder)


to.geocode <- cspp %>%
    filter(`Facility Type` == "DAY CARE CENTER") %>%
    select(`Facility Name`:`Facility Capacity`,head_start) %>%
    select(-`County Name`, -`Regional Office`) %>%
    mutate(across(c(`Facility Name`, `Licensee`, `Facility Administrator`, `Facility Address`, `Facility City`), str_to_title),
           `Facility Email` = str_to_lower(`Facility Email`))





# 
# geocoded.tidy <- geocode(to.geocode,
#                          address = location,
#                          method = "census")

geocoded.osm <- geocode(to.geocode,
                        street = `Facility Address`,
                        city = `Facility City`,
                        state = `Facility State`,
                        postalcode = `Facility Zip`,
                        return_input = TRUE,
                        method = "osm")


# geocoded.census <- geocode(to.geocode,
#                         street = `Facility Address`,
#                         city = `Facility City`,
#                         state = `Facility State`,
#                         postalcode = `Facility Zip`,
#                         return_input = TRUE,
#                         method = "census")
# 
# temp.joint <- geocoded.osm %>%
#     mutate(lat.osm = lat,
#            long.osm = long) %>%
#     left_join(geocoded.census)


trial <- geocoded.osm %>%
    filter(!is.na(long),
           !is.na(lat)) %>%
    st_as_sf(coords = c("long","lat"), remove = FALSE, crs= 4269)




library(tigris)
districts_un <- school_districts(type = "unified" , state = "CA", year = 2021, class = "sf") %>%
    st_transform(4269) %>% rename(LEA = UNSDLEA)


districts_elem <- school_districts(type = "elementary" , state = "CA", year = 2021, class = "sf") %>%
    st_transform(4269) %>% rename(LEA = ELSDLEA)


districts <- districts_un %>%
    rbind(districts_elem) %>%
#    rbind(districts_sec) %>%
    st_transform(4269)


monterey_tracts <- tracts("CA", "Monterey", class = "sf") %>%
    filter(ALAND > 0) %>% 
    st_as_sf(monterey_tracts) %>%
    st_transform(4269) %>% st_union()


monterey_county <- counties("CA", class = "sf") %>%
    filter(NAME == "Monterey")  %>%
    st_transform(4269)




districts.mry <- districts %>%
    filter(!str_detect(NAME, "Willow|Shandon|Not|Miguel|Reef|Huron|Pajaro|Aromas|Paso|Coast|Tully|Pleasant|Jefferson|Cien")) %>%
    select(NAME, INTPTLAT,INTPTLON, geometry) %>%
    mutate(County = "Monterey") %>%
    st_intersection(monterey_tracts) %>%
    st_transform(4269) %>%
    st_make_valid()



monterey_districts <- districts[st_contains(monterey_tracts, districts, sparse = FALSE),] 




tmap_options(check.and.fix = TRUE)



 # ggplot() +
 #    geom_sf(data=districts.mry, color = "black", fill=NA, size=0.5) +
 #    geom_sf(data=trial, size=1, aes(color="red")) +
 #    coord_sf(xlim = c(-120.2, -122.0), ylim = c(35.8, 36.9) ) +
 #     labs(x=NULL, y=NULL, 
 #          title="title",
 #          subtitle=today(),
 #          caption="Source: ") +
 #     theme_bw() +
 #     theme(plot.title=element_text(face="bold", family="Arial", size=13)) +
 #     theme(plot.caption=element_text(face="bold",
 #                                     family="Arial",
 #                                     size=7,
 #                                     color="gray",
 #                                     margin=margin(t=10, r=80))) +
 # 
 #     theme(legend.position="none") 
 

 tmap_mode("view")
 
 tm_shape(districts.mry) +
     tm_fill("County", alpha = .5, legend.show = FALSE) +
     tm_borders() +
     tm_text("NAME", auto.placement = TRUE) +
    tm_shape(trial) + 
     tm_dots(col = "orange", size = .04) #+
   #  tm_view(set.view = c(lat = 36.3 , lon = -121.4 ,  zoom = 8))
 
 
 #  Next steps, put into the shiny app.  
 
 
 write_rds(districts.mry,here("TKestimates","districts.rds"))
 
 write_rds(trial,here("TKestimates","trial.rds"))

 
 
 
 to.geocode %>% 
     filter(str_detect(`LEA Geographically associated`, "Salinas City")) %>%
     write_csv("Salinas City Child Care Centers.csv")
 
