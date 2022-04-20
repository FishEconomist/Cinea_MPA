CountArea <- function(basegeo, 
                      ship = TRUE,
                      posi = 1000){
  
  select_month <- c("month", "SHIPNAME")
  select_glob <- c("SHIPNAME")
  if(ship == FALSE){
    select_month <- c("month", "TripID")
    select_glob <- c( "TripID")
  }
  pointsAIS <- 
    basegeo %>% 
    group_by(month) %>%
    summarise(
      Ship_posi = 
        round(n() / posi, 0))
  
  ShipAIS <- 
    basegeo %>% 
    select(all_of(select_month)) %>%
    unique() %>%
    group_by(month) %>%
    summarise(Ship_num = n())

  pointsAISGlob <- 
    basegeo %>% 
    summarise(
      Ship_posi = 
        round(n() / posi, 0))
  
  ShipAISGlob <- 
    basegeo %>% 
    select(all_of(select_glob)) %>%
    unique() %>%
    summarise(Ship_num = n())
  
  AIS_count <- 
    ShipAIS %>%
    dplyr::left_join(
      pointsAIS,
      by = "month")
  
  AISGlob <- 
    dplyr::bind_cols(
      ShipAISGlob,
      pointsAISGlob) 
  
  AISTab <- 
    bind_rows(
      AIS_count,
      AISGlob
    )
}


graphGrid <- function(
  Grid,
  Points,
  Zone,
  Titre,
  Transformation,
  Facet = FALSE,
  response = "month"
){
  Grid_trip <- 
    dplyr::left_join(
      Grid, 
      Points,
      by = "GridId") %>%
    filter(!is.na(N))
  
  map_Grid <- 
    ggplot() +
    geom_sf(
      data = Grid_trip,
      aes(color = N,
          fill = N)
    ) +
    scale_color_gradient(low = "blue", high = "red", trans = Transformation) +
    scale_fill_gradient(low = "blue", high = "red", trans = Transformation) +
    geom_sf(
      data = Zone,
      size = 1,
      color = "orange",
      fill = NA
    ) +
    coord_sf(crs = 32630) +
    # ggtitle(Titre) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) 
  
  if(Facet){
    map_Grid <- 
      map_Grid +
      facet_wrap( as.formula(paste("~", response)),
                  drop = TRUE)
    }
    return(map_Grid)
}

graphGridExt <- function(
  Grid,
  Base_sf,
  Zone,
  GroupGraph,
  VarCalc,
  Titre,
  Transformation,
  Facet = FALSE,
  response = "month",
  Xbreaks = seq(-3.9, -3.3, by = .2),
  Nmini = 0
){
  Points <- 
    Base_sf %>%
    filter(N >= Nmini) %>%
    st_drop_geometry() %>% 
    group_by(across(all_of(GroupGraph))) %>%
    distinct(across(all_of(VarCalc))) %>%
    summarise(N = n())
  
  Grid_trip <- 
    dplyr::left_join(
      Grid, 
      Points,
      by = "GridId") %>%
    filter(!is.na(N))
  
  map_Grid <- 
    ggplot() +
    geom_sf(
      data = Grid_trip,
      aes(color = N,
          fill = N)
    ) +
    scale_color_gradient(low = "blue", high = "red", trans = Transformation) +
    scale_fill_gradient(low = "blue", high = "red", trans = Transformation) +
    geom_sf(
      data = Zone,
      size = 1,
      color = "orange",
      fill = NA
    ) +
    geom_sf(
      data = graphCropShp(
        Grid_trip,
        Territoriale
      ),
      size = 1,
      color = "darkred",
      fill = NA
    ) +
    coord_sf(crs = 32630) +
    # ggtitle(Titre) +
    scale_x_continuous(breaks = Xbreaks) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) 
  
  if(Facet){
    map_Grid <- 
      map_Grid +
      facet_wrap( as.formula(paste("~", response)),
                  drop = TRUE)
  }
  return(map_Grid)
}


graphCropShp <- function(
  Reference_sf = Zone,
  toBeCroped_sf = Habitats,
  ref_proj = 32630
){
  
  BoxRef <- st_bbox(Reference_sf %>% st_transform(4326))
  min_LAT <- as.numeric(trunc(BoxRef$ymin * 100 - 1) / 100)
  max_LAT <- as.numeric(trunc(BoxRef$ymax * 100 + 1) / 100)
  
  min_LON <- as.numeric(trunc(BoxRef$xmin * 100 - 1) / 100)
  max_LON <- as.numeric(trunc(BoxRef$xmax * 100 + 1) / 100)
  
  Croped_sf <-
    st_crop(
      toBeCroped_sf %>% st_transform(4326),
      xmin = min_LON,
      xmax = max_LON,
      ymin = min_LAT,
      ymax = max_LAT)  %>%
    st_transform(ref_proj)
  return(Croped_sf)
}
  