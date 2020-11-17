## @knitr utils

get_area_tracts <- function(area) {
  
  buffer <- units::set_units(1, m)
  
  area_tracts <- sf::st_join(
    dplyr::rename(baltimore_tracts, tract_name = name),
    sf::st_buffer(
      area,
      -2 * buffer
      ),
    by = "st_intersects"
  ) %>%
    dplyr::filter(
      name %in% area$name
      )
  
  return(area_tracts)
}

get_nearby_areas <- function(area,
                             area_type =  c("neighborhood", "council_district", "police_district", "csa",
                                            "block_group", "tract")
                             ) {
  
  area_type <- match.arg(area_type)
  
  buffer <- units::set_units(1, m)
  
  if (area_type == "neighborhood") {
    areas_to_check <- neighborhoods
  }
  else if (area_type == "council_district") {
    areas_to_check <- council_districts
  }
  else if (area_type == "police_district") {
    areas_to_check <- police_districts
  }
  else if (area_type == "csa") {
    areas_to_check <- csas
  }
  
nearby_areas <- sf::st_join(
    areas_to_check,
    sf::st_buffer(
      dplyr::select(area, area_name = name),
      2*buffer
    ),
    by = "st_intersects"
  ) %>%
    dplyr::filter(
      area_name %in% area$name
    ) %>% 
  dplyr::filter(
    !(name %in% area$name)
  ) %>% 
  dplyr::select(-area_name)

return(nearby_areas)

}

get_buffered_area <- function(area) {
  
  area_bbox <- sf::st_bbox(area)
  
  area_bbox_diagonal <- sf::st_distance(
    sf::st_point(
      c(area_bbox$xmin,
        area_bbox$ymin)
    ),
    sf::st_point(
      c(area_bbox$xmax,
        area_bbox$ymax)
    )
  )
  
  buffer_meters <- units::set_units(area_bbox_diagonal * 0.125, m)
  
  buffered_area <- sf::st_buffer(area, buffer_meters)
  
  return(buffered_area)
}


plot_acs_table <- function(selected_area_tracts,
                           table_id,
                           level = c("primary", "secondary", "tertiary"),
                           geom = c("bar")) {
  
  # Select table and tracts
  selected_area_acs_table <- overview_acs %>% 
    dplyr::filter(table == table_id) %>%
    dplyr::mutate(
      label_unit = forcats::fct_reorder2(label_unit, table, variable),
      label_primary = forcats::fct_reorder2(label_primary, table, variable),
      label_secondary = forcats::fct_reorder2(label_secondary, table, variable),
      label_tertiary = forcats::fct_reorder2(label_tertiary, table, variable)
    ) %>%
    dplyr::filter(!is.na(label_primary)) %>%
    dplyr::filter(geoid %in% selected_area_tracts$geoid)
  
  # If all secondary labels are NA, set level to primary
  if (length(selected_area_acs_table$label_secondary) == sum(is.na(selected_area_acs_table$label_secondary))) {
    level <- "primary"
  }
  
  if (level == "primary") {
    
    selected_area_acs_table <- selected_area_acs_table %>%
      dplyr::filter(!is.na(label_primary))
    
  } else if (level == "secondary") {
    
    selected_area_acs_table <- selected_area_acs_table %>%
      dplyr::filter(!is.na(label_secondary))
    
  }
  
  table_concept <- unique(selected_area_acs_table$concept)
  table_unit <- unique(selected_area_acs_table$label_unit)
  
  if (level == "primary") {
    
    selected_area_acs_table_plot <- ggplot2::ggplot() +
      ggplot2::geom_bar(
        data = selected_area_acs_table,
        ggplot2::aes(
          x = label_primary,
          weight = estimate,
          fill = label_primary
        )
      ) +
      ggplot2::guides(fill = "none")
    
  } else if (level == "secondary") {
    selected_area_acs_table_plot <- selected_area_acs_table %>%
      ggplot2::ggplot() +
      ggplot2::geom_bar(
        ggplot2::aes(
          x = label_secondary,
          weight = estimate,
          fill = label_primary
        )
      )
  }
  
  selected_area_acs_table_plot <- selected_area_acs_table_plot +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = table_concept,
      y = table_unit
    ) +
    ggplot2::theme_minimal()
  
  return(selected_area_acs_table_plot)
  
}
