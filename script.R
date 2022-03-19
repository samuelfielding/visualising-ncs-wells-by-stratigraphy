# LOAD MAGRITTR -----------------------------------------------------------

library(magrittr)



# PULL FONT FROM GOOGLE FONTS ---------------------------------------------

sysfonts::font_add_google(name = "Ubuntu", family = "ubuntu")
showtext::showtext_auto()




# PULL DATA FROM NPD AND SAVE ---------------------------------------------

current_day_compact <-
  lubridate::today() %>%
  stringr::str_remove_all(pattern = "-")

current_day_slashes <-
  lubridate::today() %>%
  format(format = "%d/%m/%Y")
  

vec_urls <-
  c("https://factpages.npd.no/ReportServer_npdpublic?/FactPages/tableview/wellbore_exploration_all&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&IpAddress=not_used&CultureCode=en&rs:Format=CSV&Top100=false",
    "https://factpages.npd.no/ReportServer_npdpublic?/FactPages/tableview/wellbore_development_all&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&IpAddress=not_used&CultureCode=en&rs:Format=CSV&Top100=false",
    "https://factpages.npd.no/ReportServer_npdpublic?/FactPages/tableview/wellbore_formation_top&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&IpAddress=not_used&CultureCode=en&rs:Format=CSV&Top100=false")

vec_names <-
  c("wells_exploration",
    "wells_development",
    "with_lithostratigraphy")


for (i in seq_along(vec_urls)) {
  
  result_i <-
    httr::GET(vec_urls[i])
  
  response_text_i <-
    result_i %>%
    httr::content(type = "text/plain",
                  encoding = "UTF-8")
  
  response_csv_i <-
    response_text_i %>%
    readr::read_csv(locale = readr::locale(encoding = "UTF-8"),
                    guess_max = 200000)
  
  response_csv_i %>%
    readr::write_csv(file = paste0(vec_names[i], "_", current_day_compact, ".csv"),
                     na = "")
  
} # end of for loop over urls



# CREATE BACKGROUND POLYGON FOR NORWAY ------------------------------------

# Change scale value to 10 to get high resolution coastline - you'll need to install the "rnaturalearthhires" package for this

background_polygon <-
  rnaturalearth::ne_countries(scale = 110,
                              country = "norway") %>%
  sf::st_as_sf()



# READ IN DATA ------------------------------------------------------------

metadata_strat <-
  readxl::read_xlsx(path = "metadata_strat.xlsx") %>%
  dplyr::select(order,
                system_period_earliest, intra_system_order,
                strat_name,
                colour)

wells_exploration <-
  readr::read_csv(file = paste0("wells_exploration", "_", current_day_compact, ".csv"),
                  locale = readr::locale(encoding = "UTF-8"),
                  guess_max = 200000)

wells_development <-
  readr::read_csv(file = paste0("wells_development", "_", current_day_compact, ".csv"),
                  locale = readr::locale(encoding = "UTF-8"),
                  guess_max = 200000)

with_lithostratigraphy <-
  readr::read_csv(file = paste0("with_lithostratigraphy", "_", current_day_compact, ".csv"),
                  locale = readr::locale(encoding = "UTF-8"),
                  guess_max = 200000)



# PREPARE DATA ------------------------------------------------------------

wells_exploration <-
  wells_exploration %>%
  dplyr::select(wellbore_id = wlbNpdidWellbore,
                datum_geodetic = wlbGeodeticDatum,
                latitude = wlbNsDecDeg,
                longitude = wlbEwDesDeg) %>%
  dplyr::mutate(well_type = "exploration")

wells_development <-
  wells_development %>%
  dplyr::select(wellbore_id = wlbNpdidWellbore,
                datum_geodetic = wlbGeodeticDatum,
                latitude = wlbNsDecDeg,
                longitude = wlbEwDesDeg) %>%
  dplyr::mutate(well_type = "development")

wells_all <-
  wells_exploration %>%
  dplyr::bind_rows(wells_development) %>%
  dplyr::distinct(wellbore_id, .keep_all = TRUE)


with_lithostratigraphy <-
  with_lithostratigraphy %>%
  # select and rename columns
  dplyr::select(wellbore_id = wlbNpdidWellbore,
                strat_top_m = lsuTopDepth,
                strat_bottom_m = lsuBottomDepth,
                strat_type = lsuLevel,
                strat_name = lsuName,
                strat_parent = lsuNameParent) %>%
  # create group column
  dplyr::mutate(strat_group_name = dplyr::case_when(strat_type == "FORMATION" ~ strat_parent,
                                                    strat_type == "GROUP" ~ strat_name,
                                                    strat_type == "MEMBER" ~ as.character(NA))) %>%
  # remove subgroups
  dplyr::mutate(strat_group_name = dplyr::case_when(grepl(pattern = " SUBGP", x = strat_group_name) ~ as.character(NA),
                                                    TRUE ~ strat_group_name)) %>%
  # join in order and colours
  dplyr::left_join(metadata_strat,
                   by = c("strat_group_name" = "strat_name")) %>%
  # clean up names
  dplyr::mutate(strat_group_name = gsub(pattern = " GP", replacement = "", x = strat_group_name)) %>%
  dplyr::mutate(strat_group_name = gsub(pattern = " SUBGP", replacement = "", x = strat_group_name)) %>%
  dplyr::mutate(strat_group_name = gsub(pattern = " \\(INFORMAL\\)", replacement = "", x = strat_group_name)) %>%
  dplyr::mutate(strat_group_name = stringr::str_to_title(strat_group_name))



# SUMMARISE WELLBORES PER GROUP -------------------------------------------

summary_wellbores_p_group <-
  with_lithostratigraphy %>%
  dplyr::filter(!is.na(strat_group_name)) %>%
  dplyr::filter(toupper(strat_group_name) != "NO DATA") %>%
  dplyr::filter(toupper(strat_group_name) != "NO GROUP DEFINED") %>%
  dplyr::filter(toupper(strat_group_name) != "UNDEFINED") %>%
  dplyr::distinct(strat_group_name,
                  system_period_earliest, intra_system_order,
                  order, colour, wellbore_id) %>%
  dplyr::group_by(strat_group_name,
                  system_period_earliest, intra_system_order,
                  order, colour) %>%
  dplyr::summarise(n_wellbores = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(n_wellbores))

wellbores_in_group <-
  with_lithostratigraphy %>%
  dplyr::filter(strat_group_name %in% summary_wellbores_p_group$strat_group_name) %>%
  dplyr::distinct(strat_group_name,
                  system_period_earliest, intra_system_order,
                  order, colour, wellbore_id) %>%
  dplyr::left_join(wells_all,
                   by = c("wellbore_id" = "wellbore_id"))

wellbores_in_group <-
  wellbores_in_group %>%
  dplyr::mutate(strat_group_name = forcats::fct_reorder(forcats::as_factor(strat_group_name), .x = order)) %>%
  dplyr::arrange(order)

wellbores_in_group_sf <-
  wellbores_in_group %>%
  dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               remove = FALSE) %>%
  sf::st_set_crs(4230) %>%
  sf::st_transform(crs = 4326)



# MAP WELLBORES FOR GROUPS ------------------------------------------------

# make vector of colours for stratigraphic system scale
colours_strat <-
  metadata_strat %>%
  dplyr::filter(!is.na(system_period_earliest)) %>%
  dplyr::distinct(system_period_earliest, colour)

vec_colour <- colours_strat$colour
names(vec_colour) <- colours_strat$system_period_earliest



# DEFINE CRS AND BOUNDING BOX FOR MAP -------------------------------------

# https://epsg.io/3035
projection_crs <- 3035

display_box_4326 <-
  sf::st_sfc(sf::st_point(c(-1, 54)),
             sf::st_point(c(42, 75)),
             crs = 4326)

display_box_proj <-
  sf::st_transform(display_box_4326,
                   crs = projection_crs) %>%
  sf::st_coordinates()



# MAKE AND SAVE PLOTS -----------------------------------------------------

# Plot with points coloured by stratigraphic system
plot_wells_map_COLOUR <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = background_polygon,
                   size = 0.1) +
  ggplot2::geom_sf(data = wellbores_in_group_sf,
                   mapping = ggplot2::aes(colour = system_period_earliest),
                   size = 1,
                   alpha = 0.8,
                   show.legend = TRUE) +
  ggplot2::scale_color_manual(values = vec_colour) +
  ggplot2::coord_sf(xlim = display_box_proj[,'X'], ylim = display_box_proj[,'Y'],
                    crs = projection_crs,
                    expand = FALSE,
                    datum = NA) +
  ggplot2::facet_wrap(dplyr::vars(strat_group_name),
                      nrow = 5,
                      strip.position = "top") +
  ggplot2::labs(title = "Stratigraphic groups penetrated on the Norwegian Continental Shelf",
                subtitle = "Exploration and development wells, NPD (2022)",
                caption = paste0("Data: https://factpages.npd.no (", current_day_slashes, "), Country outline: Natural Earth, Projection: EPSG:3035, Colours: ICS, v2022/02, Author: Sam Fielding, Licence: CC BY-SA")) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1,
                                                 title = "Earliest system for group",
                                                 title.position = "left")) +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(family = "ubuntu",
                                              color = "#4e4d47",
                                              size = 20 * 4.5),
                 
                 plot.margin = ggplot2::margin(t = 7, r = 0, b = 4, l = 0,
                                               unit = "mm"),
                 plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 plot.title = ggplot2::element_text(hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                       margin = ggplot2::margin(t = 1, r = 0, b = 5, l = 0,
                                                                                unit = "mm")),
                 plot.title.position = "plot",
                 
                 plot.caption = ggplot2::element_text(hjust = 0.5,
                                                      margin = ggplot2::margin(t = 2, r = 0, b = 0, l = 0,
                                                                               unit = "mm"),
                                                      colour = "#939184",
                                                      size = 10 * 4.5),
                 plot.caption.position = "plot",
                 
                 panel.border = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 axis.title.y = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 
                 strip.text.x = ggplot2::element_text(hjust = 1,
                                                      color = "#4e4d47",
                                                      size = 12 * 4.5),
                 strip.background = ggplot2::element_rect(fill = NA,
                                                          colour = NA),
                 
                 legend.title = ggplot2::element_text(margin = ggplot2::margin(r = 1,
                                                                               unit = "mm"),
                                                      size = 12 * 4.5),
                 legend.position = "bottom",
                 legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 legend.key = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 legend.spacing.x = ggplot2::unit(0, 'mm'),
                 legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -1,
                                                                              r = 1,
                                                                              unit = "mm"),
                                                     size = 12 * 4.5))

ggplot2::ggsave(filename = paste0("plot_wells_map_COLOUR", ".png"),
                plot = plot_wells_map_COLOUR,
                width = 250,
                height = 350,
                units = "mm",
                dpi = 600)

# Plot without colours for different stratigraphic systems
plot_wells_map_MONO <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = background_polygon,
                   size = 0.1) +
  ggplot2::geom_sf(data = wellbores_in_group_sf,
                   colour = "#305F89",
                   size = 1,
                   alpha = 0.8,
                   show.legend = FALSE) +
  ggplot2::coord_sf(xlim = display_box_proj[,'X'], ylim = display_box_proj[,'Y'],
                    crs = projection_crs,
                    expand = FALSE,
                    datum = NA) +
  ggplot2::facet_wrap(dplyr::vars(strat_group_name),
                      nrow = 5,
                      strip.position = "top") +
  ggplot2::labs(title = "Stratigraphic groups penetrated on the Norwegian Continental Shelf",
                subtitle = "Exploration and development wells, NPD (2022)",
                caption = paste0("Data: https://factpages.npd.no (", current_day_slashes, "), Country outline: Natural Earth, Projection: EPSG:3035, Colours: ICS, v2022/02, Author: Sam Fielding, Licence: CC BY-SA")) +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(family = "ubuntu",
                                              color = "#4e4d47",
                                              size = 20 * 4.5),
                 
                 plot.margin = ggplot2::margin(t = 7, r = 0, b = 4, l = 0,
                                               unit = "mm"),
                 plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 plot.title = ggplot2::element_text(hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                       margin = ggplot2::margin(t = 1, r = 0, b = 5, l = 0,
                                                                                unit = "mm")),
                 plot.title.position = "plot",
                 
                 plot.caption = ggplot2::element_text(hjust = 0.5,
                                                      margin = ggplot2::margin(t = 2, r = 0, b = 0, l = 0,
                                                                               unit = "mm"),
                                                      colour = "#939184",
                                                      size = 10 * 4.5),
                 plot.caption.position = "plot",
                 
                 panel.border = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 axis.title.y = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 
                 strip.text.x = ggplot2::element_text(hjust = 1,
                                                      color = "#4e4d47",
                                                      size = 12 * 4.5),
                 strip.background = ggplot2::element_rect(fill = NA,
                                                          colour = NA),
                 
                 legend.title = ggplot2::element_text(margin = ggplot2::margin(r = 1,
                                                                               unit = "mm"),
                                                      size = 12 * 4.5),
                 legend.position = "bottom",
                 legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 legend.key = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 legend.spacing.x = ggplot2::unit(0, 'mm'),
                 legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -1,
                                                                              r = 1,
                                                                              unit = "mm"),
                                                     size = 12 * 4.5))

ggplot2::ggsave(filename = paste0("plot_wells_map_MONO", ".png"),
                plot = plot_wells_map_MONO,
                width = 250,
                height = 350,
                units = "mm",
                dpi = 600)



