# ============================================================
# server_02_station_map.R
# Purpose: Selected station products, cross sections, rating curves, map behavior, sidebar, and module overview outputs.
# ============================================================
# BEGIN ORIGINAL BODY
  selected_measurements <- reactive({
    req(selected_code())
    get_station_measurements(con, selected_code())
  })
  
  selected_cross_sections <- reactive({
    req(selected_code())
    
    get_station_cross_sections(con, selected_code()) %>%
      dplyr::mutate(
        measurement_datetime = parse_app_datetime(measurement_datetime),
        last_update = parse_app_datetime(last_update),
        first_downloaded_at = parse_app_datetime(first_downloaded_at),
        last_downloaded_at = parse_app_datetime(last_downloaded_at),
        processed_at = parse_app_datetime(processed_at)
      )
  })
  
  selected_cross_section_vertices <- reactive({
    req(selected_code())
    
    get_station_cross_section_vertices(con, selected_code()) %>%
      dplyr::mutate(
        measurement_datetime = parse_app_datetime(measurement_datetime),
        last_update = parse_app_datetime(last_update),
        downloaded_at = parse_app_datetime(downloaded_at),
        processed_at = parse_app_datetime(processed_at),
        vertex_distance_m = as_numeric_app(vertex_distance_m),
        vertex_stage_cm = as_numeric_app(vertex_stage_cm)
      )
  })
  
  selected_cross_section_summary <- reactive({
    req(selected_code())
    
    get_station_cross_section_summary(con, selected_code()) %>%
      dplyr::mutate(
        first_cross_section_datetime = parse_app_datetime(first_cross_section_datetime),
        last_cross_section_datetime = parse_app_datetime(last_cross_section_datetime),
        first_downloaded_at = parse_app_datetime(first_downloaded_at),
        last_downloaded_at = parse_app_datetime(last_downloaded_at),
        processed_at = parse_app_datetime(processed_at)
      )
  })
  
  selected_cross_sections <- reactive({
    req(selected_code())
    
    get_station_cross_sections(con, selected_code()) %>%
      dplyr::mutate(
        cross_section_id = as.character(cross_section_id),
        measurement_datetime = parse_app_datetime(measurement_datetime),
        last_update = parse_app_datetime(last_update),
        first_downloaded_at = parse_app_datetime(first_downloaded_at),
        last_downloaded_at = parse_app_datetime(last_downloaded_at),
        processed_at = parse_app_datetime(processed_at),
        n_vertices = as_numeric_app(n_vertices),
        n_vertices_reported = as_numeric_app(n_vertices_reported),
        vertex_distance_min_m = as_numeric_app(vertex_distance_min_m),
        vertex_distance_max_m = as_numeric_app(vertex_distance_max_m),
        vertex_stage_min_cm = as_numeric_app(vertex_stage_min_cm),
        vertex_stage_max_cm = as_numeric_app(vertex_stage_max_cm)
      )
  })
  
  selected_cross_section_vertices_all <- reactive({
    req(selected_code())
    
    get_station_cross_section_vertices(con, selected_code()) %>%
      dplyr::mutate(
        cross_section_id = as.character(cross_section_id),
        cross_section_vertex_id = as.character(cross_section_vertex_id),
        measurement_datetime = parse_app_datetime(measurement_datetime),
        last_update = parse_app_datetime(last_update),
        downloaded_at = parse_app_datetime(downloaded_at),
        processed_at = parse_app_datetime(processed_at),
        vertex_order = as_numeric_app(vertex_order),
        vertex_distance_m = as_numeric_app(vertex_distance_m),
        vertex_stage_cm = as_numeric_app(vertex_stage_cm)
      )
  })
  
  selected_cross_section_summary <- reactive({
    req(selected_code())
    
    get_station_cross_section_summary(con, selected_code()) %>%
      dplyr::mutate(
        first_cross_section_datetime = parse_app_datetime(first_cross_section_datetime),
        last_cross_section_datetime = parse_app_datetime(last_cross_section_datetime),
        first_downloaded_at = parse_app_datetime(first_downloaded_at),
        last_downloaded_at = parse_app_datetime(last_downloaded_at),
        processed_at = parse_app_datetime(processed_at)
      )
  })
  
  cross_section_choice_data <- reactive({
    sections <- selected_cross_sections()
    
    if (nrow(sections) == 0) {
      return(tibble::tibble())
    }
    
    sections %>%
      dplyr::mutate(
        measurement_date = as.Date(measurement_datetime),
        section_label = paste0(
          ifelse(
            !is.na(measurement_date),
            format(measurement_date, "%Y-%m-%d"),
            "Sem data"
          ),
          ifelse(!is.na(survey_number), paste0(" | levantamento ", survey_number), ""),
          ifelse(!is.na(consistency_level), paste0(" | consist. ", consistency_level), "")
        )
      ) %>%
      dplyr::arrange(dplyr::desc(measurement_datetime), cross_section_id) %>%
      dplyr::select(cross_section_id, section_label, measurement_datetime)
  })
  
  output$cross_section_selector_ui <- renderUI({
    choice_data <- cross_section_choice_data()
    
    if (nrow(choice_data) == 0) {
      return(
        div(
          class = "empty-state",
          "Nenhuma seção transversal disponível para a estação selecionada."
        )
      )
    }
    
    choices <- stats::setNames(choice_data$cross_section_id, choice_data$section_label)
    
    selectInput(
      inputId = "selected_cross_section_id",
      label = "Seção transversal:",
      choices = choices,
      selected = choice_data$cross_section_id[[1]],
      width = "100%"
    )
  })
  
  selected_cross_section_id <- reactive({
    choice_data <- cross_section_choice_data()
    
    if (nrow(choice_data) == 0) {
      return(NA_character_)
    }
    
    selected_id <- input$selected_cross_section_id
    
    if (is.null(selected_id) || is.na(selected_id) || !(selected_id %in% choice_data$cross_section_id)) {
      return(choice_data$cross_section_id[[1]])
    }
    
    as.character(selected_id)
  })
  
  selected_cross_section_vertices <- reactive({
    section_id <- selected_cross_section_id()
    vertices <- selected_cross_section_vertices_all()
    
    if (is.na(section_id) || nrow(vertices) == 0) {
      return(tibble::tibble())
    }
    
    vertices %>%
      dplyr::filter(as.character(cross_section_id) == as.character(section_id)) %>%
      dplyr::filter(
        is.finite(vertex_distance_m),
        is.finite(vertex_stage_cm)
      ) %>%
      dplyr::arrange(vertex_order)
  })
  
  selected_cross_section_record <- reactive({
    section_id <- selected_cross_section_id()
    sections <- selected_cross_sections()
    
    if (is.na(section_id) || nrow(sections) == 0) {
      return(tibble::tibble())
    }
    
    sections %>%
      dplyr::filter(as.character(cross_section_id) == as.character(section_id)) %>%
      dplyr::slice(1)
  })
  
  selected_cross_section_date <- reactive({
    record <- selected_cross_section_record()
    
    if (nrow(record) == 0 || !"measurement_datetime" %in% names(record)) {
      return(as.Date(NA))
    }
    
    as.Date(record$measurement_datetime[[1]])
  })
  
  selected_cross_section_data <- reactive({
    selected_cross_sections()
  })
  
  make_rating_curve_points_for_cross_section <- function(curves, n_points = 120) {
    if (is.null(curves) || nrow(curves) == 0) {
      return(tibble::tibble())
    }
    
    required <- c(
      "stage_min_cm", "stage_max_cm",
      "coefficient_a", "coefficient_h0", "coefficient_n"
    )
    
    if (!all(required %in% names(curves))) {
      return(tibble::tibble())
    }
    
    curves <- curves %>%
      dplyr::mutate(
        rating_curve_id = as.character(rating_curve_id),
        rating_curve_segment_id = as.character(rating_curve_segment_id),
        segment_number = as_numeric_app(segment_number),
        stage_min_cm = as_numeric_app(stage_min_cm),
        stage_max_cm = as_numeric_app(stage_max_cm),
        coefficient_a = as_numeric_app(coefficient_a),
        coefficient_h0 = as_numeric_app(coefficient_h0),
        coefficient_n = as_numeric_app(coefficient_n),
        curve_segment_label = paste0(
          "Curva ", rating_curve_id,
          ifelse(!is.na(segment_number), paste0(" | seg. ", segment_number), "")
        )
      ) %>%
      dplyr::filter(
        is.finite(stage_min_cm),
        is.finite(stage_max_cm),
        is.finite(coefficient_a),
        is.finite(coefficient_h0),
        is.finite(coefficient_n),
        stage_max_cm > stage_min_cm
      )
    
    if (nrow(curves) == 0) {
      return(tibble::tibble())
    }
    
    points <- lapply(seq_len(nrow(curves)), function(i) {
      row <- curves[i, ]
      
      stage_cm <- seq(row$stage_min_cm, row$stage_max_cm, length.out = n_points)
      stage_m <- stage_cm / 100
      effective_stage_m <- stage_m - row$coefficient_h0
      
      discharge_m3s <- ifelse(
        effective_stage_m > 0,
        row$coefficient_a * (effective_stage_m ^ row$coefficient_n),
        NA_real_
      )
      
      tibble::tibble(
        rating_curve_id = row$rating_curve_id,
        rating_curve_segment_id = row$rating_curve_segment_id,
        curve_segment_label = row$curve_segment_label,
        stage_cm = stage_cm,
        discharge_m3s = discharge_m3s
      )
    })
    
    dplyr::bind_rows(points) %>%
      dplyr::filter(
        is.finite(stage_cm),
        is.finite(discharge_m3s),
        discharge_m3s >= 0
      )
  }
  
  selected_valid_rating_curve_segments_for_cross_section <- reactive({
    section_date <- selected_cross_section_date()
    curves <- selected_rating_curves()
    
    if (nrow(curves) == 0 || is.na(section_date)) {
      return(tibble::tibble())
    }
    
    curves <- curves %>%
      dplyr::mutate(
        valid_from = as.Date(parse_app_datetime(valid_from)),
        valid_to = as.Date(parse_app_datetime(valid_to))
      )
    
    curves %>%
      dplyr::filter(
        !is.na(valid_from),
        section_date >= valid_from,
        is.na(valid_to) | section_date <= valid_to
      )
  })
  
  selected_valid_rating_curve_points_for_cross_section <- reactive({
    make_rating_curve_points_for_cross_section(
      selected_valid_rating_curve_segments_for_cross_section()
    )
  })
  
  cross_section_top_stage_limits <- reactive({
    selected_vertices <- selected_cross_section_vertices()
    curve_points <- selected_valid_rating_curve_points_for_cross_section()
    
    values <- c(
      if ("vertex_stage_cm" %in% names(selected_vertices)) selected_vertices$vertex_stage_cm else numeric(),
      if ("stage_cm" %in% names(curve_points)) curve_points$stage_cm else numeric()
    )
    
    values <- values[is.finite(values)]
    
    if (length(values) == 0) {
      return(NULL)
    }
    
    stage_range <- range(values, na.rm = TRUE)
    padding <- max(1, diff(stage_range) * 0.05, na.rm = TRUE)
    
    c(stage_range[1] - padding, stage_range[2] + padding)
  })
  
  selected_rating_curves <- reactive({
    req(selected_code())
    get_station_rating_curves(con, selected_code())
  })
  
  selected_rating_curve_summary <- reactive({
    req(selected_code())
    get_station_rating_curve_summary(con, selected_code())
  })
  
  rating_curve_choice_data <- reactive({
    curves <- selected_rating_curves()
    
    if (nrow(curves) == 0) {
      return(tibble::tibble())
    }
    
    curves %>%
      dplyr::mutate(
        rating_curve_id = as.character(rating_curve_id),
        valid_from_date = as.Date(parse_app_datetime(valid_from)),
        valid_to_date = as.Date(parse_app_datetime(valid_to))
      ) %>%
      dplyr::group_by(rating_curve_id) %>%
      dplyr::summarise(
        valid_from_date = suppressWarnings(min(valid_from_date, na.rm = TRUE)),
        valid_to_date = suppressWarnings(max(valid_to_date, na.rm = TRUE)),
        n_segments = dplyr::n_distinct(rating_curve_segment_id),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        valid_from_label = ifelse(
          is.finite(as.numeric(valid_from_date)),
          format(valid_from_date, "%Y-%m-%d"),
          "sem início"
        ),
        valid_to_label = ifelse(
          is.finite(as.numeric(valid_to_date)),
          format(valid_to_date, "%Y-%m-%d"),
          "sem fim"
        ),
        curve_label = paste0(
          "CC ", rating_curve_id,
          " | ",
          valid_from_label,
          " a ",
          valid_to_label,
          " | ",
          n_segments,
          ifelse(n_segments == 1, " segmento", " segmentos")
        )
      ) %>%
      dplyr::arrange(valid_from_date, rating_curve_id)
  })
  
  output$rating_curve_selector_ui <- renderUI({
    choice_data <- rating_curve_choice_data()
    
    if (nrow(choice_data) == 0) {
      return(
        div(
          class = "empty-state",
          "Nenhuma curva-chave disponível para a estação selecionada."
        )
      )
    }
    
    choices <- c(
      "Todas" = "__all__",
      stats::setNames(choice_data$rating_curve_id, choice_data$curve_label)
    )
    
    selectInput(
      inputId = "selected_rating_curve_id",
      label = "Curva-chave:",
      choices = choices,
      selected = "__all__",
      width = "100%"
    )
  })
  
  selected_rating_curve_id_for_display <- reactive({
    selected_id <- input$selected_rating_curve_id
    
    if (is.null(selected_id) || is.na(selected_id) || identical(selected_id, "")) {
      return("__all__")
    }
    
    as.character(selected_id)
  })
  
  selected_rating_curves_for_plot <- reactive({
    curves <- selected_rating_curves()
    selected_id <- selected_rating_curve_id_for_display()
    
    if (nrow(curves) == 0 || identical(selected_id, "__all__")) {
      return(curves)
    }
    
    curves %>%
      dplyr::filter(as.character(rating_curve_id) == as.character(selected_id))
  })
  
  selected_rating_curve_summary_for_plot <- reactive({
    summary <- selected_rating_curve_summary()
    selected_id <- selected_rating_curve_id_for_display()
    
    if (nrow(summary) == 0 || identical(selected_id, "__all__")) {
      return(summary)
    }
    
    summary %>%
      dplyr::filter(as.character(rating_curve_id) == as.character(selected_id))
  })
  
  selected_measurements_for_rating_curve_plot <- reactive({
    measurements <- selected_measurements()
    selected_id <- selected_rating_curve_id_for_display()
    
    if (nrow(measurements) == 0 || identical(selected_id, "__all__")) {
      return(measurements)
    }
    
    curves <- selected_rating_curves_for_plot()
    
    if (nrow(curves) == 0 || !"valid_from" %in% names(curves)) {
      return(measurements)
    }
    
    validity <- curves %>%
      dplyr::mutate(
        valid_from_date = as.Date(parse_app_datetime(valid_from)),
        valid_to_date = as.Date(parse_app_datetime(valid_to))
      ) %>%
      dplyr::summarise(
        valid_from_date = suppressWarnings(min(valid_from_date, na.rm = TRUE)),
        valid_to_date = suppressWarnings(max(valid_to_date, na.rm = TRUE)),
        .groups = "drop"
      )
    
    if (!"measurement_datetime" %in% names(measurements)) {
      return(measurements)
    }
    
    measurements <- measurements %>%
      dplyr::mutate(
        measurement_date_for_filter = as.Date(parse_app_datetime(measurement_datetime))
      )
    
    if (is.finite(as.numeric(validity$valid_from_date[[1]]))) {
      measurements <- measurements %>%
        dplyr::filter(measurement_date_for_filter >= validity$valid_from_date[[1]])
    }
    
    if (is.finite(as.numeric(validity$valid_to_date[[1]]))) {
      measurements <- measurements %>%
        dplyr::filter(measurement_date_for_filter <= validity$valid_to_date[[1]])
    }
    
    measurements %>%
      dplyr::select(-measurement_date_for_filter)
  })
  
  rating_curve_summary_with_equations <- reactive({
    summary_table <- add_rating_curve_equation_display(selected_rating_curve_summary())
    curve_table <- add_rating_curve_equation_display(selected_rating_curves())
    
    summary_table <- as_display_table(summary_table)
    curve_table <- as_display_table(curve_table)
    
    # Prefer the segment-level table when available. This keeps each
    # rating-curve segment in its own row instead of collapsing multiple
    # segment equations into the same validity-window row.
    if (nrow(curve_table) > 0) {
      if (!"equation_display" %in% names(curve_table)) {
        curve_table$equation_display <- "—"
      }
      return(curve_table)
    }
    
    if (nrow(summary_table) == 0) {
      return(summary_table)
    }
    
    if (!"equation_display" %in% names(summary_table)) {
      summary_table$equation_display <- "—"
    }
    
    summary_table
  })
  
  filter_rating_curve_table_for_display <- function(data) {
    selected_id <- selected_rating_curve_id_for_display()
    
    if (is.null(data) || nrow(data) == 0 || identical(selected_id, "__all__")) {
      return(data)
    }
    
    curve_id_col <- first_existing_name(data, c("rating_curve_id", "curve_id"))
    
    if (!is.na(curve_id_col)) {
      return(
        data %>%
          dplyr::filter(as.character(.data[[curve_id_col]]) == as.character(selected_id))
      )
    }
    
    segment_id_col <- first_existing_name(data, c("rating_curve_segment_id", "segment_id"))
    
    if (!is.na(segment_id_col)) {
      selected_segments <- selected_rating_curves_for_plot()
      
      if (nrow(selected_segments) > 0 && "rating_curve_segment_id" %in% names(selected_segments)) {
        selected_segment_ids <- unique(as.character(selected_segments$rating_curve_segment_id))
        
        return(
          data %>%
            dplyr::filter(as.character(.data[[segment_id_col]]) %in% selected_segment_ids)
        )
      }
    }
    
    data
  }
  
  selected_diagnostics <- reactive({
    req(selected_code())
    
    withProgress(message = "Calculando diagnósticos da estação selecionada", value = 0.5, {
      run_on_demand_station_diagnostics(
        station_code = selected_code(),
        station_row = selected_station(),
        measurements = selected_measurements(),
        rating_curves = selected_rating_curves(),
        rating_curve_summary = selected_rating_curve_summary()
      )
    })
  })
  
  output$station_map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      setView(
        lng = app_config$map_default_lng,
        lat = app_config$map_default_lat,
        zoom = app_config$map_default_zoom
      )
  })
  
  add_station_marker_group <- function(map_proxy, data, group_label, color, radius, fill_opacity) {
    if (nrow(data) == 0) {
      return(map_proxy)
    }
    
    station_name_col <- first_existing_name(data, c("station_name", "name", "station"))
    uf_col <- first_existing_name(data, c("uf", "state", "state_abbrev"))
    municipality_col <- first_existing_name(data, c("municipality", "municipio", "city"))
    
    popup_name <- if (!is.na(station_name_col)) data[[station_name_col]] else data$station_code
    popup_uf <- if (!is.na(uf_col)) data[[uf_col]] else NA_character_
    popup_municipality <- if (!is.na(municipality_col)) data[[municipality_col]] else NA_character_
    
    data <- data %>%
      dplyr::mutate(
        popup_code = as.character(htmltools::htmlEscape(station_code)),
        popup_name = as.character(htmltools::htmlEscape(dplyr::coalesce(as.character(popup_name), "Estação sem nome"))),
        popup_municipality = ifelse(!is.na(popup_municipality), as.character(htmltools::htmlEscape(popup_municipality)), NA_character_),
        popup_uf = ifelse(!is.na(popup_uf), as.character(htmltools::htmlEscape(popup_uf)), NA_character_),
        popup_text = paste0(
          "<strong>", popup_code, "</strong><br>",
          popup_name,
          ifelse(!is.na(popup_municipality), paste0("<br>", popup_municipality), ""),
          ifelse(!is.na(popup_uf), paste0(" / ", popup_uf), ""),
          "<br><span>", group_label, "</span>"
        ),
        marker_label = paste0(station_code, " — ", dplyr::coalesce(as.character(popup_name), "Estação sem nome"))
      )
    
    map_proxy %>%
      addCircleMarkers(
        data = data,
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~station_code,
        group = group_label,
        radius = radius,
        stroke = TRUE,
        weight = 1,
        color = color,
        opacity = 0.85,
        fillColor = color,
        fillOpacity = fill_opacity,
        popup = ~popup_text,
        label = ~marker_label
      )
  }
  
  add_spatial_layer_if_present <- function(map_proxy, layers, layer_key) {
    if (!layer_key %in% names(layers)) {
      return(map_proxy)
    }
    
    layer_data <- layers[[layer_key]]
    
    if (is.null(layer_data) || nrow(layer_data) == 0) {
      return(map_proxy)
    }
    
    group_label <- spatial_map_groups[[layer_key]]
    color <- spatial_map_colors[[layer_key]]
    weight <- spatial_map_weights[[layer_key]]
    opacity <- spatial_map_opacity[[layer_key]]
    
    if (stringr::str_starts(layer_key, "rivers_")) {
      return(map_proxy %>%
               addPolylines(
                 data = layer_data,
                 group = group_label,
                 color = color,
                 weight = weight,
                 opacity = opacity
               ))
    }
    
    polygon_fill <- FALSE
    polygon_fill_color <- color
    polygon_fill_opacity <- 0
    
    if (identical(layer_key, "basins") && "basin_fill_color" %in% names(layer_data)) {
      polygon_fill <- TRUE
      polygon_fill_color <- ~basin_fill_color
      polygon_fill_opacity <- 0.15
    }
    
    map_proxy %>%
      addPolygons(
        data = layer_data,
        group = group_label,
        color = color,
        weight = weight,
        fill = polygon_fill,
        fillColor = polygon_fill_color,
        fillOpacity = polygon_fill_opacity,
        opacity = opacity,
        dashArray = spatial_map_dash_array[[layer_key]]
      )
  }
  
  observe({
    visible_groups <- normalize_map_station_layers(input$map_station_layers)
    
    visible_spatial_layers <- input$map_spatial_layers
    
    if (is.null(visible_spatial_layers)) {
      visible_spatial_layers <- intersect(spatial_map_default_layers, names(spatial_layers))
    }
    
    visible_spatial_layers <- intersect(
      spatial_map_layer_order,
      intersect(as.character(visible_spatial_layers), names(spatial_layers))
    )
    
    proxy <- leafletProxy("station_map")
    
    station_group_labels <- c(
      unlist(station_map_groups, use.names = FALSE),
      "Estação selecionada"
    )
    
    for (group_label in station_group_labels) {
      proxy <- proxy %>% clearGroup(group_label)
    }
    
    for (layer_key in names(spatial_map_groups)) {
      proxy <- proxy %>% clearGroup(spatial_map_groups[[layer_key]])
    }
    
    proxy <- proxy %>%
      clearMarkers() %>%
      clearControls()
    
    # Draw spatial context first. Spatial layers must be updated even when
    # all station layer checkboxes are unchecked.
    for (layer_key in visible_spatial_layers) {
      proxy <- add_spatial_layer_if_present(proxy, spatial_layers, layer_key)
    }
    
    river_legend_keys <- intersect(
      c("rivers_large", "rivers_medium", "rivers_small"),
      visible_spatial_layers
    )
    
    if (length(river_legend_keys) > 0) {
      proxy <- proxy %>%
        addLegend(
          position = "bottomleft",
          colors = unlist(spatial_map_colors[river_legend_keys]),
          labels = unlist(spatial_map_groups[river_legend_keys]),
          opacity = 0.85,
          title = "Rios"
        )
    }
    
    # If no station type is selected, keep only the spatial layers.
    if (length(visible_groups) == 0) {
      return(invisible(NULL))
    }
    
    map_data <- station_index %>%
      dplyr::filter(!is.na(latitude), !is.na(longitude))
    
    map_data <- map_data[
      station_matches_map_layers(map_data, visible_groups),
      ,
      drop = FALSE
    ]
    
    map_data$display_map_layer <- assign_station_display_layer(
      map_data,
      visible_groups
    )
    
    map_data <- map_data %>%
      dplyr::filter(!is.na(display_map_layer)) %>%
      dplyr::arrange(
        map_product_layer_priority_value(display_map_layer),
        station_code
      ) %>%
      dplyr::distinct(station_code, .keep_all = TRUE)
    
    group_flu_registration <- map_data %>%
      dplyr::filter(display_map_layer == "flu_registration")
    
    group_rainfall_registration <- map_data %>%
      dplyr::filter(display_map_layer == "rainfall_registration")
    
    group_flu_rainfall_registration <- map_data %>%
      dplyr::filter(display_map_layer == "flu_rainfall_registration")
    
    group_flu_with_data <- map_data %>%
      dplyr::filter(display_map_layer == "flu_with_data")
    
    group_rainfall_with_data <- map_data %>%
      dplyr::filter(display_map_layer == "rainfall_with_data")
    
    group_flu_rainfall_with_data <- map_data %>%
      dplyr::filter(display_map_layer == "flu_rainfall_with_data")
    
    # Draw broader registration layers first and more specific data layers last.
    proxy <- add_station_marker_group(
      proxy,
      group_flu_registration,
      station_map_groups$flu_registration,
      station_map_colors$flu_registration,
      radius = 2.0,
      fill_opacity = 0.25
    )
    
    proxy <- add_station_marker_group(
      proxy,
      group_rainfall_registration,
      station_map_groups$rainfall_registration,
      station_map_colors$rainfall_registration,
      radius = 1.8,
      fill_opacity = 0.18
    )
    
    proxy <- add_station_marker_group(
      proxy,
      group_flu_rainfall_registration,
      station_map_groups$flu_rainfall_registration,
      station_map_colors$flu_rainfall_registration,
      radius = 2.0,
      fill_opacity = 0.25
    )
    
    proxy <- add_station_marker_group(
      proxy,
      group_flu_with_data,
      station_map_groups$flu_with_data,
      station_map_colors$flu_with_data,
      radius = 3.2,
      fill_opacity = 0.70
    )
    
    proxy <- add_station_marker_group(
      proxy,
      group_rainfall_with_data,
      station_map_groups$rainfall_with_data,
      station_map_colors$rainfall_with_data,
      radius = 2.7,
      fill_opacity = 0.50
    )
    
    proxy <- add_station_marker_group(
      proxy,
      group_flu_rainfall_with_data,
      station_map_groups$flu_rainfall_with_data,
      station_map_colors$flu_rainfall_with_data,
      radius = 3.5,
      fill_opacity = 0.75
    )
    
    legend_keys <- names(station_map_groups)[
      names(station_map_groups) %in% unique(map_data$display_map_layer)
    ]
    
    legend_keys <- legend_keys[
      order(map_product_layer_priority_value(legend_keys))
    ]
    
    if (length(legend_keys) > 0) {
      proxy <- proxy %>%
        addLegend(
          position = "bottomright",
          colors = unlist(station_map_colors[legend_keys]),
          labels = unlist(station_map_groups[legend_keys]),
          opacity = 0.8,
          title = "Tipo de postos"
        )
    }
  })
  
  observeEvent(input$station_map_marker_click, {
    click <- input$station_map_marker_click
    clicked_code <- as.character(click$id)
    
    if (length(clicked_code) == 1 && !is.na(clicked_code) && clicked_code %in% station_index$station_code) {
      request_station_change(clicked_code, update_selector = TRUE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$cancel_station_change, {
    pending_station_code(NULL)
    removeModal()
    
    updateSelectizeInput(
      session,
      inputId = "station_select",
      choices = station_choices_with_selected(selected_station_code()),
      selected = selected_station_code(),
      server = TRUE
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$confirm_station_change, {
    new_code <- pending_station_code()
    
    if (is.null(new_code) || is.na(new_code) || new_code == "") {
      removeModal()
      return(invisible(NULL))
    }
    
    pending_station_code(NULL)
    removeModal()
    
    loaded_session_station_code(NULL)
    loaded_session_data_type(NULL)
    
    selected_station_code(as.character(new_code))
    
    updateSelectizeInput(
      session,
      inputId = "station_select",
      choices = station_choices_with_selected(new_code),
      selected = new_code,
      server = TRUE
    )
    
    go_to_acquisition_for_station(new_code)
  }, ignoreInit = TRUE)
  
  observe({
    
    visible_groups <- normalize_map_station_layers(input$map_station_layers)
    
    leafletProxy("station_map") %>%
      clearGroup("Estação selecionada")
    
    if (length(visible_groups) == 0) {
      return(invisible(NULL))
    }
    
    station <- selected_station()
    req(nrow(station) == 1)
    req(!is.na(station$latitude), !is.na(station$longitude))
    
    leafletProxy("station_map") %>%
      clearGroup("Estação selecionada") %>%
      addCircleMarkers(
        data = station,
        lng = ~longitude,
        lat = ~latitude,
        group = "Estação selecionada",
        radius = 11,
        stroke = TRUE,
        weight = 3,
        color = station_map_colors$selected,
        opacity = 1,
        fillColor = station_map_colors$selected,
        fillOpacity = 0.12,
        layerId = ~paste0("selected_", station_code),
        popup = ~paste0("<strong>Estação selecionada</strong><br>", station_code)
      )
  })
  
  observeEvent(selected_code(), {
    station <- selected_station()
    if (nrow(station) == 1 && !is.na(station$latitude) && !is.na(station$longitude)) {
      leafletProxy("station_map") %>%
        flyTo(
          lng = station$longitude[[1]],
          lat = station$latitude[[1]],
          zoom = app_config$selected_station_zoom
        )
    }
  }, ignoreInit = TRUE)
  
  output$station_title <- renderUI({
    station <- selected_station()
    title <- station_display_title(station)
    
    tagList(
      div(
        class = "selected-station-title",
        span(class = "eyebrow", "Estação selecionada"),
        h3(title$title),
        p(title$subtitle)
      )
    )
  })
  
  output$station_kpis <- renderUI({
    station <- selected_station()
    kpis <- station_kpi_fields(station)
    
    tags$div(
      class = "kpi-grid",
      purrr::pmap(
        kpis,
        function(label, value) {
          tags$div(
            class = "kpi-card",
            tags$span(class = "kpi-label", label),
            tags$strong(class = "kpi-value", value)
          )
        }
      )
    )
  })
  
  station_info_list_ui <- function(metadata, compact = FALSE) {
    if (nrow(metadata) == 0) {
      return(tags$p(class = "muted-note", "Nenhuma informação cadastral disponível."))
    }
    
    tags$div(
      class = ifelse(isTRUE(compact), "station-info-list compact", "station-info-list"),
      purrr::pmap(
        metadata,
        function(field, label, value) {
          tags$div(
            class = "station-info-row",
            tags$span(class = "station-info-label", label),
            tags$span(class = "station-info-value", value)
          )
        }
      )
    )
  }
  
  output$station_metadata <- renderUI({
    station <- selected_station()
    metadata <- station_metadata_fields(station)
    station_info_list_ui(metadata, compact = TRUE)
  })
  
  output$station_metadata_details <- renderUI({
    station <- selected_station()
    metadata <- station_metadata_detail_fields(station)
    station_info_list_ui(metadata, compact = FALSE)
  })
  
  output$station_availability_badges <- renderUI({
    station <- selected_station()
    badges <- station_availability_badge_fields(station)
    
    tags$div(
      class = "availability-list",
      purrr::pmap(
        badges,
        function(label, available, status) {
          tags$div(
            class = paste("availability-row", ifelse(isTRUE(available), "available", "unavailable")),
            tags$span(
              class = "availability-icon",
              title = ifelse(isTRUE(available), "Disponível", "Não disponível"),
              status
            ),
            tags$span(class = "availability-label", label)
          )
        }
      )
    )
  })
  
  output$station_attention <- renderUI({
    station <- selected_station()
    attention <- station_attention_fields(station)
    station_info_list_ui(attention, compact = FALSE)
  })
  
  clean_error_message <- function(message) {
    message <- gsub("\033\\[[0-9;]*m", "", as.character(message))
    message <- gsub("\n", " ", message)
    message
  }
  
  draw_empty_plot <- function(message) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    message <- paste(strwrap(clean_error_message(message), width = 70), collapse = "\n")
    text(0.5, 0.5, message, cex = 0.95)
    invisible(NULL)
  }
  
  preview_plot_theme <- function(base_size = 12) {
    # base_size_corrigido = base_size_original * 72 / res
    theme_minimal(base_size = base_size) +
      theme(
        plot.title = element_text(face = "bold", size = base_size + 4),
        plot.subtitle = element_text(size = base_size + 1),
        plot.caption = element_text(size = base_size - 2, color = "grey35"),
        axis.title = element_text(size = base_size + 3, face = "bold"),
        axis.text = element_text(size = base_size + 2),
        legend.title = element_text(face = "bold"),
        legend.text=element_text(size=base_size + 2),
        legend.position = "bottom",
        panel.grid.minor = element_line(linewidth = 0.25),
        panel.grid.major = element_line(linewidth = 0.45),
        axis.line = element_line(linewidth = 0.35, color = "black"),
        axis.ticks =  element_line(linewidth = 0.25, color = "black")
        
      )
  }
  
  logical_flag_column <- function(data, candidates, fallback = NULL) {
    if (is.null(fallback)) {
      fallback <- rep(FALSE, nrow(data))
    }
    
    column <- first_existing_name(data, candidates)
    if (is.na(column)) {
      return(as.logical(fallback))
    }
    
    result <- coerce_logical_indicator(data[[column]])
    result[is.na(result)] <- FALSE
    result
  }
  
  prepare_measurement_plot_data <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(tibble::tibble())
    }
    
    data <- dplyr::as_tibble(data)
    
    stage_col <- first_existing_name(data, c("stage_cm", "stage_cm_app", "stage", "cota_cm", "cota", "water_level_cm", "h_cm"))
    discharge_col <- first_existing_name(data, c("discharge_m3s", "discharge_m3s_app", "discharge", "vazao", "flow_m3s", "q_m3s"))
    date_col <- first_existing_name(data, c("measurement_date", "measurement_datetime", "measurement_datetime_app", "date", "datetime"))
    wetted_area_col <- first_existing_name(data, c("wetted_area_m2", "area_molhada_m2", "area_molhada", "area"))
    width_col <- first_existing_name(data, c("width_m", "largura_m", "largura"))
    mean_depth_col <- first_existing_name(data, c("mean_depth_m", "profundidade_media_m", "profundidade_media"))
    mean_velocity_col <- first_existing_name(data, c("mean_velocity_ms", "mean_velocity_m_s", "mean_velocity", "velocity_ms", "velocity_m_s", "velocidade_media_ms", "velocidade_media"))
    
    data$stage_cm <- if (!is.na(stage_col)) as_numeric_app(data[[stage_col]]) else NA_real_
    data$discharge_m3s <- if (!is.na(discharge_col)) as_numeric_app(data[[discharge_col]]) else NA_real_
    data$wetted_area_m2 <- if (!is.na(wetted_area_col)) as_numeric_app(data[[wetted_area_col]]) else NA_real_
    data$width_m <- if (!is.na(width_col)) as_numeric_app(data[[width_col]]) else NA_real_
    data$mean_depth_m <- if (!is.na(mean_depth_col)) as_numeric_app(data[[mean_depth_col]]) else NA_real_
    data$mean_velocity_ms <- if (!is.na(mean_velocity_col)) as_numeric_app(data[[mean_velocity_col]]) else NA_real_
    
    if (!is.na(date_col)) {
      if (inherits(data[[date_col]], "Date")) {
        data$measurement_date <- as.Date(data[[date_col]])
      } else {
        data$measurement_date <- as.Date(parse_app_datetime(data[[date_col]]))
      }
    } else {
      data$measurement_date <- as.Date(NA)
    }
    
    year_col <- first_existing_name(data, c("measurement_year", "year", "ano"))
    data$measurement_year <- if (!is.na(year_col)) {
      as.integer(as_numeric_app(data[[year_col]]))
    } else {
      as.integer(format(data$measurement_date, "%Y"))
    }
    
    data$stage_zero_or_negative_flag <- logical_flag_column(
      data,
      c("stage_zero_or_negative_flag", "flag_stage_le_zero", "stage_le_zero_flag"),
      !is.na(data$stage_cm) & data$stage_cm <= 0
    )
    data$discharge_zero_or_negative_flag <- logical_flag_column(
      data,
      c("discharge_zero_or_negative_flag", "flag_discharge_le_zero", "discharge_le_zero_flag"),
      !is.na(data$discharge_m3s) & data$discharge_m3s <= 0
    )
    data$repeated_stage_variable_discharge_flag <- logical_flag_column(
      data,
      c("repeated_stage_variable_discharge_flag", "same_stage_variable_discharge_flag", "repeated_stage_flag"),
      rep(FALSE, nrow(data))
    )
    data$repeated_discharge_variable_stage_flag <- logical_flag_column(
      data,
      c("repeated_discharge_variable_stage_flag", "same_discharge_variable_stage_flag", "repeated_discharge_flag"),
      rep(FALSE, nrow(data))
    )
    
    data %>%
      dplyr::mutate(
        approx_wetted_perimeter_m = ifelse(!is.na(width_m) & !is.na(mean_depth_m), width_m + 2 * mean_depth_m, NA_real_),
        approx_hydraulic_radius_m = ifelse(!is.na(wetted_area_m2) & !is.na(approx_wetted_perimeter_m) & approx_wetted_perimeter_m > 0, wetted_area_m2 / approx_wetted_perimeter_m, NA_real_),
        area_rh_two_thirds = ifelse(!is.na(wetted_area_m2) & !is.na(approx_hydraulic_radius_m) & approx_hydraulic_radius_m > 0, wetted_area_m2 * (approx_hydraulic_radius_m ^ (2 / 3)), NA_real_)
      )
  }
  
  measurement_plot_data <- reactive({
    diagnostics <- selected_diagnostics()
    plot_data <- NULL
    
    if (!is.null(diagnostics) && !is.null(diagnostics$measurement_flags) && nrow(diagnostics$measurement_flags) > 0) {
      plot_data <- diagnostics$measurement_flags
    } else {
      plot_data <- selected_measurements()
    }
    
    prepare_measurement_plot_data(plot_data)
  })
  
  first_metric_value <- function(table, candidates) {
    if (is.null(table) || nrow(table) == 0) {
      return(NA)
    }
    column <- first_existing_name(table, candidates)
    if (is.na(column)) {
      return(NA)
    }
    table[[column]][[1]]
  }
  
  metric_card <- function(label, value, note = NULL, class = "") {
    tags$div(
      class = paste("diagnostic-card", class),
      tags$span(class = "diagnostic-card-label", label),
      tags$strong(class = "diagnostic-card-value", value),
      if (!is.null(note)) tags$p(class = "diagnostic-card-note", note)
    )
  }
  
  overview_metric <- function(label, value, note = NULL) {
    tags$div(
      class = "overview-metric",
      tags$span(class = "overview-metric-label", label),
      tags$strong(class = "overview-metric-value", value),
      if (!is.null(note)) tags$small(note)
    )
  }
  
  output$discharge_rating_overview <- renderUI({
    station <- selected_station()
    title <- station_display_title(station)
    kpis <- station_kpi_fields(station)
    metadata <- station_metadata_fields(station)
    
    measurement_period <- metadata %>%
      dplyr::filter(field %in% c("discharge_start_date", "discharge_end_date")) %>%
      dplyr::select(label, value)
    
    tags$div(
      class = "subsystem-overview",
      tags$div(
        class = "overview-intro-card",
        tags$h4(title$title),
        tags$p(title$subtitle),
        tags$p("Este ambiente reúne os produtos atualmente implementados: medições de descarga, curvas-chave, seções transversais e diagnósticos de triagem.")
      ),
      tags$div(
        class = "overview-metric-grid",
        purrr::pmap(
          kpis,
          function(label, value) overview_metric(label, value)
        )
      ),
      tags$div(
        class = "overview-note-card",
        tags$strong("Período das medições"),
        if (nrow(measurement_period) == 0) {
          tags$p("Período não informado nos metadados disponíveis.")
        } else {
          tags$ul(
            purrr::pmap(
              measurement_period,
              function(label, value) tags$li(tags$strong(paste0(label, ": ")), value)
            )
          )
        }
      )
    )
  })
  
  future_module_overview <- function(title, description, availability_field = NULL) {
    station <- selected_station()
    station_title <- station_display_title(station)
    
    available <- if (is.null(availability_field)) {
      NA
    } else {
      station_indicator_value(station, flag_candidates = availability_field)
    }
    
    availability_text <- if (is.na(available)) {
      "Disponibilidade específica ainda não avaliada nesta versão."
    } else if (isTRUE(available)) {
      "Há indicação de disponibilidade para este tipo de dado nos metadados da estação."
    } else {
      "Não há indicação de disponibilidade para este tipo de dado nos metadados atuais."
    }
    
    tags$div(
      class = "section-card placeholder-card",
      tags$h3(title),
      tags$p(description),
      tags$div(
        class = "overview-note-card",
        tags$strong("Estação selecionada"),
        tags$p(paste0(station_title$title, " — ", station_title$subtitle)),
        tags$p(availability_text)
      ),
      tags$div(
        class = "limitation-box",
        "Módulo planejado para etapa futura. A estrutura já está reservada para manter o sistema integrado em um único app."
      )
    )
  }
  

