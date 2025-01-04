
# Load libraries
app_packs <- c('data.table', 'sf', 'tidyverse', 'ggiraph', 'shiny',
               'shinydashboard', 'shinyWidgets', 'igraph', 'scales',
               'plotly', 'patchwork', 'leaflet', 'leaflet.extras')
mapply(library, app_packs, character.only=TRUE)

# Load data
# -> Our processed data was written to a CSV file to save
# the need to redo the whole pre-processing.
data_path <- 'data/clean_nyc311_data.csv'
nyc_data <- fread(data_path)
# Dates always have to be converted!
nyc_data$date_created <- as.Date(nyc_data$date_created)

# Read map data
shapefile_path <- 'data/nybb.shp'
nyc_boroughs <- st_read(shapefile_path)
nyc_boroughs <- st_transform(nyc_boroughs, crs = 4326)

# Split data for the networks
unsanitary_data <- nyc_data[complaint_type == 'Unsanitary Condition', ]
related_data <- nyc_data[complaint_type != 'Unsanitary Condition', ]

ex_complaint_types <- c('Dirty Conditions', 'Unsanitary Condition',
                        'Food Establishment', 'Sanitation Condition',
                        'Industrial Waste', 'Litter Basket / Request', 
                        'Overflowing Litter Baskets', 'Rodent',
                        'Sewer', 'Water Quality')
# Define the UI
app_ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = "NYC 311 Data"),
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput("date_range",
                     "Specify a date range:",
                     start = min(nyc_data$date_created),
                     end = max(nyc_data$date_created)
                     ),
      pickerInput("complaint_types",
                  "Select the complaint types to consider:",
                  choices = ex_complaint_types,
                  multiple = TRUE,
                  options = list(`live-search` = TRUE, `actions-box` = TRUE, size = 10),
                  selected = ex_complaint_types[1:5]
                  ),
      prettyCheckboxGroup("boroughs",
                          "Select Borough(s):",
                          choices = unique(nyc_data$borough),
                          selected = unique(nyc_data$borough),
                          animation = "smooth", status = "info"
                          ),
      menuItem("Overview", tabName = "overview",
               icon = icon('magnifying-glass-chart')),
      menuItem('Performance Analysis', tabName = 'agencies',
               icon = icon('clock')),
      menuItem('Complaint Description', tabName = 'descrip',
               icon = icon('chart-bar')),
      menuItem('Time-Series Analysis', tabName = 'time_series_anal',
               icon = icon('chart-line')),
      menuItem('Interactive Networks', tabName = 'interactive_networks',
               icon = icon('diagram-project')),
      menuItem('Geo-Spatial Analysis', tabName = 'geo',
               icon = icon('map-location-dot'))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview UI
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = 'Complaints per borough by complaint type',
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("borough_dist_plot")
            ),
          box(
            title = "Distribution of complaints",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("complaint_dist_plot")
            )
        ),
        fluidRow(
          box(
            title = 'Average Resolution Time by complaint type',
            status = 'primary',
            solidHeader = TRUE,
            plotlyOutput('resolution_rate_plot')
            ),
          box(
            title = 'Complaint Moments Heatmap',
            status = 'primary',
            solidHeader = TRUE,
            plotlyOutput('complaints_heatmap')
            )
        )
      ),
      # Agencies and Performance UI
      tabItem(
        tabName = 'agencies',
        fluidRow(
          box(
            title = 'Complaints by agency',
            status = 'primary',
            solidHeader = TRUE,
            width = 8,
            height = 600,
            plotlyOutput('agencies_per_complaint', height = "500px")
            )
        ),
        fluidRow(
          box(
            title = 'Resolution Time Analysis',
            status = 'primary',
            solidHeader = TRUE,
            width = 8,
            height = 600,
            plotlyOutput('agencies_resolution_borough', height = "500px")
            )
        )
      ),
      # Description Analysis UI
      tabItem(
        tabName = 'descrip',
        fluidRow(
          box(
            title = 'Location Types for the complaints',
            status = 'primary',
            solidHeader = TRUE,
            width = 8,
            height = 600,
            plotlyOutput('hotspots_complaints', height = "500px")
            )
        ),
        fluidRow(
          box(
            title = 'Complaint descriptions',
            status = 'primary',
            solidHeader = TRUE,
            width = 8,
            height = 600,
            plotlyOutput('complaints_descript', height = "500px")
            )
        )
      ),
      # Time-Series UI
      tabItem(
        tabName = 'time_series_anal',
        fluidPage(
          titlePanel("Time-Series Analysis of Sanitation complaints"),
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                inputId = "timeline",
                label = "Toggle the desired timeline:",
                choices = list("Per day of the month" = "daily",
                               "Per day of the week" = "weekly",
                               "Per hour of the day" = "hourly"),
                selected = "daily"
              )
            ),
            mainPanel(
              plotOutput(outputId = "time_series", height = "600px")
            )
          )
        )
      ),
      # Interactive Networks UI
      tabItem(
        tabName = 'interactive_networks',
        fluidPage(
          titlePanel(HTML('Interactive Networks of Sanitation complaints<br>("Unsanitary Condition" and related complaint types)')),
          h4('> Hover your mouse above the map to retrieve information!'),
          h4('> Interactive features must be toggled from the internal sidebar below.'),
          sidebarLayout(
            sidebarPanel(
              radioButtons(inputId = 'graph_type',
                           label = 'Toggle the desired network:',
                           choices = list('"Unsanitary Condition"' = 'unsanitary',
                                          'Related complaint types' = 'related'),
                           selected = 'unsanitary'
                           ),
              conditionalPanel(
                condition = 'input.graph_type == "unsanitary"',
                sliderInput(inputId = 'unsanitary_threshold',
                            label = 'Define a threshold for Shared Complaints:',
                            min = 4000, max = 40000, value = 20000)
                              ),
              conditionalPanel(
                condition = 'input.graph_type == "unsanitary"',
                dateRangeInput(inputId = 'unsanitary_dates',
                               label = 'Specify a date range:',
                               start = min(unsanitary_data$date_created),
                               end = max(unsanitary_data$date_created))
                              ),
              conditionalPanel(
                condition = 'input.graph_type == "related"',
                sliderInput(inputId = 'related_threshold',
                            label = 'Define a threshold for Shared Complaints:',
                            min = 10, max = 10000, value = 5000)
                              ),
              conditionalPanel(
                condition = 'input.graph_type == "related"',
                checkboxGroupInput(inputId = 'complaint_types',
                                   label = 'Select the complaint types to consider:',
                                   choices = unique(related_data$complaint_type),
                                   selected = unique(related_data$complaint_type))
                              ),
              conditionalPanel(
                condition = 'input.graph_type == "related"',
                dateRangeInput(inputId = 'related_dates',
                               label = 'Specify a date range:',
                               start = min(related_data$date_created),
                               end = max(related_data$date_created))
                              ),
            ),
            mainPanel(
              girafeOutput(outputId = 'network', height = '600px')
                     )
          )
        )
      ),
      # Geo-Spatial Analysis UI
      tabItem(
        tabName = 'geo',
        fluidRow(
          box(
            title = 'Complaint Density by Location (Street)',
            solidHeader = TRUE,
            status = 'primary',
            width = 12,
            leafletOutput('hotspot_map', height = '600px')
            )
        ),
        fluidRow(
          box(
            title = 'Complaint Density per Borough',
            status = 'info',
            solidHeader = TRUE,
            width = 12,
            plotlyOutput('density_plot', height = '400px')
            )
        )
      )
    )
  )
)

# Define Server Logic
app_server <- function(input, output, session) {
  # -> Complaint Distribution Plot
  output$complaint_dist_plot <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    complaint_dist <- filtered_data %>%
      count(complaint_type, sort = TRUE)
    
    plot_ly(data = complaint_dist,
            x = ~reorder(complaint_type, n),
            y = ~n,
            type = "bar",
            marker = list(color = "viridis") 
            ) %>%
      layout(
        xaxis = list(title = "Complaint Type", tickangle = 45),
        yaxis = list(title = "Number of Complaints"),
        margin = list(b = 100)
        )
  })
  # -> Borough Complaints per Complaint Type
  output$borough_dist_plot <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    borough_complaints <- filtered_data %>%
      count(borough, complaint_type) 
    
    plot_ly(borough_complaints, x= ~borough, y= ~n,
            type = 'bar', color=~complaint_type,
            colors = 'viridis',
            text = ~paste(complaint_type, ':', n),
            hoverinfo='text'
            ) %>%
      layout(
        barmode = 'stack',
        xaxis = list(title = 'Borough', tickangle = 45),
        yaxis = list(title = 'Number of Complaints'),
        showlegend = TRUE
        )
  })
  # -> Resolution Rate over time
  output$resolution_rate_plot <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created, resolution_time) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    avg_res_time <- filtered_data %>%
      group_by(complaint_type) %>%
      summarise(avg_time = mean(resolution_time, na.rm = TRUE))
    
    plot_ly(avg_res_time, 
            x = ~complaint_type, 
            y = ~avg_time, 
            type = "bar", 
            name = "Avg Resolution Time",
            marker = list(color = 'lightblue')
            ) %>%
      layout(
        xaxis = list(title = "Complaint Type", tickangle = 45),
        yaxis = list(title = "Average Resolution Time (hours)"),
        showlegend = FALSE
        )
  })
  # -> Complaint Heatmap (time of the day VS. day of week)
  output$complaints_heatmap <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    heatmap_data <- filtered_data %>%
      count(weekday_created, hour_created)
    
    plot_ly(data = heatmap_data,
            x = ~hour_created,
            y = ~weekday_created,
            z = ~n,
            type = "heatmap",
            colors = "Reds"
            ) %>%
      layout(
        xaxis = list(title = "Hour of the Day"),
        yaxis = list(title = "Day of the Week"),
        colorbar = list(title = "Complaints Count")
        )
  })
  # -> Agency Per Complaint
  output$agencies_per_complaint <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created, agency_name) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    filtered_data %>%
      group_by(agency_name, complaint_type) %>%
      summarise(total_complaints = n(), .groups = 'drop') %>%
      plot_ly(x = ~agency_name,
              y = ~total_complaints,
              color = ~complaint_type,
              type = 'bar'
              ) %>%
      layout(
        xaxis = list(title = 'Agency', tickangle = 45, tickfont = list(size = 13)),
        yaxis = list(title = 'Number of Complaints',tickfont = list(size = 13)),
        barmode = 'stack',
        showlegend = TRUE
        )
  })
  # -> Resolution time
  output$agencies_resolution_borough <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created, agency_name,
             resolution_time) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    agency_performance <- filtered_data %>%
      group_by(agency_name, borough) %>%
      summarise(total_complaints = n(),
                avg_resolution_time = mean(resolution_time,
                                           na.rm = TRUE),
                .groups = 'drop') %>%
      filter(!is.na(avg_resolution_time))
    
    plot_ly(data = agency_performance,
            x = ~agency_name,
            y = ~avg_resolution_time,
            color = ~borough,
            size = ~total_complaints,
            type = "scatter",
            mode = "markers",
            marker = list(
              opacity = 0.7,
              sizeref = 0.5 * max(agency_performance$total_complaints) / (40^2),
              sizemode = "area",
              sizemin = 10,
              size = ~sqrt(total_complaints) * 2
              )) %>%
      layout(
        xaxis = list(title = "Agency",
                     tickangle = 45,
                     tickfont = list(size = 13)
                     ),
        yaxis = list(title = "Average Resolution Time (Hours)",
                     tickfont = list(size = 13)
                     ),
        showlegend = TRUE,
        legend = list(
          title = list(text = "Borough")
          )
      )
  })
  # -> Geographic
  output$hotspot_map <- renderLeaflet({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created, street_name,
             lat, lon) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    # Filter data for map visualization
    map_data <- filtered_data %>%
      filter(!is.na(street_name))
    
    street_counts <- map_data %>%
      group_by(street_name, borough) %>%
      summarize(lat = first(lat),
                lon = first(lon),
                count = n(), 
                .groups = "drop")
    
    pal <- colorNumeric(
      palette = 'YlOrRd',
      domain = street_counts$count
      )
    
    # Create the map
    leaflet(street_counts) %>%
      addTiles() %>% 
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~sqrt(count) * 2, 
        color = ~pal(count),
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>Street:</b> ", street_name, "<br>",
          "<b>Complaint Count:</b> ", count
          ),
        group = "Complaints") %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~count,
        title = "Complaint Count",
        opacity = 1
        )
  })
  # -> Density Plot Geo
  output$density_plot <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created, population_count) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    density_data <- filtered_data %>%
      group_by(borough, complaint_type) %>%
      summarise(
        complaints_per_capita = n() / mean(population_count, na.rm = TRUE) * 1000,
        .groups = 'drop'
        )
    
    plot_ly(density_data,
            x = ~borough,
            y = ~complaints_per_capita,
            color = ~complaint_type,
            type = "bar") %>%
      layout(
        xaxis = list(title = "Borough"),
        yaxis = list(title = "Complaints per 1000 residents")
        )
  })
  # -> Data Description
  output$hotspots_complaints <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created, location_type) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    hotspots <- filtered_data %>%
      group_by(borough, complaint_type, location_type) %>%
      summarise(count = n(), .groups = 'drop')
    
    plot_ly(hotspots,
            x=~count,
            y = ~location_type,
            color = ~complaint_type,
            type = 'bar',
            orientation = 'h',
            text = ~paste('Borough:', borough, '<br>Number of Complaints: ', count),
            hoverinfo = 'text') %>%
      layout(
        barmode='stack',
        xaxis = list(title = 'Number of Complaints'),
        yaxis = list(title = 'Location Type',
                     categoryorder = "total ascending")
        )
  })
  # -> Complaint Descripts
  output$complaints_descript <- renderPlotly({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created, complaint_descript) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    descript <- filtered_data %>%
      group_by(borough, complaint_type, complaint_descript) %>%
      summarise(count = n(), .groups = 'drop')
    
    plot_ly(descript,
            x=~count,
            y = ~complaint_descript,
            color = ~complaint_type,
            type = 'bar',
            orientation = 'h',
            text = ~paste('Borough:', borough, '<br>Number of Complaints: ', count),
            hoverinfo = 'text') %>%
      layout(
        barmode='stack',
        xaxis = list(title = 'Number of Complaints'),
        yaxis = list(title = 'Complaint Descript',
                     categoryorder = "total ascending")
        )
  })
  # -> Time Series Plot
  output$time_series <- renderPlot({
    # Filter data based on the inputs
    filtered_data <- nyc_data %>%
      select(borough, complaint_type, date_created,
             weekday_created, hour_created) %>%
      filter(date_created >= input$date_range[1],
             date_created <= input$date_range[2],
             complaint_type %in% input$complaint_types,
             borough %in% input$boroughs)
    
    # Aggregate data based on timeline
    timeline_data <- switch(
      input$timeline,
      "daily" = filtered_data %>%
        group_by(date_created, borough, complaint_type) %>%
        summarise(complaints = n(), .groups = "drop"),
      "weekly" = filtered_data %>%
        group_by(weekday_created, borough, complaint_type) %>%
        summarise(complaints = n(), .groups = "drop"),
      "hourly" = filtered_data %>%
        group_by(hour_created, borough, complaint_type) %>%
        summarise(complaints = n(), .groups = "drop"))
    
    # Determine the maximum y position for each row of plots
    max_y_values <- timeline_data %>%
      group_by(borough) %>%
      summarise(max_complaints = max(complaints, na.rm = TRUE))
    
    # Calculate the overall maximum y value for determining horizontal cuts
    overall_max_y <- max(max_y_values$max_complaints)
    
    # Plot the data
    plot <- ggplot(data = timeline_data, aes_string(
      x = ifelse(input$timeline == "daily", 'date_created',
                 ifelse(input$timeline == "weekly", 'weekday_created',
                        'hour_created')),
      y = "complaints")) +
      geom_line(aes(group = interaction(borough, complaint_type)),
                linewidth = 0.9, color = "royalblue") +
      facet_grid(borough ~ complaint_type, scales = "free_y", space = 'free') +
      labs(
        x = ifelse(input$timeline == "daily", "Day of the month",
                   ifelse(input$timeline == "weekly", "Day of the week",
                          "Hour of the day")),
        y = "#Complaints") +
      theme_minimal() +
      theme(strip.text = element_text(size = 10, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.y = element_line(colour = 'grey', size = 0.5))
    
    # Add continuous horizontal dashed lines between rows
    plot <- plot +
      geom_hline(yintercept = overall_max_y, linetype = "dashed",
                 color = "black", linewidth = 1.2)
    
    # Adjust scale on the x-axis
    if (input$timeline == "daily") {
      plot <- plot + scale_x_date(date_breaks = "1 week",
                                  date_labels = "%b/%d")
    } else if (input$timeline == 'weekly') {
      plot <- plot + scale_x_discrete(
        limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
    } else if (input$timeline == 'hourly') {
      plot <- plot + scale_x_continuous(breaks = seq(0, 23, 3))
    }
    plot
  })
  # -> Interactive Networks server logic
  output$network <- renderGirafe({
    # Select data based on inputs
    if (input$graph_type == 'unsanitary') {
      # Filter
      filtered_data <-
        unsanitary_data[date_created >= input$unsanitary_dates[1] &
                        date_created <= input$unsanitary_dates[2], ]
      # Get threshold value
      threshold <- input$unsanitary_threshold
      # Compute centroids to be nodes
      centroids <- filtered_data %>%
        group_by(zip_code) %>%
        summarise(central_lat = mean(lat),
                  central_lon = mean(lon))
    } else {
      # Filter
      filtered_data <- related_data %>%
        filter(complaint_type %in% input$complaint_types) %>%
        filter(date_created >= input$related_dates[1] &
               date_created <= input$related_dates[2])
      # Get threshold value
      threshold <- input$related_threshold
      # Compute centroids to be nodes
      centroids <- filtered_data %>%
        group_by(zip_code) %>%
        summarise(central_lat = mean(lat),
                  central_lon = mean(lon))
    }
    # Prepare the Edge list
    edges <- filtered_data %>%
      select(zip_code, complaint_type) %>%
      full_join(filtered_data, by = "complaint_type",
                suffix = c("_1", "_2"), relationship = 'many-to-many') %>%
      filter(zip_code_1 != zip_code_2) %>%
      group_by(zip_code_1, zip_code_2) %>%
      summarise(shared_complaints = n(), .groups = 'drop') %>%
      filter(shared_complaints >= threshold)  # Apply threshold
    
    gc()  # Trigger garbage collection
    
    # Create the map picture
    map <- ggplot(data=nyc_boroughs) +
      geom_sf(aes(fill=BoroName), color="black", size=0.2) +
      coord_sf() + theme_minimal() +
      ggtitle("") + scale_fill_brewer(palette="Set3") +
      labs(x='Longitude', y='Latitude', fill='Borough') +
      guides(fill=guide_legend(title = "Borough"))
    
    # Add the zip code centroids as nodes
    geo_zips <- map +
      geom_point_interactive(data = centroids,
                             aes(x = central_lon, y = central_lat,
                                 colour = as.factor(zip_code),
                                 tooltip = paste0("ZIP code: ", zip_code),
                                 data_id = zip_code),
                             size = 1.5, alpha = 0.6) +
      scale_colour_manual(values = rep('black', times = length(centroids$zip_code))) +
      guides(colour = 'none', size = 'none', alpha = 'none') +
      theme(legend.position = 'right')
    
    # If edges exist, plot them
    if (nrow(edges) > 0) {
      # Create the plot
      plot <- geo_zips +
        geom_segment_interactive(data = edges,
                                 aes(
                                   x = centroids$central_lon[match(zip_code_1, centroids$zip_code)],
                                   y = centroids$central_lat[match(zip_code_1, centroids$zip_code)],
                                   xend = centroids$central_lon[match(zip_code_2, centroids$zip_code)],
                                   yend = centroids$central_lat[match(zip_code_2, centroids$zip_code)],
                                   alpha = shared_complaints,
                                   tooltip = paste0('ZIP codes: ', zip_code_1, ' - ', zip_code_2,
                                                    '<br>', shared_complaints, " shared complaints"),
                                   data_id = paste0(zip_code_1, '-', zip_code_2)),
                                 linewidth = 0.7, colour = 'slategrey') +
        scale_alpha(range = c(0.1, 1)) +
        guides(alpha = guide_legend(title = "#(Shared complaints)"))
      
      # Highlight ZIP codes when hovering over the lines
      plot <- girafe(ggobj = plot, options = list(
        opts_hover(
          css = 'stroke:red; stroke-width:2px; fill-opacity:1;'
        )
      ))
    } else {
      # If no edges exist, plot just the map with centroids
      plot <- girafe(ggobj = geo_zips)
    }
    plot
  })
}
# Run the application
shinyApp(ui = app_ui, server = app_server)

