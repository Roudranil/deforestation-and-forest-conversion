library(shiny)
library(tidyverse)
library(tidytext)
library(scales)
library(fuzzyjoin)
library(maps)
library(countrycode)
library(ggthemes)
# library(shinydashboard)
# library(dashboardthemes)
library(bs4Dash)
library(fontawesome)

forest <- readr::read_csv('./data/forest.csv', show_col_types = FALSE)
forest_area <- readr::read_csv('./data/forest_area.csv', show_col_types = FALSE)
brazil_loss <- readr::read_csv('./data/brazil_loss.csv', show_col_types = FALSE)

forest <- forest %>% 
    mutate(net_forest_conversion = net_forest_conversion / 100) %>% 
    filter(str_length(code) == 3) %>% 
    rename(country = entity)

forest_area <- forest_area %>% 
    filter(!year %in% c(1990, 1991, 1992)) %>% 
    filter(str_length(code) == 3) %>% 
    rename(country = entity)

country_data <- forest %>% 
    filter(year == 2010) %>% 
    inner_join(maps::iso3166, by = c(code = "a3"))

country_data$continent <- countrycode(sourcevar = country_data[["country"]],
                                      origin = "country.name",
                                      destination = "continent")

plot_map_data <- map_data("world") %>% 
    as_tibble() %>% 
    filter(region != "Antarctica") %>% 
    regex_left_join(country_data, by = c(region = "mapname"))

brazil_loss <- brazil_loss %>%
    select(-entity, -code)

causes <- brazil_loss %>%
    pivot_longer(cols = commercial_crops:small_scale_clearing, names_to = "cause", values_to = "lost_forest_area") %>% 
    mutate(lost_forest_area = lost_forest_area/100) %>% 
    filter(!cause == "pasture") %>% 
    distinct(cause) %>% 
    pull(cause)

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_colors <- as.list(gg_color_hue(10))
names(gg_colors) <- causes

cause_color_map <- function(x, vars) {
    y <- c()
    n <- length(x)
    for (i in 1:n) {
        if (x[i] %in% vars) {
            y <- c(y, setNames(gg_colors[[x[i]]], x[i]))
        } else {
            y <- c(y, setNames("#CBCBCB", x[i]))
        }
    }
    return(y)
}

cause_size_map <- function(x, vars) {
    y <- c()
    n <- length(x)
    for (i in 1:n) {
        if (x[i] %in% vars) {
            y <- c(y, setNames(1, x[i]))
        } else {
            y <- c(y, setNames(0.5, x[i]))
        }
    }
    return(y)
}

header <- dashboardHeader(
    title = "Forests and Deforestation",
    tags$li(class = "dropdown",
            tags$style(".main-header .navbar {margin-left: 350px}"),
            tags$style(".main-header {font-size:20px}")
    )
)

sidebar <- dashboardSidebar(
    width = 350,
    # tags$style(".left-side, .main-sidebar {padding-top: 30px}"),
    tags$style(".main-header .navbar {margin-left: 350px}"),
    tags$style(".main-header .logo {width: 350px}"),
    sidebarMenu(
        menuItem("Introduction", tabName = "intro", icon = icon("database"))
    ),
    br(),
    sidebarMenu(
        sidebarHeader("Forest Conversion"),
        menuItem("Change in Forest Area", tabName = "conv_change", icon = icon("chart-bar")),
        menuItem("Deforestation", tabName = "conv_def", icon = icon("chart-bar")),
        menuItem("Map", tabName = "conv_map", icon = icon("map"))
    ),
    br(),
    sidebarMenu(
        sidebarHeader("Forest Area by Country"),
        menuItem("Forest areaplot", tabName = "area_max", icon = icon("chart-area"))
    ),
    br(),
    sidebarMenu(
        sidebarHeader("Brazil's Loss of Forest Cover"),
        menuItem("Biggest cause of loss", tabName = "brazil_cause", icon = icon("chart-simple"))
    ),
    br(),
    sidebarMenu(
        menuItem("Contact", tabName = "contact", icon = icon("envelope"))
    )
)

body <- dashboardBody(
    tags$style("body {font-size: 20px}"),
    tabItems(
        tabItem(
            tabName = "intro",
            h1("Introduction", style = "font-size:42px"),
            p("In this project we explore the trends of deforestation in the past 3 decades. 
              We use visualisation techniques in order to find patterns behind deforestation 
              and it's driving factors. Following this, we attempt to answer questions 
              regarding conversion of forests to land for other purposes and questions 
              regarding Brazil's excessive loss of forest cover."),
            p("Tackling deforestation begins with understanding where and when 
              are we losing forest and the driving factors behind it. 
              This is what we aim to address in this project."),
            br(),
            h2("Datasets"),
            tags$ul(
                tags$li(tags$b("Forest conversion area over the years"), ": how much net forest area of each country has been converted to land for other uses in the years 1990, 2000, 2010, 2015."),
                tags$li(tags$b("Forest area by country"), ": how much of the global forest area (in percentage) is present in each country in years from 1993 to 2020."),
                tags$li(tags$b("Factors behind loss of Brazil's forest cover by year"), ": how much forest land (in hectares) has been lost by Brazil in 2013 to various reasons.")
            ),
            br(),
            h2("Main variables that we will be looking at from each of the datasets"),
            tags$ul(
                tags$li(tags$b("Forest conversion area over the years"), ": ", tags$code("net_forest_conversion")),
                tags$li(tags$b("Forest area by country"), ": ", tags$code("forest_area")),
                tags$li(tags$b("Factors behind Brazil's loss of forest cover"), ": ", tags$code("commercial_crops, flooding_due_to_dams, natural_disturbances, pasture, selective_logging, fire, mining, other_infrastructure, roads, tree_plantations_including_palm, small_scale_clearing"))
            ),
            p("There are some other variables which are common to all the datasets, such as - ",
              code("country"), ", ",
              code("year"), "."
            )
        ),
        tabItem(
            tabName = "conv_change",
            h1("Forest conversion area over the years"),
            h2("Change in Forest Area"),
            fluidRow(
                column(
                    4,
                    selectInput("year",
                                label = "Select year",
                                choices = c(1990, 2000, 2010, 2015)),
                    br(),
                    sliderInput("num_country",
                                label = "Select the number of countries",
                                min = 4, max = 10,
                                value = 6, step = 1)
                ),
                column(
                    8,
                    plotOutput("conv_change", height = "750px", width = "85%")
                )
            )
        ),
        tabItem(
            tabName = "conv_def",
            h1("Forest conversion area over the years"),
            h2("Maximum Deforestation over the years"),
            fluidRow(
                column(
                    4,
                    selectInput("year_def",
                                label = "Select year",
                                choices = c(1990, 2000, 2010, 2015)),
                    br(),
                    sliderInput("num_country_def",
                                label = "Select the number of countries",
                                min = 4, max = 10,
                                value = 6, step = 1)
                ),
                column(
                    8,
                    plotOutput("conv_def", height = "750px", width = "85%")
                )
            )
        ),
        tabItem(
            tabName = "conv_map",
            h1("Forest conversion area over the years"),
            h2("Map visualisation of change in forest cover"),
            fluidRow(
                column(
                    4,
                    selectInput("region_map",
                                label = "Select the region to visualise",
                                choices = c("World", "Asia", "Americas", "Europe", "Africa"))
                ),
                column(
                    8,
                    plotOutput("conv_map", height = "750px", width = "85%")
                )
            )
        ),
        tabItem(
            tabName = "area_max",
            h1("Forest area of countries as a percentage of global forest area"),
            "The area plot below shows which countries have the majority of the forest cover of the world (by percentage). Select a range of years to see how that varies within the selected range.",
            fluidRow(
                column(
                    4,
                    sliderInput("range_year",
                                label = "Select the range of years",
                                max = 2020, min = 1993, step = 1,
                                value = c(1993, 2020))
                ),
                column(
                    8,
                    plotOutput("area_max",
                               height = "750px")
                )
            )
        ),
        tabItem(
            tabName = "brazil_cause",
            h1("Factors behind loss of Brazil's forest cover"),
            p("Basic analysis will show that the major driving factor behind Brazil's loss of forest cover every year from 2001 to 2013. We look at the other factors causing the loss in forest cover."),
            fluidRow(
                column(
                    4,
                    checkboxGroupInput("vars",
                                       "Choose the causes you want to visualise",
                                       choices = c("Commercial crops" = "commercial_crops",
                                                   "Flooding due to dams" = "flooding_due_to_dams",
                                                   "Natural disturbances" = "natural_disturbances",
                                                   "Selective logging" = "selective_logging",
                                                   "Forest fire" = "fire",
                                                   "Mining" = "mining",
                                                   "Other infrastructure" = "other_infrastructure",
                                                   "Roads" = "roads",
                                                   "Tree plantations" = "tree_plantations_including_palm",
                                                   "Small scale clearings" = "small_scale_clearing"),
                                       selected = causes
                                       )
                    
                )
            ),
            fluidRow(
                column(
                    6,
                    plotOutput("brazil_line",
                               height = "750px")
                ),
                column(
                    6,
                    plotOutput("brazil_bar",
                               height = "750px")
                )
            )
        ),
        tabItem(
            tabName = "contact",
            h1("Contact details"),
            p(tags$b("Name"), ": Roudranil Das"),
            p(tags$b("Email id"), ": ", tags$a(tags$span(HTML("<u>roudranil@cmi.ac.in</u>"), style = "color:#52C5FF"), href = "mailto:roudranil@cmi.ac.in"))
        )
    )
)

footer <- dashboardFooter(
    left = a(
        href = "https://github.com/Roudranil/deforestation-and-forest-conversionl",
        target = "_blank", 
        tags$span(icon("github"), style = "color:#52C5FF")
    ),
    right = a(
        href = "https://roudranil.github.io",
        tags$span("Roudranil Das", style = "color:#52C5FF")
    )
)

# controlbar <- dashboardControlbar(
#     pinned = T
# )

ui <- dashboardPage(
    title = "Forests and Deforestation",
    header = header,
    sidebar = sidebar,
    body = body,
    dark = T,
    footer = footer
)

server <- function(input, output) {
    output$conv_change <- renderPlot({
        forest %>%
            group_by(year) %>%
            slice_max(abs(net_forest_conversion), n = input$num_country) %>%
            ungroup() %>%
            mutate(country = reorder_within(country, net_forest_conversion, year)) %>%
            filter(year == as.integer(input$year)) %>% 
            ggplot(aes(net_forest_conversion, country,
                       fill = net_forest_conversion > 0)) +
            geom_col() +
            scale_x_continuous(label = comma) +
            scale_y_reordered() +
            theme(legend.position = "none") +
            labs(title = paste("Greatest change in forest area in", input$year),
                 x = "Net change in forest area over the years (in sq km)",
                 y = "")
    })
    
    output$conv_def <- renderPlot({
        forest %>% 
            group_by(year) %>% 
            slice_min(net_forest_conversion, n = input$num_country_def) %>% 
            ungroup() %>% 
            mutate(country = reorder_within(country, -net_forest_conversion, year)) %>% 
            filter(year == as.integer(input$year_def)) %>% 
            ggplot(aes(net_forest_conversion, country, fill = country)) + 
            geom_col() +
            scale_x_continuous(label = comma) +
            scale_y_reordered() +
            theme(legend.position = "none") + 
            labs(title = paste("Where did we see the most deforestation in", input$year_def, "?"),
                 x = "Net increase in forest area over the years (in sq km)",
                 y = "")
    })
    
    output$conv_map <-renderPlot({
        if (input$region_map == "World") {
            plot_map_data %>% 
                ggplot(aes(long, lat, group = group , fill = net_forest_conversion)) +
                geom_polygon(color = "black", size = 0.1) +
                scale_fill_gradient2(low = "red", high = "green") +
                theme_map() +
                labs(fill = "Net forest conversion \n(in sq km)",
                     x = "",
                     y = "")
        } else {
            if (input$region_map == "Asia") {
                plot_map_data %>% 
                    filter(continent == "Asia") %>% 
                    ggplot(aes(long, lat, group = group , fill = net_forest_conversion)) +
                    geom_polygon(color = "black", size = 0.1) +
                    scale_fill_gradient2(low = "red", high = "green") +
                    labs(fill = "Net forest conversion \n(in sq km)",
                         x = "",
                         y = "") +
                    xlim(24, 135) +
                    ylim(-5, 65) +
                    theme_map()
            } else if (input$region_map == "Africa") {
                plot_map_data %>% 
                    filter(continent == "Africa") %>% 
                    ggplot(aes(long, lat, group = group , fill = net_forest_conversion)) +
                    geom_polygon(color = "black", size = 0.1) +
                    scale_fill_gradient2(low = "red", high = "green") +
                    labs(fill = "Net forest conversion \n(in sq km)",
                         x = "",
                         y = "") +
                    xlim(-25, 60) +
                    theme_map()
            } else if (input$region_map == "Europe") {
                plot_map_data %>% 
                    filter(continent == "Europe") %>% 
                    ggplot(aes(long, lat, group = group , fill = net_forest_conversion)) +
                    geom_polygon(color = "black", size = 0.1) +
                    scale_fill_gradient2(low = "red", high = "green") +
                    labs(fill = "Net forest conversion \n(in sq km)",
                         x = "",
                         y = "") +
                    xlim(-20, 195) +
                    theme_map()
            } else if (input$region_map == "Americas") {
                plot_map_data %>% 
                    filter(continent == "Americas") %>% 
                    ggplot(aes(long, lat, group = group , fill = net_forest_conversion)) +
                    geom_polygon(color = "black", size = 0.1) +
                    scale_fill_gradient2(low = "red", high = "green") +
                    labs(fill = "Net forest conversion \n(in sq km)",
                         x = "",
                         y = "") +
                    xlim(-200, 25) +
                    theme_map()
            }
        }
    })
    
    output$area_max <- renderPlot({
        forest_area %>% 
            mutate(forest_area = forest_area/100) %>% 
            mutate(country = fct_lump(country, 9, w = forest_area)) %>% 
            group_by(country, year) %>% 
            summarise(forest_area = sum(forest_area), .groups = "drop") %>% 
            mutate(country = fct_reorder(country, -forest_area)) %>% 
            filter((input$range_year[1] <= year) & (year <= input$range_year[2])) %>% 
            ggplot(aes(year, forest_area, fill = country)) +
            geom_area() + 
            scale_y_continuous(labels = percent) +
            expand_limits(y = 0) +
            labs(x = "Year",
                 y = "Percentage of global forest area",
                 fill = "Countries") +
            scale_x_continuous(breaks = seq(input$range_year[1], input$range_year[2], 1), 
                               labels = as.character(seq(input$range_year[1], input$range_year[2], 1)))
    })
    
    output$brazil_line <- renderPlot({
        cause_color <- cause_color_map(causes, input$vars)
        cause_size <-  cause_size_map(causes, input$vars)
        
        brazil_loss %>%
            pivot_longer(cols = commercial_crops:small_scale_clearing, names_to = "cause", values_to = "lost_forest_area") %>% 
            mutate(lost_forest_area = lost_forest_area/100) %>% 
            filter(!cause == "pasture") %>% 
            ggplot(aes(x = year, y = lost_forest_area, color = cause, size = cause)) +
            geom_line() +
            scale_color_manual(values = cause_color) +
            scale_size_manual(values = cause_size) +
            scale_x_continuous(breaks = seq(2001, 2013, 1), labels = as.character(seq(2001, 2013, 1))) +
            labs(fill = "Cause of loss",
                 x = "Year",
                 y = "Forest area lost (in sq km)") +
            scale_y_continuous(labels = comma)
    })
    
    output$brazil_bar <- renderPlot({
        cause_fill <- cause_color_map(causes, input$vars)
        cause_fill <- cause_fill[input$vars]
        cause_fill <- c(cause_fill, setNames("#CBCBCB", "other_loss"))
        
        brazil_loss %>% 
            select(-pasture) %>% 
            mutate(other_loss = rowSums(across(commercial_crops:small_scale_clearing)) - rowSums(across(input$vars))) %>%
            select(c(input$vars, "year", "other_loss")) %>% 
            pivot_longer(cols = -year, names_to = "cause", values_to = "lost_forest_area") %>% 
            mutate(lost_forest_area = lost_forest_area/100) %>% 
            arrange(cause) %>% 
            ggplot(aes(fill = factor(cause, levels = c(input$vars, "other_loss")), x = year, y = lost_forest_area)) +
            geom_bar(position = position_fill(reverse = TRUE), stat = "identity", color = "black") +
            scale_fill_manual(values = cause_fill, breaks = input$vars) +
            scale_x_continuous(breaks = seq(2001, 2013, 1), labels = as.character(seq(2001, 2013, 1))) +
            labs(fill = "Cause of loss",
                 x = "Year",
                 y = "Forest area lost (in sq km)") +
            scale_y_continuous(labels = percent)
    })
}

shinyApp(
    ui = ui,
    server = server
)