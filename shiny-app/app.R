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
library(colourpicker)

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
        menuItem("Introduction", tabName = "intro"),
        menuItem("Forest Area", tabName = "area"),
        menuItem("Brazil's Forest Loss", tabName = "brazil")
    ),
    br(),
    sidebarMenu(
        sidebarHeader("Forest Conversion"),
        menuItem("Change in Forest Area", tabName = "conv_change", icon = icon("chart-simple")),
        menuItem("Deforestation", tabName = "conv_def", icon = icon("chart-line")),
        menuItem("Map", tabName = "conv_map", icon = icon("map"))
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
                tags$li(tags$b("Factors behind Brazil's loss of forest cover"), ": ", tags$code("commercial_crops, flooding_due_to_dams, natural_disasters, pasture, selective_logging, fire, mining, other_infrastructure, roads, tree_plantations_including_palm, small_scale_clearing"))
            ),
            p("There are some other variables which are common to all the datasets, such as - ",
              code("country"), ", ",
              code("year"), "."
            )
        ),
        tabItem(
            tabName = "conv_change",
            h1("Forest conversion area over the years"),
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
                )
            )
        ),
        tabItem(
            tabName = "area",
            h1("Forest area of contries as a percentage of global forest area")
        ),
        tabItem(
            tabName = "brazil",
            h1("Factors behind loss of Brazil's forest cover")
        )
    )
)

footer <- dashboardFooter(
    left = a(
        href = "https://github.com/Roudranil",
        target = "_blank", 
        icon("github")
    ),
    right = "Roudranil Das"
)

# controlbar <- dashboardControlbar(
#     pinned = T
# )

ui <- dashboardPage(
    title = "Forests and deforestation",
    header = header,
    sidebar = sidebar,
    body = body,
    dark = T,
    footer = footer
)

server <- function(input, output) {
    
}

shinyApp(
    ui = ui,
    server = server
)