library(dash)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(gapminder)
library(tidyverse)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# load data
gapminder_data <- gapminder

# function get continents
get_continents <- function() {
  return (unique(gapminder_data$continent))
}

# function get countries
get_countries <- function() {
  return (unique(gapminder_data$country))
}

create_continents_list <- function() {
  continent_list <- list()
  for (continent in get_continents()) {
    continent_list <- append(continent_list,
                             list(list("label" = continent,
                                       "value" = continent)))
  }
  return(continent_list)
}

create_countries_list <- function() {
  country_list <- list()
  for (country in get_countries()) {
    country_list <- append(country_list,
                             list(list("label" = country,
                                       "value" = country)))
  }
  return(country_list)
}

# Sync continents and countries in filter
app$callback(
  output("country-selector", 'options'),
  list(input("continent-selector", 'value')),
  function(selected_continent = "Europe") {
    valid_countries <- gapminder_data %>%
      filter(continent == selected_continent)
    valid_countries <- droplevels.factor(unique(valid_countries$country))
    
    country_options <- list()
    for (country in valid_countries) {
      country_options <- append(country_options,
                             list(list("label" = country,
                                       "value" = country)))
    }
    return(country_options)
  }
)


app$callback(
  output('top-gdp-plot', 'figure'),
  list(input('continent-selector', 'value'),
       input("country-selector", "value")),
  function(selected_continent="Europe", selected_countries=NULL) {
    plot_data <- gapminder_data %>%
    filter(year == 2007 & continent == selected_continent) %>%
    arrange(gdpPercap) %>%
    slice_max(gdpPercap, n=10)
    
    plot_data$highlight <- FALSE
  
    # If countries are selected
    if (!is.null(selected_countries)){
      selected_countries_data <- gapminder_data %>%
        filter(year == 2007 & country %in% selected_countries) %>%
        arrange(gdpPercap) %>%
        mutate(highlight = TRUE)
    }

    plot_data <- rbind(plot_data, selected_countries_data)
    plot_data <- plot_data %>%
      distinct(country, .keep_all = TRUE)

    plot <- ggplot(plot_data) +
    aes(x=gdpPercap, y=reorder(country, gdpPercap), fill=highlight) +
    geom_bar(stat = 'identity') + theme(legend.position="none")
  
    ggplotly(plot)
})

app %>% set_layout(
  h1('GapminderDash'),
  list(
    dccDropdown(
      id = "continent-selector",
      options = create_continents_list(),
      value = 'Europe'
    )
  ),
  list(
    dccDropdown(
      id = "country-selector",
      options = create_countries_list(),
      multi = TRUE
    )
  ),
  dbcCard(
    dbcCardBody(
      list(
      h4("Top GDP", className = "card-title"),
      dccGraph(id="top-gdp-plot")
      )
    )
  )
)

# Run the app
app$run_server(host = '0.0.0.0')
