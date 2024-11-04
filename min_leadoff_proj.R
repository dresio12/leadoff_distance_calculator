library(baseballr)
library(tidyverse)
library(data.table)
library(readxl)
library(shiny)
library(dplyr)
library(tidyr)

pitch_speeds <- read.csv('pitch_arsenals.csv')
catchers <- read.csv('catcher_throwing.csv')
baserunners <- read.csv('running_splits.csv')
extension <- read_excel("extension.xlsx")

#remove weird i
baserunners <- baserunners |>
  rename(year = ï..year)

pitch_speeds <- pitch_speeds |>
  rename(year = ï..year)

catchers <- catchers |>
  rename(player_id = ï..player_id)

#calculate pitch speeds in FPS
pitch_speeds <- pitch_speeds |>
  mutate(ff_fps = ff_avg_speed * 5280 / 3600,
         si_fps = si_avg_speed * 5280 / 3600,
         fc_fps = fc_avg_speed * 5280 / 3600,
         sl_fps = sl_avg_speed * 5280 / 3600,
         ch_fps = ch_avg_speed * 5280 / 3600,
         cu_fps = cu_avg_speed * 5280 / 3600,
         fs_fps = fs_avg_speed * 5280 / 3600,
         kn_fps = kn_avg_speed * 5280 / 3600,
         st_fps = st_avg_speed * 5280 / 3600,
         sv_fps = sv_avg_speed * 5280 / 3600)

#convert extension into ft.inches
extension <- extension |>
  mutate(release = 60.5 - extension)

extension <- unique(extension)
pitch_speeds <- unique(pitch_speeds)

#join extension to pitch_speeds
pitchers <- left_join(pitch_speeds, extension)

#remove the correct doubled Logan Allen's
pitchers <- pitchers[-c(129, 486), ]

#calculate time to plate (TTP) in seconds for each pitcher's pitch
#adds in an aggressive baseline 1.05S for time to release out of the stretch
#since i dont have that data

pitchers <- pitchers |>
  mutate(Fastball = (release / ff_fps) + 1.05,
         Sinker = (release / si_fps) + 1.05,
         Cutter = (release / fc_fps) + 1.05,
         Slider = (release / sl_fps) + 1.05,
         Changeup = (release / ch_fps) + 1.05,
         Curveball = (release / cu_fps) + 1.05,
         Splitter = (release / fs_fps) + 1.05,
         Knuckleball = (release / kn_fps) + 1.05,
         Sweeper = (release / st_fps) + 1.05,
         Slurve = (release / sv_fps) + 1.05
         )

#filter down pop_times
catchers <- catchers |>
  select(year, name, player_id, pop_time)

#restructure pitchers df
pitchers <- pitchers |>
  select(1,2,3,27:36) |>
  rename(pitcher = name)

pitchers <- pitchers |>
  pivot_longer(cols = c(Fastball, Sinker, Cutter, Slider, Changeup, Curveball,
               Splitter, Knuckleball, Sweeper, Slurve),
               names_to = 'pitch_type',
               values_to = 'TTP')

#remove NA TTPs
pitchers <- pitchers |>
  filter(!is.na(TTP))

#rename columns in baserunners
baserunners <- baserunners |>
  rename(ft_0_sec = seconds_since_hit_000,
         ft_5_sec = seconds_since_hit_005,
         ft_10_sec = seconds_since_hit_010,
         ft_15_sec = seconds_since_hit_015,
         ft_20_sec = seconds_since_hit_020,
         ft_25_sec = seconds_since_hit_025,
         ft_30_sec = seconds_since_hit_030,
         ft_35_sec = seconds_since_hit_035,
         ft_40_sec = seconds_since_hit_040,
         ft_45_sec = seconds_since_hit_045,
         ft_50_sec = seconds_since_hit_050,
         ft_55_sec = seconds_since_hit_055,
         ft_60_sec = seconds_since_hit_060,
         ft_65_sec = seconds_since_hit_065,
         ft_70_sec = seconds_since_hit_070,
         ft_75_sec = seconds_since_hit_075,
         ft_80_sec = seconds_since_hit_080,
         ft_85_sec = seconds_since_hit_085,
         ft_90_sec = seconds_since_hit_090)

#year and alphabetical order arrange
pitchers <- pitchers |>
  arrange(desc(year), pitcher)

catchers <- catchers |>
  arrange(desc(year), name)

# Define the UI
ui <- fluidPage(
  titlePanel("Minimum Lead Distance Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = unique(pitchers$year)),
      selectizeInput("pitcher", "Type Pitcher Name:", choices = NULL, multiple = FALSE),
      selectizeInput("pitch_type", "Select Pitch Type:", choices = NULL),
      selectizeInput("catcher", "Type Catcher Name:", choices = NULL, multiple = FALSE),
      selectizeInput("baserunner", "Select Base Runner:", choices = NULL),
      actionButton("calculate", "Calculate Minimum Lead Distance")
    ),
    
    mainPanel(
      textOutput("result")
    )
  )
)



# Define the server logic
server <- function(input, output, session) {
  
  # Update pitcher names based on the selected year
  observe({
    req(input$year)
    
    available_pitchers <- pitchers %>%
      filter(year == input$year) %>%
      select(pitcher) %>%
      distinct() %>%
      pull()
    
    updateSelectizeInput(session, "pitcher", choices = available_pitchers, server = TRUE)
  })
  
  # Update pitch types based on selected pitcher and year
  observe({
    req(input$year, input$pitcher)
    
    available_pitch_types <- pitchers %>%
      filter(year == input$year & pitcher == input$pitcher) %>%
      select(pitch_type) %>%
      distinct() %>%
      pull()
    
    updateSelectizeInput(session, "pitch_type", choices = available_pitch_types, server = TRUE)
  })
  
  # Update catcher choices based on the year
  observe({
    req(input$year)
    
    available_catchers <- catchers %>%
      filter(year == input$year) %>%
      select(name) %>%
      distinct() %>%
      pull()
    
    updateSelectizeInput(session, "catcher", choices = available_catchers, server = TRUE)
  })
  
  # Update baserunner choices based on the year
  observe({
    req(input$year)
    
    available_baserunners <- baserunners %>%
      filter(year == input$year) %>%
      select(name) %>%
      distinct() %>%
      pull()
    
    updateSelectizeInput(session, "baserunner", choices = available_baserunners, server = TRUE)
  })
  
  # Calculate the minimum lead distance on button click
  observeEvent(input$calculate, {
    req(input$year, input$pitcher, input$pitch_type, input$catcher, input$baserunner)
    
    # Get the TTP for the selected pitcher and pitch type
    pitcher_data <- pitchers %>%
      filter(year == input$year, pitcher == input$pitcher, pitch_type == input$pitch_type) %>%
      select(TTP) %>%
      pull()
    
    # Get the pop time for the selected catcher
    catcher_data <- catchers %>%
      filter(year == input$year, name == input$catcher) %>%
      select(pop_time) %>%
      pull()
    
    # Get the base runner's sprint data
    baserunner_data <- baserunners %>%
      filter(year == input$year, name == input$baserunner) %>%
      select(starts_with("ft"))
    
    # Convert the sprint data into a vector of times and distances
    sprint_times <- baserunner_data %>% unlist(use.names = FALSE)
    distances <- seq(0, 90, by = 5)  # Corresponding distances for each time
    
    # Create an interpolation function to find distance covered in a given time
    calculate_distance <- function(target_time) {
      if (target_time <= max(sprint_times)) {
        interpolated_distance <- approx(sprint_times, distances, xout = target_time)$y
        return(interpolated_distance)
      } else {
        return(90)  # If target time exceeds known data, assume they reach 90 ft
      }
    }
    
    # Total time to second base
    total_time_to_second_base <- pitcher_data + catcher_data
    
    # Calculate distance covered by the base runner in the total time
    distance_covered <- calculate_distance(total_time_to_second_base)
    
    # Minimum lead distance
    minimum_lead_distance <- max(0, 90 - distance_covered)  # Ensuring non-negative lead distance
    
    # Display the result
    output$result <- renderText({
      paste("Minimum Lead Distance:", round(minimum_lead_distance, 2), "feet")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


