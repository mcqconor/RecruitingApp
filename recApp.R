if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")

library(dplyr)
library(maps)
library(ggplot2)
library(reshape2)
library(shiny)
library(ggiraph)

get_table <- function(t, s, min_y, max_y, level, sp){
  df <- read.csv('https://raw.githubusercontent.com/mcqconor/RecruitingApp/main/base_recruiting_df.csv')
  color <- read.csv('https://raw.githubusercontent.com/mcqconor/RecruitingApp/main/team_colors.csv')
  colnames(color)[1] <- 'school'
  
  if(level == 'County'){
    df <- df %>% filter(committed_to == t) %>% 
      filter(!is.na(state_province)) %>% 
      filter(year >= min_y & year <= max_y) %>% 
      filter(stars >= s) %>% 
      group_by(state_province, county_name) %>%
      summarise(
        recruits = n()
      ) %>% ungroup() %>% 
      filter(state_province == sp) %>% arrange(desc(recruits))
  }
  else{
    df <- df %>% filter(committed_to == t) %>% 
      filter(!is.na(state_province)) %>% 
      filter(year >= min_y & year <= max_y) %>% 
      filter(stars >= s) %>% 
      group_by(state_province) %>% 
      summarise(
        recruits = n()
      ) %>% ungroup() %>% arrange(desc(recruits)) 
  }
  return(df)
}

make_maps <- function(t, s, min_y, max_y, level, sp){
  df <- read.csv('https://raw.githubusercontent.com/mcqconor/RecruitingApp/main/base_recruiting_df.csv')
  color <- read.csv('https://raw.githubusercontent.com/mcqconor/RecruitingApp/main/team_colors.csv')
  colnames(color)[1] <- 'school'
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       plot.title = element_text(size = 16),
                       legend.title = element_text(size = 14),
                       plot.caption = element_text(size = 10),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  if(level == 'County'){
    df <- df %>% filter(committed_to == t) %>% 
      filter(!is.na(state_province)) %>% 
      filter(year >= min_y & year <= max_y) %>% 
      filter(stars >= s) %>% 
      group_by(state_province, county_name) %>%
      summarise(
        recruits = n()
      ) %>% ungroup() %>% 
      mutate(
        subregion = tolower(county_name)
      )
    
    counties <- map_data("county") %>% 
      mutate(
        state_province = case_when(
          region == 'alabama' ~ 'AL',
          region == 'arizona' ~ 'AZ',
          region == 'arkansas' ~ 'AR',
          region == 'california' ~ 'CA',
          region == 'colorado' ~ 'CO',
          region == 'connecticut' ~ 'CT',
          region == 'delaware' ~ 'DE',
          region == 'district of columbia' ~ 'DC',
          region == 'florida' ~ 'FL',
          region == 'georgia' ~ 'GA',
          region == 'idaho' ~ 'ID',
          region == 'illinois' ~ 'IL',
          region == 'indiana' ~ 'IN',
          region == 'iowa' ~ 'IA',
          region == 'kansas' ~ 'KS',
          region == 'kentucky' ~ 'KY',
          region == 'louisiana' ~ 'LA',
          region == 'maine' ~ 'ME',
          region == 'maryland' ~ 'MD',
          region == 'massachusetts' ~ 'MA',
          region == 'michigan' ~ 'MI',
          region == 'minnesota' ~ 'MN',
          region == 'mississippi' ~ 'MS',
          region == 'missouri' ~ 'MO',
          region == 'montana' ~ 'MT',
          region == 'nebraska' ~ 'NE',
          region == 'nevada' ~ 'NV',
          region == 'new hampshire' ~ 'NH',
          region == 'new jersey' ~ 'NJ',
          region == 'new mexico' ~ 'NM',
          region == 'new york' ~ 'NY',
          region == 'north carolina' ~ 'NC',
          region == 'north dakota' ~ 'ND',
          region == 'ohio' ~ 'OH',
          region == 'oklahoma' ~ 'OK',
          region == 'oregon' ~ 'OR',
          region == 'pennsylvania' ~ 'PA',
          region == 'rhode island' ~ 'RI',
          region == 'south carolina' ~ 'SC',
          region == 'south dakota' ~ 'SD',
          region == 'tennessee' ~ 'TN',
          region == 'texas' ~ 'TX',
          region == 'utah' ~ 'UT',
          region == 'vermont' ~ 'VT',
          region == 'virginia' ~ 'VA',
          region == 'washington' ~ 'WA',
          region == 'west virginia' ~ 'WV',
          region == 'wisconsin' ~ 'WI',
          region == 'wyoming' ~ 'WY'
        )
      ) %>% 
      left_join(df, all.x = TRUE)
    
    g <- ggplot(data = counties %>% filter(state_province == sp), aes(x = long, y = lat, fill = recruits, group = group))+
      geom_polygon(color = 'white', size = 0.2)+
      scale_fill_continuous(high = (color %>% filter(school == t))$prim_color[1], low = 'grey')+
      labs(title = paste0("Where does ", t," get their at least ",toString(s)," Star Recruits from in ",sp,"?"),
           caption = "Data from 247Sports, cfbscrapR, @CFB_Data, simplemaps.com | By: Conor McQuiston @ConorMcQ5",
           fill = paste0("Recruits from ", toString(min_y), " to ", toString(max_y)))+
      labs(x = "", y = "")+
      my_theme()
  }
  
  else{
    df <- df %>% filter(committed_to == t) %>% 
      filter(!is.na(state_province)) %>% 
      filter(year >= min_y & year <= max_y) %>% 
      filter(stars >= s) %>% 
      group_by(state_province) %>% 
      summarise(
        recruits = n()
      ) %>% ungroup()  
    
    states <- map_data("state") %>% 
      mutate(
        state_province = case_when(
          region == 'alabama' ~ 'AL',
          region == 'arizona' ~ 'AZ',
          region == 'arkansas' ~ 'AR',
          region == 'california' ~ 'CA',
          region == 'colorado' ~ 'CO',
          region == 'connecticut' ~ 'CT',
          region == 'delaware' ~ 'DE',
          region == 'district of columbia' ~ 'DC',
          region == 'florida' ~ 'FL',
          region == 'georgia' ~ 'GA',
          region == 'idaho' ~ 'ID',
          region == 'illinois' ~ 'IL',
          region == 'indiana' ~ 'IN',
          region == 'iowa' ~ 'IA',
          region == 'kansas' ~ 'KS',
          region == 'kentucky' ~ 'KY',
          region == 'louisiana' ~ 'LA',
          region == 'maine' ~ 'ME',
          region == 'maryland' ~ 'MD',
          region == 'massachusetts' ~ 'MA',
          region == 'michigan' ~ 'MI',
          region == 'minnesota' ~ 'MN',
          region == 'mississippi' ~ 'MS',
          region == 'missouri' ~ 'MO',
          region == 'montana' ~ 'MT',
          region == 'nebraska' ~ 'NE',
          region == 'nevada' ~ 'NV',
          region == 'new hampshire' ~ 'NH',
          region == 'new jersey' ~ 'NJ',
          region == 'new mexico' ~ 'NM',
          region == 'new york' ~ 'NY',
          region == 'north carolina' ~ 'NC',
          region == 'north dakota' ~ 'ND',
          region == 'ohio' ~ 'OH',
          region == 'oklahoma' ~ 'OK',
          region == 'oregon' ~ 'OR',
          region == 'pennsylvania' ~ 'PA',
          region == 'rhode island' ~ 'RI',
          region == 'south carolina' ~ 'SC',
          region == 'south dakota' ~ 'SD',
          region == 'tennessee' ~ 'TN',
          region == 'texas' ~ 'TX',
          region == 'utah' ~ 'UT',
          region == 'vermont' ~ 'VT',
          region == 'virginia' ~ 'VA',
          region == 'washington' ~ 'WA',
          region == 'west virginia' ~ 'WV',
          region == 'wisconsin' ~ 'WI',
          region == 'wyoming' ~ 'WY'
        )
      ) %>% 
      left_join(df, all.x = TRUE)
    
    g <- ggplot(data = states, aes(x = long, y = lat, group = group, fill = recruits))+
      geom_polygon(color = 'white', size = 0.2)+
      scale_fill_continuous(high = (color %>% filter(school == t))$prim_color[1], low = 'grey')+
      labs(title = paste0("Where does ", t," get their at least ",toString(s)," Star Recruits from?"),
           caption = "Data from 247Sports, cfbscrapR, @CFB_Data, simplemaps.com | By: Conor McQuiston @ConorMcQ5",
           fill = paste0("Recruits from ", toString(min_y), " to ", toString(max_y)))+
      labs(x = "", y = "")+
      theme(panel.grid.minor=element_blank(), panel.grid.major = element_blank())+
      theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "white"))+
      theme(panel.background = element_rect(fill = "white", color = "white", size = 0.5, linetype = "solid"))+
      theme(panel.grid.minor=element_blank(), panel.grid.major = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
      theme(legend.position = "bottom",
            legend.background = element_rect(fill = 'white'),
            plot.title = element_text(hjust = 0.5, size = 18),
            plot.subtitle = element_text(hjust = 0.5, size = 14),
            panel.grid.major.y = element_blank(),
            plot.background = element_rect(fill = "white"),
            axis.ticks = element_blank())
  }
  
  return(g)
}

ui <- pageWithSidebar(
  
  #app title
  headerPanel("County Level Recruiting Data"),
  
  #Sidebar input
  sidebarPanel(
    
    #Dropdown menu to select which school
    selectInput(
      inputId = "school",
      label = "School:",
      c("Air Force","Akron","Alabama","Appalachian State","Arizona","Arizona State","Arkansas","Arkansas State","Army","Auburn","Ball State","Baylor","Boise State",
        "Boston College","Bowling Green","Buffalo","BYU","California","Central Michigan","Charlotte","Cincinnati","Clemson","Coastal Carolina","Colorado","Colorado State",
        "Duke","East Carolina","Eastern Michigan","Florida International","Florida","Florida Atlantic","Florida State","Fresno State","Georgia","Georgia Southern",
        "Georgia State","Georgia Tech","Hawai\'i","Houston","Illinois","Indiana","Iowa","Louisiana Monroe","Louisiana Tech","Louisville","LSU","Marshall","Maryland",
        "Memphis","Miami","Miami (OH)","Michigan","Michigan State","Middle Tennessee","Minnesota","Mississippi State","Missouri","Navy","NC State","Nebraska","Nevada",
        "New Mexico","New Mexico State","North Carolina","North Texas","Northern Illinois","Northwestern","Notre Dame","Ohio","Ohio State","Oklahoma","Oklahoma State",
        "Old Dominion","Ole Miss","Oregon","Oregon State","Penn State","Pittsburgh","Purdue","Rice","Rutgers","San Diego State","San José State","SMU","South Alabama",
        "South Carolina","South Florida","Southern Mississippi","Stanford","Syracuse","TCU","Temple","Tennessee","Texas","Texas A&M","Texas State","Texas Tech","Toledo",
        "Troy","Tulane","Tulsa","UAB","UCF","UCLA","Connecticut","UMass","UNLV","USC","UTEP","UT San Antonio","Utah","Utah State","Vanderbilt","Virginia","Virginia Tech",
        "Wake Forest","Washington","Washington State","West Virginia","Western Kentucky","Western Michigan","Wisconsin","Wyoming")
    ),
    
    #Dropdown menu to select which school
    selectInput(
      inputId = "state_province",
      label = "State:",
      c('AL','AK','AR','AZ','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MO','MS','MT','NC','ND','NE','NH','NJ','NM',
        'NV','NY','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WI','WV','WY')
    ),
    
    #minimum stars slider
    sliderInput(
      "stars",
      "Minimum Stars:",
      min = 1,
      max = 5,
      value = 4
    ),
    
    sliderInput(
      "years",
      "Time Period",
      min = 2000,
      max = 2020,
      value = c(2000, 2020),
      sep = ""
    ),
    
    selectInput(
      "level",
      "Which level would you like to see?",
      c('County','State')
    ),
    
    actionButton(
      "update",
      "Show Distribution"
    )
  ),
  
  #Output panel
  mainPanel(
    
    # Hide errors
    tags$style(type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    plotOutput("distPlot", height = 720,
               hover = hoverOpts(
                 id = 'plot_hover'
               )),
    tableOutput('table')
    
  )
)

#Define server logic
server <- function(input, output){
  observeEvent(input$update, {
    output$distPlot <- renderPlot({
        (make_maps(input$school, input$stars, input$years[1], input$years[2], input$level, input$state_province))
    })
    output$table <- renderTable({
      get_table(input$school, input$stars, input$years[1], input$years[2], input$level, input$state_province)
    })
    })
}
  

shinyApp(ui, server)
