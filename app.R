if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(urbnmapr)) devtools::install_github("UrbanInstitute/urbnmapr")

library(dplyr)
library(ggplot2)
library(reshape2)
library(shiny)
library(plotly)
library(stringr)
library(urbnmapr)

df <- read.csv('base_recruiting_df.csv')
key_states <- read.csv('key_states.csv')
color <- read.csv('team_colors.csv')
colnames(color)[1] <- 'school'

teams <- c("Air Force","Akron","Alabama","Appalachian State","Arizona","Arizona State","Arkansas","Arkansas State","Army","Auburn","Ball State","Baylor","Boise State",
           "Boston College","Bowling Green","Buffalo","BYU","California","Central Michigan","Charlotte","Cincinnati","Clemson","Coastal Carolina","Colorado","Colorado State",
           "Duke","East Carolina","Eastern Michigan","Florida International","Florida","Florida Atlantic","Florida State","Fresno State","Georgia","Georgia Southern",
           "Georgia State","Georgia Tech","Hawai\'i","Houston","Illinois","Indiana","Iowa","Iowa State",
           "Kansas",
           "Kansas State",
           "Kent State",
           "Kentucky",
           "Louisiana Monroe","Louisiana Tech","Louisville","LSU","Marshall","Maryland",
           "Memphis","Miami","Miami (OH)","Michigan","Michigan State","Middle Tennessee","Minnesota","Mississippi State","Missouri","Navy","NC State","Nebraska","Nevada",
           "New Mexico","New Mexico State","North Carolina","North Texas","Northern Illinois","Northwestern","Notre Dame","Ohio","Ohio State","Oklahoma","Oklahoma State",
           "Old Dominion","Ole Miss","Oregon","Oregon State","Penn State","Pittsburgh","Purdue","Rice","Rutgers","San Diego State","San José State","SMU","South Alabama",
           "South Carolina","South Florida","Southern Mississippi","Stanford","Syracuse","TCU","Temple","Tennessee","Texas","Texas A&M","Texas State","Texas Tech","Toledo",
           "Troy","Tulane","Tulsa","UAB","UCF","UCLA","Connecticut","UMass","UNLV","USC","UTEP","UT San Antonio","Utah","Utah State","Vanderbilt","Virginia","Virginia Tech",
           "Wake Forest","Washington","Washington State","West Virginia","Western Kentucky","Western Michigan","Wisconsin","Wyoming")

make_maps_data <- function(t, s, min_y, max_y, level, sp){
  df <- df %>% select(year, committed_to, stars, state_province, county_name)
  
  if(level == 'County'){
    df <- df %>% filter(committed_to == t) %>% 
      filter(!is.na(state_province)) %>% 
      filter(year >= min_y & year <= max_y) %>% 
      filter(stars >= s) %>% 
      group_by(state_province, county_name) %>%
      summarise(
        recruits = n()
      ) %>% ungroup()
    
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
  }
  
  df$school <- t
  df$min_year <- min_y
  df$max_year <- max_y
  df$min_stars <- s
  
  return(df)
}

make_key_state_data <- function(type_p, team, p, k_l){
  
  if(k_l == 'County'){
    df <- df %>% 
      filter(committed_to == team) %>% 
      filter(year <= p & year > p - 5) %>% 
      filter(state_province %in% (key_states %>% filter(committed_to == team & end_year == p))$state_province) %>% 
      group_by(state_province, county_name) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup()
  }
  
  else{
    df <- df %>% 
      filter(committed_to == team) %>% 
      filter(year <= p & year > p - 5) %>% 
      filter(state_province %in% (key_states %>% filter(committed_to == team & end_year == p))$state_province) %>% 
      group_by(state_province) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup()
  }
  
  key_states <- key_states %>% filter(committed_to == team) %>% rename("Year" = "end_year", "pre_share"="share") %>% mutate(Share = round(100*pre_share, 2))
  
  if(type_p == 'time'){
    return(key_states)
  }
  else{
    return(df)
  }
  
}

make_maps <- function(t, s, min_y, max_y, level, sp){
  df <- df %>% select(year, committed_to, stars, state_province, county_name)
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       plot.title = element_text(size = 16, hjust = 0.5),
                       legend.title = element_text(size = 14),
                       plot.caption = element_text(size = 10),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "right",
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
    
    counties <- get_urbn_map(map = "counties", sf = TRUE) %>% 
      mutate(
        state_province = state_abbv
      ) %>% left_join(df, all.x = TRUE)
    
    g <- ggplot(data = counties %>% filter(state_province == sp), aes(fill = recruits, text = county_name))+
      geom_sf(color = 'black', size = 0.2)+
      scale_fill_continuous(high = (color %>% filter(school == t))$prim_color[1], low = 'grey', na.value = 'white')+
      labs(title = paste0(t," ",toString(s),"+ Stars - ",sp, " ", toString(min_y), "-", toString(max_y)),
           fill = paste0("Recruits from ", toString(min_y), " to ", toString(max_y)))+
      labs(x = "", y = "")+
      my_theme()
    
    return(ggplotly(g))
    
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
    
    states <- get_urbn_map(map = "states", sf = TRUE) %>% 
      mutate(
        state_province = state_abbv
      ) %>% left_join(df, all.x = TRUE)
    
    g <- ggplot(data = states, aes(fill = recruits, text = paste0('State: ',state_province)))+
      geom_sf(color = 'black', size = 0.2)+
      scale_fill_continuous(high = (color %>% filter(school == t))$prim_color[1], low = 'grey', na.value = 'white')+
      labs(title = paste0(t," ",toString(s),"+ Stars ", toString(min_y), " - ",toString(max_y)),
           fill = "")+
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
    
    return(ggplotly(g))
  }
  
  
}

make_key_state_plots <- function(type_p, team, p, k_l){
  
  if(k_l == 'County'){
    df <- df %>% 
      filter(committed_to == team) %>% 
      filter(year <= as.numeric(p) & year > as.numeric(p) - 5) %>% 
      filter(state_province %in% (key_states %>% filter(committed_to == team & end_year == p))$state_province) %>% 
      group_by(state_province, county_name) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup() %>% 
      mutate(
        area = paste0(county_name, " ", state_province)
      )
  }
  
  else{
    df <- df %>% 
      filter(committed_to == team) %>% 
      filter(year <= as.numeric(p) & year > as.numeric(p) - 5) %>% 
      filter(state_province %in% (key_states %>% filter(committed_to == team & end_year == p))$state_province) %>% 
      group_by(state_province) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup() %>% 
      mutate(
        area = state_province
      )
  }
  
  my_theme_time <- function () { 
    theme_bw() + theme(plot.title = element_text(size = 16, hjust = 0.5),
                       legend.title = element_text(size = 14),
                       plot.caption = element_text(size = 10),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "right",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  if(type_p == 'time'){
    
    key_states <- key_states %>% filter(committed_to == team) %>% rename("Year" = "end_year", "pre_share"="share") %>% mutate(Share = round(100*pre_share, 2))
    
    g <- ggplot(data = key_states, aes(x = Year, y = Share, color = state_province, text = paste0('State: ', state_province)))+
      scale_color_brewer(palette = 'Paired')+
      scale_x_continuous(breaks = c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))+
      labs(title = paste0(team, ' Key States 2007-2020'),
           x = 'Final Year of 5 Year Cycle',
           y = '% of Recruits from 5 Year Cycle',
           color = 'State')+
      geom_point(size = 5)+
      geom_line(size = 1)+
      ylim(0,100)+
      my_theme_time()
    
    return(ggplotly(g, tooltip = c("x","y","text")))
  }
  
  else if(type_p == 'bar'){
    
    g <- ggplot(data = df, aes(x = count, y = reorder(area, count), text = paste0('Recruits: ', count, '\nRegion: ', area)))+
      labs(title = paste0(team, ' Recruits from Key ',k_l," ",toString(as.numeric(p)-4),"-",toString(as.numeric(p))),
           x = 'Total Recruits in 5 Year Cycle',
           y = '')+
      geom_col(color = (color %>% filter(school == team))$sec_color[1], fill = (color %>% filter(school == team))$prim_color[1])+
      my_theme_time()
    
    return(ggplotly(g, tooltip = c("text")))
  }
  
  else{
    
    my_theme <- function () { 
      theme_bw() + theme(axis.title = element_blank(),
                         axis.text = element_blank(),
                         axis.ticks = element_blank(),
                         plot.title = element_text(size = 16, hjust = 0.5),
                         legend.title = element_text(size = 14),
                         plot.caption = element_text(size = 10),
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(), 
                         legend.position = "right",
                         panel.border = element_blank(), 
                         strip.background = element_rect(fill = 'white', colour = 'white'))
    }
    
    if(k_l == 'County'){
      mapper <- get_urbn_map(map = "counties", sf = TRUE) %>% 
        mutate(
          state_province = state_abbv
        ) %>% left_join(df) %>% rename('Recruits'='count')
      
      g <- ggplot(data = mapper %>% filter(state_province %in% df$state_province), aes(fill = Recruits, text = paste0('Region: ',area)))+
        geom_sf(color = 'black', size = 0.2)+
        scale_fill_continuous(high = (color %>% filter(school == team))$prim_color[1], low = 'grey', na.value = 'white')+
        labs(title = paste0(team,' Key Counties ',toString(as.numeric(p)-4),'-',toString(as.numeric(p))),
             fill = "Total Recruits")+
        labs(x = "", y = "")+
        my_theme()
      
      return(ggplotly(g, tooltip = c("fill","text")))
    }
    
    else{
      mapper <- get_urbn_map(map = "states", sf = TRUE) %>% 
        mutate(
          state_province = state_abbv
        ) %>% left_join(df) %>% rename('Recruits'='count')
      
      g <- ggplot(data = mapper %>% filter(state_province %in% df$state_province), aes(fill = Recruits, text = paste0('Region: ',area)))+
        geom_sf(color = 'black', size = 0.2)+
        scale_fill_continuous(high = (color %>% filter(school == team))$prim_color[1], low = 'grey', na.value = 'white')+
        labs(title = paste0(team,' Key States ',toString(as.numeric(p)-4),'-',toString(as.numeric(p))),
             fill = "Total Recruits")+
        labs(x = "", y = "")+
        my_theme()
      
      return(ggplotly(g, tooltip = c("fill","text")))
    }
    
  }
}

ui <- navbarPage(
  "FBS Recruiting Geography",
  tabPanel(
    "Overview",
    fluidPage(
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            inputId = "school",
            label = "School:",
            teams
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
            min = 2002,
            max = 2020,
            value = c(2002, 2020),
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
          ),
          
          downloadButton("downloadData","Download"),
          
          h5("By Conor McQuiston, @ConorMcQ5 on Twitter"),
          h6("Data from 247Sports, simplemaps.com, @CFB_Data. Built with @cfbscrapR. Inspired by @BigPlaidChad.")
        ),
        
        mainPanel(
          # Hide errors
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"),
          fluidRow(
            (plotlyOutput("distPlot", width = "100%"))
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Footprint Analysis",
    fluidPage(
      sidebarLayout(
        
        sidebarPanel(
          
          radioButtons(
            "key_type",
            "What kind of graph do you want to see?",
            choices = c("Map"='map',"Bar"='bar',"Time"='time'),
          ),
          
          selectInput(
            inputId = "focus_school",
            label = "School:",
            teams
          ),
          
          selectInput(
            "period",
            "Last Year of Cycle",
            c(2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007)
          ),
          
          radioButtons(
            "key_level",
            "Which level would you like to see?",
            c('County'='County','State'='State')
          ),
          
          actionButton(
            "key_update",
            "Show Distribution"
          ),
          
          downloadButton("key_downloadData","Download"),
          
          h4("Key States = 1-5 States it takes to get to 70% of Program\'s Recruits Over 5 Year Period"),
          h5("By Conor McQuiston, @ConorMcQ5 on Twitter"),
          h6("Data from 247Sports, simplemaps.com, @CFB_Data. Built with @cfbscrapR. Inspired by @BigPlaidChad.")
        ),
        
        mainPanel(
          # Hide errors
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"),
          fluidRow(
            (plotlyOutput("key_distPlot", width = "100%"))
          )
        )
      )
    )
  )
  
)
#Define server logic
server <- function(input, output){
  observeEvent(input$update, {
    output$distPlot <- renderPlotly({
      (make_maps(input$school, input$stars, input$years[1], input$years[2], input$level, input$state_province))
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$school,input$level,toString(input$years[1]),"_",toString(input$years[2]),input$state_province,'_minstars',toString(input$stars), ".csv")
    },
    content = function(file) {
      write.csv(make_maps_data(input$school, input$stars, input$years[1], input$years[2], input$level, input$state_province), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$key_update, {
    output$key_distPlot <- renderPlotly({
      (make_key_state_plots(input$key_type, input$focus_school, input$period, input$key_level))
    })
  })
  
  output$key_downloadData <- downloadHandler(
    filename = function() {
      paste0(input$focus_school,input$key_level,toString(input$period),input$key_type, ".csv")
    },
    content = function(file) {
      write.csv(make_key_state_data(input$key_type, input$focus_school, input$period, input$key_level), file, row.names = FALSE)
    }
  )
}


shinyApp(ui, server)
