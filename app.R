library(zoo)
library(tidyverse)
library(lubridate)
library(readxl)
library(shiny)
library(DT)
# library(plotly)
library(scales)
library(shinyWidgets)

towns <- read_csv("https://data.ct.gov/resource/28fr-iqnx.csv") %>%
  arrange(town) %>%
  pull(town) %>%
  unique()

all_town_pop <- read_xlsx("pop_towns2018.xlsx", skip = 10) %>%
  rename(population  = 2,
         town = Town)


ui <- fluidPage(
  titlePanel("Connecticut Covid Town/Area Cases"),
  
  # fluidRow(
  #   column(9, plotOutput("casesByDatePlot"))
  # ),
  # 
  sidebarLayout(
    sidebarPanel(
                fluidRow(
                 column(3, selectizeInput("town", "Select Town(s)", choices = towns, 
                                          select = 'Hartford', multiple = TRUE)),
                 column(3, checkboxInput("separate", "Separate Towns?", value = FALSE)),
                 column(3, checkboxInput("per100k", "Scale by per 100k", value = TRUE),
                        downloadButton("download_plot", "Download Chart"))
                 )
                ),
  
    mainPanel(
      fluidRow(
            column(5, plotOutput('casesByDatePlot', width = "640px", height = "600px"))
            
              )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  

  output$test <- renderText({
    towns <- input$town
    towns[length(towns)]
  })
  plot_reactive <- reactive({

    towns <- input$town
    
    towns <- str_replace_all(towns, "[[:space:]]", "%20")
    
    town_cases_raw <- NULL
    for(i in 1:length(towns) ){

      town_cases_raw <- read_csv(paste0("https://data.ct.gov/resource/28fr-iqnx.csv?town=", towns[i]) ) %>%
        bind_rows(town_cases_raw, .)
    }

    #get rid ot space escape
    towns <- str_replace_all(towns,  "%20", " ")
    
    

    all_dates <- seq(min(town_cases_raw$lastupdatedate), max(town_cases_raw$lastupdatedate), 'days') %>%
      enframe() %>%
      rename(days_from_origin = name,
             date = value) %>%
      mutate(date = as.Date(date) %>% force_tz("America/New_York")
      )

    all_dates <- all_dates %>%
      slice(rep(1:n(), length(towns)) )%>%
      mutate(town = rep(towns, each = nrow(all_dates)) )


    town_cases <- town_cases_raw %>%
      arrange(lastupdatedate) %>%
      mutate(date = lastupdatedate %>% as.Date() %>% force_tz("America/New_York")) %>%
      right_join(all_dates, by = c("date", "town")) %>%
      left_join(all_town_pop, by = "town") %>%
      group_by(town) %>%
      fill(towntotalcases:numberofindeterminates) %>%
      mutate_if(is.numeric, ~coalesce(as.numeric(.), as.numeric(0)) ) %>%
      mutate(new_cases = towntotalcases - lag(towntotalcases, default = 0),
             new_tests = peopletested - lag(peopletested, default = 0),
             new_deaths = towntotaldeaths - lag(towntotaldeaths, default = 0),
             new_negatives = numberofnegatives - lag(numberofnegatives, default = 0),
      ) %>%
      mutate_at(vars(starts_with("new_")), ~if_else(. < 0, 0, .) ) %>%
      mutate(roll_cases = rollmean(new_cases, 7, fill = 0),
             roll_tests = rollmean(new_tests, 7, fill = 0),
             roll_deaths = rollmean(new_deaths, 7, fill = 0),
             roll_negatives = rollmean(new_negatives, 7, fill = 0),
             roll_pos_pct = roll_cases / (roll_cases + roll_negatives),
             roll_case_rate = roll_cases*(100000/population),
             month = month(date),
             month_color = ifelse(month(date, label = FALSE) %% 2,"grey","white")

      ) %>%
      ungroup()

    x <- "date"
    if(input$per100k){
      y <- "roll_case_rate"
      title_text <- paste0(paste(towns, collapse = ", ")," Cases per 100,000 (7-day avg)")
    }else{
      y <- "roll_cases"
      title_text <- paste0(paste(towns, collapse = ", ")," Cases (7-day avg)")
    }


    if(input$separate){

      plot <- ggplot(town_cases, aes_string(x = x, y = y, group = 'town', col = 'town'))

    }else{
      plot <- town_cases %>%
        group_by(date) %>%
        summarize(roll_cases = sum(roll_cases),
                  roll_case_rate = sum(roll_case_rate*population)/sum(population)) %>%
        ggplot(aes_string(x = x, y = y))
    }
    
    if(input$per100k){  plot <- plot + geom_hline(yintercept = 10, color = 'red', linetype = 2)
}
    
    plot <- plot +
      geom_point() +
      geom_line() +
      scale_x_date(date_breaks = '1 month') +
      theme_bw() +
      ylab("") +
      xlab("Date") +
      ggtitle(title_text) +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          plot.title = element_text(size = 20))

    return(plot)
  })

  
  output$casesByDatePlot <- renderPlot({
    plot_reactive()
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){paste0(paste(input$town, collapse = "_"), '.png', sep = '')},
    content = function(file){
      ggsave(file, plot = plot_reactive())
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


