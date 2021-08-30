library(zoo)
library(tidyverse)
library(lubridate)
library(readxl)
library(shiny)
library(DT)
# library(plotly)
library(scales)
library(shinyWidgets)
library(plotly)

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
                 column(8, selectizeInput("town", "Select Town(s)", choices = towns, 
                                          select = 'Hartford', multiple = TRUE))
                 ),
                fluidRow(
                 column(8, checkboxInput("separate", "Separate Towns?", value = FALSE))
                 ),
                fluidRow(
                 column(8, 
                        dateRangeInput("dates", 
                                       label = h3("Date Range"),
                                       start = "2020-03-24", 
                                       end = as.character(ymd(Sys.Date()) ),
                                       min = "2020-03-24", 
                                       max = as.character(ymd(Sys.Date()) )
                                       )
                        )
                 ),
                fluidRow(
                 column(8, 
                        # checkboxInput("per100k", "Scale by per 100k", value = TRUE),
                        selectInput("dataColumn", "Select Data (7-Day Avg)",
                                    choices = c("Cases",
                                                "Cases per 100K", 
                                                "Pos Test Pct", 
                                                "Deaths",
                                                "Tests"), 
                                    selected = "Cases per 100K"),
                        downloadButton("download_plot", "Download Chart"))
                 )
                ),
  
    mainPanel(
      fluidRow(
            column(5, plotlyOutput('casesByDatePlot', width = "640px", height = "600px"))
            
              ),
      dataTableOutput("data_table")
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  js <- c(
    "table.on('draw.dt', function(){",
    "  var PageInfo = table.page.info();",
    "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
    "    cell.innerHTML = i + 1 + PageInfo.start;",
    "  });",
    "})")

  output$test <- renderText({
    towns <- input$town
    towns[length(towns)]
  })
  
  data_reactive <- reactive({
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
             update_year = year(date),
             month = month(date),
             month_color = ifelse(month(date, label = FALSE) %% 2,"grey","white"),
             month_year = paste0(month, "/", substr(update_year,3,4) )
             
      ) %>%
      ungroup() %>% 
      filter(!is.na(lastupdatedate ))
    
    
    town_cases <- town_cases %>% 
      filter(date >= ymd(input$dates[1]),
             date <= ymd(input$dates[2]) 
             )
    
    return(town_cases)
  })
  
  plot_reactive <- reactive({

    towns <- input$town
    towns <- str_replace_all(towns, "[[:space:]]", "%20")
    towns <- str_replace_all(towns,  "%20", " ")
    
    
    town_data <- data_reactive()

    x <- "date"
    if(input$dataColumn == "Cases per 100K"){
      y <- "roll_case_rate"
      title_text <- paste0(paste(towns, collapse = ", "), " Cases per 100,000 (7-day avg)")
    }else if(input$dataColumn == "Cases"){
      y <- "roll_cases"
      title_text <- paste0(paste(towns, collapse = ", "), " Cases (7-day avg)")
    }else if(input$dataColumn == "Pos Test Pct"){
      y <- "roll_pos_pct"
      title_text <- paste0(paste(towns, collapse = ", "), " Test Positiviey Pct (7-day avg)")
    }else if(input$dataColumn == "Deaths"){
      y <- "roll_deaths"
      title_text <- paste0(paste(towns, collapse = ", "), " Deaths (7-day avg)")
    }else if(input$dataColumn == "Tests"){
      y <- "roll_tests"
      title_text <- paste0(paste(towns, collapse = ", "), " Tests (7-day avg)")
    }


    if(input$separate){

      plot <- ggplot(town_data, aes_string(x = x, y = y, group = 'town', col = 'town'))

    }else{
      plot <- town_data %>%
        group_by(date) %>%
        summarize(roll_cases = sum(roll_cases),
                  roll_case_rate = sum(roll_case_rate*population)/sum(population),
                  roll_tests = sum(roll_tests), 
                  roll_deaths = sum(roll_deaths),
                  roll_pos_pct = sum(roll_pos_pct),
                  population = sum(population),
                  new_cases = sum(new_cases), 
                  new_tests = sum(new_tests),
                  new_deaths = sum(new_deaths), 
                  new_negatives = sum(new_negatives)
                  ) %>%
        ggplot(aes_string(x = x, y = y))
    }
    
    if(input$dataColumn == "Cases per 100K"){  
      plot <- plot + geom_hline(yintercept = 10, color = 'red', linetype = 2)
      }
    
    plot <- plot +
      geom_point() +
      geom_line() +
      # scale_x_date(date_breaks = '1 month') +
      theme_bw() +
      ylab("") +
      xlab("Date") +
      ggtitle(title_text) +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          plot.title = element_text(size = 20))

    plot <- ggplotly(plot)
    
    return(plot)
  })

  
  output$casesByDatePlot <- renderPlotly({#renderPlot({
    plot_reactive()
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){paste0(paste(input$town, collapse = "_"), '.png', sep = '')},
    content = function(file){
      ggsave(file, plot = plot_reactive())
    }
  )
  
  
  output$data_table = renderDataTable({
    data <- data_reactive() %>% 
      mutate(date = as.Date(date)) %>% 
      select(town, population, date,
             starts_with("roll_"), 
             starts_with("new_")
             ) %>% 
      arrange(desc(date), town)
    
    ## round some columns

    cols = colnames(data)

    colnames(data) <- cols %>% 
      str_replace_all(., "_", " ") %>% 
      str_to_title()
    
    num.cols <- ncol(data)
    
    
    data <- data %>%
      mutate(`Roll Pos Pct` = `Roll Pos Pct`*100) %>% 
      datatable(colnames = cols,
                rownames = T,
                filter = 'top',
                style = 'bootstrap',
                class = 'table-condensed table-hover table-responsive ', # table-nowrap didn't work
                extensions = c('ColReorder',  'FixedHeader', 'KeyTable', 'Buttons'),
                # escape=9, ## was for if you have HTML.  diff.PIR had HTML for the arrows, but doesn't anymore
                options = list(
                  colReorder = TRUE, ## formatRound stops working. so gotta round manually above
                  #scrollX = TRUE, fixedColumns = TRUE, ## alignment gets messed up
                  fixedHeader = T,
                  # keys = TRUE, don't like that
                  dom = 'Brtip', ## each letter means something. Forget what.
                  buttons = c('copy', 'csv', 'excel', I('colvis')),
                  pageLength = 50,
                  columnDefs = list(list(
                    orderSequence = c('desc', 'asc')#,
                    # className = 'dt-center', #centers columns but only the targets and that's being used to hide passer_id
                    # targets = c(3,4), ### hide 3rd column
                    # visible = FALSE ## hide that column
                  ))
                ),
                
                callback = JS(js)### reference js code that keeps rownames in order even when sorted
      ) %>%
      formatStyle(c("Roll Cases", "Roll Tests", "Roll Deaths", "Roll Negatives", 
                    "Roll Pos Pct", "Roll Case Rate"),
                  backgroundSize = '95% 95%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatRound(c("Roll Cases", "Roll Tests", "Roll Deaths", "Roll Negatives", 
                   "Roll Pos Pct", "Roll Case Rate"), digits=1)
    

    
    
    data
    

  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


