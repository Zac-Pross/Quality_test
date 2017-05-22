library(shiny)
library(shinyjs)
library(tidyverse)
library(broom)
library(plotly)
library(shinythemes)
library(RColorBrewer)
library(ggthemes)
library(formattable)
library(DT)
library(psych)
library(lazyeval)
library(rpart)
library(rpart.plot)
# library(rattle)
library(caret)
library(purrr)
library(lubridate)
library(naniar)
library(visdat)
library(gridExtra)

colour_custom <- brewer.pal(3, 'Dark2')


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

plot_theme <- 
  theme(
    # plot.margin = unit(c(1,1,1,1), "cm"), 
    axis.title.x = element_text(colour = "#636363"
                                # , vjust = -2
    ),
    axis.title.y = element_text(colour = "#636363"
                                # , vjust = -0.5
    ),
    axis.text.x = element_text(colour = "#636363"),
    axis.text.y = element_text(colour = "#636363"),
    axis.ticks = element_line(colour = "#636363"),
    axis.line = element_line(colour = "#636363"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = 'none')

align_center <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

col_formatter <-
  formatter("span", style = x ~ style(color = 'white',
                                      'border-radius' = '4px',
                                      'padding-right' = '4px',
                                      'background-color' = 
                                        ifelse(x < 0.8, csscolor('#e41a1c'), 
                                               ifelse(x >= 0.8 & x < 0.95, csscolor('#ff7f00'),
                                                      csscolor('#4daf4a')))))

col_formatter2 <-
  formatter("span", style = x ~ style(color = 'white',
                                      'border-radius' = '4px',
                                      'padding-right' = '4px',
                                      'background-color' = 
                                        ifelse(x >= 0.03, csscolor('#e41a1c'), 
                                               ifelse(x > 0.0 & x < 0.03, csscolor('#ff7f00'),
                                                      csscolor('#4daf4a')))))

appCSS <- "
#loading-content {
position: absolute;
padding: 10% 0 0 0;
background: #2d3e50;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

t <- list(
  family = "helvetica"
)

m <- list(
  l = 75,
  r = 75,
  b = 75,
  t = 10,
  pad = 0
)

date_vec <- c('Date', 'date', 
              'Birth_date', 'birth_date', 'Birth_Date', 
              'birth_Date', 'birthdate', 'Birthdate', 
              'BirthDate', 'birthDate', 'Birth', 'birth',
              'DateBirth', 'Datebirth', 'datebirth', 
              'dateBirth', 'Date_Birth', 'Date_birth',
              'date_birth', 'date_Birth')


server <- function (input, output, session) {
  
  options(shiny.maxRequestSize=1000*1024^2) 
  
  session$onSessionEnded(stopApp)
  
  # Simulate work being done for 1 second
  Sys.sleep(2)
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade") 
  
  
  # Load the data when the user inputs a file
  theData <- reactive({
    infile <- input$datfile        
    if(is.null(infile))
      return(NULL)        
    d <- read.csv(infile$datapath, header = T, stringsAsFactors = F)
    d
  })
  
  date_filter <- reactive({
    
    if (input$input_filter %in% date_vec) {
      
      temp_dmy <- dmy(theData()[[input$input_filter]])
      temp_myd <- myd(theData()[[input$input_filter]])
      temp_ymd <- ymd(theData()[[input$input_filter]])
      temp_ydm <- ydm(theData()[[input$input_filter]])
      temp_dym <- dym(theData()[[input$input_filter]])
      temp_mdy <- mdy(theData()[[input$input_filter]])
      
      na_length <- function(x) {
        mean(is.na(x)) <= 0.1
      }
      
      date_format <- function(x) {
        theData()[x(theData()[[input$input_filter]]) >= input$input_filter_value[1] & 
                    x(theData()[[input$input_filter]]) <= input$input_filter_value[2], ]
      }
      
      if (na_length(temp_dmy) == T) {
        date_format(dmy)
      } else if (na_length(temp_myd) == T) {
        date_format(myd) 
      } else if (na_length(temp_ymd) == T) {
        date_format(ymd)
      } else if (na_length(temp_ydm) == T) {
        date_format(ydm) 
      } else if (na_length(temp_dym) == T) {
        date_format(dym) 
      } else if (na_length(temp_mdy) == T) {
        date_format(mdy)
      }
    }
  })
  
  output$go_table <- renderUI({
    if (!is.null(theData())) {
      actionButton('go_table', 'Render table')
    }
  })
  
  miss_data <- reactive({
    
    if (input$input_group == 'No grouping') {
      
      if (input$input_filter != 'No filter' & !(input$input_filter %in% date_vec)) {
        
        filter_criteria <- interp(~ which_column %in% input$input_filter_value, 
                                  which_column = as.name(input$input_filter))
        
        d <- 
          theData() %>% 
          filter_(filter_criteria) %>% 
          summarise_each(funs(percent(1 - mean(is.na(.)), 1)))
        
      } else if (input$input_filter != 'No filter' & (input$input_filter %in% date_vec)) {
        
        d <- 
          date_filter() %>% 
          summarise_each(funs(percent(1 - mean(is.na(.)), 1)))
        
      } else if (input$input_filter == 'No filter') {
        
        d <- 
          theData() %>% 
          summarise_each(funs(percent(1 - mean(is.na(.)), 1)))
      }
      
    } else {
      
      if (input$input_filter != 'No filter' & !(input$input_filter %in% date_vec)) {
        
        filter_criteria <- interp(~ which_column %in% input$input_filter_value, 
                                  which_column = as.name(input$input_filter))
        
        d <- 
          theData() %>%
          filter_(filter_criteria) %>%
          group_by_(input$input_group) %>%
          summarise_each(funs(percent(1 - (mean(is.na(.))), 1)))
        
      } else if (input$input_filter != 'No filter' & (input$input_filter %in% date_vec)) {
        
        d <- 
          date_filter() %>% 
          group_by_(input$input_group) %>% 
          summarise_each(funs(percent(1 - (mean(is.na(.))), 1)))
        
      } else if (input$input_filter == 'No filter') {
        d <- 
          theData() %>%
          group_by_(input$input_group) %>%
          summarise_each(funs(percent(1 - (mean(is.na(.))), 1)))
      }
    }
    d
  })
  
  tree_data <- reactive({
    
    if (input$input_radio_tree == 'Manual Select') {    
      
      d <- theData()[, input$input_group_tree]
      
      d <-  
        d %>% 
        select(-contains('date'), -contains('name'), -contains('time')) %>% 
        # mutate_if(is.character, as.factor) %>% 
        # select_if(is.numeric) %>% 
        mutate(rowMiss = rowSums(is.na(theData())) / length(names(theData())))
      d
      
    } else {
      
      d <-  
        theData() %>% 
        select(-contains('date'), -contains('name'), -contains('time')) %>% 
        # mutate_if(is.character, as.factor) %>%
        # select_if(is.numeric) %>% 
        mutate(rowMiss = rowSums(is.na(theData())) / length(names(theData())))
      d
      
    }
  })
  
  output$input_group_tree <- renderUI({
    
    if (input$input_radio_tree == 'Manual Select') {
      checkboxGroupInput('input_group_tree', 'Choose variables',
                         choices = names(theData()))
    } else {
      NULL
    }
  })
  
  output$go_tree <- renderUI({
    if (!is.null(theData())) {
      actionButton('go_tree', 'Render tree')
    }
  })
  
  tree_model <- reactive({
    
    tc <- trainControl("cv", 
                       10,
                       selectionFunction = 'oneSE')
    
    rpart.grid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
    # rpart.grid <- expand.grid(.cp = 0.015)
    
    group <- match('rowMiss', names(tree_data()))
    
    train.rpart <- train(x = tree_data()[, -group], y = tree_data()[[group]], 
                         method    = "rpart",
                         control = rpart.control(maxdepth = 4),
                         trControl = tc, 
                         tuneGrid  = rpart.grid,
                         na.action = na.pass)
    train.rpart
    
  })
  
  output$input_radio_tree <- renderUI({
    if (!is.null(theData())) {
      radioButtons("input_radio_tree", "Variable Selection Type:",
                   choices = c("All", "Manual Select"),
                   selected = "All", inline = T)
    } else {
      NULL
    }
  })
  
  
  
  output$input_radio_alpha <- renderUI({
    if (!is.null(theData())) {
      radioButtons("input_radio_alpha", "Variable Selection Type:",
                   choices = c("All", "Manual Select"),
                   selected = "All", inline = T)
    } else {
      NULL
    }
  })
  
  output$input_select_alpha <- renderUI({
    
    alpha_names <- theData() %>% select_if(is.numeric) %>% names()
    
    if (input$input_radio_alpha == 'Manual Select') {
      checkboxGroupInput('input_select_alpha', "Choose variables", 
                         choices = alpha_names)
    } else {
      NULL
    }
  })
  
  alpha_data <- reactive({
    
    if (input$input_radio_alpha == 'Manual Select') {    
      
      d <- theData()[ , input$input_select_alpha]
      
      d %>% 
        select_if(is.numeric) %>% 
        psych::alpha(.)
      
    } else {
      theData() %>% 
        select_if(is.numeric) %>% 
        psych::alpha(.)
    }
  })
  
  
  output$go_alpha <- renderUI({
    if (!is.null(theData())) {
      actionButton('go_alpha', 'Render results')
    }
  })
  
  output$input_radio_alpha <- renderUI({
    if (!is.null(theData())) {
      radioButtons("input_radio_alpha", "Variable Selection Type:",
                   choices = c("All", "Manual Select"),
                   selected = "All", inline = T)
    } else {
      NULL
    }
  })
  
  
  
  
  ########################################
  output$input_select_likert_distribution <- renderUI({
    
    if (!is.null(theData())) {
      selectInput('input_select_likert_distribution', "Choose Likert item", 
                  choices = names(theData()))
    } else {
      NULL
    }
  })
  
  output$go_likert_distribution <- renderUI({
    if (!is.null(theData())) {
      actionButton('go_likert_distribution', 'Render plot')
    } else {
      NULL
    }
  })
  
  quantiles <- reactive({
    
    if (is.character(theData()[[input$input_select_likert_distribution]])) {
      
      i <- 0
      vec <- 0
      for (i in seq_along(theData()[[input$input_select_likert_distribution]])) {
        vec[i] <- i
      }
      
      quantile(theData()[['vec']], c(0.25, 0.75), na.rm = T)
      
    } else {
      
      quantile(theData()[[input$input_select_likert_distribution]], c(0.25, 0.75), na.rm = T)
      
    }
  })
  
  q <- c(paste('25%'), paste('75%'))
  
  
  likert_distribution_plot_data <- reactive({
    # d <- theData()[, input$input_select_likert_distribution]
    d <- 
      theData() %>% 
      dplyr::select_(input$input_select_likert_distribution) %>% 
      group_by_(input$input_select_likert_distribution) %>% 
      summarise(Count = n())
    
    d$quantiles <- ifelse(d[ , input$input_select_likert_distribution] == quantiles()[[1]], paste('25%'),
                          ifelse(d[ , input$input_select_likert_distribution] == quantiles()[[2]], paste('75%'),
                                 NA))
    
    d
  })
  
  
  
  scale_likert <- function(dt, x)
    if (is.character(dt[[x]])) {
      scale_x_discrete()
    } else {
      scale_x_continuous(breaks = seq(min(dt[[x]], na.rm = T), 
                                      max(dt[[x]], na.rm = T), by = 1))
    }
  
  
  # scale_likert <- reactive({
  #   if (is.character(likert_distribution_plot_data()[[input$input_select_likert_distribution]])) {
  #     scale_x_discrete()
  # 
  #   } else if (is.numeric(likert_distribution_plot_data()[[input$input_select_likert_distribution]])) {
  #     scale_x_continuous(breaks = seq(min(likert_distribution_plot_data()[, input$input_select_likert_distribution], 
  #                                         na.rm = T), 
  #                                     max(likert_distribution_plot_data()[, input$input_select_likert_distribution],
  #                                         na.rm = T), 
  #                                     by = 1))
  #   } else {
  #     NULL
  #   }
  # })
  
  
  likert_distribution_plot_data_reactive <- eventReactive(input$go_likert_distribution, {
    d <- 
      likert_distribution_plot_data() %>% 
      ggplot(aes_string(x = input$input_select_likert_distribution, y = 'Count')) + 
      geom_text(aes_string(label = 'quantiles'),
                size = 5,
                colour = '#636363',
                vjust = -1,
                data = likert_distribution_plot_data()[likert_distribution_plot_data()$quantiles %in% q, ]) +
      geom_bar(stat = 'identity', fill = colour_custom[[2]], colour = 'white') +
      plot_theme +
      theme(axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5))) +
      scale_likert(theData(), input$input_select_likert_distribution)
    
    d
  })
  
  output$likert_distribution_plot <- renderPlot({
    likert_distribution_plot_data_reactive()
  })
  
  
  likert_distribution_table_data <- reactive({
    d <-
      data.frame(Mode = Mode(theData()[[input$input_select_likert_distribution]]),
                 Quantile1 = quantile(theData()[[input$input_select_likert_distribution]], 
                                      0.25, 
                                      na.rm = T),
                 Quantile2 = quantile(theData()[[input$input_select_likert_distribution]], 
                                      0.75, 
                                      na.rm = T),
                 IQR = IQR(theData()[[input$input_select_likert_distribution]], na.rm = T))
    d
  })
  
  likert_distribution_table_data_reactive <- eventReactive(input$go_likert_distribution, {
    d <- likert_distribution_table_data()
    d
    
  })
  
  
  output$likert_distribution_table <- renderTable({
    likert_distribution_table_data_reactive()
  })
  
  
  ###################################################################################
  output$input_select_date <- renderUI({
    
    if (!is.null(theData())) {
      selectInput('input_select_date', "Choose date variable", 
                  choices = names(theData()))
    } else {
      NULL
    }
  })
  
  output$input_select_name <- renderUI({
    
    if (!is.null(theData())) {
      selectInput('input_select_name', "Choose name variable", 
                  choices = c('No name', names(theData())))
    } else {
      NULL
    }
  })
  
  output$go_date <- renderUI({
    if (!is.null(theData())) {
      actionButton('go_date', 'Render plot')
    } else {
      NULL
    }
  })
  
  output$input_filter_date <- renderUI({
    if (input$input_select_date %in% date_vec) {
      
      temp_dmy <- dmy(theData()[[input$input_select_date]])
      temp_myd <- myd(theData()[[input$input_select_date]])
      temp_ymd <- ymd(theData()[[input$input_select_date]])
      temp_ydm <- ydm(theData()[[input$input_select_date]])
      temp_dym <- dym(theData()[[input$input_select_date]])
      temp_mdy <- mdy(theData()[[input$input_select_date]])
      
      na_length <- function(x) {
        mean(is.na(x)) <= 0.1
      }
      
      min_date <- function(x) {
        min(x(theData()[[input$input_select_date]]))
      }
      
      max_date <- function(x) {
        max(x(theData()[[input$input_select_date]]))
      }
      
      if (na_length(temp_dmy) == T) {
        d <- c(min_date(dmy), max_date(dmy))
        
      } else if (na_length(temp_myd) == T) {
        d <- c(min_date(myd), max_date(myd))
        
      } else if (na_length(temp_ymd) == T) {
        d <- c(min_date(ymd), max_date(ymd))
        
      } else if (na_length(temp_ydm) == T) {
        d <- c(min_date(ydm), max_date(ydm))
        
      } else if (na_length(temp_dym) == T) {
        d <- c(min_date(dym), max_date(dym))
        
      } else if (na_length(temp_mdy) == T) {
        d <- c(min_date(mdy), max_date(mdy))
      }
      
      # dateRangeInput('input_filter_value', 'Date range',
      #                min = d[[1]], max = d[[2]],
      #                start = d[[1]], end = d[[2]])
      
      sliderInput("input_filter_date",
                  "Choose Date Range:", 
                  min = d[[1]], max = d[[2]], 
                  value = c(d[[1]], d[[2]]))
      
    } else {
      NULL
    }
  })
  
  date_filter2 <- reactive({
    
    if (input$input_select_date %in% date_vec) {
      
      temp_dmy <- dmy(theData()[[input$input_select_date]])
      temp_myd <- myd(theData()[[input$input_select_date]])
      temp_ymd <- ymd(theData()[[input$input_select_date]])
      temp_ydm <- ydm(theData()[[input$input_select_date]])
      temp_dym <- dym(theData()[[input$input_select_date]])
      temp_mdy <- mdy(theData()[[input$input_select_date]])
      
      na_length <- function(x) {
        mean(is.na(x)) <= 0.1
      }
      
      date_format <- function(x) {
        theData()[x(theData()[[input$input_select_date]]) >= input$input_filter_date[1] & 
                    x(theData()[[input$input_select_date]]) <= input$input_filter_date[2], ]
      }
      
      if (na_length(temp_dmy) == T) {
        date_format(dmy)
      } else if (na_length(temp_myd) == T) {
        date_format(myd) 
      } else if (na_length(temp_ymd) == T) {
        date_format(ymd)
      } else if (na_length(temp_ydm) == T) {
        date_format(ydm) 
      } else if (na_length(temp_dym) == T) {
        date_format(dym) 
      } else if (na_length(temp_mdy) == T) {
        date_format(mdy)
      }
    }
  })
  
  date_data <- reactive({
    
    if (!is.null(theData())) {
      
      if (input$input_select_name == 'No name') {
        
        e <- 
          data.frame(date_filter2()[ , input$input_select_date])
        
      } else {
        e <- 
          date_filter2()[ , c(input$input_select_date, input$input_select_name)]
      }
      
      d <- 
        e %>% 
        mutate(Date = dmy(.[[1]]))
      
      if (mean(is.na(d$Date)) > 0.1) {
        d <- 
          e %>% 
          mutate(Date = ymd(.[[1]]))
      }
      
      if (mean(is.na(d$Date)) > 0.1) {
        d <- 
          e %>% 
          mutate(Date = mdy(.[[1]]))
      }
      
      if (mean(is.na(d$Date)) > 0.1) {
        d <- 
          e %>% 
          mutate(Date = myd(.[[1]]))
      }
      
      if (mean(is.na(d$Date)) > 0.1) {
        d <- 
          e %>% 
          mutate(Date = ydm(.[[1]]))
      }
      
      if (mean(is.na(d$Date)) > 0.1) {
        d <- 
          e %>% 
          mutate(Date = dym(.[[1]]))
      }
      
      if (input$input_select_name == 'No name') {
        
        d <-
          d %>%
          distinct(Date) %>%
          arrange(Date) %>%
          mutate(
            Between = as.difftime(difftime(Date, lag(Date, 1),
                                           units = 'days')))
        d
      } else {
        
        d <-
          d %>%
          group_by_(input$input_select_name) %>%
          arrange(Date) %>%
          distinct(Date) %>% 
          nest() %>%
          mutate(
            Between =
              map(data, ~ as.difftime(difftime(.x$Date, lag(.x$Date, 1),
                                               units = 'days')))) %>%
          unnest(data, Between)
        
        d
      }
      
      # d %>%
      #   group_by_(input$input_select_name) %>%
      #   summarise(mean_days_since_last = mean(Between, na.rm = T))
      
    } else {
      NULL
    }
  })
  
  date_summary <- reactive({
    
    if (input$input_select_name == 'No name') {
      date_data() %>% 
        summarise(mean_days_since_last = round(mean(Between, na.rm = T), 1)) %>% 
        arrange(desc(mean_days_since_last))
      
    } else {
      date_data() %>%
        group_by_(input$input_select_name) %>%
        summarise(mean_days_since_last = round(mean(Between, na.rm = T), 1)) %>%
        arrange(desc(mean_days_since_last))
    }
  })
  
  date_summary_table_reactive <- eventReactive(input$go_date, {
    
    if (!is.null(date_summary())) {
      
      date_summary()
      
    } else {
      data.frame()
      
    }
  })
  
  output$date_summary_table <- renderDataTable({
    date_summary_table_reactive()
  })
  
  date_line_plot_reactive <- eventReactive(input$go_date, {
    if (!is.null(date_data())) {
      
      if (input$input_select_name == 'No name') {
        d <- 
          date_data() %>% 
          ggplot(aes_string(x = 'Date', y = 'Between')) + 
          geom_point(colour = colour_custom[[2]]) +
          geom_line(colour = colour_custom[[2]]) +
          plot_theme +
          theme(legend.position = 'none') +
          scale_color_brewer(palette = 'Dark2')
        # facet_wrap(~date_data()[[1]])
        
        ggplotly(d)
        
      } else {
        choose_facets_var <- input$input_select_name
        facet_var <- facet_wrap(as.formula(paste("~", choose_facets_var)))
        
        d <- 
          date_data() %>% 
          ggplot(aes_string(x = 'Date', y = 'Between', colour = input$input_select_name)) + 
          geom_point() +
          geom_line() +
          theme(legend.position = 'none') +
          facet_var +
          plot_theme +
          scale_colour_brewer(palette = 'Dark2')
        # facet_wrap(~date_data()[[1]])
        
        ggplotly(d)
      }
      
    } else {
      d <- data.frame() %>% 
        ggplot() + 
        geom_point() +
        theme_classic() +
        theme(axis.line = element_blank())
      
      ggplotly(d)
    }
    
  })
  
  output$date_line_plot <- renderPlotly({
    date_line_plot_reactive()
  })
  
  date_bar_plot_reactive <- eventReactive(input$go_date, {
    if (!is.null(date_summary()) & input$input_select_name != 'No name') {
      d <- 
        date_summary() %>% 
        ggplot(aes_string(x = input$input_select_name, y = 'mean_days_since_last', 
                          fill = input$input_select_name)) + 
        geom_bar(stat = 'identity') +
        plot_theme +
        theme(legend.position = 'none') +
        scale_fill_brewer(palette = 'Dark2')
      
      ggplotly(d)
      
    } else {
      d <- data.frame() %>% 
        ggplot() + 
        geom_blank() +
        theme_classic() +
        theme(axis.line = element_blank())
      
      ggplotly(d)
    }
  })
  
  output$date_bar_plot <- renderPlotly({
    date_bar_plot_reactive()
  })
  
  
  output$input_group <- renderUI({
    if (!is.null(theData())) {
      selectInput('input_group', "Group var", 
                  choices = c('No grouping', names(theData())))
    } else {
      NULL
    }
  })
  
  output$input_filter <- renderUI({
    if (!is.null(theData())) {
      selectInput('input_filter', "Filter var", 
                  choices = c('No filter', names(theData())))
    } else {
      NULL
    }
  })
  
  output$input_filter_value <- renderUI({
    # if (input$input_filter != 'No filter' & !(input$input_filter %in% date_vec)) {
    
    if (input$input_filter != 'No filter') {
      
      if(!(input$input_filter %in% date_vec) & 
         !(is.numeric(theData()[[input$input_filter]]))) {
        
        checkboxGroupInput('input_filter_value', "Filter value", 
                           choices = unique(theData()[ , input$input_filter]),
                           inline = T)
        
      } else if (input$input_filter %in% date_vec) {
        
        temp_dmy <- dmy(theData()[[input$input_filter]])
        temp_myd <- myd(theData()[[input$input_filter]])
        temp_ymd <- ymd(theData()[[input$input_filter]])
        temp_ydm <- ydm(theData()[[input$input_filter]])
        temp_dym <- dym(theData()[[input$input_filter]])
        temp_mdy <- mdy(theData()[[input$input_filter]])
        
        na_length <- function(x) {
          mean(is.na(x)) <= 0.1
        }
        
        min_date <- function(x) {
          min(x(theData()[[input$input_filter]]))
        }
        
        max_date <- function(x) {
          max(x(theData()[[input$input_filter]]))
        }
        
        if (na_length(temp_dmy) == T) {
          d <- c(min_date(dmy), max_date(dmy))
          
        } else if (na_length(temp_myd) == T) {
          d <- c(min_date(myd), max_date(myd))
          
        } else if (na_length(temp_ymd) == T) {
          d <- c(min_date(ymd), max_date(ymd))
          
        } else if (na_length(temp_ydm) == T) {
          d <- c(min_date(ydm), max_date(ydm))
          
        } else if (na_length(temp_dym) == T) {
          d <- c(min_date(dym), max_date(dym))
          
        } else if (na_length(temp_mdy) == T) {
          d <- c(min_date(mdy), max_date(mdy))
        }
        
        # dateRangeInput('input_filter_value', 'Date range',
        #                min = d[[1]], max = d[[2]],
        #                start = d[[1]], end = d[[2]])
        
        sliderInput("input_filter_value",
                    "Choose date range:", 
                    min = d[[1]], max = d[[2]], 
                    value = c(d[[1]], d[[2]]))
        
      } else if (is.numeric(theData()[[input$input_filter]])) {
        
        min_val <- function(x) {
          min(x[[input$input_filter]], na.rm = T)
        }
        
        max_val <- function(x) {
          max(x[[input$input_filter]], na.rm = T)
        }
        
        sliderInput("input_filter_value",
                    "Choose range:", 
                    min = min_val(theData()), max = max_val(theData()), 
                    value = c(min_val(theData()), max_val(theData())))
      }
      
      
    } else {
      NULL
    }
  })
  
  
  format_fun <- reactive({
    
    if (input$input_group != 'No grouping') {
      format_group <- 0
      for (i in names(miss_data()[, -1])) {
        format_group[i] <- paste("col_formatter")
      }
      
      formattable(miss_data(), format_group)
      
    } else {
      
      format_team <- 0
      for (i in names(miss_data())) {
        format_team[i] <- paste("col_formatter")
      }
      
      formattable(miss_data(), format_team)
      
    }
  })
  
  format_table_reactive <- eventReactive(input$go_table, {
    if (!is.null(theData())) {
      
      # formattable
      as.datatable(format_fun(), options = list(scrollX = T))
      
    } else {
      NULL
    }
  })
  
  
  output$format_table <- renderDataTable({
    
    format_table_reactive()
    
  })
  
  miss_tree_reactive <- eventReactive(input$go_tree, {
    if (!is.null(tree_model())) {
      
      rpart.plot(tree_model()$finalModel,
                 extra = 100, 
                 yesno = 2, 
                 branch = 0, 
                 branch.lty = 3, 
                 # tweak = 0.7,
                 shadow.col = 'lightgrey', 
                 nn = T,
                 cex = 1.2)
      
    } else {
      NULL
    }
  })
  
  output$miss_tree <- renderPlot({
    miss_tree_reactive()
  })
  
  alpha_data_reactive <- eventReactive(input$go_alpha, {
    
    if (!is.null(theData())) {
      
      alpha_data()
      
    } else {
      NULL
    }
  })
  
  output$alpha_table <- renderPrint({
    alpha_data_reactive()
  })
  
  output$input_missing_plot_select <- renderUI({
    radioButtons('input_missing_plot_select', 'Select plot type', 
                 choices = c('Bar', 'Points', 'Heatmap', 'Shadow'))
  })
  
  output$go_missing_plot <- renderUI({
    if (!is.null(theData())) {
      actionButton('go_missing_plot', 'Render plot')
    }
  })
  
  output$input_missing_var1 <- renderUI({
    if (!is.null(theData())) {
      selectInput('input_missing_var1', 'Choose variable', 
                  choices = names(theData()))
    }
  })
  
  output$input_missing_var2 <- renderUI({
    if (!is.null(theData())) {
      selectInput('input_missing_var2', 'Choose variable', 
                  choices = names(theData()))
    }
  })
  
  # observeEvent(input$input_missing_plot_select, {
  #   if (input$input_missing_plot_select != 'Bar') {
  #     show(id = 'input_missing_var1')
  #     show(id = 'input_missing_var2')
  #   } else {
  #     hide(id = 'input_missing_var1')
  #     hide(id = 'input_missing_var2')
  #     
  #   }
  # })
  
  missing_plots_data <- reactive({
    d <- data.frame(theData()[, c(input$input_missing_var1, input$input_missing_var2)])
    d
  })
  
  
  missing_points_reactive <- eventReactive(input$go_missing_plot, {
    
    if (!is.null(theData())) {
      
      if (input$input_missing_plot_select == 'Bar') {
        d <- gg_missing_var(theData())
        d
        
      } else if (input$input_missing_plot_select == 'Points') {
        
        d <- missing_plots_data() %>% 
          ggplot(aes_string(x = input$input_missing_var1,
                            y = input$input_missing_var2)) + 
          geom_missing_point(size = 1.5) + 
          plot_theme +
          theme(axis.text = element_text(size = rel(1.5)), 
                axis.title = element_text(size = rel(1.5))) +
          scale_colour_brewer(palette = 'Dark2', direction = -1)
        d
        
      } else if (input$input_missing_plot_select == 'Heatmap') {
        
        d <- vis_miss(missing_plots_data()) + 
          plot_theme +
          theme(axis.text = element_text(size = rel(1.5)), 
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)),
                legend.title = element_text(size = rel(1.5)),
                legend.position = 'right')
        d
        
      } else if (input$input_missing_plot_select == 'Shadow') {
        
        shadow_var_hist <- paste0('~', names(missing_plots_data()[2]), '_NA')
        
        shadow_var_density <- paste0(names(missing_plots_data()[2]), '_NA')
        
        facet_var <- facet_wrap(as.formula(paste("~", shadow_var_hist)), ncol = 1)
        
        p1 <- 
          bind_shadow(missing_plots_data()) %>% 
          ggplot(aes_string(x = 'missing_plots_data()[1]')) + 
          geom_histogram(na.rm = TRUE) +
          facet_var +
          plot_theme +
          theme(strip.text = element_text(size = rel(1.5)),
                axis.text = element_text(size = rel(1.5)), 
                axis.title = element_text(size = rel(1.5))) +
          labs(x = paste(input$input_missing_var1, collapse = " "))
        # facet_wrap(~.[[2]], ncol = 1)
        
        p2 <- 
          bind_shadow(missing_plots_data()) %>% 
          ggplot(aes_string(x = 'missing_plots_data()[1]', colour = shadow_var_density)) + 
          geom_density(na.rm = TRUE) +
          plot_theme +
          theme(legend.position = c(0.90,0.95),
                legend.text = element_text(size = rel(1.5)),
                legend.title = element_text(size = rel(1.5)),
                axis.text = element_text(size = rel(1.5)), 
                axis.title = element_text(size = rel(1.5))) +
          labs(x = paste(input$input_missing_var1, collapse = " "))
        
        gridExtra::grid.arrange(p1,p2,ncol = 2)
        
      }
      
    }
  })
  
  output$missing_points_plot <- renderPlot({
    missing_points_reactive()
  })
  
  
  #
  #
  #
  #
  #
  #
  #
  
  output$input_group_outlier <- renderUI({
    if (!is.null(theData())) {
      
      selectInput('input_group_outlier', "Group var", 
                  choices = c('No grouping', names(theData())))
    } else {
      NULL
    }
  })
  
  output$input_filter_outlier <- renderUI({
    if (!is.null(theData())) {
      
      selectInput('input_filter_outlier', "Filter var", 
                  choices = c('No filter', names(theData())))
    } else {
      NULL
    }
  })
  
  output$input_filter_outlier_value <- renderUI({
    if (input$input_filter_outlier != 'No filter') {
      
      if(!(input$input_filter_outlier %in% date_vec) & 
         !(is.numeric(theData()[[input$input_filter_outlier]]))) {
        
        checkboxGroupInput('input_filter_outlier_value', "Filter value", 
                           choices = unique(theData()[ , input$input_filter_outlier]),
                           inline = T)
        
      } else if (input$input_filter_outlier %in% date_vec) {
        # dateRangeInput('input_filter_value', 'Date range')
        
        temp_dmy <- dmy(theData()[[input$input_filter_outlier]])
        temp_myd <- myd(theData()[[input$input_filter_outlier]])
        temp_ymd <- ymd(theData()[[input$input_filter_outlier]])
        temp_ydm <- ydm(theData()[[input$input_filter_outlier]])
        temp_dym <- dym(theData()[[input$input_filter_outlier]])
        temp_mdy <- mdy(theData()[[input$input_filter_outlier]])
        
        na_length <- function(x) {
          mean(is.na(x)) <= 0.1
        }
        
        min_date <- function(x) {
          min(x(theData()[[input$input_filter_outlier]]))
        }
        
        max_date <- function(x) {
          max(x(theData()[[input$input_filter_outlier]]))
        }
        
        if (na_length(temp_dmy) == T) {
          d <- c(min_date(dmy), max_date(dmy))
          
        } else if (na_length(temp_myd) == T) {
          d <- c(min_date(myd), max_date(myd))
          
        } else if (na_length(temp_ymd) == T) {
          d <- c(min_date(ymd), max_date(ymd))
          
        } else if (na_length(temp_ydm) == T) {
          d <- c(min_date(ydm), max_date(ydm))
          
        } else if (na_length(temp_dym) == T) {
          d <- c(min_date(dym), max_date(dym))
          
        } else if (na_length(temp_mdy) == T) {
          d <- c(min_date(mdy), max_date(mdy))
        }
        
        # dateRangeInput('input_filter_value', 'Date range',
        #                min = d[[1]], max = d[[2]],
        #                start = d[[1]], end = d[[2]])
        
        sliderInput("input_filter_outlier_value",
                    "Choose date range:", 
                    min = d[[1]], max = d[[2]], 
                    value = c(d[[1]], d[[2]]))
        
      } else if (is.numeric(theData()[[input$input_filter_outlier]])) {
        
        min_val <- function(x) {
          min(x[[input$input_filter_outlier]], na.rm = T)
        }
        
        max_val <- function(x) {
          max(x[[input$input_filter_outlier]], na.rm = T)
        }
        
        sliderInput("input_filter_outlier_value",
                    "Choose range:", 
                    min = min_val(theData()), max = max_val(theData()), 
                    value = c(min_val(theData()), max_val(theData())))
      }
      
    } else {
      NULL
    }
    
  })
  
  
  format_fun_outlier <- reactive({
    
    if (input$input_group_outlier != 'No grouping') {
      format_group <- 0
      for (i in names(outlier_data()[, -1])) {
        format_group[i] <- paste("col_formatter2")
      }
      
      formattable(outlier_data(), format_group)
      
    } else {
      
      format_team <- 0
      for (i in names(outlier_data())) {
        format_team[i] <- paste("col_formatter2")
      }
      
      formattable(outlier_data(), format_team)
      
    }
  })
  
  format_table_outlier_reactive <- eventReactive(input$go_outlier, {
    if (!is.null(theData())) {
      
      # formattable
      as.datatable(format_fun_outlier(), options = list(scrollX = T))
      
    } else {
      NULL
    }
  })
  
  
  output$format_outlier_table <- renderDataTable({
    
    format_table_outlier_reactive()
    
  })
  
  
  date_filter_outlier <- reactive({
    
    if (input$input_filter_outlier %in% date_vec) {
      
      temp_dmy <- dmy(theData()[[input$input_filter_outlier]])
      temp_myd <- myd(theData()[[input$input_filter_outlier]])
      temp_ymd <- ymd(theData()[[input$input_filter_outlier]])
      temp_ydm <- ydm(theData()[[input$input_filter_outlier]])
      temp_dym <- dym(theData()[[input$input_filter_outlier]])
      temp_mdy <- mdy(theData()[[input$input_filter_outlier]])
      
      na_length <- function(x) {
        mean(is.na(x)) <= 0.1
      }
      
      date_format <- function(x) {
        theData()[x(theData()[[input$input_filter_outlier]]) >= input$input_filter_outlier_value[1] & 
                    x(theData()[[input$input_filter_outlier]]) <= input$input_filter_outlier_value[2], ]
      }
      
      if (na_length(temp_dmy) == T) {
        date_format(dmy)
      } else if (na_length(temp_myd) == T) {
        date_format(myd) 
      } else if (na_length(temp_ymd) == T) {
        date_format(ymd)
      } else if (na_length(temp_ydm) == T) {
        date_format(ydm) 
      } else if (na_length(temp_dym) == T) {
        date_format(dym) 
      } else if (na_length(temp_mdy) == T) {
        date_format(mdy)
      }
    }
  })
  
  output$go_outlier <- renderUI({
    if (!is.null(theData())) {
      actionButton('go_outlier', 'Render table')
    }
  })
  
  outlier_data <- reactive({
    
    z_out <- function(x) {
      percent(sum(scale(as.numeric(x)) >= 5, na.rm = T) / length(x), 1)
    }
    
    if (input$input_group_outlier == 'No grouping') {
      
      if (input$input_filter_outlier != 'No filter' & !(input$input_filter_outlier %in% date_vec)) {
        
        filter_criteria <- interp(~ which_column %in% input$input_filter_outlier_value, 
                                  which_column = as.name(input$input_filter_outlier))
        
        d <- 
          theData() %>% 
          filter_(filter_criteria) %>% 
          summarise_if(is.numeric, z_out)
        
      } else if (input$input_filter_outlier != 'No filter' & (input$input_filter_outlier %in% date_vec)) {
        
        d <- 
          date_filter_outlier() %>% 
          summarise_if(is.numeric, z_out)
        
      } else if (input$input_filter_outlier == 'No filter') {
        
        d <- 
          theData() %>% 
          summarise_if(is.numeric, z_out)
      }
      
    } else {
      
      if (input$input_filter_outlier != 'No filter' & !(input$input_filter_outlier %in% date_vec)) {
        
        filter_criteria <- interp(~ which_column %in% input$input_filter_outlier_value, 
                                  which_column = as.name(input$input_filter_outlier))
        
        d <- 
          theData() %>%
          filter_(filter_criteria) %>%
          group_by_(input$input_group_outlier) %>% 
          summarise_if(is.numeric, z_out)
        
        
      } else if (input$input_filter_outlier != 'No filter' & (input$input_filter_outlier %in% date_vec)) {
        
        d <- 
          date_filter_outlier() %>% 
          group_by_(input$input_group_outlier) %>%   
          summarise_if(is.numeric, z_out)
        
        
      } else if (input$input_filter_outlier == 'No filter') {
        d <- 
          theData() %>%
          group_by_(input$input_group_outlier) %>%
          summarise_if(is.numeric, z_out)
      }
    }
    d
  })
  
  
  output$go_duplicate <- renderUI({
    if (!is.null(theData())) {
      actionButton('go_duplicate', 'Render results')
    }
  })
  
  output$input_filter_duplicate <- renderUI({
    if (!is.null(theData())) {
      
      selectInput('input_filter_duplicate', "Filter var", 
                  choices = c('No filter', names(theData())))
    } else {
      NULL
    }
  })
  
  output$input_filter_duplicate_value <- renderUI({
    if (input$input_filter_duplicate != 'No filter') {
      
      if(!(input$input_filter_duplicate %in% date_vec) & 
         !(is.numeric(theData()[[input$input_filter_duplicate]]))) {
        
        checkboxGroupInput('input_filter_duplicate_value', "Filter value", 
                           choices = unique(theData()[ , input$input_filter_duplicate]),
                           inline = T)
        
      } else if (input$input_filter_duplicate %in% date_vec) {
        # dateRangeInput('input_filter_value', 'Date range')
        
        temp_dmy <- dmy(theData()[[input$input_filter_duplicate]])
        temp_myd <- myd(theData()[[input$input_filter_duplicate]])
        temp_ymd <- ymd(theData()[[input$input_filter_duplicate]])
        temp_ydm <- ydm(theData()[[input$input_filter_duplicate]])
        temp_dym <- dym(theData()[[input$input_filter_duplicate]])
        temp_mdy <- mdy(theData()[[input$input_filter_duplicate]])
        
        na_length <- function(x) {
          mean(is.na(x)) <= 0.1
        }
        
        min_date <- function(x) {
          min(x(theData()[[input$input_filter_duplicate]]))
        }
        
        max_date <- function(x) {
          max(x(theData()[[input$input_filter_duplicate]]))
        }
        
        if (na_length(temp_dmy) == T) {
          d <- c(min_date(dmy), max_date(dmy))
          
        } else if (na_length(temp_myd) == T) {
          d <- c(min_date(myd), max_date(myd))
          
        } else if (na_length(temp_ymd) == T) {
          d <- c(min_date(ymd), max_date(ymd))
          
        } else if (na_length(temp_ydm) == T) {
          d <- c(min_date(ydm), max_date(ydm))
          
        } else if (na_length(temp_dym) == T) {
          d <- c(min_date(dym), max_date(dym))
          
        } else if (na_length(temp_mdy) == T) {
          d <- c(min_date(mdy), max_date(mdy))
        }
        
        # dateRangeInput('input_filter_value', 'Date range',
        #                min = d[[1]], max = d[[2]],
        #                start = d[[1]], end = d[[2]])
        
        sliderInput("input_filter_duplicate_value",
                    "Choose date range:", 
                    min = d[[1]], max = d[[2]], 
                    value = c(d[[1]], d[[2]]))
        
      } else if (is.numeric(theData()[[input$input_filter_duplicate]])) {
        
        min_val <- function(x) {
          min(x[[input$input_filter_duplicate]], na.rm = T)
        }
        
        max_val <- function(x) {
          max(x[[input$input_filter_duplicate]], na.rm = T)
        }
        
        sliderInput("input_filter_duplicate_value",
                    "Choose range:", 
                    min = min_val(theData()), max = max_val(theData()), 
                    value = c(min_val(theData()), max_val(theData())))
      }
      
    } else {
      NULL
    }
    
  })
  
  
  date_filter_duplicate <- reactive({
    
    if (input$input_filter_duplicate %in% date_vec) {
      
      temp_dmy <- dmy(theData()[[input$input_filter_duplicate]])
      temp_myd <- myd(theData()[[input$input_filter_duplicate]])
      temp_ymd <- ymd(theData()[[input$input_filter_duplicate]])
      temp_ydm <- ydm(theData()[[input$input_filter_duplicate]])
      temp_dym <- dym(theData()[[input$input_filter_duplicate]])
      temp_mdy <- mdy(theData()[[input$input_filter_duplicate]])
      
      na_length <- function(x) {
        mean(is.na(x)) <= 0.1
      }
      
      date_format <- function(x) {
        theData()[x(theData()[[input$input_filter_duplicate]]) >= input$input_filter_duplicate_value[1] & 
                    x(theData()[[input$input_filter_duplicate]]) <= input$input_filter_duplicate_value[2], ]
      }
      
      if (na_length(temp_dmy) == T) {
        date_format(dmy)
      } else if (na_length(temp_myd) == T) {
        date_format(myd) 
      } else if (na_length(temp_ymd) == T) {
        date_format(ymd)
      } else if (na_length(temp_ydm) == T) {
        date_format(ydm) 
      } else if (na_length(temp_dym) == T) {
        date_format(dym) 
      } else if (na_length(temp_mdy) == T) {
        date_format(mdy)
      }
    }
  })
  
  
  duplicate_data <- reactive({
    
    if (input$input_filter_duplicate == 'No filter') {
      
      d <- theData()[which(duplicated(theData()) | duplicated(theData(), fromLast = TRUE)), ]
      
    } else if (input$input_filter_duplicate != 'No filter' & !(input$input_filter_duplicate %in% date_vec)) {
      
      filter_criteria <- interp(~ which_column %in% input$input_filter_duplicate_value, 
                                which_column = as.name(input$input_filter_duplicate))
      
      d <- 
        theData() %>% 
        filter_(filter_criteria)
      
      d <- d[which(duplicated(d) | duplicated(d, fromLast = TRUE)), ]
      
      
    } else if (input$input_filter_duplicate != 'No filter' & (input$input_filter_duplicate %in% date_vec)) {
      
      d <- date_filter_duplicate()
      d <- d[which(duplicated(d) | duplicated(d, fromLast = TRUE)), ]
    }
    d
  })
  
  duplicate_reactive_print <- eventReactive(input$go_duplicate, {
    datatable(duplicate_data(), options = list(scrollX = T))
  })
  
  output$duplicate_table <- renderDataTable({
    duplicate_reactive_print()
  })
  
  
  
}

shinyApp(ui = ui, server = server)
