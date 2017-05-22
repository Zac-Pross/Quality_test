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


ui <- tagList(
  useShinyjs(),
  inlineCSS(appCSS), 
  # Loading message
  div(
    id = "loading-content",
    h2("Loading ...")
  ),
  navbarPage('Data quality app', selected = 'Front page',
             theme = shinytheme("flatly"),
             tags$head(
               tags$style(HTML("
                               @import url('//fonts.googleapis.com/css?family=Helvetica');
                               
                               h3 {
                               font-family: 'Helvetica';
                               color: #2d3e50}
                               
                               h2 {
                               font-family: 'Helvetica;
                               color: #2d3e50
                               }  
                               ")
               )
               ),
             tabPanel('Front page',
                      sidebarLayout(
                        column(4, 
                               fluidRow(align = 'center',
                                        fileInput('datfile', '')
                               )
                        ),
                        column(8)
                      )
             ),
             navbarMenu('Missings',
                        tabPanel('Missings: table',
                                 fluidPage(
                                   fluidRow(
                                     column(12, align = 'center',
                                            h3('Percentage of complete observations')
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(4, 
                                            align = 'center',
                                            uiOutput('input_group')
                                     ),
                                     column(4, align = 'center',
                                            uiOutput('go_table')
                                     ),
                                     column(4, align = 'center',
                                            uiOutput('input_filter')
                                     )
                                   ),
                                   fluidRow(
                                     column(12, align = 'center',
                                            align_center(uiOutput('input_filter_value'))
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     dataTableOutput('format_table')
                                   )
                                 )
                        ),
                        tabPanel('Missings: plots',
                                 fluidPage(
                                   column(2,
                                          tags$head(tags$style("#missing_points_plot{height:80vh !important;}")),
                                          fluidRow(
                                            column(12, align = 'center',
                                                   h3('Complete cases: Plots'),
                                                   br(),
                                                   uiOutput('go_missing_plot')
                                            )
                                          ),
                                          br(),
                                          fluidRow(
                                            fluidRow(
                                              uiOutput('input_missing_var1')
                                            ),
                                            fluidRow(
                                              uiOutput('input_missing_var2')
                                            )
                                          ),
                                          uiOutput('input_missing_plot_select')
                                   ),
                                   column(1, 
                                          h3('')
                                   ),
                                   column(9, align = 'right',
                                          fluidRow(
                                            plotOutput('missing_points_plot')
                                          )
                                   )
                                 )
                        ),
                        tabPanel('Missings: prediction tree',
                                 fluidPage(
                                   column(3,
                                          tags$head(tags$style("#miss_tree{height:80vh !important;}")),
                                          fluidRow(
                                            column(12, align = 'center',
                                                   fluidRow(
                                                     h3('Predict: Percentage Complete / Row')
                                                   ),
                                                   br(),
                                                   fluidRow(
                                                     column(6, align = 'left',
                                                            fluidRow(
                                                              uiOutput('input_radio_tree'),
                                                              uiOutput('input_group_tree')
                                                            )
                                                     ),
                                                     br(),
                                                     column(6,
                                                            fluidRow(
                                                              uiOutput('go_tree')
                                                            )
                                                     )
                                                   )
                                            )
                                          )
                                   ),
                                   column(9, align = 'center',
                                          fluidRow(
                                            plotOutput('miss_tree')
                                          )
                                   )
                                 )
                        )
             ),
             navbarMenu('Likert reliability',
                        tabPanel('Cronbach\'s Alpha',
                                 fluidPage(
                                   column(4,
                                          column(12, align = 'center',
                                                 fluidRow(
                                                   h3('Survey reliability: Cronbach\'s Alpha')
                                                 ),
                                                 br(),
                                                 column(6,
                                                        fluidRow(align = 'left',
                                                                 uiOutput('input_radio_alpha'),
                                                                 uiOutput('input_select_alpha')
                                                        )
                                                 ),
                                                 br(),
                                                 column(6,
                                                        fluidRow(
                                                          uiOutput('go_alpha')
                                                        )
                                                 )
                                          )
                                   ),
                                   column(8, align = 'left',
                                          fluidRow(
                                            verbatimTextOutput('alpha_table')
                                          )
                                   )
                                 )
                        ),
                        tabPanel('Distribution',
                                 fluidPage(
                                   fluidRow(
                                     column(12, align = 'center',
                                            h3('Survey results: Visualise Distribution')
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     tags$head(tags$style(
                                       "#likert_distribution_plot{height:60vh !important;}")
                                     ),
                                     column(6, align = 'center',
                                            uiOutput('input_select_likert_distribution')
                                     ),
                                     column(6, align = 'center',
                                            uiOutput('go_likert_distribution')
                                     )
                                   ),
                                   fluidRow(
                                     column(12, align = 'center',
                                            tableOutput('likert_distribution_table')
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(12, align = 'center',
                                            plotOutput('likert_distribution_plot')
                                     )
                                   )
                                 )
                        )
             ),
             navbarMenu('Frequency',
                        tabPanel('Frequency of observations',
                                 fluidPage(
                                   fluidRow(
                                     column(3,
                                            tags$head(tags$style("#date_line_plot{height:80vh !important;}")),
                                            fluidRow(
                                              column(12, align = 'center',
                                                     h3('Number of days between observations: Plot'),
                                                     br(),
                                                     uiOutput('go_date')
                                              )
                                            ),
                                            br(),
                                            fluidRow(
                                              column(6,
                                                     uiOutput('input_select_date')
                                              ),
                                              column(6,
                                                     uiOutput('input_select_name')
                                              )
                                            ),
                                            fluidRow(
                                              column(12, align = 'center',
                                                     align_center(uiOutput('input_filter_date'))
                                              )
                                            ),
                                            fluidRow(
                                              dataTableOutput('date_summary_table')
                                            )
                                     ),
                                     column(1, h3('')
                                     ),
                                     column(8,
                                            fluidRow(
                                              plotlyOutput('date_line_plot')
                                            )
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     plotlyOutput('date_bar_plot')
                                   )
                                 )
                        )
             ),
             navbarMenu('Extreme outliers',
                        tabPanel('Table',
                                 fluidPage(
                                   fluidRow(
                                     fluidRow(
                                       column(12, align = 'center',
                                              h3('Percentage of extreme outliers')
                                       )
                                     ),
                                     br(),
                                     fluidRow(
                                       column(4, align = 'center',
                                              uiOutput('input_group_outlier')
                                       ),
                                       column(4, align = 'center',
                                              uiOutput('go_outlier')
                                       ),
                                       column(4, align = 'center',
                                              uiOutput('input_filter_outlier')
                                       )
                                     ),
                                     fluidRow(
                                       column(12, align = 'center',
                                              align_center(uiOutput('input_filter_outlier_value'))
                                       )
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     dataTableOutput('format_outlier_table')
                                   )
                                 )
                        )
             ),
             navbarMenu('Duplicates',
                        tabPanel('Duplicate observations',
                                 fluidPage(
                                   fluidRow(
                                     column(12, align = 'center',
                                            h3('Duplicate observations')
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(6, align = 'center',
                                            uiOutput('input_filter_duplicate')
                                     ),
                                     column(6, align = 'center',
                                            uiOutput('go_duplicate')
                                     )
                                   ),
                                   fluidRow(
                                     column(12, align = 'center',
                                            align_center(uiOutput('input_filter_duplicate_value'))
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(12, align = 'center',
                                            fluidRow(
                                              dataTableOutput('duplicate_table')
                                            )
                                     )
                                   )
                                 )
                        )
             )
               )
             )
