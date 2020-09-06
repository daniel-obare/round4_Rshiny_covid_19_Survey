require(dplyr)
require(tidyr)
require(magrittr)
require(ggplot2)
require(ggthemes)
require(hrbrthemes)
theme_set(theme_ipsum())
require(shiny)
require(shinydashboard)
################################################################
covsurvey <- round4_Rshiny_covid_19_Survey %>% mutate(q100_sex = replace_na(round4_Rshiny_covid_19_Survey$q100_sex, 'undefined'),
                                                      q101_age = replace_na(round4_Rshiny_covid_19_Survey$q101_age, 37),
                                                      consent = replace_na(round4_Rshiny_covid_19_Survey$consent, 'not_known'),
                                                      curr_location_fin = replace_na(round4_Rshiny_covid_19_Survey$curr_location_fin, 'not_sure'),
                                                      rcodef = factor(replace_na(round4_Rshiny_covid_19_Survey$rcodef, 'uncertain')))
##################################################################
ui <- dashboardPage(
  dashboardHeader(title = 'COVID-19 SURVEY'),
  dashboardSidebar(
    sidebarMenu(
      h4('Panels', align = 'center'),
      menuItem('Interviewer', tabName = 'teams'),
      menuItem('Consent', tabName = 'consent'),
      menuItem('Result Code', tabName = 'results'),
      menuItem('Current Location', tabName = 'current'),
      menuItem('Frequency Tables', tabName = 'tables'),
      h4('Global Options', align = 'center'),
      sliderInput(inputId = 'ageid', label = 'Age', min = 15, max = 80, value = c(25, 45)),
      radioButtons(inputId = 'sexid', label = 'Sex', choices = unique(covsurvey$q100_sex), selected = 'Male'),
      selectInput(inputId = 'consentid', label = 'Consent', choices = unique(covsurvey$consent), selected = 'Yes', selectize = TRUE),
      selectInput(inputId = 'teamid', label = 'Team', choices = unique(covsurvey$team), selected = 1, selectize = TRUE)
    )
  ),
  #
  dashboardBody(
                tabItems(
                  tabItem(tabName = 'teams',
                          fluidRow(box(plotOutput('interviewerplot'), width = 12), title = 'Interviewer'),
                          fluidRow(box(tableOutput('interviewertable'), width = 8), title = 'Interviewer Frequency Table')
                          ),
                  tabItem(tabName = 'consent', h2('Analysis of consent', align = 'center'),
                          fluidRow(box(plotOutput('consentplot'), width = 12))
                          ),
                  tabItem(tabName = 'results',
                          fluidRow(box(plotOutput('rcodefplot'), width = 12, title = 'Result Code')), 
                          fluidRow(box(tableOutput('rcodeftable'), width = 6, title = 'Result Code Frequency Table'))
                         ),
                  tabItem(tabName = 'current', 
                          fluidRow(box(plotOutput('locationplot'), width = 12), title = 'Location'),
                          fluidRow(box(tableOutput('loctable'), width = 6, title = 'Location Frequency Table'))
                          ),
                  tabItem(tabName = 'tables', h3('Frequency Tables', align = 'center'),
                          fluidRow(box(tableOutput('sextable'), width = 4, title = 'Gender Frequency Table'),
                                   box(tableOutput('calltable'), width = 4, title = 'Call Count Frequency Table'),
                                   box(tableOutput('consenttable'), width = 4, title = 'Consent Frequency Table')),
                          fluidRow(box(tableOutput('currloctable'), width = 4, title = 'Current Location Frequency Table'),
                                   box(tableOutput('teamtable'), width = 4, title = 'Team Frequency Table'),
                                   box(tableOutput('agetable'), width = 4, title = 'Age Summary Table'))
                          )
                )
))
###################################################################
server <- function(input, output) {
  global_filter <- reactive({
    covsurvey %>% 
      filter(team == input$teamid,
             consent == input$consentid,
             q100_sex == input$sexid,
             q101_age >= input$ageid[1], q101_age <= input$ageid[2])
  })
  ################################################################
  ################################################################
  output$interviewerplot <- renderPlot({
    global_filter() %>% ggplot(aes(interviewer))+
      geom_bar(fill ='steelblue', col = 'white')+
      theme(axis.title = element_text(size = 12),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14))
      
  })
  #
  output$interviewertable <- renderTable({
    global_filter() %>% select(1,2) %>% group_by(team, interviewer) %>% 
      summarise(No._of_interviewers = n_distinct(interviewer),
                Freq = n())
  })
  #
  output$consentplot <- renderPlot({
    covsurvey %>% select(8) %>% group_by(consent) %>% summarise(Freq = n()) %>% 
      ggplot(aes(x = '', y = Freq, fill = consent))+
      geom_bar(stat = 'identity', width = 1, col = 'white')+
      coord_polar(theta = 'y', start = 0)+
      geom_text(aes(y = Freq, label = Freq), color = "white", position = position_stack(vjust = 0.5))+
      theme_void()+
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666"))
  })
  #
  output$rcodefplot <- renderPlot({
    ggplot(global_filter(), aes(rcodef))+geom_bar(fill = 'steelblue')+
      theme(axis.title = element_text(size = 12),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14),
            axis.text.x = element_text(face = 'bold', size = 12, angle = 90),
            axis.text.y = element_text(face = 'bold', size = 12))
    
    })
  #
  output$rcodeftable <- renderTable({
    global_filter() %>% select(10) %>% group_by(rcodef) %>%  summarise(Freq = n())
  })
  #
  output$locationplot <- renderPlot({
    global_filter() %>% ggplot(aes(curr_location_fin))+
      geom_bar(fill = 'steelblue')+
      theme(axis.title = element_text(size = 12),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14),
            axis.text.x = element_text(face = 'bold', size = 12, angle = 90),
            axis.text.y = element_text(face = 'bold', size = 12))+
      scale_fill_brewer(palette = 'set1')
  })
  ##############################################################################
  summary_table <- function(col_num, col_name) {
    covsurvey %>% select(col_num) %>% group_by(y) %>% summarise(Freq = n())
  }
  ##############################################################################
  output$loctable <- renderTable({
    global_filter() %>% select(9) %>% group_by(curr_location_fin) %>%  summarise(Freq = n())
  })
  #
  output$sextable <- renderTable({
    covsurvey %>% select(4) %>% group_by(q100_sex) %>% summarise(Freq = n())
  })
  #
  output$calltable <- renderTable({
    covsurvey %>% select(7) %>% group_by(call_outc) %>% summarise(Freq = n())
  })
  #
  output$consenttable <- renderTable({
    covsurvey %>% select(8) %>% group_by(consent) %>% summarise(Freq = n())
  })
  #
  output$currloctable <- renderTable({
    covsurvey %>% select(9) %>% group_by(curr_location_fin) %>% summarise(Freq = n())
  })
  #
  output$teamtable <- renderTable({
    covsurvey %>% select(1) %>% group_by(team) %>% summarise(Freq = n())
  })
  #
  output$agetable <- renderTable({
    covsurvey %>% select(5) %>% summarise(Average = mean(q101_age),
                                          Median = median(q101_age),
                                          Max_Age = max(q101_age),
                                          Min_Age = min(q101_age))
  })
  #
}
######################################################################
shinyApp(ui, server)