library(shiny); library(dplyr); library(shinythemes); library(plotly)
library(ggplot2); library(tidyr); library(sf); library(tmap); library(leaflet)
library(DT)

T1 <- read.csv("T1.csv") %>% 
    mutate_all(~round(., 4))

T2 <- read.csv('T2.csv')

# Define UI for application that draws a histogram
ui <- navbarPage("Datos Rabia", theme = shinytheme('flatly'),
                 tabPanel("Nivel federal",
                          # Sidebar with a slider input for number of bins
                          sidebarLayout(
                              sidebarPanel(
                                  width = 2,
                                  tags$h4("Datos totales a nivel federal"),
                                  hr(),
                                  tags$h6("Los datos acumulados sobre la campana de vacunacion anti-rabica se muestran agregados a nivel federal. 
                                          Los valores de perros y gatos menores a un año, machos, se muestran en proporciones")
                                  # sliderInput(
                                  #     "bins",
                                  #     "Number of bins:",
                                  #     min = 1,
                                  #     max = 50,
                                  #     value = 30
                                  # )
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(type = 'tabs',
                                        tabPanel(title = "Table1", dataTableOutput("T1")),
                                        tabPanel(title = 'Plot1', plotlyOutput('P1.1', height = 150)),
                                        # tabPanel(title = 'Plot2', plotlyOutput('P1.2', height = 150)),
                                        tabPanel(title = 'Plot3', plotlyOutput('P1.3', height = 150)),
                                        tabPanel(title = 'Plot4', plotlyOutput('P1.4', height = 150))
                                        )
                          )),
                 #### Mapas estatales por año ####
                 tabPanel("Nivel Estatal"),
                 
                 ###### Data query ######
                 tabPanel("Datos",
                          sidebarLayout(sidebarPanel(width = 3,
                                                     tags$h4("Datos de la campana de vacunacion por estado"),
                                                     tags$h6("Los datos agregados por estado y año pueden ser filtrados y descargados en esta pestaña. 
                                                             Selecciona los años que deseas usando el menu y presiona el boton para ver la tabla"),
                                                     tags$h6('La tabla se puede ordenar por columna y descargar en diferentes archivos'),
                                                     shiny::selectInput(inputId = 'year', label = 'Años', 
                                                                        choices = c('2016', '2017', '2018', '2019', '2020'), multiple = T, 
                                                                        selected = c('2017', '2018', '2019', '2020')),
                                                     hr(),
                                                     actionButton(inputId = "DataTB", label = "Filtrar los datos")
                                                     ),
                                        mainPanel(type = 'tabs',
                                                  tabPanel(title = "Table2", dataTableOutput("T2")),)))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ######## Data query ########
    DQ <- eventReactive(input$DataTB,{
        T2 %>% 
            filter(year %in% input$year)
    })
    
    ###### Outpud DT query #######
    output$T2 <- renderDataTable(datatable(data = DQ(),
                                           colnames = c('Año', 'Entidad', 'Poblacion Personas', 'Total Perros', 'Total Gatos', 'Perros Vacunados', 'Gatos Vacunados', 'Proyecciones Perros', 'Proyecciones Gatos',  'Perros < 1 a', 'Gatos < 1 a', 'Perros machos', 'Gatos machos', 'Perros en Control', 'SIR Agresiones'),
                                       extensions = 'Buttons',
                                       options = list(dom = "Blfrtip",
                                                      buttons = list("copy", 
                                                                     list(extend = "collection",
                                                                          buttons = c("csv", "excel", "pdf"),
                                                                          text = "Download")), # end of buttons customization
                                                      # customize the length menu
                                                      lengthMenu = list(c(10, 10, -1), # declare values
                                                                        c(10, 10, "All") # declare titles
                                                      ), # end of lengthMenu customization
                                                      pageLength = 10
                                                      
                                                      
                                       ) # end of options
    ))
    
    output$P1.1 <- renderPlotly({
        T1 %>% 
            plot_ly(., x = ~as.character(year), y = ~TotalPerros, type = 'scatter', mode = 'lines', 
                    line = list(color = 'rgba(200, 30, 30, 1)'),
                    name = "Perros") %>% 
            add_trace(., x = ~as.character(year), y = ~TotalGatos, type = 'scatter', mode = 'lines', 
                    line = list(color = 'rgba(30, 30, 200, 1)'),
                    name = "Gatos") %>% 
            layout(title = "Total por año",
                   xaxis = list(title = 'Año'),
                   yaxis = list(title = 'Total'))
    })
    
    output$P1.2 <- renderPlotly({
        T1 %>% 
            plot_ly(., x = ~as.character(year), y = ~TotalGatos, type = 'scatter', mode = 'lines', 
                    line = list(color = 'rgba(30, 30, 200, 1)'),
                    name = "Total de gatos por año") %>% 
            layout(title = "Total de gatos por año",
                   xaxis = list(title = 'Año'),
                   yaxis = list(title = 'Total'))
    })
    
    output$P1.3 <- renderPlotly({
        T1 %>% 
            plot_ly(., x = ~as.character(year), y = ~pMd, type = 'scatter', mode = 'lines', 
                    line = list(color = 'rgba(200, 30, 30, 1)'),
                    name = "Perros") %>% 
            add_trace(., x = ~as.character(year), y = ~pMc, type = 'scatter', mode = 'lines', 
                      line = list(color = 'rgba(30, 30, 200, 1)'),
                      name = "Gatos") %>% 
            layout(title = "Proporcion de machos",
                   xaxis = list(title = 'Año'),
                   yaxis = list(title = 'Total'))
    })
    
    output$P1.4 <- renderPlotly({
        T1 %>% 
            plot_ly(., x = ~as.character(year), y = ~pm1a, type = 'scatter', mode = 'lines', 
                    line = list(color = 'rgba(200, 30, 30, 1)'),
                    name = "Perros") %>% 
            add_trace(., x = ~as.character(year), y = ~gm1a, type = 'scatter', mode = 'lines', 
                      line = list(color = 'rgba(30, 30, 200, 1)'),
                      name = "Gatos") %>% 
            layout(title = "Proporcion de nacidos por año",
                   xaxis = list(title = 'Año'),
                   yaxis = list(title = 'Total'))
    })

    output$T1 <- renderDataTable(datatable(data = T1, colnames = c('Año', 'Total perros', 'Total gatos', 'Perros < 1 a', 'Gatos < 1 a', 'Perros machos', 'Gatos machos', 'Perros en control'),
                                                     extensions = 'Buttons',
                                                     options = list(dom = "Blfrtip",
                                                         buttons = list("copy", 
                                                                        list(extend = "collection",
                                                                             buttons = c("csv", "excel", "pdf"),
                                                                             text = "Download")), # end of buttons customization
                                                         # customize the length menu
                                                         lengthMenu = list(c(10, 10, -1), # declare values
                                                                              c(10, 10, "All") # declare titles
                                                         ), # end of lengthMenu customization
                                                         pageLength = 10
                                                         
                                                         
                                                     ) # end of options
    )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
