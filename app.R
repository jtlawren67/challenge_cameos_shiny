library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(plotly)
library(lubridate)
library(scales)
library(dplyr)

cameo <- readRDS('./data/cameo_clean.RDS')

#Created Summarized Table
cameo_tbl <- cameo %>% 
  arrange(name, ds) %>%
  group_by(name) %>% 
  summarize(
    num_data_points = n(),
    first_data_point = min(ds),
    last_data_point = max(ds),
    min_price = min(newprice),
    avg_price = mean(newprice) %>% round(2),
    max_price = max(newprice),
    price_range = max(newprice)-min(newprice),
    price_changes = sum((newprice != lag(newprice)), na.rm = T)
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme('cosmo'), 
  title = "The Challenge Cameo Tracker",
  tags$script(src="https://kit.fontawesome.com/6e244b2d77.js"),
  tags$head(
    tags$style(HTML("
        .navbar .navbar-nav {float: right}
        .navbar .navbar-header {float: left, width = 10%}
      ")),
  ),
  navbarPage(
    collapsible = F,
    title = div(img(src = 'logo.png', height = "50px"), "Cameo Price Tracker", style='padding-top: 0px;'),
    tabPanel(
      "Explore By Person",
      sidebarLayout(
        sidebarPanel(
          virtualSelectInput(
            "challenger_select",
            label ="Challengers:  (Click box next to 'Search' for Select All)",
            choices = cameo_tbl$name,
            keepAlwaysOpen =TRUE,
            multiple = TRUE,
            hideClearButton = FALSE,
            search = TRUE,
            disableSelectAll = FALSE,
            optionsCount = 7
            
          ),
          sliderInput(
            "date_select",
            min = min(ymd(cameo$ds)),
            max = max(ymd(cameo$ds)),
            label = h5(tags$b("Date Range:")),
            value = c(min(ymd(cameo$ds)), max(ymd(cameo$ds)))
          ),
          actionButton("reset_button", "Reset")
        ),
        mainPanel(
          fluidRow(
            plotlyOutput("plotOutput")
          ),
          fluidRow(
            dataTableOutput("plotTable")
          )
        )
      )
    ),
    tabPanel(
      "Explore by Amount",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          sliderInput(
            "amt_select",
            label = "Set Amount Range",
            min = min(cameo$newprice),
            max = max(cameo$newprice),
            value = c(min(cameo$newprice), max(cameo$newprice)),
            pre = "$"
          ),
          actionButton("reset_amt", "Reset")
        ),
        mainPanel(
          width =9,
          plotlyOutput("plotAmtOutput") 
        )
      ),
    ),
    tabPanel(
      "Summary Table",
      dataTableOutput("summaryTable")
    ),
    tabPanel(
      "About",
      wellPanel(
        h2("About this App"),
        HTML("<h4>This app was a personal project to track the Cameo asking prices over time for cast members of MTV's The Challenge franchise.  
           <br /><br />
           It was constructed by running a web scraper on <a href='https://www.cameo.com/browse/reality-tv/mtv/the-challenge'> Cameo's Challenge page </a> every week from May 26th, 2021 through July 10th, 2022.
           <br /><br />
           <b>NOTE:</b> Data from 2/20/22 to 3/7/22 was lost due to user error when the Web Scraper silently failed for a few weeks.
           <br /><br />
           Check out some other Challenge related analyses (and other stuff too) on my <a href='http://jlaw.netlify.app'>blog</a>!
           </h4>")
      )
      
    ),
  ),
  
  ###FOOTER
  HTML("
    <hr style = 'border-top: 1px solid #D4D4D4;'>
    <div align = 'right' style = 'padding-right: 15px; padding-top: 15px'>
      <a href='https://github.com/jtlawren67/'> 
      <i class='fa fa-github' style='font-size:20px; margin: 0px 5px'></i>
      </a>
      <a href='http://jlaw.netlify.app/'> 
      <i class='fa-sharp fa-solid fa-globe' style='font-size:20px; margin: 0px 5px'></i>
      </a>
    </div>
    ")
)
 
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    output$plotOutput <- renderPlotly({
      cameo %>% 
        filter(
          name %in% input$challenger_select,
          between(ymd(ds), input$date_select[1], input$date_select[2])
        ) %>% 
        plot_ly(x = ~ymd(ds),
                y = ~newprice,
                color = ~name,
                type = "scatter",
                mode = "lines",
                hoverinfo = 'text',
                text = ~paste0(name,"<br />",ymd(ds),": ", dollar(newprice))) %>%
        layout(
          title = "Cameo Prices",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Price", tickprefix = "$"),
          showlegend = FALSE
        )
      
    })
    
    output$plotTable <- renderDataTable({
      datatable(cameo_tbl %>% filter(name %in% input$challenger_select),
                rowname = F,
                colnames = c(
                  'Challenger Name',
                  "Number of Data Points",
                  "Earliest Date",
                  "Latest Date",
                  "Min. Price",
                  "Avg. Price",
                  "Max. Price",
                  "Price Range",
                  "Price Changes"
                ),
                options=list(
                             paging=FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = 1:7))
                ) 
      ) %>% 
        formatCurrency(columns = c(5, 6, 7, 8))
                
    })
    
    observeEvent(input$reset_button, {
      updateVirtualSelect("challenger_select", selected = "")
      updateSliderInput(session, "date_select", value = c(min(ymd(cameo$ds)), max(ymd(cameo$ds))))
    })
    
    output$plotAmtOutput <- renderPlotly({
      cameo %>% 
        filter(
          between(newprice, input$amt_select[1], input$amt_select[2])
        ) %>% 
        plot_ly(x = ~ymd(ds),
                y = ~newprice,
                color = ~name,
                type = "scatter",
                mode = "lines",
                hoverinfo = 'text',
                text = ~paste0(name,"<br />",ymd(ds),": ", dollar(newprice))) %>%
        layout(
          title = "Cameo Prices",
          xaxis = list(title = ""),
          yaxis = list(title = "Price", tickprefix = "$"),
          legend = list(orientation = 'h')
        )
      
    })
    
    observeEvent(input$reset_amt, {
      updateSliderInput(session, "amt_select", value = c(min(cameo$newprice), max(cameo$newprice)))
    })
    
    output$summaryTable <- renderDataTable({
      datatable(cameo_tbl,
                rowname = F,
                colnames = c(
                  'Challenger Name',
                  "Number of Data Points",
                  "Earliest Date",
                  "Latest Date",
                  "Min. Price",
                  "Avg. Price",
                  "Max. Price",
                  "Price Range",
                  "Price Changes"
                ),
                caption = "Summary of Cameo Asking Prices 5/26/21 - 7/10/22",
                options=list(scrollY=400,
                             paging=FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = 1:7))
                             )
                ) %>%
        formatCurrency(columns = c(5, 6, 7, 8))
    }
    )

  
}

# Run the application 
shinyApp(ui = ui, server = server)
