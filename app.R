
library(shiny)
library(plotly)
library(DT)


# import data
df_cars <- read.csv("cars_part.csv", stringsAsFactors = FALSE)
imagesdir <- paste0(getwd(),'/imagecars/')
rownumba <- row.names(df_cars)


# UI
ui <- fluidPage(

  plotlyOutput("carsplot"),

  fluidRow(
    column(6,
       plotOutput("selectedCar")
    ),
    column(6,
       DTOutput('df_selected_table')
    )
  )

)


# Server logic
server <- function(input, output) {
    
    # plot cars
    output$carsplot <- renderPlotly({
      
      plot_ly(df_cars, x = ~City.mpg, y = ~Width, customdata = rownumba) %>%
        add_markers(
          text = ~filename,
          color = ~Driveline,
          marker = list(size=10)
        )

    })
    
    
    # NB: I need a df_selected() reactive var because:
    # - lasso creates df_selected() which is rendered into output$df_selected_table
    # - row (car) to display is clicked in df_selected_table -> idx_selected
    # - the correct car to display is found with rownames(df_selected())[idx_selected]
    
    df_selected <- reactive({
      req(event_data("plotly_selected"))
      rows <- event_data("plotly_selected")$customdata %>% as.integer()
      
      df_cars[rows,] %>%
        select(filename, City.mpg, Width, Driveline)
    })
    
    
    # display table of lasso-selected cars
    output$df_selected_table <- renderDT({
      df_selected()
    }, selection = 'single')  # 'single' to provide interactive image display
    
    
    
    # display car image, either hovered or selected in the table
    output$selectedCar <- renderImage({
      
      idx <- NULL
      
      if (!is.null(input$df_selected_table_rows_selected)) {
        idx_selected <- input$df_selected_table_rows_selected
        idx <- rownames(df_selected())[idx_selected] %>% as.integer
      }
      
      if (!is.null(event_data("plotly_hover")$customdata)) {
        idx <- event_data("plotly_hover")$customdata %>% as.integer()
      }
      
      if (!is.null(idx)) {
        list(src = paste0(imagesdir,df_cars$filename[idx]), width = 500)    
      } else {
        list(src = paste0(imagesdir,'empty.png'), width = 500)
      }
      
    }, deleteFile = FALSE)
    
    
}

# Run the application 
shinyApp(ui, server, options = list(display.mode = "showcase"))
