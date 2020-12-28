
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
       DTOutput('df_selected')
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
    
    

    # display table of lasso-selected cars
    # NB: I need to create a df_sample reactive - which is rendered as df_selected -
    # since I need to select the appropriate index in the image selection below.
    # To be more precise, when the lasso selection creates a table, to get the right
    # image to display, I need to retrieve it *not* from the original df_cars, but
    # from the df_car index which is stored inside df_sample
    df_sample <- reactive({
      req(event_data("plotly_selected"))
      rows <- event_data("plotly_selected")$customdata %>% as.integer()
      
      df_cars[rows,] %>%
        select(filename, City.mpg, Width, Driveline)
    })
    
    output$df_selected <- renderDT({
      df_sample()
    }, selection = 'single')  # 'single' to provide interactive image display
    
    
    
    # display car image, either hovered or selected in the table
    output$selectedCar <- renderImage({
      
      idx <- NULL
      
      if (!is.null(input$df_selected_rows_selected)) {
        idx_selected <- input$df_selected_rows_selected
        idx <- rownames(df_sample())[idx_selected] %>% as.integer
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
