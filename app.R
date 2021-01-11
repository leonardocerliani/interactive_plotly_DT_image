
library(shiny)
library(plotly)
library(DT)


# import data
df_cars <- read.csv("cars_part.csv", stringsAsFactors = FALSE)
imagesdir <- paste0(getwd(),'/imagecars/')


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
    
    # Cars scatterplot
    output$carsplot <- renderPlotly({
      
      p <- plot_ly(
        df_cars, 
        x = ~City.mpg, 
        y = ~Width, 
        customdata = rownames(df_cars)
      )
      
      p %>% add_markers(
        text = ~filename, 
        color = ~Driveline, 
        marker = list(size=10)
      )

    })
    
    
    # Reactive expression storing the lasso-selected markers
    #
    # I need a df_selected() reactive because:
    # - lasso creates df_selected() which is rendered into output$df_selected_table
    # - row (car) to display is clicked in df_selected_table -> idx_selected
    # - the correct car to display is found with rownames(df_selected())[idx_selected]
    df_selected <- reactive({
      # req(event_data("plotly_selected"))
      rows_selected <- event_data("plotly_selected")$customdata %>% as.integer()
      
      df_cars[rows_selected,] %>% select(filename, Driveline, City.mpg, Width)
    })
    
    
    # display table of lasso-selected cars
    output$df_selected_table <- renderDT(
      df_selected(), 
      selection = 'single',        # 'single' to provide interactive image display 
      rownames = FALSE,            # hide row names/number
      options = list(dom = 't')    # use 'ft' to have search field
    )  
    
    
    # display car image, either hovered or selected in the table
    output$selectedCar <- renderImage({
      
      idx <- NULL # starting state: nothing is hovered or clicked
    
      # when a row of df_selected is clicked
      idx_df_selected <- input$df_selected_table_rows_selected
      if (!is.null(idx_df_selected)) {
        idx <- rownames(df_selected())[idx_df_selected] %>% as.integer
      }

      # when a marker is hovered
      idx_hover <- event_data("plotly_hover")$customdata
      if (!is.null(idx_hover)) {
        idx <- idx_hover %>% as.integer()
      }

      # if neither of the above condition is true
      if (!is.null(idx)) {
        list(src = paste0(imagesdir,df_cars$filename[idx]), width = 500)
      } else {
        list(src = paste0(imagesdir,'empty.png'), width = 500)
      }

      
    }, deleteFile = FALSE)
    
    
}

# Run the application 
shinyApp(ui, server)






































