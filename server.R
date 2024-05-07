# The purpose of this app is to let user see the dwelling type data 
# for each census subdivision in the data.
function(input, output, session) {
  
  # Read and prepare data
  data <- read.csv("data/data_cleaned.csv")
  data_details <- read.csv("data/data_details_wide.csv")

  subdivisions <- data %>%
    # Only pick the census subdivisions with population greater than 50,000
    filter(Population..2021 > 50000) %>%
    # Sort the census subdivisions from the highest population to the lowest
    arrange(- Population..2021) %>%
    pull(GEO_NAME)
  
  # Update selectizeInput. 
  # Its choices should be from the subdivisions variable.
  # By default, the first two subdivisions should be selected.
  
  updateSelectizeInput(session, "subdivision", choices = subdivisions, selected = subdivisions[1:2])
  
  # Create a reactive expression to filter the data based on user input
  shown_subdivisions <- reactive({
    data %>%
      filter(GEO_NAME %in% input$subdivision)
  })
  
  # In the selectizeInput function, we want to keep at least one census subdivision selected at all times.
  observeEvent(input$subdivision, {
    if (length(input$subdivision) == 0) {
      updateSelectizeInput(session, "subdivision", selected = subdivisions[1])
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  # Call the module subdivision_dwellingsUI
  selected_subdivision <- subdivision_dwellingsServer(id = "dwellingsmain", shown_subdivisions)
  
  # Here, we will place the server portion of a module, named subdivision_details, that we will create later. 
  # The purpose of this module is to show more details about the selected census subdivision in the table.
  # There is one parameter that we need to pass to the module: the selected census subdivision.
  subdivision_detailsServer(id = "detailsmain", subdivision = selected_subdivision, data_details = data_details)
}
