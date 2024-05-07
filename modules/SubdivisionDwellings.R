# Here, we put the definition of the module subdivision_dwellings.
# We will abstract out the code in server.R and ui.R that deals with the dwelling types of the selected census subdivision.

# Define the module UI
subdivision_dwellingsUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Create a title for the module
    tags$h3("Dwelling types in the selected census subdivision"),
    # Create a table to display the dwelling types of the selected census subdivision
    DTOutput(ns("table"))
  )
}

# Define the module server
# The server function will take the following parameters:
# id: The ID of the module
# shown_subdivisions: A reactive expression that filters the data based on the selected census subdivision
subdivision_dwellingsServer <- function(id, shown_subdivisions) {
  moduleServer(id, function(input, output, session) {
    # Create a reactive expression to get the selected census subdivision
    selected_subdivision <- reactive({
      row_id_selected <- input$table_rows_selected
      shown_subdivisions()[row_id_selected, "DGUID"]
    })
    
    dwelling_columns <-
      c(
        "Single.detached.house",
        "Semi.detached.house",
        "Row.house",
        "Apartment.or.flat.in.a.duplex",
        "Apartment.in.a.building.that.has.fewer.than.five.storeys",
        "Apartment.in.a.building.that.has.five.or.more.storeys"
      )
    # Use the code in server.R to create the data table
    output$table <- renderDT({
      data <- shown_subdivisions() %>%
        select(GEO_NAME, all_of(c("Total.private.dwellings", dwelling_columns))) %>%
        transform_to_proportions("Total.private.dwellings", remove_total = FALSE) %>%
        rename(
          Subdivision = GEO_NAME,
          `Total private dwellings` = Total.private.dwellings,
          `Single detached house` = Single.detached.house,
          `Semi detached house` = Semi.detached.house,
          `Row house` = Row.house,
          Duplex = Apartment.or.flat.in.a.duplex,
          `Apartment (less than 5 storeys)` = Apartment.in.a.building.that.has.fewer.than.five.storeys,
          `Apartment (5 or more storeys)` = Apartment.in.a.building.that.has.five.or.more.storeys
        )
      
      data %>%
        datatable(selection = list(mode = "single", selected = 1, target = "row"), options = list(
          initComplete = JS(
            change_font_size(11)
          )
        )) %>%
        DT_percentage_format(percentage_cols = colnames(data)[!colnames(data) %in% c("Subdivision", "Total private dwellings")])
    })
    
    return(selected_subdivision)
  })
}