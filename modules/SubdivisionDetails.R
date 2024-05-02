# Here, we put the definition of the module subdivision_details.
# In server.R, we call the subdivision_detailsServer function with the selected census subdivision as a parameter.
# In ui.R, we call the subdivision_detailsUI function to display the UI portion of the module.

# Define the module UI
subdivision_detailsUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Create a title for the module
    tags$h3("Demographic details of the selected census subdivision"),
    # (1) Table for demographic portrait
    tags$h4("Demographic portrait"),
    DTOutput(ns("table_portrait")),
    # (2) Table for mother tongue
    tags$h4("Mother tongue"),
    DTOutput(ns("table_mother_tongue")),
    # (3) Table for education
    tags$h4("Education"),
    DTOutput(ns("table_education")),
    # (4) Table for immigration
    tags$h4("Immigration"),
    DTOutput(ns("table_immigiration"))
  )
}

# Define the module server
subdivision_detailsServer <- function(id, subdivision, data_details){
  moduleServer(id, function(input, output, session) {
    # We need to categorize the following characteristics into four categories: portrait, mother tongue, education and immigration.
    # [3] "GEO_NAME"                                                                                                                                            
    # [4] "Average.age.of.the.population"                                                                                                                       
    # [5] "Average.household.size"                                                                                                                              
    # [6] "Average.total.income.in.2020.among.recipients...."                                                                                                   
    # [7] "English"                                                                                                                                             
    # [8] "French"                                                                                                                                              
    # [9] "Arabic"                                                                                                                                              
    # [10] "Tagalog..Pilipino..Filipino."                                                                                                                        
    # [11] "Punjabi..Panjabi."                                                                                                                                   
    # [12] "Urdu"                                                                                                                                                
    # [13] "Italian"                                                                                                                                             
    # [14] "Spanish"                                                                                                                                             
    # [15] "Mandarin"                                                                                                                                            
    # [16] "Yue..Cantonese."                                                                                                                                     
    # [17] "Immigrants"                                                                                                                                          
    # [18] "Total...Secondary..high..school.diploma.or.equivalency.certificate.for.the.population.aged.15.years.and.over.in.private.households...25..sample.data"
    # [19] "No.high.school.diploma.or.equivalency.certificate"                                                                                                   
    # [20] "With.high.school.diploma.or.equivalency.certificate"     
    
    # Create a reactive expression to filter the data based on the selected census subdivision
    filtered_data_portrait <- reactive({
      # This table contains age, household size and income data
      data_details %>%
        filter(DGUID %in% subdivision()) %>%
        select(
          `2021 Population` = `Population..2021`,
          `2016-21 Population Percentage Growth` = `Population.percentage.change..2016.to.2021`,
          `Subdivision name` = GEO_NAME,
          `Average age` = Average.age.of.the.population,
          `Average household size` = Average.household.size,
          `Average income` = `Average.total.income.in.2020.among.recipients....`
        )
    })
    
    # Render the portrait able with the filtered data
    output$table_portrait <- renderDT({
      datatable(filtered_data_portrait())
    })
    
    filtered_data_mother_tongue <- reactive({
      # This table contains mother tongue data
      colnames(data_details)
      data_details %>%
        filter(DGUID %in% subdivision()) %>%
        transform_to_proportions("Population..2021") %>%
        select(`Subdivision name` = GEO_NAME, English, French, Arabic, Tagalog = `Tagalog..Pilipino..Filipino.`, Punjabi = `Punjabi..Panjabi.`, Urdu, Italian, Spanish, Mandarin, Cantonese = `Yue..Cantonese.`)
    })
    
    # Render the mother tongue table with the filtered data
    output$table_mother_tongue <- renderDT({
      datatable(filtered_data_mother_tongue())
    })
    
    filtered_data_education <- reactive({
      # This table contains education data
      data_details %>%
        filter(DGUID %in% subdivision()) %>%
        transform_to_proportions("Population..2021") %>%
        select(`Subdivision name` = GEO_NAME, `Less than high school` = `No.high.school.diploma.or.equivalency.certificate`, `High school or more` = `With.high.school.diploma.or.equivalency.certificate`)
    })
    
    output$table_education <- renderDT({
      datatable(filtered_data_education())
    })
    
    filtered_data_immigration <- reactive({
      # This table contains immigration data
      data_details %>%
        filter(DGUID %in% subdivision()) %>%
        transform_to_proportions("Population..2021") %>%
        select(`Subdivision name` = GEO_NAME, Immigrants = Immigrants)
    })
    
    output$table_immigiration <- renderDT({
      datatable(filtered_data_immigration())
    })
  })
}
