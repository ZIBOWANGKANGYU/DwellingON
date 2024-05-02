# A function that transforms counts in a data frame to proportions.
# This function will be used in the following reactive expressions:
# - filtered_data_mother_tongue
# - filtered_data_education
# - filtered_data_immigration
# The function should have two parameters:
# - data: A data frame with counts. One of the columns in the data frame is the total count.
# - total_col: The name of the column that contains the total count.
# - remove_total: A logical value indicating whether the total count column should be removed from the output data frame. The default value is TRUE.
# The function only calculates the proportions for columns that are numeric: either integer or double.
# If a column is not numeric, it should be left as is.
# The function should return a data frame with the same columns as the input data frame, but with the counts transformed to proportions.
# The total count column should be removed if remove_total is TRUE.

transform_to_proportions <- function(data, total_col, remove_total = TRUE) {
  # Check that data is a data frame
  stopifnot(is.data.frame(data))
  # Check that total_col is a single string
  stopifnot(is.character(total_col) && length(total_col) == 1)
  # Check that total_col is a column in data
  stopifnot(total_col %in% colnames(data))
  
  data <- data %>%
    mutate(across(c(where(is.numeric), -all_of(total_col)), ~ . / !!sym(total_col)))
  
  if(remove_total) {
    data <- data %>%
      select(-all_of(total_col))
  }
  
  return(data)
}
