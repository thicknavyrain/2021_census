library(readr)
library(dplyr)
library(tidyr)
library(here)

# Set the working directory to the location of the script
setwd(here())

# Adjust the `col_types` accordingly if the types differ
column_types <- cols(
  .default = col_guess(),
  h13n2 = col_double(), # Column 73 should be double
  s08e = col_character() # Column 96 should be character
)

# Read the CSV file with the specified column types
census_2021 <- read_csv('housingandsanitation_10%_20221011d.csv', col_types = column_types)

# Rename columns
census_2021 <- rename(census_2021, 
                      dwelling_type = h01, 
                      wall_material = h02, 
                      roof_material = h03, 
                      floor_material = h04, 
                      tenancy = h05, 
                      ownership = h06, 
                      rooms = h07a, 
                      new_roof_material = newh03)

g <- function(df, selected_columns) {
  results <- list()
  
  for (col in selected_columns) {
    # Ensure the column exists in the dataframe
    if (!col %in% names(df)) {
      stop(paste("Column", col, "does not exist in the dataframe."))
    }
    
    # Dynamically refer to columns using tidy evaluation
    grouped_df <- df %>%
      group_by(region, distcode, subdist, !!sym(col)) %>%
      summarise(count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = !!sym(col), values_from = count, values_fill = list(count = 0))

    # Store the result in the list with the column name as the key
    results[[col]] <- grouped_df
  }
  
  return(results)
}


# List of new column names to be processed
selected_columns <- c('dwelling_type', 'wall_material', 'roof_material', 
                      'floor_material', 'tenancy', 'ownership', 'rooms', 
                      'new_roof_material')

# Applying the function
result <- g(census_2021, selected_columns)

# Output results to CSV
for (col in names(result)) {
# Ensure the output directory exists
  dir.create("R_outputs", showWarnings = FALSE)

# Save the CSV file in the "R_outputs" directory
  write_csv(result[[col]], paste0("R_outputs/", col, '.csv'))
}
