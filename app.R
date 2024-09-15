library(shiny)
library(readxl)
library(DT)
library(dplyr)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Excel File Upload and Parsing with Group Labels"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File",
                accept = c(".xls", ".xlsx")),
      textOutput("additional_text"),
      tags$hr()
    ),

    mainPanel(
      DTOutput("contents")
    )
  )
)

# Define server logic to read and display the file contents
server <- function(input, output) {
  output$additional_text <- renderText({
    req(input$file1)  # Ensure file is uploaded
    
    filename <- input$file1$name
    
    # Extract the date from the file name
    date_string <- gsub(".*_(\\d{2}_\\d{2}_\\d{4})\\.xlsx$", "\\1", filename)
    date_string <- gsub("_", "-", date_string)  # Replace underscores with hyphens
    
    # Try to convert the date, and catch any errors
    result <- tryCatch({
      date <- as.Date(date_string, format = "%m-%d-%Y")
      
      # Check if date conversion results in NA
      if (is.na(date)) {
        stop("File name in incorrect format, please include date at the end with underscores in between. Format: mm_dd_yyyy")
      }
      
      # If date is valid, return the formatted text
      paste("Master Spreadsheet Date Identifier:", date)
      
    }, error = function(e) {
      # If an error occurs, return the error message
      return(paste("Error:", e$message))
    })
    
    # Return the result (either date or error message)
    result
  })
  
  output$contents <- renderDT({
    req(input$file1)
    
    # Read the uploaded Excel file
    ext <- tools::file_ext(input$file1$datapath)
    validate(need(ext == "xls" || ext == "xlsx", "Please upload an Excel file"))
    
    # Read the file using readxl
    df <- read_excel(input$file1$datapath, col_names = TRUE,sheet = 1,skip = 4)
    
    # Identify group rows (assuming they only have values in the first column and NA in others)
    group_rows <- which(!is.na(df[[1]]) & rowSums(is.na(df)) == ncol(df) - 1)
    first_grade_group_rows <- group_rows[which((group_rows + 1) == lead(group_rows))]  # Group followed by another group
    second_grade_group_rows <- group_rows[which((group_rows + 1) != lead(group_rows)| is.na(lead(group_rows)))]  # Group followed by products
    #print(group_rows)
    #print(first_grade_group_rows)
    #print(second_grade_group_rows)
    # Create a new column for groups and forward-fill the group names
    df <- df %>%
      mutate(FirstOrder_Group = NA) %>%
      mutate(FirstOrder_Group = ifelse(row_number() %in% first_grade_group_rows, df[[1]], NA)) %>%
      tidyr::fill(FirstOrder_Group, .direction = "down") %>%
      mutate(SecondOrder_Group = NA) %>%
      mutate(SecondOrder_Group = ifelse(row_number() %in% second_grade_group_rows, df[[1]], NA)) %>%
      tidyr::fill(SecondOrder_Group, .direction = "down") %>%
      filter(!row_number() %in% c(first_grade_group_rows, second_grade_group_rows)) # Remove the group header rows
      
    
    df <- df %>%
      mutate(FirstOrder_Group = ifelse(is.na(FirstOrder_Group), "No Assigned Group", FirstOrder_Group)) %>%
      mutate(SecondOrder_Group = ifelse(is.na(SecondOrder_Group), "No Assigned Group", SecondOrder_Group)) %>%
      select(FirstOrder_Group, SecondOrder_Group, everything())  # Move the Group column to the front
    
    # Render the table
    datatable(df)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
