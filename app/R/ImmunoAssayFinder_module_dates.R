
datesUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    
    # switch for if dark mode should be active or not
    input_dark_mode(),
    
    fluidRow(tags$h3("Summary of included target lists")),
    
    fluidRow(tags$p("This list summarizes when the respective lists for each 
                    panel and method were retrieved for the app, and the number 
                    of targets included in said lists.")),
    
    # ouput table
    DTOutput(ns("table_dates"))
    
  ) # end tagList
} # end UI module function


# Module server function
datesServer <- function(id, df) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      # Extract only the methods and the dates from which the lists are from
      dates_table <- df %>%
        add_count(technique, panel) %>% # add number of targets per technique and panel
        select(technique, panel, n, list_date) %>% # select relevant columns
        unique() %>% # keep only unique entries (one row per technique and panel)
        arrange(technique, panel) %>% # arrange technique and panel alphabetically
        # rename column names for human eyes:
        `colnames<-`(str_to_title(colnames(.))) %>%
        rename("Number of targets" = "N",
               "Version" = "List_date")
      
      # Send the table to the defined UI
      output$table_dates <- renderDT(dates_table,
                                     options = list(
                                       paging = TRUE,
                                       pageLength = 50))
      
    } # end module function
  ) # end moduleServer
} # end module server function
