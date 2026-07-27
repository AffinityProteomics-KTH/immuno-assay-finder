
uniprotUI <- function(id, up_path, ec_path) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  # UI-elements ####
  tagList(
    
    page_sidebar(
      title = "Search tool to extract UniProt IDs of interest",
      
      
      fluidRow(
        # UniProtKB text:
        tags$p("Latest update of meta data from ", 
               tags$a("UniProtKB",
                      target = "_blank",
                      href = "https://www.uniprot.org/"),
               paste0(" was done on ", str_extract(up_path, "\\d{4}-\\d{2}-\\d{2}"), "."),
               
               tags$br(),
               # Exocarta version text:
               tags$a("ExoCarta",
                      target = "_blank",
                      href = "http://exocarta.org/index.html"),
               paste0(" version ",
                      str_extract(ec_path, "(?<=_)\\d(?=\\.)"),
                      " is used."),
               
               tags$br(),
               # Note on inclusion-text:
               "Please note that this search function only covers proteins 
                      that are included in at least one method under the Methods-tab.")),
      
      # Search input ####
      sidebar = sidebar(
        width = 400,
        helpText("Start typing in the search fields to get suggestions to choose 
                 from. You can use more than one search field, the results will 
                 be combined into one table."),
        
        accordion(
          open = FALSE, # No open accordion's from start
          ## Names ####   
          accordion_panel(
            title = "Names",
            icon = icon("n",
                        lib = "font-awesome"),
            ### alt names ####
            selectizeInput(inputId = ns("search_alt"), 
                           label = "Alternative gene names/synonyms/entry/protein names", 
                           choices = NULL, 
                           multiple = TRUE),
            
            ### enzymes ####
            selectizeInput(inputId = ns("search_enz"), 
                           label = "Enzyme Commission number", 
                           choices = NULL, 
                           multiple = TRUE),
          ),
          
          ## Gene Ontology (GO) ####   
          accordion_panel(
            title = "Gene Ontology (GO)",
            icon = icon("tags",
                        lib = "glyphicon"),
            ### goterms ####
            selectizeInput(inputId = ns("search_goterm"), 
                           label = "All", 
                           choices = NULL, 
                           multiple = TRUE),
            
            ### goterms biological processes ####
            selectizeInput(inputId = ns("search_goterm_bio"), 
                           label = "Biological Process", 
                           choices = NULL, 
                           multiple = TRUE),
            
            ### goterms molecular functions ####
            selectizeInput(inputId = ns("search_goterm_mol"), 
                           label = "Molecular Function", 
                           choices = NULL, 
                           multiple = TRUE),
            
            ### goterms cellular component ####
            selectizeInput(inputId = ns("search_goterm_cell"), 
                           label = "Cellular Component", 
                           choices = NULL, 
                           multiple = TRUE),
            
            ### goid ####
            selectizeInput(inputId = ns("search_goid"), 
                           label = "GO IDs", 
                           choices = NULL, 
                           multiple = TRUE),
          ),
          
          ## Associations ####   
          accordion_panel(
            title = "Associations",
            icon = icon("circle-nodes",
                        lib = "font-awesome"),
            ### disease ####
            selectizeInput(inputId = ns("search_disease"), 
                           label = "Diseases", 
                           choices = NULL, 
                           multiple = TRUE),
            
            ### pathway ####
            selectizeInput(inputId = ns("search_pathway"),
                           label = "Pathways",
                           choices = NULL,
                           multiple = TRUE),
            
            ### interactions ####
            selectizeInput(inputId = ns("search_interact"), 
                           label = "Interactions", 
                           choices = NULL, 
                           multiple = TRUE),
            
            ### species ####
            selectizeInput(inputId = ns("search_species"), 
                           label = "Species", 
                           choices = NULL, 
                           multiple = TRUE),
          ),
          
          ## Locations ####   
          accordion_panel(
            title = "Locations",
            icon = icon("location-dot",
                        lib = "font-awesome"),
            
            ### Subcellular ####
            selectizeInput(inputId = ns("search_subcell"), 
                           label = "Subcellular", 
                           choices = NULL, 
                           multiple = TRUE),
          ),
        ), # end general accordion
        
        ## ExoCarta switch ####
        ## switch to control if results should match all search terms
        helpText("Should only proteins present in ExoCarta be included?"),
        input_switch(id = ns("ec_search"), 
                     label = "ExoCarta",
                     value = FALSE),
        helpText("ExoCarta is a small extracellular vesicle (sEV) protein, RNA and lipid database.",
                 "If all search fields are empty, the toggle with provide the full list of available targets also in ExoCarta."),
        
        ## And/Or switch ####
        ## switch to control if results should match all search terms
        helpText(tags$hr(),
                 "Should all search criteria be met in each results entry?"),
        input_switch(id = ns("strict_search"), 
                     label = "Strict search",
                     value = FALSE),
        helpText("The combination of at least one entry per used search field will have to match in each result entry.",
                 tags$hr()),
        
        ## Search ####
        # action button to render results table
        actionButton(inputId = ns("search"), 
                     label = "Search", 
                     icon = icon("search",
                                 lib = "glyphicon"),
                     class = "btn-success",
                     style = "color: #000000; background-color: #A7C947; border_color: #A7C947"), 
        
        ## Export ####
        ## button to download table of filtered results
        downloadButton(outputId = ns("export"),
                       label = "Export results",
                       icon = icon("download",
                                   lib = "glyphicon"),
                       style = "color: #000000; background-color: #A48FA9; border_color: #A48FA9"),
        
        # switch for if dark mode should be active or not
        input_dark_mode()
        
      ), # end sidebar
      
      # Output ####
      textOutput(ns("copytext")),
      
      fluidRow(
        column(width = 10,
               div(verbatimTextOutput(ns("uniprotids")))),
        
        column(width = 2,
               div(uiOutput(ns("copy_uniprot"))))
      ),
      
      column(width = 12,
             div(DTOutput(ns("table_uniprot")),
                 style = "font-size:80%"))
      
    ) # end page_sidebar
  ) # end tagList
} # end UI module function


# Module server function
uniprotServer <- function(id, uniprot_db, search_list) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      # SelectizeInput update ####
      # make selectInput run server-side (R instead of JavaScript)
      updateSelectizeInput(session, 'search_alt', 
                           choices = search_list$altnames, server = TRUE)
      
      updateSelectizeInput(session, 'search_enz', 
                           choices = search_list$enzymes, server = TRUE)
      
      updateSelectizeInput(session, 'search_goterm', 
                           choices = search_list$go_terms, server = TRUE)
      
      updateSelectizeInput(session, 'search_goterm_bio', 
                           choices = search_list$go_terms_bio, server = TRUE)
      
      updateSelectizeInput(session, 'search_goterm_mol', 
                           choices = search_list$go_terms_mol, server = TRUE)
      
      updateSelectizeInput(session, 'search_goterm_cell', 
                           choices = search_list$go_terms_cell, server = TRUE)
      
      updateSelectizeInput(session, 'search_goid', 
                           choices = search_list$go_ids, server = TRUE)
      
      updateSelectizeInput(session, 'search_disease', 
                           choices = search_list$diseases, server = TRUE)
      
      updateSelectizeInput(session, 'search_pathway', 
                           choices = search_list$pathways, server = TRUE)
      
      updateSelectizeInput(session, 'search_interact', 
                           choices = search_list$interacts, server = TRUE)
      
      updateSelectizeInput(session, 'search_species', 
                           choices = search_list$species, server = TRUE)
      
      updateSelectizeInput(session, 'search_subcell', 
                           choices = search_list$subcellar, server = TRUE)
      
      
      # Function to extract IDs
      extract_ids <- function(tmp_data = uniprot_db, input_id, colname){
        
        # tmp_results <- NA
        
        if(!is.null(input[[input_id]])){
          ## create regular expression based on search entries
          tmp_reg_ex <- input[[input_id]] %>% 
            paste0(collapse = "|") %>% 
            str_replace_all("\\(", "\\\\(") %>% 
            str_replace_all("\\)", "\\\\)") %>% 
            str_replace_all("\\[", "\\\\[") %>% 
            str_replace_all("\\]", "\\\\]") %>% 
            str_replace_all("\\.", "\\\\.") %>% 
            str_replace_all("\\-", "\\\\-")
          
          ## extract rows matching the regex and selected columns for table to show
          # tmp_results <- 
          tmp_data %>% 
            filter(str_detect(!!sym(colname), regex(tmp_reg_ex, ignore_case = TRUE))) %>% 
            return()
        }
        
        # return(tmp_results)
      }
      
      # search_table ####
      search_table <- reactive({ 
        
        ## Extract results ####
        ### on alt names ####
        if(!is.null(input$search_alt)){
          
          results_names_alt <- uniprot_db %>% 
            select(entry, gene_names, entry_name) %>% 
            mutate(entry_name = str_extract(entry_name, "^.*(?=_)")) %>% 
            separate_longer_delim(gene_names,
                                  delim = " ") %>% 
            unite("names", gene_names, entry_name, sep = ",") %>% 
            separate_longer_delim(names,
                                  delim = ",") %>% 
            unique() %>% 
            filter(names %in% input$search_alt) %>% 
            {filter(uniprot_db, entry %in% .$entry)}
          
          results_names_prot <- extract_ids(input_id = "search_alt",
                                            colname = "protein_names")
          
          results_names <- bind_rows(results_names_alt, 
                                     results_names_prot) %>% 
            unique()
          
          rm(results_names_alt, results_names_prot)
        }
        
        ### on enzyme numbers  ####
        results_enz <- extract_ids(input_id = "search_enz", 
                                   colname = "protein_names")
        
        ### on goterms ####
        results_goterm <- extract_ids(input_id = "search_goterm", 
                                      colname = "gene_ontology_go")
        
        ### on goterms bio ####
        results_goterm_bio <- extract_ids(input_id = "search_goterm_bio", 
                                          colname = "gene_ontology_biological_process")
        
        ### on goterms mol ####
        results_goterm_mol <- extract_ids(input_id = "search_goterm_mol", 
                                          colname = "gene_ontology_molecular_function")
        
        ### on goterms cell ####
        results_goterm_cell <- extract_ids(input_id = "search_goterm_cell", 
                                           colname = "gene_ontology_cellular_component")
        
        ### on goid ####
        results_goid <- extract_ids(input_id = "search_goid", 
                                    colname = "gene_ontology_ids")
        
        ### on disease ####
        if(!is.null(input$search_disease)){
          ## create regular expression based on search entries
          reg_ex <- input$search_disease %>% 
            paste0(collapse = "|") %>% 
            str_replace_all("\\(", "\\\\(") %>% 
            str_replace_all("\\)", "\\\\)")
          
          ## extract rows matching the regex and selected columns for table to show
          results_disease <- uniprot_db %>% 
            filter(str_detect(involvement_in_disease, regex(reg_ex, ignore_case = TRUE))) 
        }
        
        ### on pathway ####
        results_pathway <- extract_ids(input_id = "search_pathway", 
                                       colname = "pathway")
        
        ### on interact ####
        results_interact <- extract_ids(input_id = "search_interact", 
                                        colname = "interacts_with")
        
        ### on species ####
        if(!is.null(input$search_species)){
          results_species <- uniprot_db %>% 
            filter(organism %in% input$search_species)
        }
        
        ### on subcellular ####
        results_subcellar <- extract_ids(input_id = "search_subcell",
                                         colname = "subcellular_location_cc")
        
        if(!is.null(input$search_subcell)){
          ## create regular expression based on search entries
          reg_ex <- input$search_subcell %>% 
            paste0(collapse = "|")
          
          ## extract rows matching the regex and selected columns for table to show
          results_subcellar <- uniprot_db %>% 
            filter(str_detect(subcellular_location_cc, regex(reg_ex, ignore_case = TRUE)))
        }
        
        ## combine results ####
        ## extract variable names for found results
        queries <- ls(pattern = "results") %>% 
          subset(str_detect(., "_table", negate = TRUE))
        
        queries_list <- mget(queries)
        
        ## combine results into one table
        results_table_org <- queries_list %>% 
          {do.call(bind_rows, args = c(., .id = "query"))}
        
        # if no search input is present and exocarta switch is TRUE, 
        # then all that are in exocarta are displayed
        if(map(queries_list, is.null) %>% unlist() %>% all() & 
           input$ec_search){
          results_table_org <- uniprot_db %>% 
            filter(exocarta == "yes") %>% 
            mutate(query = "exocarta")
        }
        
        ## check if there are any results to move on with
        validate(need(nrow(results_table_org) > 0,
                      "No search results."))
        
        ### strict search ####
        ## extract number of queries with any search results
        n_query <- results_table_org %>% 
          pull(query) %>% 
          unique() %>% 
          length()
        
        ## filter out only results in all queries (if switch is TRUE)
        if(input$strict_search){
          results_table <- results_table_org %>% 
            add_count(entry) %>% # add number of rows per uniprot id
            filter(n == n_query) # only keep rows present in all queries
          
          ## Check that there are any results to move on with
          validate(need(nrow(results_table) > 0,
                        "No search results."))
        } else {
          results_table <- results_table_org %>% 
            add_count(entry) # add number of rows per uniprot id (one row per query result)
          
        }
        
        ### exocarta ####
        ## filter out only results also in exocarta (if switch is TRUE)
        if(input$ec_search){
          results_table <- results_table %>% 
            filter(exocarta == "yes")
        }
        
        ## Check that there are any results to print
        validate(need(nrow(results_table) > 0,
                      "No search results."))
        
        ## adjust output ####
        ## adapt which columns are shown based on search
        columns_sel <-  c("organism",
                          "entry", 
                          "gene_names_primary", 
                          "gene_names_synonym",
                          "gene_names_ordered_locus",
                          "gene_names_orf",
                          "protein_names",
                          "exocarta",
                          ifelse(!is.null(input$search_goterm), 
                                 "gene_ontology_go", NA),
                          ifelse(!is.null(input$search_goterm_bio), 
                                 "gene_ontology_biological_process", NA),
                          ifelse(!is.null(input$search_goterm_mol), 
                                 "gene_ontology_molecular_function", NA),
                          ifelse(!is.null(input$search_goterm_cell), 
                                 "gene_ontology_cellular_component", NA),
                          ifelse(!is.null(input$search_goid), 
                                 "gene_ontology_ids", NA),
                          ifelse(!is.null(input$search_disease),
                                 "involvement_in_disease", NA),
                          ifelse(!is.null(input$search_pathway), 
                                 "pathway", NA),
                          ifelse(!is.null(input$search_interact), 
                                 "interacts_with", NA),
                          ifelse(!is.null(input$search_subcell), 
                                 "subcellular_location_cc", NA)) %>% 
          subset(!is.na(.))
        
        ## format result table with desired columns, unique entries etc.
        results_table <- results_table %>% 
          select(all_of(columns_sel)) %>% 
          unique() %>% 
          arrange(gene_names_primary) %>% 
          `colnames<-`(str_remove(colnames(.), "gene_names_") %>% 
                         str_remove("_cc$") %>% 
                         str_replace_all("_", " ") %>% 
                         str_to_sentence() %>% 
                         str_replace("Orf", "ORF")%>% 
                         str_replace("carta", "Carta")) %>% 
          select_if(function(x){!all(is.na(x))})
        
        # Add column with UniProt-links
        results_table$`UniProt ID` <- sapply(results_table$Entry,
                                             function (n)
                                             {
                                               ## tags$a function produces adequately escaped links
                                               as.character(tags$a(n, 
                                                                   href=paste0("https://www.uniprot.org/uniprotkb/", 
                                                                               n),
                                                                   target="_blank"))
                                             })
        
        # Put links-column in the beginning
        results_table <- results_table %>% 
          select(`UniProt ID`, everything())
        
        return(results_table)
        
      }) %>% # end reactive extraction
        bindEvent(input$search,
                  input$strict_search,
                  input$ec_search,
                  ignoreInit = TRUE)
      
      # uniprot regex ####
      # Extract the UniProt IDs to allow easy copying to main-tab
      uniprot_regex <- reactive({ 
        search_table() %>% 
          pull(Entry) %>% 
          paste0(collapse = "|")
      }) %>% # end reactive extraction
        bindEvent(input$search,
                  input$strict_search,
                  input$ec_search,
                  ignoreInit = TRUE)
      
      # create text above uniprot regex
      copy_text <- reactive({
        
        c("Copy below regular expression of the result UniProt IDs and 
        paste them in the Search field under the Methods-tab 
        to see which methods cover the targets. You can also export the 
        results in this tab, and import the file in the Methods-tab.")
        
      }) %>% # end reactive extraction
        bindEvent(input$search,
                  input$strict_search,
                  input$ec_search,
                  ignoreInit = TRUE)
      
      # Output ####
      ## Make copying instruction text appear before results
      output$copytext <-  renderText({ copy_text() })
      
      ## Add copy-to-clipboard button
      output$copy_uniprot <- renderUI({
        ns <- session$ns
        
        rclipButton(
          inputId = ns("copybtn"),
          label = "",
          clipText = uniprot_regex(),
          icon = icon("copy",
                      lib = "glyphicon"),
          style = "font-size:80%"
        )
      }) %>% # end renderUI copy-to-clipboard
        bindEvent(input$search,
                  input$strict_search,
                  input$ec_search,
                  ignoreInit = TRUE)
      
      ## Print regular expression of UniProt IDs
      output$uniprotids <- renderText({ uniprot_regex() })
      
      ## Send the table to the defined UI
      output$table_uniprot <- renderDT(search_table() %>% 
                                         select(-Entry),
                                       options = list(
                                         paging = TRUE,
                                         scrollX = TRUE,
                                         pageLength = 50), 
                                       escape = c(-2))
      
      ## Export table ####
      output$export <- downloadHandler(
        filename = "target_uniprot.tsv",
        content = function(file) {
          write_tsv(search_table() %>%
                      select(-`UniProt ID`) %>% 
                      rename(`UniProt ID` = Entry) %>% 
                      relocate(`UniProt ID`, .before = everything()), 
                    file)
        }
      ) # end downloadHandler
      
    } # end module function
  ) # end moduleServer
} # end module server function
