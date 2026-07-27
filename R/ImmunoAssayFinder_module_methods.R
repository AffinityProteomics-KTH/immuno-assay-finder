
methodsUI <- function(id, choices) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tmp_choices <- choices
  
  tagList(
    
    page_sidebar(
      title = "Browse, Compare, and Select the best Immunoassay for your application",
      
      # Filtering-sidebar ####
      sidebar = sidebar(
        width = 350,
        accordion(
          open = FALSE, # No open accordion's from start
          
          ## Import ####
          accordion_panel(
            # Text above import
            title = "Import a list:", 
            icon = icon("upload",
                        lib = "glyphicon"),
            helpText(tags$p("Formatting requirements:",
                            tags$br(),
                            "First column needs to be named 'UniProt ID' (without '').",
                            tags$br(),
                            "Any additional columns can be included, these will be kept in the export.",
                            tags$br(),
                            "The file needs to be one of .csv, .txt, .tsv, or .xlsx.",
                            tags$br(),
                            "Press 'Generate' to filter based on imported file.")),
            fileInput(inputId = ns("import"), 
                      label = NULL, 
                      multiple = FALSE,
                      accept = c(".csv", ".txt", ".tsv", ".xlsx"),
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"),
            
            ### Undo import ####
            # action button to undo import
            actionButton(inputId = ns("undo_import"),
                         label = "Undo import",
                         icon = icon("fast-backward",
                                     lib = "glyphicon"),
                         class = "btn-success", 
                         style = "color: #000000; background-color: #43858B; border_color: #43858B; padding:4px; font-size:80%"), 
          ), # end import accordion
          
          ## Search ####
          accordion_panel(
            # Text above text search
            title = "Gene name (HGNC) and/or UniProt ID search:", 
            icon = bsicons::bs_icon("search"),
            helpText("Separate entries with ; (no surrounding spaces) 
            or write as a regular expression 
                   (not case sensitive)."),
            helpText("To be specific in your search, preceed the Gene/ID with ^, 
                     and end it with [$:] (regular expression syntax), e.g. '^MAPT[$:]'."),
            helpText("If you can't find your target of interest, 
                     double check under the UniProt IDs-tab that you are using 
                     the correct Gene name or UniProt ID in your search."),
            helpText("Leave empty to browse all covered genes."),
            # Add box for gene/uniprot search
            textInput(inputId = ns("search_text"), 
                      label = NULL, 
                      value = "")
          ), # end search accordion
          
          # Filtering menu
          accordion_panel(
            # Text above filtering options
            title = "Filter options:",
            icon = bsicons::bs_icon("filter-circle"),
            helpText("Leaving empty is the same as selecting all options."),
            
            ## Species ####
            # Option to select one or more species to include:
            accordion_panel(
              title = "Species",
              checkboxGroupInput(inputId = ns("filter_spec"), 
                                 label = NULL,
                                 choices = tmp_choices$species_choices, 
                                 selected = NULL),
            ),
            
            ## Panels ####
            # Option to select one or more panels to include:
            accordion_panel(
              title = "Panels",
              checkboxGroupInput(inputId = ns("filter_panel"), 
                                 label = NULL,
                                 choices = tmp_choices$panels_choices, 
                                 selected = NULL),
            ),
            
            ## Technique ####
            # Option to select one or more techniques to include:
            accordion_panel(
              title = "Techniques",
              checkboxGroupInput(inputId = ns("filter_tech"), 
                                 label = NULL,
                                 choices = tmp_choices$techs_choices, 
                                 selected = NULL),
            ),
            
            ## Curation ####
            # Option to select one or more levels of curation to include:
            accordion_panel(
              title = "Uniprot curation",
              checkboxGroupInput(inputId = ns("filter_cura"), 
                                 label = NULL,
                                 choices = tmp_choices$cura_choices, 
                                 selected = NULL)
            ),
            
            ## Quantification ####
            # Option to select one or more levels of euantification to include:
            accordion_panel(
              title = "Quantification",
              checkboxGroupInput(inputId = ns("filter_quant"), 
                                 label = NULL,
                                 choices = tmp_choices$quant_choices, 
                                 selected = NULL)
            )
          ), # end filter accordion
          
          # Sorting ####          
          accordion_panel(
            # Text above sorting options
            title = "Sorting options:",
            icon = bsicons::bs_icon("sort-alpha-down"), 
            # Select what to sort the targets on (level 1)
            selectInput(inputId = ns("prot_sort1"), 
                        label = "Target (level 1)", 
                        choices = c("gene", "uniprot_id", "species"), 
                        selected = "species",
                        multiple = FALSE),
            # Select what to sort the targets on (level 2)
            selectInput(inputId = ns("prot_sort2"), 
                        label = "Target (level 2)", 
                        choices = c("gene", "uniprot_id", "species"), 
                        selected = "gene",
                        multiple = FALSE)
          ) # end sorting accordion
          
        ), # end global accordion
        
        # Buttons/toggle ####
        ## Generate ####
        ## action button to rerender heatmap based on filtering
        helpText("Press 'Generate' to update figure, table and export based on above filtering/sorting."),
        actionButton(inputId = ns("generate"), 
                     label = "Generate", 
                     icon = icon("play",
                                 lib = "glyphicon"),
                     class = "btn-success",
                     style = "color: #000000; background-color: #A7C947; border_color: #A7C947"), 
        
        ## Reset ####
        # action button to reset filtering (to not have to untick everything)
        # add "clear" for each filtering section as well?
        actionButton(inputId = ns("reset"),
                     label = "Reset filtering",
                     icon = icon("fast-backward",
                                 lib = "glyphicon"),
                     class = "btn-success",
                     style = "color: #000000; background-color: #43858B; border_color: #43858B"), 
        
        ## Export ####
        ## button to download table of filtered results
        downloadButton(outputId = ns("export"),
                       label = "Export results",
                       icon = icon("download",
                                   lib = "glyphicon"),
                       style = "color: #000000; background-color: #A48FA9; border_color: #A48FA9"),
        
        ## Panel switch ####
        ## switch to control if techniques should be split up by panels
        input_switch(id = ns("incl_panel"), 
                     label = "Split technologies into panels",
                     value = FALSE),
        
        ## Availability switch ####
        ## switch if only methods with available methods should be shown
        input_switch(id = ns("excl_empty"), 
                     label = "Show only methods with available assay",
                     value = FALSE),
        
        ## Curation switch ####
        ## switch if curation annotation (coloring) should be included or not
        input_switch(id = ns("incl_curation"), 
                     label = "Include curation annotation",
                     value = TRUE),
        
        ## Dark mode ####
        ## switch for if dark mode should be active or not
        input_dark_mode()
        
      ), # end sidebar
      
      # Output ####
      
      fluidRow(
        fluidRow(tags$p("Browse the targets covered by technologies available at 
                        the Affinity Proteomics Unit, SciLifeLab. Please note 
                        that this summary is based on lists from assay providers 
                        and includes only methods with readily and publicly 
                        available target information. 
                        We update these lists regularly (see the tab 'List dates' 
                        for the current versions); however, the absence of 
                        an assay for a specific protein does not necessarily 
                        mean that one has not recently become available from 
                        one of the providers. If your target of interest is not 
                        listed, please contact us. We will be happy to verify 
                        the latest availability with the assay providers and, 
                        if needed, discuss the possibility of developing 
                        a new assay ourselves for you."),
                 tags$p("Assays with absolute quantification are marked with an asterisk (*).",
                        tags$br(),
                        "Gene names followed by a colon (:) signifies that a specific form is targeted.")),
      
        ## Plot area ####
        column(width = 8,
               (div(style='width:100%;overflow-x: scroll;overflow-y: scroll;', 
                    uiOutput(ns("heatmap")))) ), 
        
        ## Coverage table ####
        column(width = 4,
               div(h4("Coverage of all targets in heatmap"),
                   DTOutput(ns("table_coverage")),
                   style = "width:100%;font-size:80%")) #
        
      )
    ) # end page_sidebar
  ) # end tagList
} # end UI module function



# Module server function
methodsServer <- function(id, df, choices) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      # Curation inclusion ####
      # create vector for curation-fill, based on inclusion switch
      df_mod_cur <- reactive({ 
        
        tmp_data <- df
        
        if(input$incl_curation){
          tmp_data_cur <- tmp_data %>% 
            mutate(curation_ann = factor(uniprot_source))
        } else {
          tmp_data_cur <- tmp_data %>% 
            mutate(curation_ann = factor("not_used"))
        }
        
        return(tmp_data_cur)
        
      }) %>% # end reactive curation
        bindEvent(input$incl_panel, 
                  input$excl_empty, 
                  input$incl_curation,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      # Split panel option ####
      # Create vector for axis based on split panel option
      df_mod_panel <- reactive({ 
        
        tmp_data <- df_mod_cur() %>% 
          mutate(quantification_label = ifelse(quantification == "Absolute", 
                                               "*", ""))
        
        if(input$incl_panel){
          tmp_data_panel <- tmp_data %>% 
            mutate(technique_axis = technique_panel %>% 
                     factor())
        } else {
          tmp_data_panel <- tmp_data %>% 
            mutate(technique_axis = technique %>% 
                     factor())
        }
        
        return(tmp_data_panel)
        
      }) %>% # end reactive panel
        bindEvent(input$incl_panel, 
                  input$excl_empty,
                  input$incl_curation,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      # Import file ####
      rv <- reactiveValues(import_table = NULL)
      
      observe({
        req(input$import)
        file_import <- input$import
        ext <- tools::file_ext(file_import$datapath)
        
        validate(need(ext %in% c("csv", "txt", "xlsx", "tsv"), 
                      "Please upload a csv, tsv/txt, or xlsx file."))
        
        if(ext == "xlsx"){
          import_tmp <- read_xlsx(path = file_import$datapath, 
                                  sheet = 1)
        } else {
          import_tmp <- data.table::fread(file_import$datapath) %>% 
            as_tibble()
        }
        
        rv$import_table <- import_tmp
      })
      
      # Filtering ####
      df_mod_sub <- reactive({
        
        tmp_data <- df_mod_panel()
        
        ## subset on import ####
        if(!is.null(input$import) & 
           !is.null(rv$import_table)){
          tmp_data <- df_mod_panel()
          
          validate(need(colnames(rv$import_table)[1] == "UniProt ID",
                        "First column must be named 'UniProt ID', without the surrounding ''."))
          
          tmp_data <- tmp_data %>% 
            filter(uniprot_id %in% (rv$import_table %>% 
                                      pull(`UniProt ID`) %>% 
                                      unlist() %>% 
                                      str_split("[;, ]") %>% 
                                      unlist()))
        }
        
        ## subset on genes/uniprot ids ####
        if(input$search_text != ""){
          filter_string <- input$search_text %>%
            str_replace(";$", "") %>% 
            str_replace_all("(?<=[\\d\\w]);", "|")
          
          
          tmp_data <- tmp_data %>%
            filter(str_detect(gene, regex(filter_string, ignore_case = T)) |
                     str_detect(uniprot_id, regex(filter_string, ignore_case = T)))
        }
        
        ## subset on species ####
        if(!is.null(input$filter_spec)){
          tmp_data <- tmp_data %>% 
            filter(species %in% input$filter_spec)
        }
        
        ## Subset on panel ####
        if(!is.null(input$filter_panel)){
          tmp_data <- tmp_data %>% 
            filter(panel %in% input$filter_panel) %>% 
            mutate(technique_axis = factor(technique_axis,
                                           levels = unique(technique_axis)))
        }
        
        ## Subset on tech ####
        if(!is.null(input$filter_tech)){
          tmp_data <- tmp_data %>% 
            filter(technique %in% input$filter_tech) %>% 
            mutate(technique_axis = factor(technique_axis,
                                           levels = unique(technique_axis)))
        }
        
        ## Subset on curation ####
        if(!is.null(input$filter_cura)){
          tmp_data <- tmp_data %>% 
            filter(uniprot_source %in% input$filter_cura)
        }
        
        ## Subset on quantification ####
        if(!is.null(input$filter_quant)){
          tmp_data <- tmp_data %>% 
            filter(quantification %in% input$filter_quant)
        }
        
        # Check if there are any targets left after subsetting
        if(nrow(tmp_data) == 0){
          stop("No results with current filtering.")
        }
        
        ## sort ####
        ## Sort subsetted table based on sorting input
        req(input$prot_sort1)
        
        tmp_data <- tmp_data %>% 
          dplyr::arrange(dplyr::desc(!!rlang::sym(input$prot_sort1)),
                         dplyr::desc(!!rlang::sym(input$prot_sort2))) %>% 
          mutate(target = factor(target, 
                                 levels = unique(target), 
                                 ordered = TRUE))
        
        return(tmp_data)
        
      }) # end reactive df_mod_sub
      
      # Update choices ####
      updated_choices <- choices
      
      ## species ####
      observe({
        updated_choices$species_choices <- df_mod_sub() %>%
          pull(species) %>%
          unique() %>%
          sort()
        
        updateCheckboxGroupInput(session,
                                 inputId = "filter_spec",
                                 choices = updated_choices$species_choices,
                                 selected = input$filter_spec)
      }) %>%
        bindEvent(input$filter_panel,
                  input$filter_tech,
                  input$filter_cura,
                  input$filter_quant,
                  input$generate,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      ## panels ####
      observe({
        updated_choices$panels_choices <- df_mod_sub() %>%
          pull(panel) %>%
          unique() %>%
          sort()
        
        updateCheckboxGroupInput(session,
                                 inputId = "filter_panel",
                                 choices = updated_choices$panels_choices,
                                 selected = input$filter_panel)
      }) %>%
        bindEvent(input$filter_spec,
                  input$filter_tech,
                  input$filter_cura,
                  input$filter_quant,
                  input$generate,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      ## techniques ####
      observe({
        updated_choices$techs_choices <- df_mod_sub() %>%
          pull(technique) %>%
          unique() %>%
          sort()
        
        updateCheckboxGroupInput(session,
                                 inputId = "filter_tech",
                                 choices = updated_choices$techs_choices,
                                 selected = input$filter_tech)
      }) %>%
        bindEvent(input$filter_spec,
                  input$filter_panel,
                  input$filter_cura,
                  input$filter_quant,
                  input$generate,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      
      ## curation ####
      observe({
        updated_choices$cura_choices <- df_mod_sub() %>%
          pull(uniprot_source) %>%
          unique() %>%
          sort()
        
        updateCheckboxGroupInput(session,
                                 inputId = "filter_cura",
                                 choices = updated_choices$cura_choices,
                                 selected = input$filter_cura)
      }) %>%
        bindEvent(input$filter_spec,
                  input$filter_panel,
                  input$filter_tech,
                  input$filter_quant,
                  input$generate,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      ## quantification ####
      observe({
        updated_choices$quant_choices <- df_mod_sub() %>%
          pull(quantification) %>%
          unique() %>%
          sort()
        
        updateCheckboxGroupInput(session,
                                 inputId = "filter_quant",
                                 choices = updated_choices$quant_choices,
                                 selected = input$filter_quant)
      }) %>%
        bindEvent(input$filter_spec,
                  input$filter_panel,
                  input$filter_tech,
                  input$filter_cura,
                  input$generate,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      
      
      # Reset choices/import ####
      observeEvent(input$undo_import, {
        rv$import_table <- NULL
        shinyjs::reset('import')
      })
      
      observeEvent(c(input$reset, input$import), {
        shinyjs::reset('search_text')
        shinyjs::reset('filter_spec')
        shinyjs::reset('filter_panel')
        shinyjs::reset('filter_tech')
        shinyjs::reset('filter_cura')
        shinyjs::reset('filter_quant')
      })
      
      # Generate table for plot #### 
      df_plot <- reactive({
        
        if(input$excl_empty){
          tmp_data <- df_mod_sub() %>% 
            mutate(technique_axis = factor(technique_axis, 
                                           levels = unique(technique_axis) %>% 
                                             sort()),
                   technique_panel = factor(technique_panel, 
                                            levels = unique(technique_panel) %>% 
                                              sort())) %>% 
            rename("Method" = technique_axis,
                   "Gene, UniProt ID, Species" = target)
          
        } else {
          tmp_data <- df_mod_sub() %>% 
            rename("Method" = technique_axis,
                   "Gene, UniProt ID, Species" = target)
        }
        
        return(tmp_data)
        
      }) %>% # end reactive df_plot
        bindEvent(input$generate, 
                  input$incl_panel, 
                  input$excl_empty, 
                  input$incl_curation,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      # nrows ####
      ## extract number of rows there will be in plot,
      # used for size adjustments
      n_rows <- reactive({
        
        df_plot() %>% 
          select(`Gene, UniProt ID, Species`) %>% # target in df_mod_sub
          unique() %>% 
          nrow() 
        
      }) %>% # end reactive nrow
        bindEvent(input$generate, 
                  input$incl_panel, 
                  input$excl_empty, 
                  input$incl_curation,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      # text size ####
      # Calculate plot text size to make labels readable regardless
      text_size <- reactive({
        
        n_rows() %>% 
          {case_when(. < 200 ~ 12, 
                     . < 500 ~ 10, 
                     . < 3000 ~ 8, 
                     TRUE ~ 6)} 
        
      }) %>% # end reactive text_size
        bindEvent(input$generate, 
                  input$incl_panel, 
                  input$excl_empty, 
                  input$incl_curation,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      # Create plot ####
      output$heatmap <- renderUI({
        ns <- session$ns
        
        output$heatmap2 <- renderPlot({
          
          # Calculate intercepts for gridlines
          x_grid <- isolate(df_plot()) %>% 
            {levels(.$Method)} %>% 
            length() %>% 
            {seq(0.5, . + 0.5, 1)}
          
          y_grid <- isolate(df_plot()) %>% 
            select(`Gene, UniProt ID, Species`) %>% # target in df_mod_sub
            unique() %>% 
            nrow() %>% 
            {seq(0.5, . + 0.5, 1)}
          
          # Generate plot
          patchwork::wrap_elements(ggplot(data = isolate(df_plot()),
                                          mapping = aes(x = Method,
                                                        y = `Gene, UniProt ID, Species`)) +
                                     geom_vline(xintercept=x_grid, colour='white') +
                                     geom_hline(yintercept=y_grid, colour='white') +
                                     geom_tile(aes(fill = curation_ann),
                                               color = "white",
                                               lwd = 1,
                                               linetype = 1, 
                                               show.legend = input$incl_curation) +
                                     geom_text(aes(label = quantification_label), 
                                               color = "white", 
                                               vjust = 0.77,
                                               size = text_size()) +
                                     coord_fixed() +
                                     scale_x_discrete(expand = c(0, 0), 
                                                      drop = FALSE,
                                                      sec.axis = dup_axis(), 
                                                      name = NULL) +
                                     scale_y_discrete(expand = c(0, 0), sec.axis = dup_axis()) +
                                     scale_fill_manual(values = c(rev(viridis(3)), "#A7C947") %>% 
                                                         `names<-`(c("AI", "Company", "Manual", "not_used")), 
                                                       name = "Gene-Uniprot curation") +
                                     theme(legend.position = "left",
                                           legend.justification = "top",
                                           axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0.5), 
                                           axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                                           text = element_text(size = text_size()),
                                           panel.grid = element_blank()))
        }) 
        
        ## height ####
        # Calculate output height to make plot readable regardless
        height_string <- reactive({
          
          height_output <- n_rows() %>%
            {(. * 4) + 1000}
          
          tmp_string <- height_output %>%
            paste0(., "px")
          
          return(tmp_string)
          
        }) # end reactive height
        
        plotOutput(ns('heatmap2'),
                   width = "100%", 
                   height = height_string())
        
      }) %>% # end output heatmap
        bindEvent(input$generate, 
                  input$incl_panel, 
                  input$excl_empty, 
                  input$incl_curation,
                  ignoreNULL = FALSE,
                  ignoreInit = TRUE)
      
      # Coverage table ####
      
      ## Create table ####
      coverage_table <- reactive({
        
        n_target <- df_plot() %>% 
          pull(`Gene, UniProt ID, Species`) %>% 
          unique() %>% 
          length()
        
        tmp_table <- df_plot() %>% 
          count(technique_panel, 
                .drop = FALSE) %>% 
          rename("Technique: Panel" = technique_panel) %>% 
          mutate(Targets = paste0(n, " / ", n_target),
                 "Coverage [%]" = round(n/n_target*100, 1)) %>% 
          select(-n)
        
        return(tmp_table)
      }) %>% # end reactive coverage_table
        bindEvent(input$generate, 
                  input$incl_panel, 
                  input$excl_empty, 
                  input$incl_curation,
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      ## Print table ####
      observe({
        output$table_coverage <- renderDT(coverage_table(),
                                          rownames = FALSE,
                                          options = list(
                                            paging = TRUE,
                                            scrollX = TRUE,
                                            pageLength = 50,
                                            layout = list(topStart = "info")))
      })
      
      # Export ####
      
      ## Create table ####
      export_table <- reactive({
        
        tmp_table <- df_plot() %>% # df_mod_sub
          mutate(quant_short = str_extract(quantification, "^.{3}")) %>% # extract "Abs" or "Rel" for shorter values in export table
          select(`Gene, UniProt ID, Species`, Method, quant_short) %>% 
          unique() %>% 
          arrange(Method) %>% 
          pivot_wider(names_from = Method,
                      values_from = quant_short,
                      values_fill = list("No"),
                      values_fn = list) %>%
          reframe(across(where(is.list), unlist),
                  .by = !where(is.list)) %>% 
          split(.$`Gene, UniProt ID, Species`) %>% 
          lapply(function(y) y %>% 
                   mutate(across(-matches("UniProt"), ~ unique(.x) %>% 
                                   sort() %>% 
                                   paste0(collapse = "/"))) %>% unique()) %>% 
          do.call(bind_rows, .) %>% 
          separate_wider_delim(`Gene, UniProt ID, Species`, 
                               delim = ", ",
                               names = c("Gene", "UniProt ID", "Species")) %>% 
          add_count(`UniProt ID`, name = "N same ID") %>% 
          relocate(`N same ID`, .after = Species)
        
        if(!is.null(rv$import_table)){  
          tmp_table <- rv$import_table %>% 
            `colnames<-`(ifelse(colnames(.) != "UniProt ID",
                                paste0("user_", colnames(.)),
                                colnames(.))) %>% 
            left_join(tmp_table, 
                      by = "UniProt ID")
        }
        
        return(tmp_table)
      }) %>% # end reactive export_table
        bindEvent(input$generate, 
                  input$incl_panel,
                  input$excl_empty, 
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE)
      
      ## Export table ####
      output$export <- downloadHandler(
        filename = function() {
          
          paste0(ifelse(!is.null(input$import),
                        input$import$name %>% 
                          str_remove(paste0("\\.", 
                                            tools::file_ext(input$import$name))),
                        "target"), 
                 "_panels.tsv")
        },
        content = function(file) {
          write_tsv(export_table(), 
                    file)
        }
      ) # end downloadHandler
      
    } # end module function
  ) # end moduleServer
} # end module server function
