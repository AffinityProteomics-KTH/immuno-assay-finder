
#-------------     Script info   -----------------
# Developed by Affinity Proteomics Unit, Stockholm
# Initiated, and mainly written, by Ceke Hellström

#-------------     Load packages    -----------------
{
  library(tidyverse) # for tidy handling
  library(shiny) # for basic shiny functions
  library(bslib) # for advanced bootstrapping shiny-functions
  library(viridis) # for colors
  library(readxl) # for reading excel read_xlsx()
  library(bsicons) # for icons
  library(data.table) # for fread
  library(DT) # for interactive table
  library(janitor) # for cleaner column names
  library(shinyjs) # for extra shiny functions using JavaScript
  library(rclipboard) # for copy to clipboard button
  library(patchwork) # for wrap_elements
  library(uniprotREST) # for retrieving from UniProtKB  (not in app, but in separate script, incl here for refs)
  library(shinyvalidate) # for InputValidator
  
  # Packages used with ::
  # bsicons
  # tools
  # data.table
  # dplyr
  # rlang
  # patchwork
}

# #-------------    Load data (uniprot)    -----------------
## Find path to latest extraction APP VERSION
uniprot_path <- list.files(pattern = "uniprot_ext", recursive = TRUE) %>%
  rev() %>%
  .[1]

## Read file
uniprot_database_org <- read_csv(uniprot_path)

# #-------------    Load data (exocarta)    -----------------
### Find paths to ExoCarta files
exocarta_path <- list.files(pattern = "ExoCarta", recursive = TRUE)

## Read files
exocarta_database_org <- read_delim(exocarta_path) %>% 
  clean_names()

## Curate ExoCarta-tables
exocarta_database <- exocarta_database_org %>%
  filter(content_type == "protein") %>% 
  select(gene_symbol, species) %>% 
  unique() %>% 
  mutate(exocarta = "yes") %>% 
  rename(gene_names_primary = gene_symbol)

## Add ExoCarta to Uniprot-table
uniprot_database <- uniprot_database_org %>% 
  mutate(species = str_extract(organism, "^.+?(?=\\s\\()")) %>% 
  left_join(exocarta_database,
            by = join_by(species, gene_names_primary)) %>% 
  select(-species) %>% 
  mutate(exocarta = ifelse(is.na(exocarta), "no", exocarta))

#-------------    Load data (methods)    -----------------
# # Retrieve path of latest input masterfile APP VERSION
df_path <- list.files(pattern = "Biomarkers", recursive = TRUE) %>%
  subset(str_detect(., "~", negate = TRUE)) %>%
  rev() %>%
  .[1]

# Read the main mapping file, excel-format to remove one copy-paste step in preparing the input
# NOTE: Update the sheet-indices in the lapply-function below if more techniques are added.
df_org <- lapply(1:5, function(i) 
  read_xlsx(df_path, 
            sheet = i,
            range = cell_cols("A:H"), # See columns below, update if more standard columns are added.
            col_names = TRUE,
            col_type = c("text", # uniprot_id
                         "text", # gene
                         "text", # technique
                         "text", # panel
                         "text", # quantification
                         "text", # species
                         "text", # uniprot_source
                         "date") # list_date, end col_type
  )) %>% # end read_xlsx, lapply
  do.call(bind_rows, .) %>% 
  filter(!is.na(technique)) %>% 
  mutate(list_date = format(list_date,"%Y-%m-%d"))

# Expected columns in df (first 8 in each sheet):
# - A: uniprot_id      (can be "P01375" or "P29459; P29460")
# - B: gene            (can be "TNF" or "IL12A; IL12B")
# - C: technique       (e.g. "Olink T96", "Ella SimplePlex", "Quanterix SR-X", etc. plus panel name for Olink and NULISA)
# - D: panel           (e.g. "Cytokine", "Inflammation", "Flexible" etc. as provided by the companies)
# - E: quantification  (can be "Absolute" or "Relative")
# - F: species         (e.g. "Human", "Mouse", "Rat" etc.)
# - G: uniprot_source  (how the uniprot/gene matching has been done, can be one of Company, Manual, or AI)
# - H: list_date       (date of the received list targets are based on, format YYYY-MM-DD)

#-------------    Restructure input method data    -----------------
# Expand rows so each UniProt–Gene pair gets its own row:
#  - split uniprot_id and gene into lists
#  - pair them by POSITION (1st UniProt with 1st gene, 2nd with 2nd, etc.)
#  - drop the original combined columns
#  - unnest to get one row per (uniprot_id, gene, technique)
#  
#  NOTE: if this produce warning of introduced NA or mismatching lengths, 
#  check that all with multiple IDs/genes have matching number of the other (see above).
df_long <- df_org %>%
  mutate(
    # Split on "; " (semicolon + optional spaces)
    uniprot_list = str_split(uniprot_id, ";\\s*"),
    gene_list    = str_split(gene,      ";\\s*")
  ) %>%
  # build (uniprot_id, gene) pairs position-wise
  mutate(
    pairs = map2(uniprot_list, gene_list, ~{
      # Safety check: if lengths differ, we keep only the shortest pair,
      # and issue a warning so this can be investigated.
      if (length(.x) != length(.y)) {
        warning("Length mismatch in row: uniprot_id vs gene. Truncating to shortest.")
      }
      n <- min(length(.x), length(.y))
      tibble(
        uniprot_id = .x[seq_len(n)],
        gene       = .y[seq_len(n)]
      )
    })
  ) %>%
  # We no longer need the original combined columns or the lists
  select(-uniprot_id, -gene, -uniprot_list, -gene_list) %>%
  # Expand the list-column "pairs" into regular rows
  unnest(pairs)

# Add variable to allow for heatmap plotting
df_mod <- df_long %>% 
  mutate(exist = 1) %>% 
  mutate(technique_panel = paste0(technique, ": ",
                                  panel) %>% 
           factor(),
         target = paste0(gene, ", ", uniprot_id, ", ", species))

#-------------    Extract subsets/choices Methods-tab  -----------------
{
  init_choices <- list(
    # Extract possible species to choose from
    species_choices = df_mod %>% 
      pull(species) %>% 
      unique() %>% 
      sort(),
    
    # Extract possible panels to choose from
    panels_choices = df_mod %>% 
      pull(panel) %>% 
      unique() %>% 
      sort(),
    
    # Extract possible techniques to choose from
    techs_choices = df_mod %>% 
      pull(technique) %>% 
      unique() %>% 
      sort(),
    
    # Extract possible curation levels to choose from
    cura_choices = df_mod %>% 
      pull(uniprot_source) %>% 
      unique() %>% 
      sort(),
    
    # Extract possible quantification levels to choose from
    quant_choices = df_mod %>% 
      pull(quantification) %>% 
      unique() %>% 
      sort()
  )
}

#-------------    Extract subsets/choices Uniprot-tab  -----------------

{
  uniprot_choices <- list(
    ## Names ####
    ### Alternative gene names/synonyms/entry/protein names ####
    altnames = c(
      # alt/synonym names:
      uniprot_database %>% 
        pull(gene_names) %>% # select only relevant variable
        str_split("\\s") %>% # split strings on space
        unlist() %>%  # transform into vector
        str_trim(), # remove start/trailing spaces
      # entry names:
      uniprot_database %>% 
        pull(entry_name) %>% # select only relevant variable
        str_extract("^.*(?=_)"), # extract string preceeding "_"
      # protein names:
      uniprot_database %>% 
        pull(protein_names) %>% # select only relevant variable
        unlist() %>%# transform into vector
        str_remove_all(" \\(EC (\\d|\\.|-)*\\)") %>% # remove Enzyme Commission numbers (used separately below)
        str_split("[;:] ") %>% # split strings on delimiter within "Cleaved into" or "Includes", and treat them the same as rest below 
        unlist() %>% # transform into vector
        str_remove("(Cleaved into|Includes)") %>%  # remove "Cleaved into"/"Includes" tags from strings
        str_remove_all("(\\]$|\\[$)") %>%  # remove left over brackets at the end of strings
        str_trim() %>% # remove start/trailing spaces
        str_replace("(?=( \\(|$))", ")") %>% # add ending parentheses after first protein name (to match pattern of the rest of the strings)
        paste0("(", .) %>% # add starting parentheses to all strings (to match pattern of the rest of the strings)
        regmatches(., gregexpr("(\\(([^()]|(?1))*\\))", ., perl=TRUE)) %>% # extract things within matching parentheses
        unlist() %>% # transform into vector
        str_remove_all("(^\\(|\\)$)")) %>% # remove start and end parentheses) %>% 
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ### Enzymes
    enzymes = uniprot_database %>% 
      pull(protein_names) %>% # select only relevant variable
      str_extract_all("(?<=\\()EC (\\d|\\.|-)*(?=\\))") %>% # extract EC-number from strings
      unlist() %>% # sort in alpha-numerical order
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ## GO-terms ####
    ### All combined ####
    # (at least 1 not included in the others) 
    go_terms = uniprot_database %>% 
      pull(gene_ontology_go) %>% # select only relevant variable
      str_remove_all("\\[GO:\\d{7}\\]") %>% # remove GO id from strings
      str_split(";") %>% # split strings on ";"
      unlist() %>% # transform into vector
      str_trim() %>% # remove start/trailing spaces
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ### Biological processes  ####
    # (at least 1 not in general go_terms)
    go_terms_bio = uniprot_database %>% 
      pull(gene_ontology_biological_process) %>% # select only relevant variable
      str_remove_all("\\[GO:\\d{7}\\]") %>% # remove GO id from strings
      str_split(";") %>% # split strings on ";"
      unlist() %>% # transform into vector
      str_trim() %>% # remove start/trailing spaces
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ### Molecular function ####
    go_terms_mol = uniprot_database %>% 
      pull(gene_ontology_molecular_function) %>% # select only relevant variable
      str_remove_all("\\[GO:\\d{7}\\]") %>% # remove GO id from strings
      str_split(";") %>% # split strings on ";"
      unlist() %>% # transform into vector
      str_trim() %>% # remove start/trailing spaces
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ### Cellular component ####
    go_terms_cell = uniprot_database %>% 
      pull(gene_ontology_cellular_component) %>% # select only relevant variable
      str_remove_all("\\[GO:\\d{7}\\]") %>% # remove GO id from strings
      str_split(";") %>% # split strings on ";"
      unlist() %>% # sort in alpha-numerical order
      str_trim() %>% # remove start/trailing spaces
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ### GO-IDs ####
    go_ids = uniprot_database %>% 
      pull(gene_ontology_ids) %>% # select only relevant variable
      str_split(";") %>% # split strings on ";"
      unlist() %>% # transform into vector
      str_trim() %>% # remove start/trailing spaces
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ## Associations ####
    ### Disease associations ####
    diseases = uniprot_database %>% 
      pull(involvement_in_disease) %>% # select only relevant variable
      str_split("DISEASE: ") %>% # split on DISEASE tag
      unlist() %>% # transform into vector
      str_extract("^.+?(?= \\[MIM)") %>% # remove MIM-codes
      str_remove("\\(.+?\\)$") %>% # remove disease abbreviation
      str_remove("\\[.+?\\]: ") %>% # remove isoform specific tag
      subset(!(is.na(.) | . == "")) %>% # remove empty split results
      c(., # combine all extracted with main disease tag below
        str_remove(., "with .+") %>% # first remove anything with "with ...."
          str_extract("^.+?(?=( \\d|, | $))")) %>% # then extract the first part of the string
      str_trim() %>% # remove start/trailing spaces
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ### Pathways ####
    pathways = uniprot_database %>%
      pull(pathway) %>% # select only relevant variable
      subset(!is.na(.)) %>% # remove empty entries
      str_split("PATHWAY: ") %>% # Split on PATHWAY, a fleg preciding each major pathway entry
      unlist() %>% # transform into vector
      str_remove_all(" \\{ECO.+?\\}\\.") %>% # Remove any ECO & PubMed-IDs
      str_split(";") %>% # split strings on ";"
      unlist() %>%# transform into vector
      str_remove(": step .*") %>% # remove e.g. "step 1/2"
      str_remove("\\[.+?\\]:") %>% # remove e.g. [Isoform 1]
      subset(!(. %in% c("", " "))) %>% # remove left over empty elements
      str_trim() %>% # trim trailing white spaces
      unique() %>% # only keep unique entries
      ifelse(str_detect(., "^[a-z][A-Z]"), .,
             gsub("\\b([a-z])", "\\U\\1", ., perl=TRUE)) %>% # capitalize first letter, unless it has the structure lower case-upper case (e.g. dTMP), and keep the rest as is
      sort(), # sort in alpha-numerical order
    
    ### Interactions ####
    interacts = uniprot_database %>% 
      pull(interacts_with) %>% # select only relevant variable
      str_split(";") %>% # split strings on ";"
      unlist() %>% # transform into vector
      str_trim() %>% # remove start/trailing spaces
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ### Species ####
    species = uniprot_database %>% 
      pull(organism) %>% # select only relevant variable
      unique() %>% # only keep unique strings
      sort(), # sort in alpha-numerical order
    
    ## Location ####
    ### Subcellar Location ####
    subcellar = uniprot_database %>% 
      pull(subcellular_location_cc) %>% # select only relevant variable
      str_extract_all("(?<=[:.,;]\\s)(\\w|-|\\s)*(?=( \\{))") %>% # extract strings containing letters/-/spaces preceeded by one of ":.,;" and followed by "{"
      unlist() %>% # transform into vector
      unique() %>% # only keep unique strings
      sort() %>% # sort in alpha-numerical order
      str_to_sentence()
    
  ) # end search-list
}

#-------------    Set user input (UI)  -----------------

ui <- page_fluid(

  useShinyjs(), # set up shinyjs, used for reset button for example
  rclipboardSetup(), # set up copy-to-clipboard (js-based)
  
  titlePanel(title = span(img(src = "SciLifeLab_symbol_green.png", 
                              height = 35, 
                              alt = "SciLifeLab logo"), 
                          "SciLifeLab - ImmunoAssayFinder"),
             tags$head(tags$link(rel = "icon", 
                                 type = "image/png", 
                                 href = "favicon.png"),
                       tags$title("ImmunoAssayFinder"))),
  
  navset_underline(
    
    ### Methods-tab content ####
    nav_panel("Methods",
              icon = icon("th",
                          lib = "glyphicon"),
              
              methodsUI("methods", choices = init_choices)
              
    ),
    
    ### Uniprot-tab content ####
    nav_panel("UniProt IDs",
              icon = icon("tag",
                          lib = "glyphicon"),
              
              uniprotUI("uniprot", 
                        up_path = uniprot_path,
                        ec_path = exocarta_path)
              
    ), # end uniprot nav_panel
    
    ### Dates-tab content ####
    nav_panel("List dates",
              icon = icon("list-alt",
                          lib = "glyphicon"),
              
              datesUI("dates")
              
    ), # end List dates nav_panel
    
    ### References-tab content ####
    nav_panel("References",
              icon = icon("info-sign",
                          lib = "glyphicon"),
              
              refsUI("refs")
              
    ) # end References nav_panel
    
  ), # end navset_underline
) # end page_fluid

#-------------    Use data (Server)  -----------------

server <- function(input, output, session) {
  
  ### Methods-tab content ####
  methodsServer("methods", 
                df = df_mod, 
                choices = init_choices)
  
  ### Uniprot-tab content ####
  uniprotServer("uniprot", 
                uniprot_db = uniprot_database, 
                search_list = uniprot_choices)
  
  ### Dates-tab content ####
  datesServer("dates", df = df_mod)
  
  ### References-tab content ####
  refsServer("refs")
} # end server

#-------------    Generate app    -----------------

shinyApp(ui, server)
