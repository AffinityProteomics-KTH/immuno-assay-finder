
refsUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    
    # switch for if dark mode should be active or not
    input_dark_mode(),
    
    fluidRow(tags$h3("References and resources")),
    
    # Methods
    fluidRow(tags$h4("Methods")),
    
    fluidRow(tags$ul(style = "padding-left: 45px;",
                     tags$li(tags$a(HTML(paste("Ella™ ProteinSimple (bio-techne", 
                                               tags$sup("®"), ")", sep = "")), 
                                    target = "_blank",
                                    href = "https://www.bio-techne.com/instruments/simple-plex")),
                     
                     # "https://www.mesoscale.com/en/products_and_services/assay_kits"
                     
                     tags$li(tags$a("NULISA™ (Alamar Biosciences)",
                                    target = "_blank",
                                    href = "https://alamarbio.com/technology/nulisa-platform/")),
                     
                     tags$li(tags$a(HTML(paste("Olink", tags$sup("®"), sep = "")),
                                    target = "_blank",
                                    href = "https://olink.com/")),
                     
                     tags$li(tags$a(HTML(paste("R&D Systems Luminex", tags$sup("®"), 
                                               " assays (bio-techne", tags$sup("®"), ")", 
                                               sep = "")),
                                    target = "_blank",
                                    href = "https://www.rndsystems.com/products/luminex-assays-and-high-performance-assays")),
                     
                     tags$li(tags$a(HTML(paste("Quanterix Simoa", tags$sup("®"), sep = "")),
                                    target = "_blank",
                                    href = "https://www.quanterix.com/simoa-technology/")))),
    
    ### NOTE ON REFERENCES:
    # --------------------- 
    ## "APA Style 7th edition"-formatting used below, as exported from Mendeley Reference Manager
    ## Exception: R-package references do not follow this format, for those the formatting is
    ## based on the output of report::cite_packages() with defaults. Format may vary slightly 
    ## based on how package authors have filled in the information
    # --------------------- 
    
    # UniProt ####  
    fluidRow(tags$h4("UniProtKB")),
    
    fluidRow(tags$ul(style = "padding-left: 45px;",
                     tags$li("The UniProt Consortium. (2025). UniProt: the Universal Protein Knowledgebase in 2025.",
                             tags$a("Nucleic Acids Research, 53(D1), D609–D617.",
                                    target = "_blank",
                                    href = "https://doi.org/10.1093/nar/gkae1010")),
                     
                     tags$li("Ahmad, S., Jose da Costa Gonzales, L., Bowler-Barnett, E. H., Rice, D. L., Kim, M., Wijerathne, S., Luciani, A., Kandasaamy, S., Luo, J., Watkins, X., Turner, E., Martin, M. J., & the UniProt Consortium. (2025). The UniProt website API: facilitating programmatic access to protein knowledge.",
                             tags$a("Nucleic Acids Research, 53(W1), W547–W553.",
                                    target = "_blank",
                                    href = "https://doi.org/10.1093/nar/gkaf394")) #,
                     
                     # tags$li("Soudy, M., Anwar, A. M., Ahmed, E. A., Osama, A., Ezzeldin, S., Mahgoub, S., & Magdeldin, S. (2020). UniprotR: Retrieving and visualizing protein sequence and functional information from Universal Protein Resource (UniProt knowledgebase).",
                     #         tags$a("Journal of Proteomics, 213, 103613.",
                     #                target = "_blank",
                     #                href = "https://doi.org/10.1016/j.jprot.2019.103613"))) # Much faster to download from UniProts webpage in the end
    )),
    
    
    # ExoCarta ####  
    fluidRow(tags$h4("ExoCarta")),
    
    fluidRow(tags$ul(style = "padding-left: 45px;",
                     tags$li("Keerthikumar, S., Chisanga, D., Ariyaratne, D., al Saffar, H., Anand, S., Zhao, K., Samuel, M., Pathan, M., Jois, M., Chilamkurti, N., Gangoda, L., & Mathivanan, S. (2016). ExoCarta: A Web-Based Compendium of Exosomal Cargo.",
                             tags$a("Journal of Molecular Biology, 428(4), 688–692.",
                                    target = "_blank",
                                    href = "https://doi.org/10.1016/j.jmb.2015.09.019")),
                     
                     
                     tags$li("Mathivanan, S., Fahner, C. J., Reid, G. E., & Simpson, R. J. (2012). ExoCarta 2012: database of exosomal proteins, RNA and lipids.",
                             tags$a("Nucleic Acids Research, 40(D1), D1241–D1244.",
                                    target = "_blank",
                                    href = "https://doi.org/10.1093/nar/gkr828")),
                     
                     tags$li("Mathivanan, S., & Simpson, R. J. (2009). ExoCarta: A compendium of exosomal proteins and RNA.",
                             tags$a("PROTEOMICS, 9(21), 4997–5000.",
                                    target = "_blank",
                                    href = " https://doi.org/10.1002/pmic.200900351")))
    ),
    
    # R-packages ####
    fluidRow(tags$h4("R-packages")),
    
    fluidRow(tags$ul(style = "padding-left: 45px;",
                     report::cite_packages() %>% # extract citation information for used packages, the rest is to add different HTML-formatting to different parts.
                       str_split("\\\n  - ") %>%
                       unlist() %>%
                       str_remove("^  - ") %>%
                       str_remove_all("\\*") %>% 
                       lapply(function(y) {
                         tags$li(y %>% 
                                   str_split("<") %>%
                                   unlist() %>%
                                   str_split("(?=, (R package|viridis))") %>% 
                                   unlist() %>% 
                                   str_remove_all("[_>]") %>%
                                   str_remove("\\.$") %>%
                                   str_replace(" - ", ": ") %>% 
                                   lapply(function(x) {
                                     if(str_detect(x, "^https")){
                                       tags$a(x,
                                              target = "_blank",
                                              href = x,
                                              .noWS = c('after'))
                                     } else {
                                       x %>% 
                                         str_remove("doi:.*$") %>% 
                                         str_split("(?<=\\d{4}\\)\\. )") %>%
                                         unlist() %>%
                                         str_split('(?<=(:\\s|“|”))') %>%
                                         unlist() %>%
                                         lapply(function(z) {
                                           if(str_detect(z, '[:]')){
                                             HTML(paste0(tag("b", list(str_remove(z, ": "))), 
                                                  ":"))
                                           } else if(str_detect(z, '[”]')){
                                             HTML(paste0(tag("b", list(str_remove(z, "”"))), 
                                                         "”"), .noWS = "before")
                                           } else {
                                             z
                                           }
                                         })
                                     }
                                   })
                         )
                       })
    )),
    
    # Script ####  
    fluidRow(tags$h4("App")),
    
    fluidRow(tags$p("Author: Ceke Hellström",
                    tags$br(),
                    "Version: 1.1.0",
                    tags$br(),
                    "Owner: ",
                    tags$a("Affinity Proteomics, SciLifeLab",
                           target = "_blank",
                           href = "https://www.scilifelab.se/units/affinity-proteomics/")))
    
  ) # end tagList
} # end UI module function

# Module server function
refsServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      
    } # end module function
  ) # end moduleServer
} # end module server function
