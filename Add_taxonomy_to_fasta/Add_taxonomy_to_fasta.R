library(Biostrings)
library(rentrez)
library(xml2)

# Read the FASTA file
fasta_file <- "example_input.fasta"
sequences <- readAAStringSet(fasta_file)

# Function to extract species name from sequence ID
extract_species <- function(id) {
  strsplit(id, "\\|")[[1]][1]
}

# Function to fetch multiple taxonomic levels from NCBI using rentrez and xml2
	# line 31 ('tax_names[2:5]') dictates what taxonomic levels are retrieved, adjust numbers to get different results
fetch_taxonomy <- function(species) {
  species <- gsub("_", " ", species)  # Replace underscores with spaces
  cat("Modified species name:", species, "\n")  # Debugging statement
  taxon_search <- entrez_search(db = "taxonomy", term = paste0(species, "[ORGN]"))
  if (length(taxon_search$ids) > 0) {
    tax_id <- taxon_search$ids[1]
    attempt <- 1
    max_attempts <- 3
    while (attempt <= max_attempts) {
      try({
        tax_rec <- entrez_fetch(db = "taxonomy", id = tax_id, rettype = "xml")
        tax_xml <- read_xml(tax_rec)
        tax_lineage <- xml_find_all(tax_xml, "//LineageEx/Taxon/ScientificName")
        tax_names <- xml_text(tax_lineage)
        if (length(tax_names) >= 5) {
          taxonomy <- paste(tax_names[2:5], collapse = ";")
          taxonomy <- gsub(" ", "_", taxonomy)  # Replace spaces with underscores
          return(taxonomy)
        } else {
          return(NA)
        }
      }, silent = TRUE)
      attempt <- attempt + 1
      Sys.sleep(2)  # Wait for 2 seconds before retrying
    }
    return(NA)
  } else {
    return(NA)
  }
}

# Process each sequence
for (i in seq_along(sequences)) {
  species <- extract_species(names(sequences)[i])
  cat("Processing species:", species, "\n")  # Debugging statement
  taxonomy <- fetch_taxonomy(species)
  cat("Fetched taxonomy:", taxonomy, "\n")  # Debugging statement
  if (!is.na(taxonomy) && length(taxonomy) > 0) {
    names(sequences)[i] <- paste0(gsub(" ", "_", names(sequences)[i]), "|", taxonomy)
  }
}

# Write the modified sequences to a new FASTA file
writeXStringSet(sequences, "example_output.fasta")

# Information on the R session this code was written in
sessionInfo()
# R version 4.4.1 (2024-06-14)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sonoma 14.7.1
# 
# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: US/Pacific
# tzcode source: internal
# 
# attached base packages:
# [1] stats4    stats     graphics  grDevices utils     datasets  methods  
# [8] base     
# 
# other attached packages:
# [1] xml2_1.3.6          rentrez_1.2.3       Biostrings_2.72.1  
# [4] GenomeInfoDb_1.40.1 XVector_0.44.0      IRanges_2.38.1     
# [7] S4Vectors_0.42.1    BiocGenerics_0.50.0
# 
# loaded via a namespace (and not attached):
#  [1] XML_3.99-0.17           R6_2.5.1                zlibbioc_1.50.0        
#  [4] GenomeInfoDbData_1.2.12 UCSC.utils_1.0.0        cli_3.6.3              
#  [7] compiler_4.4.1          httr_1.4.7              tools_4.4.1            
# [10] curl_6.0.0              rlang_1.1.4             jsonlite_1.8.9         
# [13] crayon_1.5.3           

