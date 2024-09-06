# [R script for RIME analysis (Antibody Validation)]
# written by -- Soyoung Jeon
#
# [Input]: a CSV file received from Spectrus
# [Process]
#
# a) use column I (#Unique) to filter unwanted proteins (default cutoff value: 2)
# b) parse protein name and gene name using column L
#
# [Output] a spreadsheet with enriched protein list
#
# [Usage] Rscript rime_abq.R -f db.proteins.csv -b KMT2D -q 48605 -p 02AD -i Rockefeller
#
# [parameters]
# fname: the input file path
# cutoff: # unique peptides for filtering
# protein name: the target protein
# quote: project QUOTE number
# project_id : project ID from file maker
# inst: project institution

# Load required packages
library(readr)
library(stringr)
library(rvest)
library(openxlsx)
library(jsonlite)
library(argparse)
library(dplyr)

#----- main -----
###################
# parse arguments #
###################
parser <- ArgumentParser(description = "Process input file and select values based on cutoff")

parser$add_argument("-f", "--fname", default = "db.proteins.csv",
                    help = "input file name [default %(default)s]")
parser$add_argument("-c", "--cutoff", type = "double", default = 2,
                    help = "cutoff value (values >= cutoff will be selected) [default %(default)s]")
parser$add_argument("-b", "--bait", required = TRUE,
                    help = "protein bait name")
parser$add_argument("-q", "--quote", required = TRUE,
                    help = "quote number")
parser$add_argument("-p", "--project", required = TRUE,
                    help = "project ID")
parser$add_argument("-i", "--inst", required = TRUE,
                    help = "institution short name")

args <- parser$parse_args()

fname <- args$fname
cutoff <- args$cutoff
pname <- toupper(args$bait)
quote <- args$quote
project_id <- args$project
inst <- args$inst


####################
# setup output dir #
####################

output_dir <- file.path(dirname(fname), paste0("P-", project_id, " ", inst, " ", pname, " RIME Antibody Validation Data ", quote))

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

####################
# start processing #
####################

# Read input data
data <- read_csv(fname, col_names = TRUE)

# Filter data based on #Unique cutoff
filtered_data <- data %>%
  filter(`#Unique`>= cutoff)

# Parse gene name and protein description
filtered_data <- filtered_data %>%
  mutate(
    `Gene Name` = str_extract(Description, "(?<=GN=)\\w+(-\\w+)?"),
    `Protein Name` = str_remove(Description, " OS=.*")
  )

# Handle the exception for IgG
igg_mask <- str_detect(filtered_data$Description, "RecName: Full=Immunoglobulin G-binding protein G; Short=IgG-binding protein G;")
filtered_data$`Gene Name`[igg_mask] <- "IgG-binding protein G"
filtered_data$`Protein Name`[igg_mask] <- "Immunoglobulin G-binding protein G"

# Combine data with gene name and protein description
output_data <- filtered_data %>%
  select(Accession, `Gene Name`, `Protein Name`, `Coverage(%)`, matches("^#"))

# Sort data
output_data <- output_data %>%
  arrange(`Gene Name`, desc(`#Unique`))

# Remove duplicate gene names, keeping the one with max #Unique
output_data_uniq <- output_data %>%
  distinct(`Gene Name`, .keep_all = TRUE)

#####################
# write spreadsheet #
#####################

# 1) workbook with duplicated genes (for references)
xlsx_name <- "Sample Report Dup.xlsx"
wb <- createWorkbook()
addWorksheet(wb, pname)
writeData(wb, pname, output_data)
saveWorkbook(wb, xlsx_name, overwrite = TRUE)

# 2) final output workbook
xlsx_name = paste0("Sample Report for ", pname, " RIME Antibody Validation ", quote, ".xlsx")

wb = createWorkbook()  

addWorksheet(wb, sheetName = paste0("proteins", project_id))
writeData(wb, paste0("proteins", project_id), data)

addWorksheet(wb, sheetName = pname)
writeData(wb, sheet = pname, x = output_data_uniq, startRow = 1, startCol = 1, colNames = TRUE)

headerStyle <- createStyle(fontColour = "#FFFFFF", fontSize = 11, textDecoration = "bold", fgFill = "#7030A0", halign = "center", border = "TopBottomLeftRight")
addStyle(wb, sheet = pname, headerStyle, rows = 1, cols = 1:8, gridExpand = TRUE)

# highlight the target protein in the final enriched list if found
highlight_id = which(output_data_uniq$`Gene Name`== pname)
if(length(highlight_id)==0){
  message(pname," is NOT in the enriched list")
}else{
  highlightStyle <- createStyle(fgFill = "#F4B084")
  addStyle(wb, sheet = pname, highlightStyle, rows = highlight_id + 1, cols = 1:8, gridExpand = TRUE)
  message(pname," is FOUND in the enriched list")
}

# add definition of terms
writeData(wb, sheet = pname, x = "Definition of Terms", startRow = 2, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 2)
termTitleStyle <- createStyle(fontSize = 14, textDecoration = "bold", fontColour = "#333333", fgFill = "#F4B084", halign = "center")
addStyle(wb, sheet = pname, termTitleStyle, rows = 2, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "", startRow = 3, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 3)
addStyle(wb, sheet = pname, termTitleStyle, rows = 3, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "Coverage %", startRow = 4, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 4)
termStyle <- createStyle(fontSize = 12, textDecoration = "bold", fontColour = "#333333", fgFill = "#F4B084", halign = "left")
addStyle(wb, sheet = pname, termStyle, rows = 4, cols = 10:13, gridExpand = TRUE)

cov_desc = "% protein coverage is calculated by dividing the total number of peptide residues detected in a protein by the total length of the protein sequence. (For example, 227 aa detected/595 aa of total Estrogen Receptor = 38% coverage)"
writeData(wb, sheet = pname, x = cov_desc, startRow = 5, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 5:12)
descStyle <- createStyle(fontSize = 12, wrapText = TRUE, fontColour = "#333333", fgFill = "#F4B084", halign = "left")
addStyle(wb, sheet = pname, descStyle, rows = 5:12, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "", startRow = 13, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 13)
addStyle(wb, sheet = pname, descStyle, rows = 13, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "#Peptide", startRow = 14, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 14)
addStyle(wb, sheet = pname, termStyle, rows = 14, cols = 10:13, gridExpand = TRUE)

pep_desc = "The total number of peptides detected and assigned to an identified protein. If a protein has already been identified (at least 1 unique peptide), common (non-unique) peptides are distributed proportionally amongst the identified proteins that share those common peptides."
writeData(wb, sheet = pname, x = pep_desc, startRow = 15, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 15:23)
addStyle(wb, sheet = pname, descStyle, rows = 15:23, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "", startRow = 24, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 24)
addStyle(wb, sheet = pname, descStyle, rows = 24, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "#Unique", startRow = 25, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 25)
addStyle(wb, sheet = pname, termStyle, rows = 25, cols = 10:13, gridExpand = TRUE)

uniq_desc = "The number of peptides unique to the identified protein. To increase confidence in the proteins identified, only proteins with at least 2 unique peptides are shown in this column."
writeData(wb, sheet = pname, x = uniq_desc, startRow = 26, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 26:31)
addStyle(wb, sheet = pname, descStyle, rows = 26:31, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "", startRow = 32, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 32)
addStyle(wb, sheet = pname, descStyle, rows = 32, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "#Spec", startRow = 33, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 33)
addStyle(wb, sheet = pname, termStyle, rows = 33, cols = 10:13, gridExpand = TRUE)

spec_desc = "Total spectrum count. The total number of all peptides detected. The same peptide can be detected multiple times."
writeData(wb, sheet = pname, x = spec_desc, startRow = 34, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 34:38)
addStyle(wb, sheet = pname, descStyle, rows = 34:38, cols = 10:13, gridExpand = TRUE)

writeData(wb, sheet = pname, x = "", startRow = 39, startCol = 10)
mergeCells(wb, sheet = pname, cols = 10:13, rows = 39)
addStyle(wb, sheet = pname, descStyle, rows = 39, cols = 10:13, gridExpand = TRUE)

# writeData(wb, sheet = pname, x = "DB Hits", startRow = 40, startCol = 10)
# mergeCells(wb, sheet = pname, cols = 10:13, rows = 40)
# addStyle(wb, sheet = pname, termStyle, rows = 40, cols = 10:13, gridExpand = TRUE)

# db_desc = "All proteins on the samples list were searched through a number of public protein databases. If the name of a database is listed by the protein, then a publication has found this protein as an interactor with the target protein. For the full list and URLs of the databases, please go to the \"RIME Data File Descriptions\" file"
# writeData(wb, sheet = pname, x = db_desc, startRow = 41, startCol = 10)
# mergeCells(wb, sheet = pname, cols = 10:13, rows = 41:50)
# addStyle(wb, sheet = pname, descStyle, rows = 41:50, cols = 10:13, gridExpand = TRUE)

saveWorkbook(wb, file = paste(output_dir,"//", xlsx_name, sep = ""), overwrite = TRUE)







