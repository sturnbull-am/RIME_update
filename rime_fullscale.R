# [R script for RIME analysis (Full scale)]
# written by -- Soyoung Jeon
#
# [Input]: a CSV file and html folder received from Spectrus
# ** needs to be two duplicates + two IgG for each target condition
#
# [Process] 
# 1. filter anything with #Unique < cutoff1
# 2. filter anything with #spec (for each sample) < cutoff2
# 3. extract gene names and protein descriptions 
# 4. remove duplicated gene names 
# 5. generate venn diagram 
# 6. generate enriched gene list
# 7. perform functional enrichment (stringdb) and generate barplot
# 8. perform interaction network analysis (stringdb) and generate network image
#
#
# [Output]
# 1. Sample_Report.xlsx: proteins data after cutoff1 applied (one with unique and another with duplicated genes)
# 2. Enriched_protein.xlsx
# 3. Analysis_Functional_Enrichment.xlsx
# 4. Analysis_Interaction_Network.xlsx
# 5. Graphics/ folder with 1) barplot_functional_enrichment.png 2) interaction_network.png 3)venn_protein_enrichment.png
#
# [Usage] Rscript rime_fullscale.R -b KMT2D

# [parameters]
# fname: the input file path 
# cutoff1: #unique peptides for filtering
# cutoff2: #spectral counts for filtering
# bait: the target protein name [Required]


# load libraries
library(VennDiagram)
library(openxlsx)
library(httr)
library(rvest)
library(readr)
library(ggplot2)
library(dplyr)
library(rbioapi)
library(argparse)

#----- subroutines -----
############ parse arguments ############
parser <- ArgumentParser(description = "Process input file and select values based on cutoff")

parser$add_argument("-f", "--fname", default = "db.proteins.csv",
                    help = "input file name [default %(default)s]")
parser$add_argument("-c1", "--cutoff1", type = "double", default = 2,
                    help = "Unique peptide cutoff value [default %(default)s]")
parser$add_argument("-c2", "--cutoff2", type = "double", default = 6,
                    help = "Spectral counts cutoff value [default %(default)s]")
parser$add_argument("-b", "--bait", required = TRUE,
                    help = "protein bait name")
parser$add_argument("-n", "--nsam", type = "double", default = 4,
                    help = "Number of samples/reactions (including IgG controls [default %(default)s]")
parser$add_argument("-o", "--organism", type = "character", default = "human",
                    help = "organism")

args <- parser$parse_args()
fname <- args$fname
cutoff1 <- args$cutoff1
cutoff2 <- args$cutoff2
pname <- toupper(args$bait)
nsam <- as.numeric(args$nsam)
org <- as.character(args$organism)

genome_id <- 0
if (org == "human" || org == "hs" ||  org == "hg38" || org == "hg19") {
  genome_id <- 9606
} else if (org == "mouse" || org = "mm" ||  org == "mm10" || org == "mm9") {
  genome_id <- 10090
} else if (org == "rat" || org == "rn" ||  org == "rn6" || org == "rn5") {
  genome_id <- 10116
} else if (org == "fly" || org == "dm6") {
  genome_id <- 7227
} else {
  warning("organism unsupported or entered incorrectly. 
           Defaulting to human taxonomy.")
  genome_id <- 9606
}



############ setup output dir ############
mydir <- dirname(fname)
devdir <- paste(mydir, "/deliverables", sep="")
if(!dir.exists(devdir)){
  dir.create(devdir)
}

outdir <- paste(devdir, "/Graphics", sep="")
if(!dir.exists(outdir)){
  dir.create(outdir)
}


############ xlsxGen #############
# generate supporting spreadsheets for filtered_dat_full and filtered_dat_uniq
xlsxGen <- function(xlsx_name, outdat, nsam, pname, savedir){
  sheet <- addWorksheet(wb, sheetName = pname)
  
  # set Header1
  writeData(wb, sheet, "Coverage for Each Sample", startRow = 1, startCol = 4, colNames = FALSE)
  writeData(wb, sheet, "Spectral Count for Each Sample", startRow = 1, startCol = 4 + nsam, colNames = FALSE)
  writeData(wb, sheet, "Summary for All Samples Combined", startRow = 1, startCol = 4 + nsam*2 , colNames = FALSE)
  
  mergeCells(wb, sheet, cols = 4:(4 + nsam - 1), rows = 1)
  mergeCells(wb, sheet, cols = (4 + nsam):(4 + nsam*2 - 1), rows = 1)
  mergeCells(wb, sheet, cols = (4 + nsam*2):(4 + nsam*2 + 1), rows = 1)
  header1style <- createStyle(fontSize = 11, textDecoration = "bold", fontColour = "#333333", fgFill = "#F4B084", halign = "center", wrapText = TRUE)
  addStyle(wb, sheet, header1style, rows = 1, cols = 4:(4 + nsam*2 + 1), gridExpand = TRUE)
  
  # set Header2
  writeData(wb, sheet, outdat, startRow = 2, startCol = 1, colNames = TRUE, rowNames = FALSE)
  header2Style <- createStyle(fontColour = "#FFFFFF", fontSize = 11, textDecoration = "bold", fgFill = "#7030A0", halign = "center", border = "TopBottomLeftRight", wrapText = TRUE)
  addStyle(wb, sheet, header2Style, rows = 2, cols = 1:ncol(outdat), gridExpand = TRUE)
  
  # highlight the target protein if found
  outdat_colN <- ncol(outdat)
  highlight_id <- which(outdat$`Gene Name`== pname)
  if(length(highlight_id) > 0){
    for(i in 1:length(highlight_id)){
      addStyle(wb, sheet, style = createStyle(fgFill = "lightpink"), rows = highlight_id[i] + 2, cols = 1:outdat_colN, gridExpand = TRUE)
    }
  }
  
  ## Borders
  nrows <- nrow(outdat) + 2
  for (col in c(4, 4 + nsam, 4 + nsam*2)) {
    addStyle(wb, sheet, style = createStyle(border = "left", borderColour = "black", borderStyle = "thick"), rows = 1:nrows, cols = col, stack = TRUE)
  }
  
  for (col in c(4 + nsam - 1, 4 + nsam*2 - 1, 4 + nsam*2 + 1)) {
    addStyle(wb, sheet, style = createStyle(border = "right", borderColour = "black", borderStyle = "thick"), rows = 1:nrows, cols = col, stack = TRUE)
  }
  
  ## Definition of terms
  writeData(wb, sheet = pname, x = "Definition of Terms", startRow = 2, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 2)
  termTitleStyle <- createStyle(fontSize = 14, textDecoration = "bold", fontColour = "#333333", fgFill = "#F4B084", halign = "center")
  addStyle(wb, sheet = pname, termTitleStyle, rows = 2, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "", startRow = 3, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 3)
  addStyle(wb, sheet = pname, termTitleStyle, rows = 3, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "Coverage %", startRow = 4, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 4)
  termStyle <- createStyle(fontSize = 12, textDecoration = "bold", fontColour = "#333333", fgFill = "#F4B084", halign = "left")
  addStyle(wb, sheet = pname, termStyle, rows = 4, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  cov_desc = "% protein coverage is calculated by dividing the total number of peptide residues detected in a protein by the total length of the protein sequence. (For example, 227 aa detected/595 aa of total Estrogen Receptor = 38% coverage)"
  writeData(wb, sheet = pname, x = cov_desc, startRow = 5, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 5:12)
  descStyle <- createStyle(fontSize = 12, wrapText = TRUE, fontColour = "#333333", fgFill = "#F4B084", halign = "left")
  addStyle(wb, sheet = pname, descStyle, rows = 5:12, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "", startRow = 13, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 13)
  addStyle(wb, sheet = pname, descStyle, rows = 13, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "#Peptide", startRow = 14, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 14)
  addStyle(wb, sheet = pname, termStyle, rows = 14, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  pep_desc = "The total number of peptides detected and assigned to an identified protein. If a protein has already been identified (at least 1 unique peptide), common (non-unique) peptides are distributed proportionally amongst the identified proteins that share those common peptides."
  writeData(wb, sheet = pname, x = pep_desc, startRow = 15, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 15:23)
  addStyle(wb, sheet = pname, descStyle, rows = 15:23, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "", startRow = 24, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 24)
  addStyle(wb, sheet = pname, descStyle, rows = 24, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "#Unique", startRow = 25, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 25)
  addStyle(wb, sheet = pname, termStyle, rows = 25, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  uniq_desc = "The number of peptides unique to the identified protein. To increase confidence in the proteins identified, only proteins with at least 2 unique peptides are shown in this column."
  writeData(wb, sheet = pname, x = uniq_desc, startRow = 26, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 26:31)
  addStyle(wb, sheet = pname, descStyle, rows = 26:31, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "", startRow = 32, startCol = 10)
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 32)
  addStyle(wb, sheet = pname, descStyle, rows = 32, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "#Spec", startRow = 33, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 33)
  addStyle(wb, sheet = pname, termStyle, rows = 33, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  spec_desc = "Total spectrum count. The total number of all peptides detected. The same peptide can be detected multiple times."
  writeData(wb, sheet = pname, x = spec_desc, startRow = 34, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 34:38)
  addStyle(wb, sheet = pname, descStyle, rows = 34:38, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  writeData(wb, sheet = pname, x = "", startRow = 39, startCol = (ncol(outdat)+2))
  mergeCells(wb, sheet = pname, cols = (ncol(outdat)+2):(ncol(outdat)+5), rows = 39)
  addStyle(wb, sheet = pname, descStyle, rows = 39, cols = (ncol(outdat)+2):(ncol(outdat)+5), gridExpand = TRUE)
  
  saveWorkbook(wb, file = paste(savedir, "//", xlsx_name, sep = ""), overwrite = TRUE)
}

#----- main -----
##################### start processing #####################
# data filtering based on #unique peptides
print(paste("processing samples in ", fname, sep=""))
dat <- read.csv(fname, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
filtered_dat_raw <- dat[which(dat$`#Unique`>= cutoff1), ]
# obtain gene name
startpos <- regexpr("GN=\\w+ |GN=\\w+-\\w+ ", filtered_dat_raw$Description)
len <- attr(startpos, "match.length")
gname <- substr(filtered_dat_raw$Description, startpos + 3, startpos + len - 2)

# obtain protein desc (i.e. protein name)
endpos <- regexpr(" OS=",filtered_dat_raw$Description)
pdesc <- substr(filtered_dat_raw$Description, 1, endpos - 1)

# handle an exception (igG) where description does not have GN and OS info
igG_id <- grep("RecName: Full=Immunoglobulin G-binding protein G; Short=IgG-binding protein G;", filtered_dat_raw$Description)
gname[igG_id] <- "IgG-binding protein G"
pdesc[igG_id] <- "Immunoglobulin G-binding protein G"

# combine gname and pdesc with actual data
nCol_dat <- ncol(filtered_dat_raw)
coverage_set <- filtered_dat_raw[, 6:(6 + nsam - 1)]
spec_set <- filtered_dat_raw[, (nCol_dat - 3 - nsam + 1):(nCol_dat - 3)]
colnames(spec_set) <- gsub("#Spec ", "", colnames(spec_set))
combined_stats <- cbind(filtered_dat_raw$`Coverage (%)`, filtered_dat_raw$`#Peptides`, filtered_dat_raw$`#Unique`)
filtered_dat_full <- as.data.frame(cbind(filtered_dat_raw$Accession, gname, pdesc, coverage_set, spec_set, combined_stats))
colnames(filtered_dat_full) <- c("Accession", "Gene Name", "Protein Name",
                                 colnames(coverage_set), colnames(spec_set),
                                 "#Peptides", "#Unique")

# sort first by gname alphabetically and then unique in decreasing order
filtered_dat_full <- filtered_dat_full[order(filtered_dat_full$`Gene Name`, -filtered_dat_full$`#Unique`, decreasing = FALSE), ]

# remove gene duplicates - keep the one with max unique
filtered_dat_uniq <- filtered_dat_full[!duplicated(filtered_dat_full$`Gene Name`), ]
# rename coverage set columns
colnames(filtered_dat_uniq) <- gsub("Coverage\\(\\%\\) ", "", colnames(filtered_dat_uniq))

# remove the entry with empty gene names
filtered_dat_uniq_rmEmp <- filtered_dat_uniq[which(filtered_dat_uniq$`Gene Name` != ""), ]


##################### enriched protein lists and sample report generation#####################
# apply 2nd layer filtering based on filtered_dat_uniq_rmEmp
# filtered_protein is a list containing gene names passed cutoff2 for each sample (column # = nsam)
nCol_out <- ncol(filtered_dat_uniq_rmEmp)
spec_set <- filtered_dat_uniq_rmEmp[, (nCol_out - 2 - nsam + 1):(nCol_out - 2)]
filtered_protein_list <- apply(spec_set, 2, function(x) as.character(filtered_dat_uniq_rmEmp$`Gene Name`[which(x >= cutoff2)]))

# convert list to a matrix with equal column length (filled with NAs)
filtered_protein_mat <- sapply(filtered_protein_list,
                               function(x, m) c(x, rep(NA, m - length(x))),
                               max(lengths(filtered_protein_list)))

# for each target:
# 1. create VENN diagram using the filtered protein lists
# 2. create a sheet containing enriched protein lists in three subsets of venn
xlsx_name <- "Enriched_protein.xlsx"

wb <- createWorkbook()

fig_name <- paste0(outdir, "//venn_protein_enrichment.png")
iDs <- c(nsam - 1, nsam, 1 , 2)
venn_dat_list <- filtered_protein_list[iDs] # order: igG R1, igG R2, sample R1, sample R2
names(venn_dat_list) <- sub("#Spec ", "", names(venn_dat_list))

# generate Venns
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger") #suppress log
venn.diagram(venn_dat_list, category.names = names(venn_dat_list), na = "remove", filename = fig_name,
             imagetype = "png", 
             col = "transparent",
             fill = c("cornflowerblue", "green", "yellow", "darkorchid1"), alpha = 0.50,
             label.col = c("orange", "white", "darkorchid4", "white",
                           "white", "white", "white", "white", "darkblue", "white",
                           "white", "white", "white", "darkgreen", "white"), cex = 1.5,
             fontfamily = "sans",
             fontface = "bold",
             cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"),
             cat.cex = 1,
             cat.pos = 0,
             cat.dist = c(0.2,0.2,0.12,0.12),
             cat.fontfamily = "sans")

# obtain the elements in each region of venn
partitions <- get.venn.partitions(venn_dat_list)

# extract regions in order: unique targetX_r1, intersection r1 and r2, unique targetX_r2
enriched_list = list(partitions["..values.."][[1]][[12]], 
                     partitions["..values.."][[1]][[4]], 
                     partitions["..values.."][[1]][[8]])

# convert to a matrix form by appending NAs to make the same column length (for spreadsheet generation)
enriched_mat <- sapply(enriched_list,
                       function(x, m) c(x, rep(NA, m - length(x))), max(lengths(enriched_list)))

unique_nA = partitions["..count.."][[1]][[12]]
common_nAB = partitions["..count.."][[1]][[4]]
unique_nB = partitions["..count.."][[1]][[8]]


# prepare output df
enriched_df <- as.data.frame(matrix(data = NA, max(unique_nA, common_nAB, unique_nB), 9), stringsAsFactors = FALSE)
enriched_df[, c(1, 4, 7)] <- enriched_mat # gene names

# obtain info (desc, spec) using enriched gene names
enriched_idlist <- lapply(enriched_list,
                          function(g) lapply(paste0("^", g, "$"), grep, filtered_dat_uniq_rmEmp$`Gene Name`, perl = TRUE))

enriched_desclist <- lapply(enriched_idlist, function(x) filtered_dat_uniq_rmEmp$`Protein Name`[as.numeric(x)])
enriched_desclist <- lapply(seq_along(enriched_idlist), function(i) {
  gene_names <- filtered_dat_uniq_rmEmp$`Gene Name`[as.numeric(enriched_idlist[[i]])]
  
  additional_protein_names <- lapply(gene_names, function(gene) {
    rows <- filtered_dat_full %>%
      filter(`Gene Name` == gene) %>%
      pull(`Protein Name`)
    
    if (length(unique(rows)) > 1) {  # Check for unique values
      paste(unique(rows), collapse = "|") 
    } else {
      rows[1]
    }
  })
  
  enriched_desclist[[i]] <- additional_protein_names
})

enriched_speclist <- lapply(enriched_idlist, function(x) spec_set[as.numeric(x), 1:2])

# r1 info
if(unique_nA != 0){
  enriched_df[1:unique_nA, 2:3] <- data.frame(as.character(enriched_desclist[[1]]),
                                              enriched_speclist[[1]][, 1],
                                              stringsAsFactors = FALSE)
}

# common info
if(common_nAB != 0){
  enriched_df[1:common_nAB, 5:6] <- data.frame(as.character(enriched_desclist[[2]]),
                                               apply(enriched_speclist[[2]], 1, mean),
                                               stringsAsFactors = FALSE)
}

# r2 info
if(unique_nB != 0){
  enriched_df[1:unique_nB, 8:9] <- data.frame(as.character(enriched_desclist[[3]]),
                                              enriched_speclist[[3]][, 2],
                                              stringsAsFactors = FALSE)
}

# sort by spec
for(j in 1:3){ # 3 subsets, each has 3 columns
  enriched_df[, (j * 3 - 2):(j * 3)] <- enriched_df[order(enriched_df[, 3 * (j - 1) + 3], decreasing = TRUE), (j * 3 - 2):(j * 3)]
}
# generate final enriched spreadsheet
sheet_name <- sub(" _R1", "", names(venn_dat_list)[3])
addWorksheet(wb, sheetName = sheet_name)

# header text
name1 <- paste(unique_nA, " elements included exclusively in ", names(venn_dat_list)[3], sep="") 
name2 <- paste(common_nAB, " common elements in ", names(venn_dat_list)[3], " and ", names(venn_dat_list)[4], sep="")
name3 <- paste(unique_nB, " elements included exclusively in ", names(venn_dat_list)[4], sep="")
name4 <- as.data.frame(t(c("Gene", "Protein", "#Spec")))
name5 <- as.data.frame(t(c("Gene", "Protein", "Avg. #Spec")))

writeData(wb, sheet_name, name1, startRow = 2, startCol = 2, colNames = FALSE)
writeData(wb, sheet_name, name2, startRow = 2, startCol = 5, colNames = FALSE)
writeData(wb, sheet_name, name3, startRow = 2, startCol = 8, colNames = FALSE)

mergeCells(wb, sheet_name, cols = 2:4, rows = 2)
mergeCells(wb, sheet_name, cols = 5:7, rows = 2)
mergeCells(wb, sheet_name, cols = 8:10, rows = 2)
header1style <- createStyle(fontSize = 11, textDecoration = "bold", fontColour = "#333333", fgFill = "#F4B084", halign = "center")
addStyle(wb, sheet_name, header1style, rows = 2, cols = 2:10, gridExpand = TRUE)

# set Header2
writeData(wb, sheet_name, name4, startRow = 3, startCol = 2, colNames = FALSE, rowNames = FALSE)
writeData(wb, sheet_name, name5, startRow = 3, startCol = 5, colNames = FALSE, rowNames = FALSE)
writeData(wb, sheet_name, name4, startRow = 3, startCol = 8, colNames = FALSE, rowNames = FALSE)
header2Style <- createStyle(fontColour = "#FFFFFF", fontSize = 11, textDecoration = "bold", fgFill = "#7030A0", halign = "center", border = "TopBottomLeftRight")
addStyle(wb, sheet_name, header2Style, rows = 3, cols = 2:10, gridExpand = TRUE)
  
# add main data
writeData(wb, sheet_name, enriched_df, startCol = 2, startRow = 4, colNames = FALSE, rowNames = FALSE)

# set border lines
borderstyle <- createStyle(border = "right",borderStyle = "thick", borderColour = "black")
addStyle(wb, sheet_name, borderstyle, rows = 2:(common_nAB + 2), cols = 4, stack = TRUE)
addStyle(wb, sheet_name, borderstyle, rows = 2:(common_nAB + 2), cols = 7, stack = TRUE)

# highlight target protein
highlight_id = which(enriched_df[, 4] == pname)
if(length(highlight_id)==0){
  message(sheet_name, ": ", pname," is NOT in the enriched list")
}else{
  message(sheet_name, ": ", pname," is FOUND in the enriched list")
  for(i in 1:length(highlight_id)){
    addStyle(wb, sheet_name, style = createStyle(fgFill = "lightpink"), rows = highlight_id[i] + 3, cols = 5:7, gridExpand = TRUE)
  }
}


saveWorkbook(wb, file = paste(devdir, "//", xlsx_name, sep = ""), overwrite = TRUE)


# write Sample Report
# 1) spreadsheet with filtered data (duplicated genes)
xlsx_name <- paste(pname, "Sample_Report_wdup.xlsx")
wb <- createWorkbook()
addWorksheet(wb, sheetName = "proteins_raw")
writeData(wb, "proteins_raw", dat)
xlsxGen(xlsx_name, filtered_dat_full, nsam, pname, mydir)

# 2) spreadsheet with filtered data (unique genes)
xlsx_name = "Sample_Report.xlsx"
wb <- createWorkbook()
addWorksheet(wb, sheetName = "proteins_raw")
writeData(wb, "proteins_raw", dat)
xlsxGen(xlsx_name, filtered_dat_uniq_rmEmp, nsam, pname, devdir)





#####################STRING analysis #####################
genes <- enriched_list[[2]]

## Functional Enrichment analysis
string_enrichment <- rba_string_enrichment(ids=genes,
                                           split_df = FALSE,
                                           species = genome_id)

wb <- createWorkbook()
if(nrow(string_enrichment) != 0)
{
  string_enrichment <- string_enrichment[order(string_enrichment$`fdr`, decreasing = FALSE), ] %>%
    select(-c("ncbiTaxonId", "preferredNames"))
  
  string_enrichment$inputGenes <- sapply(string_enrichment$inputGenes, function(x) paste(x, collapse = " "))

  unique_category <- unique(string_enrichment$category)
  for (c in unique_category) {
    category_data <- string_enrichment %>% filter(category == !!c)
    addWorksheet(wb, sheetName = c)
    writeData(wb, sheet = c, category_data)
  }
} else
{
  string_enrichment <- data.frame("category" = character(0),
                                  "term" = character(0),
                                  "number_of_genes" = numeric(0),
                                  "number_of_genes_in_background" = numeric(0),
                                  "ncbiTaxonId" = numeric(0),
                                  "inputGenes" = list(),
                                  "preferredNames" = list(),
                                  "p_value" = numeric(0),
                                  "fdr" = numeric(0),
                                  "description" = character(0)
                                  )
  addWorksheet(wb, sheetName = "NA")
  writeData(wb, sheet = "NA", string_enrichment)
}
saveWorkbook(wb, paste0(devdir, "//Analysis_Functional_Enrichment.xlsx"), overwrite = TRUE)

top_10_rows <- string_enrichment %>% arrange(fdr) %>% head(10)
## Top 10 Enrichment barplot
if(nrow(top_10_rows) != 0)
{
  # Select the top 10 rows based on the fdr column
  # Create a new column combining description and term
  top_10_rows$label <- paste(top_10_rows$description, "(", top_10_rows$term, ")", sep = " ")


  p <- ggplot(top_10_rows, aes(x = reorder(label, -fdr), y = number_of_genes, fill = -fdr)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("FDR: %.2e", fdr)), hjust = 1.1, size = 4 ) +
    labs(x = "", y = "Number of Genes") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    scale_fill_gradient(low = "#A6CEE3", high = "salmon") +
    coord_flip()

  ggsave(file.path(outdir, "barplot_functional_enrichment.png"), p, width = 13, height = 8, dpi = 300)
}


## Interaction Network
int_net <- rba_string_interactions_network(ids = genes,
                                           required_score = 500,
                                           species = genome_id)

wb <- createWorkbook()
if(nrow(int_net) != 0)
{
  int_net <- int_net[order(int_net$`score`, decreasing = TRUE), ] %>%
    select(-c("stringId_A","stringId_B","ncbiTaxonId")) %>%
    dplyr::rename("InteractorA" = "preferredName_A",
         "InteractorB" = "preferredName_B")

  addWorksheet(wb, sheetName = "STRING_interactions_network")
  writeData(wb, "STRING_interactions_network", int_net)

  highlight_id = which(int_net[, 1] == pname |int_net[, 2] == pname)
  for(i in 1:length(highlight_id)){
    addStyle(wb, "STRING_interactions_network", style = createStyle(fgFill = "lightpink"), rows = highlight_id[i] + 1, cols = 1:ncol(int_net), gridExpand = TRUE)
  }
} else
{
  int_net <- data.frame("stringId_A" = character(0),
                        "stringId_B" = character(0),
                        "preferredName_A" = character(0),
                        "preferredName_B" = character(0),
                        "ncbiTaxonId" = character(0),
                        "score" = numeric(0),
                        "nscore" = numeric(0),
                        "fscore" = numeric(0),
                        "pscore" = numeric(0),
                        "ascore" = numeric(0),
                        "escore" = numeric(0),
                        "dscore" = numeric(0),
                        "tscore" = numeric(0)
                        )
  addWorksheet(wb, sheetName = "STRING_interactions_network")
  writeData(wb, "STRING_interactions_network", int_net)
}

saveWorkbook(wb, paste0(devdir, "//Analysis_Interaction_Network.xlsx"), overwrite = TRUE)



# Network image
if(length(genes) != 0)
{
  graph1 <- rba_string_network_image(ids = genes, 
                                   image_format = "image", 
                                   save_image = file.path(normalizePath(outdir), "interaction_network.png"), 
                                   required_score = 500, 
                                   network_flavor = "confidence",
                                   network_type = "functional",
                                   species = genome_id)
}
# "confidence": line's thickness is an indicator of the interaction's confidence score.
# "functional": edge's indicate both physical and functional associations.

