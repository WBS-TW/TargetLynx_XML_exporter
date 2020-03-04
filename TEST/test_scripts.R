
library(tidyverse)
library(xml2)
library(pheatmap)
library(DT)
library(RColorBrewer)
library(profvis)
library(microbenchmark)
#library(sloop) # for S3 OOP


data <- read_xml("D:/R_projects/TargetLynx_XML_exporter/Data/MH16-001-PCBs_batch3.xml")

data <- read_xml("D:/R_projects/TargetLynx_XML_exporter/Data/TQS_FC_180615-fish_and_mammal_Greenland.xml")

#### Extract the amounts (analconc) for all sample files ####

length_samples <- xml_length(xml_child(xml_child(xml_child(data, 3), 1), 2))
table_comps <- NULL
table_sample <- NULL
table_amounts <- NULL

profvis({ 
sample_amounts <- for (i in 1:length_samples) {
  
  table_comps <- NULL
  table_sample <- NULL
  
  sample <- xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), i) #last child is individual sample
  sample_name <- xml_attrs(sample)[["name"]]
  sample_type <- xml_attrs(sample)[["type"]]
  length_compounds <- length(xml_find_all(sample, ".//COMPOUND")) # find and get length of all compound nodes (usually -1 of sample)
  
  for (j in 1:length_compounds) {
    compound <- xml_child(sample, j) #last child is individual compounds #last child is individual compounds
    analconc <- round(as.double(xml_attrs(xml_child(compound, 1))[["analconc"]]), 2)
    compound_name <- xml_attrs(compound)[["name"]]
    table_comp <- tibble(compound = compound_name, analconc = analconc)
    table_comps <- rbind(table_comps, table_comp)
  }
  sample_comps = data.frame(t(table_comps[,-1]))
  colnames(sample_comps) <- table_comps$compound
  sample_comps <- sample_comps %>%
    mutate(sample_name = sample_name, sample_type = sample_type) %>% select(sample_name, sample_type, everything())
  table_amounts <- rbind(table_amounts, sample_comps)
  
  rm(list = c("table_comp", "table_comps", "sample_comps"))
  
}
})


#####create xpath link to the SAMPLE node
profvis({
xpath_sample_name <- paste0("/QUANDATASET/GROUPDATA/GROUP/SAMPLELISTDATA/SAMPLE")
xpath_cmpds_name <- paste0("/QUANDATASET/GROUPDATA/GROUP/SAMPLELISTDATA/SAMPLE[", 1, "]/COMPOUND")
xpath_peak <- paste0("/QUANDATASET/GROUPDATA/GROUP/SAMPLELISTDATA/SAMPLE[", 1, "]/COMPOUND[", 1, "]/PEAK")

length_cmpds <- length(xml_find_all(data, xpath_cmpds_name))
length_samples <- length(xml_find_all(data, xpath_sample_name))
df <- as_tibble(matrix(NA, ncol = sum(length_cmpds,2), nrow = length_samples))
compounds_name <- as.character(rep(NA, length_cmpds))

## for loop


for (i in 1:length_samples) {
  
  sample_name <- unname(xml_attrs(xml_find_all(data, xpath_sample_name))[[i]]["name"])
  sample_type <- unname(xml_attrs(xml_find_all(data, xpath_sample_name))[[i]]["type"])
  colnames(df)[1] <- "sample_name"
  colnames(df)[2] <- "sample_type"
  
  df[i, 1] <- sample_name
  df[i, 2] <- sample_type
  
  
  for (j in 1:length_cmpds) {
    
    xpath_peak <- paste0("/QUANDATASET/GROUPDATA/GROUP/SAMPLELISTDATA/SAMPLE[", i, "]/COMPOUND[", j, "]/PEAK")
    compounds_name[j] <- xml_attr(xml_find_first(data, paste0("/QUANDATASET/GROUPDATA/GROUP/SAMPLELISTDATA/SAMPLE[1]/COMPOUND[", j, "]")), "name")
    colnames(df)[sum(j,2)] <- compounds_name[j]
    analconc <- xml_attr(xml_find_first(data, xpath_peak), "analconc")
    df[i, sum(j,2)] <- analconc
    
  }
}
})

## END fast vectorization 1


## Fast vectorization 2

length_samples <- xml_length(xml_child(xml_child(xml_child(data, 3), 1), 2))
length_cmpds <- xml_length(xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), 1))-1L

names_samples <- as.character(rep(NA, length_samples))
names_cmpds <- as.character(rep(NA, length_cmpds))
type_sample <- as.character(rep(NA, length_samples))


for (i in 1:length_samples) {
  names_samples[i] <- xml_attrs(xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), i))[["name"]]
  type_sample[i] <- xml_attrs(xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), i))[["type"]]
}


for (j in 1:length_cmpds) {
  names_cmpds[j] <- xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), 1), j))[["name"]]
}


table_amounts <- matrix(nrow = length_samples, ncol = length_cmpds, dimnames = list(names_samples, names_cmpds))


for (k in 1:length_samples) {
 for (m in 1:length_cmpds) {
   table_amounts[k,m] <-as.double(xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), k), m), 1))[["analconc"]])
 }
}

# find_analconc <- function(x, y){
#   table_amounts <- as_tibble(matrix(nrow = length_samples, ncol = length_cmpds, dimnames = list(names_samples, names_cmpds)))
#   analconc <- xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), x), y), 1))[["analconc"]]
#   table_amounts <- as.data.frame(analconc)
#   return(table_amounts)
# }


table_amounts <- as_tibble(table_amounts) %>%
  mutate(sample_type = type_sample) %>%
  mutate(sample_name = names_samples) %>%
  select(sample_name, sample_type, everything())



#### Extract the recoveries () for all sample files ####


length_samples <- xml_length(xml_child(xml_child(xml_child(data, 3), 1), 2))
table_comps <- NULL
table_sample <- NULL
table_recovery <- NULL

sample_recovery <- for (i in 1:length_samples) {
  
  table_comps <- NULL
  table_sample <- NULL
  
  sample <- xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), i) #last child is individual sample
  sample_name <- xml_attrs(sample)[["name"]]
  sample_type <- xml_attrs(sample)[["type"]]
  length_compounds <- length(xml_find_all(sample, ".//COMPOUND")) # find and get length of all compound nodes (usually -1 of sample)
  
  for (j in 1:length_compounds) {
    compound <- xml_child(sample, j) #last child is individual compounds #last child is individual compounds
    percrecovery <- round(as.double(xml_attrs(xml_child(compound, 1))[["percrecovery"]]),0)
    compound_name <- xml_attrs(compound)[["name"]]
    table_comp <- tibble(compound = compound_name, percrecovery = percrecovery)
    table_comps <- rbind(table_comps, table_comp)
  }
  sample_comps = data.frame(t(table_comps[,-1]))
  colnames(sample_comps) <- table_comps$compound
  sample_comps <- sample_comps %>%
    mutate(sample_name = sample_name, sample_type = sample_type) %>% select(sample_name, sample_type, everything())
  table_recovery <- rbind(table_recovery, sample_comps)
  
  rm(list = c("table_comp", "table_comps", "sample_comps"))
  
}


## END of recoveries

rec_color <- sapply(table_recovery, is.numeric)

DT::datatable(table_recovery,
              extensions = 'FixedColumns',
              options = list(
                pageLength = 20,
                dom = 't',
                scrollX = TRUE,
                fixedColumns = list(leftColumns = 3))
              ) %>%
                formatStyle(names(table_recovery[rec_color]),
                            color = styleInterval(c(0, 25, 50, 75, 125, 150), 
                                                  c("red", "red", "blue", "black", "black", "blue", "red"))
                )



# Plotting



test2 <- test %>%
  #keep(is.numeric) %>% 
  gather(key = "key", value = "value", -sample_name, -sample_type) %>%
  mutate(value = replace_na(value, 0)) %>%
  ggplot(aes(sample_type)) +
  facet_wrap(~ key, scales = "free") +
  geom_point(aes(y = value)) +
  theme_bw()

test2






test2 <- table_recovery %>%
  #keep(is.numeric) %>% 
  gather(key = "key", value = "value", -c(sample_name, sample_type)) %>%
  mutate(value = replace_na(value, 0)) %>%
  ggplot(aes(sample_type)) +
  facet_wrap(~ key, scales = "free") +
  geom_point(aes(y = value)) +
  theme_bw()
test2


# plot heatmap using pheatmap


mat <- table_amounts %>%
  select_if(is.numeric) %>%
  select_if(function(x){!all(is.na(x))})

rownames(mat) <- table_amounts$sample_name

sample_id <- table_amounts %>% select(sample_type)
rownames(sample_id) <- table_amounts$sample_name

min(mat, na.rm = TRUE)
max(mat, na.rm = TRUE)

breaksList <- seq(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE), length.out = 10)

pheatmap(mat, cluster_rows = FALSE, cluster_cols = FALSE, annotation_row = sample_id,
         scale = "none",
         color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)),
         breaks = breaksList)


# plot recovery using pheatmap


mat <- table_recovery %>%
  select_if(is.numeric) %>%
  select_if(function(x){!all(is.na(x))})

rownames(mat) <- table_recovery$sample_name

sample_id <- table_recovery %>% select(sample_type)
rownames(sample_id) <- table_recovery$sample_name

#breaksList <- seq(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE), by = 10)
breaksRecovery <- seq(0, 200, length.out = 10)

pheatmap(mat, cluster_rows = FALSE, cluster_cols = FALSE, annotation_row = sample_id,
         scale = "none",
         color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksRecovery)),
         breaks = breaksRecovery)




### Remove IS and RS




## RInno

install.packages(c("shiny", "jsonlite", "magrittr", "httr", "tidyverse", "shinydashboard", "xml2", "DT", "pheatmap", "writexl", "RColorBrewer"),
                 lib = "D:/R_projects/Packages", dependencies = TRUE)

create_app(
  app_name    = "XML_export", # this just gives your app a name to use on various parts of your app  
  app_dir     = "D:/R_projects/RInno_test/app", # location of my app
  # pkgs        = c("shiny"),  # Don't use this argument
  include_R   = TRUE,     # Download R and install it with your app
  include_Rtools = TRUE,
  privilege   = "lowest",   # Does not require Admin installation
  default_dir = "userdesktop",  # Install to desktop to avoid issues with servers
  #R_flags = '/SILENT'   # Install R 
)


## END RInno

