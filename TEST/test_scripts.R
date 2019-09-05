
library(tidyverse)
library(xml2)
library(pheatmap)
#library(sloop) # for S3 OOP


data <- read_xml("D:/R_projects/TargetLynx_XML_exporter/Data/MH16-001-PCBs_batch3.xml")

data <- read_xml("D:/R_projects/TargetLynx_XML_exporter/Data/TQS_FC_180615-fish_and_mammal_Greenland.xml")

#### Extract the amounts (analconc) for all sample files ####

length_samples <- xml_length(xml_child(xml_child(xml_child(data, 3), 1), 2))
table_comps <- NULL
table_sample <- NULL
table_amounts <- NULL

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
  select_if(is.numeric)
rownames(mat) <- table_amounts$sample_name

sample_id <- table_amounts %>% select(sample_type)
rownames(sample_id) <- table_amounts$sample_name

pheatmap(mat, cluster_rows = FALSE, cluster_cols = FALSE, annotation_row = sample_id)















