

# Get amounts from analconc node----------------------
get_amounts <- function(data, decimal = 2, blanks = FALSE, standards = FALSE) {
  data <- data
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
      analconc <- round(as.double(xml_attrs(xml_child(compound, 1))[["analconc"]]), decimal)
      compound_name <- xml_attrs(compound)[["name"]]
      table_comp <- tibble(compound = compound_name, analconc = analconc)
      table_comps <- rbind(table_comps, table_comp)
    }
    sample_comps = data.frame(t(table_comps[,-1]))
    colnames(sample_comps) <- table_comps$compound
    sample_comps <- sample_comps %>%
      mutate(sample_name = sample_name, sample_type = sample_type) %>% select(sample_name, sample_type, everything())
    table_amounts <- rbind(table_amounts, sample_comps) %>%
      filter(sample_type != "")  # delete empty rows

    if(standards == TRUE) {
      table_amounts <- table_amounts %>%
        filter(sample_type != "Standard")
    }
    if(blanks == TRUE) {
      table_amounts <- table_amounts %>%
        filter(sample_type != "Blank")
    }

    rm(list = c("table_comp", "table_comps", "sample_comps"))
  }
  return(table_amounts)
}




#Get recoveries from percrecovery----------------------

get_recovery <- function(data, blanks = FALSE, standards = FALSE) {
  data <- data
  length_samples <- xml_length(xml_child(xml_child(xml_child(data, 3), 1), 2))
  table_comps <- NULL
  table_sample <- NULL
  table_recovery <- NULL
  
  sample_amounts <- for (i in 1:length_samples) {
    
    table_comps <- NULL
    table_sample <- NULL
    
    sample <- xml_child(xml_child(xml_child(xml_child(data, 3), 1), 2), i) #last child is individual sample
    sample_name <- xml_attrs(sample)[["name"]]
    sample_type <- xml_attrs(sample)[["type"]]
    length_compounds <- length(xml_find_all(sample, ".//COMPOUND")) # find and get length of all compound nodes (usually -1 of sample)
    
    for (j in 1:length_compounds) {
      compound <- xml_child(sample, j) #last child is individual compounds #last child is individual compounds
      percrecovery <- round(as.double(xml_attrs(xml_child(compound, 1))[["percrecovery"]]), 1)
      compound_name <- xml_attrs(compound)[["name"]]
      table_comp <- tibble(compound = compound_name, percrecovery = percrecovery)
      table_comps <- rbind(table_comps, table_comp)
    }
    sample_comps = data.frame(t(table_comps[,-1]))
    colnames(sample_comps) <- table_comps$compound
    sample_comps <- sample_comps %>%
      mutate(sample_name = sample_name, sample_type = sample_type) %>% select(sample_name, sample_type, everything())
    table_recovery <- rbind(table_recovery, sample_comps) %>%
      filter(sample_type != "")  # delete empty rows
    
    if(standards == TRUE) {
      table_recovery <- table_recovery %>%
        filter(sample_type != "Standard")
    }
    if(blanks == TRUE) {
      table_recovery <- table_recovery %>%
        filter(sample_type != "Blank")
    }
    
    rm(list = c("table_comp", "table_comps", "sample_comps"))
  }
  return(table_recovery)
}



#plot Amounts function----------------------------

plot_Amounts <- function(data, gather_select = -c(sample_name, sample_type), plot_x = sample_type) {
  
  gather_select <- enquo(gather_select)
  plot_x <- enquo(plot_x)
  
  plot_data <- data %>%
    #keep(is.numeric) %>% 
    gather(key = "key", value = "value", !!gather_select) %>%
    mutate(value = replace_na(value, 0)) %>%
    ggplot(aes(!!plot_x)) +
    facet_wrap(~ key, scales = "free") +
    geom_violin(aes(y = value)) +
    geom_point(aes(y = value)) +
    theme_bw()
  return(plot_data)
}

#plot heatmap function-----------------------------

# Heatmap for amounts
plot_heatmap <- function(data, scale, cluster_rows, cluster_cols) {
  
  mat <- data %>%
    select_if(is.numeric) %>%
    select_if(function(x){!all(is.na(x))})
  
  rownames(mat) <- data$sample_name
  
  annotation_cols <- data %>% select(sample_type)
  rownames(annotation_cols) <- data$sample_name
  
  pheatmap(mat, annotation_row = annotation_cols, scale = scale, 
           cluster_rows = cluster_rows, cluster_cols = cluster_cols,
           border_color = "black")
  
}

# Heatmap for recoveries
plot_recovery_heatmap <- function(data, scale, cluster_rows, cluster_cols) {
  
  mat <- data %>%
    select_if(is.numeric) %>%
    select_if(function(x){!all(is.na(x))})
  
  rownames(mat) <- data$sample_name
  
  annotation_cols <- data %>% select(sample_type)
  rownames(annotation_cols) <- data$sample_name
  
  breaksRecovery <- seq(0, 200, length.out = 10)
  
  pheatmap(mat, annotation_row = annotation_cols, scale = scale, 
           cluster_rows = cluster_rows, cluster_cols = cluster_cols,
           border_color = "black",
           color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksRecovery)),
           breaks = breaksRecovery)
  
}

