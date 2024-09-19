

generate_and_save_heatmap <- function(dds_obj, gene_vector, set_name, group,output_name) {
  normalized_counts <- counts(dds_obj, normalized=TRUE)
  selected_genes_counts <- normalized_counts[rownames(normalized_counts) %in% gene_vector,]
  
  # Create the annotation data frame
  annotation_df <- as.data.frame(colData(dds_obj)[, group, drop=FALSE])
  
  # Order the columns by group
  group_order <- order(annotation_df[[group]])
  selected_genes_counts <- selected_genes_counts[, group_order]
  annotation_df <- annotation_df[group_order, , drop=FALSE]
  
  # Optionally, define colors for the annotation
  annotation_colors <- list(
    group = c("Group1" = "red", "Group2" = "blue", "Group3" = "green")  # Customize as needed
  )
  
  filename <- paste0("heatmap_", set_name,output_name, ".png")
  
  pheatmap(selected_genes_counts,
           scale = "row",
           clustering_distance_rows = "euclidean",
           clustering_distance_cols = "euclidean",
           clustering_method = "complete",
           show_rownames = TRUE,
           show_colnames = F,
           annotation_col = annotation_df,
           annotation_colors = annotation_colors,
           color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
           fontsize_row = 10,
           fontsize_col = 10,
           main = paste("Heatmap for", set_name),
           filename = filename,
           cluster_cols = FALSE)  # Do not cluster the columns
}


# # Example how to use with a named list of pathways
# group_column <- "Statin"
# 
# lapply(names(list_pathways), function(set_name) {
#   gene_vector <- list_pathways[[set_name]]
#   generate_and_save_heatmap(dds_male, gene_vector, set_name, "Statin","_Male")
# })

