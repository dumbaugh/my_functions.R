create_gene_boxplots <- function(vsd, genes_of_interest, output_pdf = "gene_expression_boxplots.pdf") {
  # Open a PDF graphics device
  pdf(output_pdf, width = 8, height = 6)
  
  # Loop through each gene in the list
  for (gene in genes_of_interest) {
    # Extract data for the gene of interest
    gene_data <- assay(vsd)[gene, , drop = FALSE]
    gene_data <- as.data.frame(t(gene_data))
    colnames(gene_data) <- gene
    gene_data$condition <- colData(vsd)$Severity
    gene_data$Gender <- colData(vsd)$GENDER
    
    # Create the boxplot with statistical annotations
    p <- ggplot(gene_data, aes(x = condition, y = get(gene), color = Gender)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
      stat_compare_means(method = "wilcox.test", aes(label = ..p.signif..),size=6) +  # Add statistical comparison
      theme_minimal() +
      labs(title = paste("Expression of", gene),
           x = "Condition",
           y = "Expression (VST)")
    
    # Print the plot to the PDF device
    print(p)
  }
  
  # Close the PDF graphics device
  dev.off()
}
