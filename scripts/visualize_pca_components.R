# Taken from https://juliasilge.com/blog/cocktail-recipes-umap/

library(tidyverse)
library(tidymodels)
library(tidytext)

visualize_pca_components <- function(tidied_pca_prepped, n_components = 2, max_features = 8){
  tidied_pca_prepped %>%
    filter(component %in% paste0("PC", 1:n_components)) %>%
    mutate(component = fct_inorder(component),
           terms = reorder_within(terms, abs(value), component)) %>%
    group_by(component) %>%
    top_n(max_features, abs(value)) %>%
    ungroup() %>%
    ggplot(aes(abs(value), terms, fill = value > 0)) +
    geom_col() +
    facet_wrap(~component, scales = "free_y") +
    labs(
      x = "Absolute value of contribution",
      y = NULL, 
      fill = "Positive?"
    )
}

