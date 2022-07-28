# Taken from https://juliasilge.com/blog/cocktail-recipes-umap/

library(tidyverse)
library(tidymodels)
library(tidytext)

visualize_pca_features <- function(tidied_pca_prepped, n_components = 2){
  tidied_pca_prepped %>%
    filter(component %in% paste0("PC", 1:n_components)) %>%
    mutate(component = fct_inorder(component)) %>%
    ggplot(aes(value, terms, fill = terms)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~component, nrow = 1) +
    labs(y = NULL)
}

