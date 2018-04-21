# ###################################################### #
# Import & enrich JCR graphml-files exported with Python #
# ###################################################### #

library(igraph)
library(futile.logger)
library(here)

load_graphs <- function(full_path_dir) {
  full_filenames <- list.files(full_path_dir, full.names = FALSE)
  years = sapply(strsplit(sapply(strsplit(full_filenames, '_'), '[[', 2), '\\.'), '[[', 1)
  full_filenames <- list.files(full_path_dir, full.names = TRUE)
  full_filenames <- setNames(full_filenames, years)
  
  graphs <- list()
  for (yr in names(full_filenames)) {
    flog.info(paste('Processing', yr))
    g <- read.graph(full_filenames[[yr]], format='graphml')
    V(g)$name <- V(g)$label # set names
    V(g)$data_year <- yr
    V(g)$jcr_year <- as.character(as.integer(yr) + 1)
    E(g)$weight <- E(g)$relatedness
    graphs[[yr]] <- g
  }
  
  graphs
  
}

wd <- getwd()

setwd(Sys.getenv('DATADIR_HIVA_LOCAL'))
G.soc <- load_graphs('JCR/matrices/full/social/')
G.sci <- load_graphs('JCR/matrices/full/science/')
setwd(wd)

saveRDS(G.soc, file=here('data/processed/jcr_full_graphs', 'jcr_socsci_2003-2015.rda'))
saveRDS(G.sci, file=here('data/processed/jcr_full_graphs', 'jcr_sci_2003-2015.rda'))

rm(load_graphs, wd, G.soc, G.sci)
