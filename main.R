########################################
########################################
########################################
## = Autores
##
## - Luis Manuel Román García
## - Miguel Alonso Vilchis
##
## -------------------------------------
## = Descripción
##
## Código para construcción de red
## basada en distancias carreteras.
##
########################################
########################################
########################################

########################################
## Libraries
########################################
source("00-load.R")
source("01-utils.R")
source("02-cluster.R")

###############################################
##########         MAIN        ################
###############################################
set.seed(123454321)
data        <- read.csv("./data/data_censo.csv",
                       stringsAsFactors = FALSE)
## data[,1]    <- NULL
names(data) <- c("ent",
                "mun",
                "nom_mun",
                "nom_loc",
                "lon",
                "lat",
                "pob")

## Work with Chiapas
## 7 <- chis
chis_mun    <- dplyr::filter(data, ent == 7 & mun %in% c(107,108)  )
Encoding(chis_mun$nom_loc) <- "UTF-8"
chis_mun$nom_loc <- iconv(chis_mun$nom_loc, "UTF-8", "UTF-8",sub='')
chis_points <- dplyr::select(chis_mun, lon, lat, pob,nom_loc)

if (!file.exists("distance_matrix.RData")) {
    distance_matrix <- new.env(hash = TRUE)
}else {
    attach("distance_matrix.RData")
}

if (!file.exists("road_hash.RData")) {
  road_hash <- new.env(hash = TRUE)
}else {
  attach("road_hash.RData")
}

data              <- chis_points
distance_matrix_  <- distance_matrix
road_hash_        <- road_hash

## --------------------------------------------------
## Using differents parameters
## --------------------------------------------------

run_test <- function(pop_criterion) {
  #Constants
  min_pop_centroids <- c(1000, 25,25,25,25)
  mode              <- 'driving'
  plot_with_labels <- TRUE
  show_history_plot <- TRUE
  with_real_distance <- FALSE
  without_road <- iterative_clustering(data,
                               distance_matrix_,
                               road_hash_,
                               min_pop_centroids = min_pop_centroids,
                               min_pop_criterion = pop_criterion,
                               mode = mode,
                               plot_with_labels = plot_with_labels,
                               show_history_plot = show_history_plot,
                               with_real_distance = with_real_distance)
  return (list("without_road"=without_road))
}  
max_min_min <- run_test(c(FALSE,TRUE))
max_min_min[[1]]$plot

max_pop_always  <- run_test(c(FALSE))
max_pop_always[[1]]$plot



min_pop_always   <- run_test(c(TRUE))
min_pop_always[[1]]$plot


#  with_road <-  iterative_clustering(data,
#                                     distance_matrix_,
#                                     road_hash_,
#                                     min_pop_centroids = min_pop_centroids,
#                                     min_pop_criterion = pop_criterion,
#                                     mode = mode,
#                                     build_with_road = TRUE,
#                                     plot_with_labels = plot_with_labels,
#                                     show_history_plot= show_history_plot,
#                                     with_real_distance = with_real_distance)
#  return (list("with_road"=with_road, "without_road"=without_road))
#}



## --------------------------------------------------
## Save hash table
## --------------------------------------------------
save(distance_matrix,
     file = "distance_matrix.RData")
save(road_hash,
     file = "road_hash.RData")
