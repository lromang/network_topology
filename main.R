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

## Work with Aguascalientes
## 1 <- Ags
ags_mun    <- dplyr::filter(data, ent == 1)
ags_points <- dplyr::select(ags_mun, lon, lat, pob)

if (!file.exists("distance_matrix.RData")) {
    distance_matrix <- new.env(hash = TRUE)
}else {
    attach("distance_matrix.RData")
}

## --------------------------------------------------
## Testing
## --------------------------------------------------
data              <- ags_points
distance_matrix_  <- distance_matrix
min_pop_centroids <- c(1000, 25, 25, 25, 25, 25)
min_pop_criterion <- FALSE
mode              <- 'driving'

## test
test <- iterative_clustering(data,
                            distance_matrix_,
                            min_pop_centroids = min_pop_centroids,
                            min_pop_criterion = min_pop_criterion,
                            mode = mode)

## --------------------------------------------------
## Save hash table
## --------------------------------------------------
save(distance_matrix,
     file = "distance_matrix.RData")
