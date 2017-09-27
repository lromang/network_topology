########################################
########################################
########################################
## Entregables para el miercoles
#  Escoger un municipio (Aguascalientes)  menor numero de poblados
#  Correr muchas veces, -> 1000 clusters con distancias euclideana
#                          500, 200, 100, 50 y correr el algoritmo
#  Graficar numero de clusters vs km de fibra
## TO DO
## - API de connectivity
## - Euc cluste based on population inside
## - box of connected node.
## - Iterative voronoi???
## - Municipio:
##   - stats: Mean, Max pop, Min pop and thresholds.
## - Distance matrxi based on clusters
## - Clustering with centroids
## - K-means based on population
## Whish list
## - Point's cloud perimeter
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

data        <- read.csv("./data/dataCenso.csv", stringsAsFactors = FALSE)
data[,1]    <- NULL
names(data) <- c("ent",
                "mun",
                "nom_mun",
                "nom_loc",
                "lon",
                "lat",
                "pob")

## Work with Aguascalientes
ags_mun    <- dplyr::filter(data, nom_mun == 'Aguascalientes')
ags_points <- dplyr::select(ags_mun, lon, lat, pob)

## Get voronoi with euclidean distance
if (!file.exists("distance_matrix.RData")) {
    distance_matrix <- new.env(hash = TRUE)
}else {
    attach("distance_matrix.RData")
}

##
test_clusts <- get_clusts(ags_points[sample(20,20), ], distance_matrix, nclusts = 2)

## Save hash table
save(distance_matrix, file = "distance_matrix.RData")
