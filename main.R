#Entregables para el miercoles
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
## Libraries
########################################
source("00-load.R")
source("01-utils.R")
source("02-cluster.R")

###############################################
##########          MAIN       ################
###############################################

data <- read.csv("./data/dataCenso.csv", stringsAsFactors = FALSE)
data[,1] <- NULL
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
    distance_matrix <- new.env(hash=TRUE)
}else {
    attach("distance_matrix.RData")
}
test_data <- ags_points[1:50, ]

total_result <- c()
for (centroids in c(1000,500,200,50,5,2)) {
    if (centroids < length(test_data[[1]])) {
    test_vor <- get_cluster_voronoi(test_data, 
                                distance_matrix, 
                                coord_cols = 1:2,
                                centroids 
                                )
    total_result <- rbind(total_result, test_vor)
    }
}

save(distance_matrix, file = "distance_matrix.RData")

