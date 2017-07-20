#Entregables para el miercoles
#  Escoger un municipio (Aguascalientes)  menor numero de poblados
#  Correr muchas veces, -> 1000 clusters con distancias euclideana
#                          500, 200, 100, 50 y correr el algoritmo
#  Graficar numero de clusters vs km de fibra

########################################
## Libraries
########################################
library(rjson)
library(RJSONIO)
library(RCurl)
library(plyr)
library(stringr)
library(data.table)
library(caret)
library(dplyr)
library(geosphere)
library(ggplot2)
library(deldir)
library(tidyr)
library(rje)
library(sp)
library(SDMTools)
library(maps)
library(maptools)
library(spatstat)
library(rgeos)
library(rgdal)
library(cluster)
library(WeightedCluster)
library(ggmap)
########################################
## Functions
########################################

##-------------------------------------
## get_directions
##-------------------------------------
get_directions <- function(origin, destino, mode = "driving"){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving directions between two given points.
    ## origin  = geografic point in (latitude, longitude) format
    ## destiny = geografic point in (latitude, longitude) format
    ##-------------------------------------
    base        <- "https://maps.googleapis.com/maps/api/directions/json?"
    origin      <- paste0("origin=",
                         paste(origin, collapse = ",")
                         )
    destiny     <- paste0("destination=",
                         paste(destiny, collapse = ",")
                         )
    mode        <- paste0("mode=", mode)
    key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
    query       <- paste(base, origin, destiny, mode, key, sep = "&")
    route       <- fromJSON(getURL(query))$routes[[1]]$legs
    steps       <- route[[1]]$steps
    list(
        "durations"  = ldply(steps, function(t)t <- t$duration$text)[, 1],
        "distance"   = ldply(steps, function(t)t <- t$distance$text)[, 1],
        "start_loc"  = ldply(steps, function(t)t <- t$start_location),
        "end_loc"    = ldply(steps, function(t)t <- t$end_location)
        )
}


##-------------------------------------
## get distance matrix
##-------------------------------------
get_distance_matrix <- function(points, mode = 'driving', coords_cols = 1:2){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving distance between each point.
    ## points  = geografic points in (latitude, longitude) format
    ## RETURNS:
    ## Upper triangular matrix with driving distance
    ## between the points.
    ##-------------------------------------
    ## Tree Matrix
    tree_matrix <- c()
    ## Distance Matrix
    dist_matrix <- matrix(nrow = nrow(points),
                                    ncol = nrow(points))
    ## Fill in matrices
    for(i in 1:(nrow(points) - 1)){
        for(j in (i + 1):nrow(points)){
            ## Distance Matrix
            dist_matrix[i, j] <- get_num_distance(points[i,coords_cols], points[j,coords_cols], mode)
            dist_matrix[j, i] <- dist_matrix[i, j]
            ## Tree Matrix
            tree_matrix         <- rbind(tree_matrix,
                                        data.frame(
                                            'x'    = points[i, 2],
                                            'y'    = points[i, 1],
                                            'xend' = points[j, 2],
                                            'yend' = points[j, 1],
                                            'p'    = dist_matrix[i, j]
                                        ))
            ## Make sure of simmetry
            tree_matrix         <- rbind(tree_matrix,
                                        data.frame(
                                            'xend'  = points[i, 2],
                                            'yend'  = points[i, 1],
                                            'x'     = points[j, 2],
                                            'y'     = points[j, 1],
                                            'p'     = dist_matrix[i, j]
                                        ))
        }
    }
    ## Diag = 0
    diag(dist_matrix) <- 0
    ## Dissimilarity object
    dist_matrix <- as.dist(dist_matrix)
    ## Result
    result <- list()
    result[[1]] <- dist_matrix
    result[[2]] <- tree_matrix
    ## Return
    result
}

##-------------------------------------
## get tree clust
##-------------------------------------
get_tree_clust <- function(tree_m, points){
    tree_m$cluster <- NA
    clustered_tree <- c()
    for(i in unique(points$cluster)){
        data_clust <- dplyr::filter(points, cluster == i)
        for(j in 1:nrow(data_clust)){
            tree_origins <- dplyr::filter(tree_m,
                                         x == data_clust$lon[j] &
                                         y == data_clust$lat[j])
            in_clust <- which(tree_origins$xend %in% data_clust$lon &
                             tree_origins$yend %in% data_clust$lat)
            tree_origins$cluster[in_clust] <- i
            clustered_tree <- rbind(clustered_tree, na.omit(tree_origins))
        }
    }
    clustered_tree
}


##------------------------------------- NO
## get distance
##-------------------------------------
get_num_distance <- function(origin, destiny, mode = 'driving'){
##-------------------------------------
## This function uses Google's API directions to
## calculate the driving distance between two given points.
## origin  = geografic point in (latitude, longitude) format
## destiny = geografic point in (latitude, longitude) format
##-------------------------------------
base        <- "https://maps.googleapis.com/maps/api/directions/json?"
origin      <- paste0("origin=",
paste(origin, collapse = ",")
)
destiny     <- paste0("destination=",
paste(destiny, collapse = ",")
)
mode        <- paste0("mode=", mode)
key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
query       <- paste(base, origin, destiny, mode, key, sep = "&")
system(paste0("curl ", "'", query, "' | jq '.", "[\"routes\"][0][\"legs\"][0][\"distance\"][\"value\"]",
"'",
" > tmp.txt"))
## route       <- RJSONIO::fromJSON(getURL(query))$routes[[1]]$legs[[1]]$distance$value
## route
distance    <- readr::parse_number(readLines('tmp.txt'))
system('rm tmp.txt')
distance
}

##-------------------------------------
## get clusts
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
##-------------------------------------
get_clusts <- function(points, nclusts = 2,  mode = 'driving'){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving distance between each point.
    ## and then creates the clusters
    ## points  = geografic points in (latitude, longitude, poblation) format
    ## RETURNS list with 3 entries:
    ## entry 1 = distance matrix with driving distance
    ## between the points.
    ## entry 2 = clusters
    ## entry 3 = plot of clusters
    ##-------------------------------------
    names(points) <- c('lat', 'lon','pob')
    ## Distance & Tree matrices
    dist_tree <- get_distance_matrix(points, mode)
    dist_m    <- dist_tree[[1]]
    tree_m    <- dist_tree[[2]]
    ## Clusters
    #clusts <- pam(dist_m, diss = TRUE, k = nclusts)
    clusts  <- wcKMedoids(dist_m, k = nclusts, weights=points$pob)
    ## plot
    points$cluster <- as.factor(clusts$clustering)

    base_map = get_map( location=c(min(points$lon),min(points$lat),
                                  max(points$lon),max(points$lat)),
                        zoom=11, maptype="roadmap")

    aux_map = ggmap(base_map)
    clust_plot <- aux_map +
        geom_point(data=points,
                  aes(x   = lon,
                  y   = lat,
                  col = cluster),
                  size = 2,
                  alpha = .7) +
        theme(panel.background = element_blank(),
              axis.title = element_text(face = "bold",
                                        color = "#1972b9"),
              legend.title = element_text(face = "bold",
                                          color = "#424242"),
              panel.grid.major = element_line(colour = "#BDBDBD",
                                              linetype = "dotted"),
              panel.grid.minor = element_line(colour = "#E0E0E0",
                                              linetype = "dotted")) +
        ylab("Lat") + xlab("Lon") +
        scale_colour_discrete(name = "Clusters")
    ## Add Trees
    tree_cluster_filter <- get_tree_clust(tree_m, points)
    for(i in unique(tree_cluster_filter$cluster)){
        prim_clust <- prim(dplyr::filter(tree_cluster_filter, cluster == i))
        clust_plot <- clust_plot +
            geom_segment(
                data = prim_clust,
                aes(x = x, y = y, xend = xend, yend = yend),
                col = "gray",
                linetype = 2
            )
    }

    ##
    print(clust_plot)
    ## Result
    result <- list()
    result[[1]] <- dist_m
    result[[2]] <- clusts$clustering
    result[[3]] <- clust_plot
    ## Return
    result
}

##-------------------------------------
## get_altitude
##-------------------------------------
get_altitude <- function(locations){
    ##-------------------------------------
    ## This function uses Google's API "directions" to
    ## calculate the driving distance between two points given.
    ## locations  = list of points in (latitude, longitude) format
    ##-------------------------------------
    base        <- "https://maps.googleapis.com/maps/api/elevation/json?"
    locations   <- paste0(base, "locations=",
                         paste(
                             laply(locations,
                                   function(t) t <- paste(t, collapse = ",")),
                             collapse = "|")
                         )
    key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
    query       <- paste(locations, key, sep = "&")
    fromJSON(getURL(query))
}

##-------------------------------------
## Prim
##-------------------------------------
## G = [x, y, xend, yend, p]
prim <- function(G){
    ## Generate set of vertex
    G$id_o  <- paste0(G[,1],G[,2])
    G$id_d  <- paste0(G[,3],G[,4])
    nodes   <- unique(G$id_o)
    n_nodes <- length(nodes)
    node_0  <- sample(length(nodes),1)
    V0      <- nodes[node_0]
    T       <- c()
    k       <- 1
    ## Start iteration
    while(k <= (n_nodes - 1)){
    candidates <- dplyr::filter(G, id_o %in% V0 & !(id_d %in% V0))
    enter      <- candidates[which(candidates$p == min(candidates$p))[1], ]
    T          <- rbind(T, enter)
    V0         <- c(V0, enter$id_d)
    k          <- k + 1
    }
    T
}


###############################################
##########          MAIN       ################
###############################################

data <- read.csv("./pob/data/dataCenso.csv", stringsAsFactors = FALSE)
data[,1] <- NULL
names(data) <- c("ent",
                "mun",
                "nom_mun",
                "nom_loc",
                "lon",
                "lat",
                "pob")

selec <- dplyr::filter(data, ent %in% c("7"))
points <- selec[seq(1,10),c("lat","lon","pob")]
get_clusts(points)
