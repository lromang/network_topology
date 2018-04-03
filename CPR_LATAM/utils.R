###########################################
## = Authors
##
## - Luis Manuel Román García
## - Miguel Alonso Vilchis
##
## ----------------------------------------
## = Description
##
## Multiple functions for general geog-
## graphical tasks.
##
###########################################

## ----------------------------------------
## libraries
## ----------------------------------------
source('libraries.R')

## ----------------------------------------
## Global Google API Keys
## ----------------------------------------
google_keys <-  readLines('../google_keys.key')
this_key    <- 1

############################################################################
#                             Distance stuff                               #
############################################################################

## ----------------------------------------
## Handles Queries
## ----------------------------------------
handle_queries <- function(base){
    url      <- paste(base,
                     google_keys[this_key],
                     sep = "&key=")
    curl     <- getCurlHandle()
    resp     <- getURL(url, curl = curl)
    info_url <- getCurlInfo(curl)
    if(info_url$response.code == 403){
        this_key <- this_key + 1
        if(this_key <= length(google_keys)){
            handle_queries(base)
        }else{
            print('NO MORE QUERIES!!!')
            quit()
        }
    }else if(info_url$response.code == 200){
        return(resp)
    }else{
        return({})
    }
}

## ----------------------------------------
## Get Distance To Road
## ----------------------------------------
distance_to_road <- function(point_){
    ## ----------------------------------------
    ## This function uses Google's API Roads to
    ## calculate the nearest road to a given point.
    ## point = geografic point in (lat, lon)
    ## ----------------------------------------
    res   <- -1
    base  <- "https://roads.googleapis.com/v1/nearestRoads?"
    point <- paste0("points=", paste(point_, collapse = ","))
    query <- paste0(base, point)
    resp  <- RJSONIO::fromJSON(handle_queries(query))
    if(length(resp) > 0 && length(resp$snappedPoints) > 0){
        nearest_road <- resp$snappedPoints[[1]]$location
        res          <-  distGeo(point_[2:1],
                                as.numeric(nearest_road)[2:1])
    }
    res
}

## ----------------------------------------
## Get Distance To Road (bulk)
## ----------------------------------------
distances_to_road <- function(points){
    apply(points, 1, function(t) t <- distance_to_road(t))
}


##-------------------------------------
## get distance (COBOL)
##-------------------------------------
get_num_distance <- function(origin, destiny, distance_matrix_, mode = 'driving'){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving distance between two given points.
    ## origin  = geografic point in (latitude, longitude) format
    ## destiny = geografic point in (latitude, longitude) format
    ##-------------------------------------
    ## Check if origin destiny is in dataframe
    key_part1 <- paste(origin, collapse = ",")
    key_part2 <- paste(destiny, collapse = ",")
    key_1     <- paste0(key_part1, key_part2)
    key_2     <- paste0(key_part2, key_part1)
    if (!is.null(distance_matrix_[[key_1]] )) {
        return (distance_matrix_[[key_1]])
    }
    if (!is.null(distance_matrix_[[key_2]])) {
        return (distance_matrix_[[key_2]])
    }
    if (is.na(destiny[1])) {
        return(0)
    }
    ## Get Distance (START)
    base        <- "https://maps.googleapis.com/maps/api/directions/json?"
    origin_str  <- paste0("origin=", paste(origin, collapse = ","))
    destiny_str <- paste0("destination=", paste(destiny, collapse = ","))
    mode        <- paste0("mode=", mode)
    repeat{
        google_key  <- google_keys[this_key]
        key         <- paste0("key=", google_key)
        query       <- paste(base, origin_str, destiny_str, mode, key, sep = "&")
        system(paste0("curl ",
                      "'",
                      query,
                      "' | jq '.",
                      "[\"routes\"][0][\"legs\"][0][\"distance\"][\"value\"]",
                      "'",
                      " > intermedio.txt"))

        distance    <- readr::parse_number(readLines('intermedio.txt'))
        if(is.na(distance) && length(google_keys) > this_key) {
            print("CHANGE KEY")
            this_key    <- this_key + 1
            google_key  <- google_keys[this_key]
            key         <- paste0("key=",google_key)
        }

        if(length(google_keys) == this_key){
            print('NO MORE KEYS')
            quit()
        }
    }
    print(query)
    print(distance)
    if (length(distance) <= 0 || distance == 0){ #Try with geosphere distance
        distance <- distm (c(origin[,2], origin[,1]),
                          c(destiny[,2], destiny[,1]), fun = distHaversine)[1]
        print(distance)
    }
    ## Get Distance (END)
    if(distance >= 0) {
        distance_matrix_[[key_1]] <- distance
    }
    if (file.exists("intermedio.txt")) {
        system('rm intermedio.txt')
    }
    distance
}


##-------------------------------------
## get distance
##-------------------------------------
get_num_distance_mk <- function(origin,
                               destiny,
                               distance_matrix_,
                               mode = 'driving'){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving distance between two given points.
    ## origin  = geografic point in (latitude, longitude) format
    ## destiny = geografic point in (latitude, longitude) format
    ##-------------------------------------
    ## Check if origin destiny is in dataframe
    key_part1 <- paste(origin, collapse = ",")
    key_part2 <- paste(destiny, collapse = ",")
    key_1     <- paste0(key_part1, key_part2)
    key_2     <- paste0(key_part2, key_part1)
    if (!is.null(distance_matrix_[[key_1]] )) {
        return (distance_matrix_[[key_1]])
    }
    if (!is.null(distance_matrix_[[key_2]])) {
        return (distance_matrix_[[key_2]])
    }
    if (is.na(destiny[1])) {
        return(0)
    }
    ## Get Distance (START)
    base        <- "https://maps.googleapis.com/maps/api/directions/json?"
    origin_str  <- paste0("origin=", paste(origin, collapse = ","))
    destiny_str <- paste0("destination=", paste(destiny, collapse = ","))
    mode        <- paste0("mode=", mode)
    query       <- paste(base, origin_str, destiny_str, mode, sep = "&")
    resp        <- handle_queries(query)
    if(length(resp) > 0 && resp$routes[[1]]$legs[[1]]$distance$value){
        distance <- resp$routes[[1]]$legs[[1]]$distance$value
        distance_matrix_[[key_1]] <- distance
    }else{
        distance <- distm(origin[,2:1],
                         destiny[,2:1],
                         fun = distHaversine)[1]
    }
    distance
}

##-------------------------------------
## get distance matrix
##-------------------------------------
get_distance_matrix <- function(points, distance_matrix_, mode = 'driving', coords_cols = 2:1){
  ##-------------------------------------
  ## This function uses Google's API directions to
  ## calculate the driving distance between each point.
  ## points  = geografic points in (latitude, longitude) format
  ## RETURNS:
  ## 1.- Upper triangular matrix with driving distance
  ## between the points.
  ## 2.- Tree Matrix
  ##-------------------------------------
  ## Tree Matrix
  tree_matrix <- c()
  ## Distance Matrix
  dist_matrix <- matrix(nrow = length(points[[1]]),
                        ncol = length(points[[1]]))
    ## Fill in matrices
    print(' INIT FOR (DISTANCE MATRIX) ')
    for(i in 1:(length(points[[1]]) - 1)){
      for(j in (i + 1):length(points[[1]])){
          print(paste0('i: ', i, ' | of: ', length(points[[1]])))
          ## Distance Matrix
          dist_matrix[i, j] <- get_num_distance(points[i, coords_cols],
                                               points[j, coords_cols],
                                               distance_matrix_ ,
                                               mode)
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


############################################################################
#                               Clusterize stuff                           #
############################################################################


##-------------------------------------
## Get Partition by Criterion
##-------------------------------------
get_partition <- function(data, min_pop_criterion = TRUE){
  ## Agregar el criterio de más disperso
  ## o menos disperso ... seguramente por
  ## heurísitca
  pops_centroid <- data[,sum(pob), by = cluster]
  if(min_pop_criterion){
    ## Min pop centroid
    min_cluster <- pops_centroid$cluster[which(pops_centroid$V1 == min(pops_centroid$V1))]
    ans         <-  dplyr::filter(data, cluster == min_cluster)
  }else{
    ## Max pop centroid
    max_cluster <- pops_centroid$cluster[which(pops_centroid$V1 == max(pops_centroid$V1))]
    ans         <-  dplyr::filter(data, cluster == max_cluster)
  }
  ans
}


## ------------------------------------
## vanilla clusterize
## ------------------------------------
vanilla_get_clusters <- function(points, centers, mode = 'driving', distance_matrix_){
  ## Weights
  weights      <- points[,3]/sum(points[,3])
  clust_assign <- c()
  for(i in 1:nrow(points)){
    ## Init Distances
    cent_dist <- plyr::dlply(centers, 1,
                             function(t) t <- get_num_distance(origin  = points[i,2:1],
                                                               destiny = t,
                                                               distance_matrix_ = distance_matrix_,
                                                               mode = 'driving'
                             )/weights[i]
    )
    clust_assign[i] <- which(cent_dist == min(unlist(cent_dist)))[1]
  }
  clust_assign
}

## ------------------------------------
## vanilla update centers
## ------------------------------------
vanilla_update_centers <- function(points, assign){
  points$assign  <- assign
  p              <- data.table(points)
  centers        <- p[, list('lat'= mean(lat),
                             'lon'= mean(lon)),
                      by = assign]
  centers$assign <- NULL
  centers
}


##-------------------------------------
## Vanilla Distance
##-------------------------------------
vanilla_k_means <- function(points, n_centers,
                            mode  = 'driving',
                            distance_matrix_,
                            max_iter = 100){
  ## ----------------------------------------
  ## points  = lon, lat, pob
  ## centers = n_centers
  ## No need for distance_matrix...changes
  ## every time
  ## ----------------------------------------

  ## Initial Centers
  centers <- data.frame('lat' = sample(seq(min(points$lat),
                                           max(points$lat),
                                           by = .001), n_centers),
                        'lon' = sample(seq(min(points$lon),
                                           max(points$lon),
                                           by = .001), n_centers)
  )
  ## Initial Assignment
  clust_assign <- vanilla_get_clusters(points,
                                       centers,
                                       mode,
                                       distance_matrix_)
  iters        <- 1
  repeat{
    centers        <- vanilla_update_centers(points, clust_assign)
    n_clust_assign <- vanilla_get_clusters(points,
                                           centers,
                                           mode,
                                           distance_matrix_)
    if(n_clust_assign == clust_assign || iters >= max_iter){
      break
    }
    ## Update values
    clust_assign   <- n_clust_assign
    iters          <- iters + 1
  }
  ## Return results
  list('centers' = centers, 'clusts' = n_clust_assign)
}

##-------------------------------------
## Get Nearest Point
##-------------------------------------
get_nearest_point <- function(point, data){
  Wdistance <- distGeo(data[, 1:2], point)#/data$pob
  data[which(Wdistance == min(Wdistance))[1], 1:2]
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
