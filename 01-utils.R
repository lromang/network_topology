##################################################
##################################################
## General distance functions
##################################################
##################################################

google_keys <-  readLines('google_keys.key')
this_key    <- 1
##-------------------------------------
## get distance
##-------------------------------------
get_num_distance <- function(origin, destiny, distance_matrix_, mode = 'driving'){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving distance between two given points.
    ## origin  = geografic point in (latitude, longitude) format
    ## destiny = geografic point in (latitude, longitude) format
    ##-------------------------------------
    #Check if origin destiny is in dataframe
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
    origin      <- paste0("origin=", paste(origin, collapse = ","))
    destiny     <- paste0("destination=", paste(destiny, collapse = ","))
    mode        <- paste0("mode=", mode)
    google_key  <- google_keys[this_key]
    key         <- paste0("key=", google_key)
    query       <- paste(base, origin, destiny, mode, key, sep = "&")
    system(paste0("curl ", "'", query, "' | jq '.", "[\"routes\"][0][\"legs\"][0][\"distance\"][\"value\"]",
    "'",
    " > intermedio.txt"))

    distance    <- tryCatch ({
                   readr::parse_number(readLines('intermedio.txt'))
                  }, warning = function(w){ #problem with parse, try next key
                    if (length(google_keys) >= this_key +1) { #We have another key to try
                      this_key    <- this_key +1
                      google_key  <- google_keys[this_key]
                      key         <- paste0("key=",google_key)
                      return(tryCatch({
                         RJSONIO::fromJSON(getURL(query))$routes[[1]]$legs[[1]]$distance$value
                      }, error = function(w){0}))
                     }else {
                       print("NO MAS REQUEST POR HOY")
                       stopifnot(TRUE)
                     } })

    print(origin)
    print(destiny)
    print(distance)
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
## get distance matrix
##-------------------------------------
get_distance_matrix <- function(points, distance_matrix_, mode = 'driving', coords_cols = 1:2){
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
    dist_matrix <- matrix(nrow = length(points[[1]]),
                                    ncol = length(points[[1]]))
    ## Fill in matrices
    for(i in 1:(length(points[[1]]) - 1)){
        for(j in (i + 1):length(points[[1]])){
            ## Distance Matrix
            if (points[i,coords_cols][[coords_cols[1]]] == points[j,coords_cols][[coords_cols[1]]] &&
              points[i,coords_cols][[coords_cols[2]]] == points[j,coords_cols][[coords_cols[2]]]
              ) {
                next
              }

            dist_matrix[i, j] <- get_num_distance(points[i,coords_cols], points[j,coords_cols],distance_matrix_ ,mode)
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
