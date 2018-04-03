##################################################
##################################################
############## ITERATIVE CLUSTERING ##############
##################################################
##################################################

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



##-------------------------------------
## Get Tree Parameters
##-------------------------------------
get_tree_param <- function(centers, data, m_tree, radius = 1000){
    centers$pob     <- get_coverage(centers, data, radius)[[1]]
    centers$cluster <- 1:nrow(centers)
    ## Add Trees
    tree_cluster_filter <- get_tree_clust(tree_m, centers)
    ## Get length of trees
    trees_length        <- c()
    k                   <- 1
    for(i in unique(tree_cluster_filter$cluster)){
        prim_clust      <- prim(dplyr::filter(tree_cluster_filter, cluster == i))
        ## length of tree
        trees_length[k] <- sum(prim_clust$p)
        k               <- k + 1
        clust_plot      <- clust_plot +
                          geom_segment( data = prim_clust,
                                   aes(x = x, y = y, xend = xend, yend = yend),
                                   col = "gray",linetype = 2)
    }

    ##
    print(clust_plot)
}


##-------------------------------------
## Get Nearest Point
##-------------------------------------
get_nearest_point <- function(point, data){
    Wdistance <- distGeo(data[, 1:2], point)#/data$pob
    data[which(Wdistance == min(Wdistance))[1], 1:2]
}

##-------------------------------------
## Get Coverage
##-------------------------------------
get_coverage <- function(centers, data, radius = 1000){
    ## Radius in meters
    center_pop    <- c()
    tot_in_radius <- rep(FALSE, nrow(data))
    for(i in 1:nrow(centers)){
        in_radius     <- distGeo(data[,1:2], centers[i, 1:2]) < radius
        center_pop[i] <- sum(data$pob[in_radius & !(tot_in_radius > 0)])
        tot_in_radius <- tot_in_radius + in_radius ## Get data already added
    }
    list(center_pop, tot_in_radius > 0)
}

##-------------------------------------
## Build Network
##-------------------------------------
build_net <- function(data,
                     distance_matrix_,
                     mode,
                     centroids,
                     connected_node,
                     road = FALSE,
                     alpha = .7){
    results <- list()
    ## Get Clusters (of all points, with euclidean distance)
    dist_road <- distances_to_road(data[,2:1]) ## (MIKE) Guardar esto en tabla hash key = 'latlon'
    if (centroids > 1) {
        if(!road){
            clusts   <- flexclust::kcca(data[,1:2],
                                       k       = centroids,
                                       weights = (alpha * data$pob/sum(data$pob) +
                                                  ((1 - alpha) * ( 1 - dist_road/sum(dist_road)))
                                       )
                                       )
            clusters <- as.factor(clusts@cluster)
            centers  <- rbind(clusts@centers, connected_node)
        } else {
            clusts   <- vanilla_k_means(data[,1:3],
                                       n_centers = centroids,
                                       mode      = 'driving',
                                       distance_matrix_,
                                       max_iter = 100)
            clusters <- as.factor(clusts[[2]])
            centers  <- rbind(clusts[[1]][,2:1], connected_node)
        }
        ## Connected_node from the previous iteration
        cluster_data         <- data
        cluster_data$cluster <- clusters
        total_centers        <- length(unique(clusters))
        cluster_name         <- unique(clusters)
        ## Only work with valid points
        ## Last point is connected_node 
        centers              <- lapply(1:total_centers, function(idx){
            get_nearest_point(centers[idx,1:2],
                              dplyr::filter(cluster_data, cluster == cluster_name[idx]))}
            )
        centers <-do.call(rbind,centers)
        ## Distance matrix of centroids!!!!
        ## Need to solve population problem
        dist_tree    <- get_distance_matrix(data.frame(centers),
                                           distance_matrix_,
                                           mode)
        results[[1]] <- dist_tree[[1]]
        results[[2]] <- dist_tree[[2]]
    } else {
        clusters <- as.factor(1)
        centers <- connected_node
    }
    results[[3]] <- clusters
    results[[4]] <- centers
    ## Return
    results
}


##-------------------------------------
## Get Init Clustering
##-------------------------------------
clusterize <- function(data,
                      min_pop_centroids,
                      first_iter = FALSE,
                      distance_matrix_,
                      mode       = 'driving',
                      connected_node = c(0, 0),
                      road = FALSE){
    ## Para evitar que haya tantos clusters como puntos
    min_pop_centroids <- min(min_pop_centroids, sum(data$pob)/2) 
    ## Número de clusters para empezar la iteración
    centroids    <- floor((nrow(data) * .5) + 1)
    cluster_data <- data.table(data)
    centers      <- c()
    dist_m       <- list()
    tree_m       <- list()
    results      <- list()
    repeat{
        if(first_iter){
            cclusters <- flexclust::kcca(data[,1:2],
                                        k       = centroids,
                                        weights = data$pob/sum(data$pob))
            clusters  <- cclusters@cluster
            centers   <- cclusters@centers
        } else {
            ## Non Euclidean Clustering
            non_euc_res <- build_net(data,
                                    distance_matrix_,
                                    mode,
                                    centroids,
                                    connected_node,
                                    road)
            ## Res
            dist_m      <- non_euc_res[[1]]
            tree_m      <- non_euc_res[[2]]
            clusters    <- non_euc_res[[3]]
            centers     <- non_euc_res[[4]]
        }
        ## Check condition
        cluster_data$cluster <- clusters
        ## Hacer clusters más grandes -> más poblados
        centroids_next       <- max(floor(centroids / 2), 2)
        ## Sacar población de cluster menos poblado
        min_pop_clust        <- min(cluster_data[,sum(pob), by = cluster]$V1)
        ## Ver si se cumple el criterio poblacional y si tenemos
        ## al menos dos clusters
        if(min_pop_clust >= min_pop_centroids || centroids_next <= 2){
            print(sprintf('Min Pop Clust = %i, Centroids = %i',
                          min_pop_clust,
                          centroids))
            break
        }
        ## Se hace la actualización después de verificar para
        ## no intentar clusterizar con un solo centroide
        centroids <- centroids_next
    }

    ## Results
    results[[1]] <- cluster_data
    results[[2]] <- centers
    results[[3]] <- dist_m
    results[[4]] <- tree_m
    ## Return
    results
}

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

##-------------------------------------
## iterative clustering
##-------------------------------------
iterative_clustering <- function(data,
                                distance_matrix_,
                                ## Población mínima por cluster en cada iteración. 
                                min_pop_centroids = seq(1000, 100, by = -100), 
                                ## Si se va a usar este criterio o no... actualmente alternativa es max pop
                                ## podría ser también el cluster más disperso o el menos disperso o
                                ## mezclas y ver cómo cambia...
                                min_pop_criterion = TRUE,
                                mode = 'driving'){
    ## ------------------------------
    ## Initial solution
    ## ------------------------------
    ## Data should be (lon, lat, pob)!!!
    ## First iteration return a partition with euclidian distance
    clustered_res    <- clusterize(data,
                                  min_pop_centroids[1],
                                  first_iter       = TRUE,
                                  distance_matrix_ = distance_matrix_)
    centers          <- clustered_res[[2]]
    clustered_data   <- clustered_res[[1]]
    ## First partition
    partitioned_data <- get_partition(clustered_data,
                                     min_pop_criterion)
    ## Connected_node
    connected_node   <- centers[unique(partitioned_data$cluster), ]
    ## Get Nearest Locality
    connected_node   <- get_nearest_point(connected_node, partitioned_data)
    cluster_plot     <- plot_init_cluster(clustered_res[[1]])

    ## ------------------------------
    ## Iterative Network Construction
    ## ------------------------------
    all_trees        <- list()
    iter_index       <- 1
    length_net       <- c()
    total_pob        <- c()
    n_partitions     <- length(unique(clustered_data$cluster))
    ## MAIN LOOP
    while(sum(partitioned_data$pob) > min_pop_centroids[length(min_pop_centroids)] &&
          nrow(partitioned_data)    > 1 &&
          iter_index + 1 <= length(min_pop_centroids) ){
              ## Clusterize Data
              intermediate_data <- clusterize(data              = partitioned_data,
                                             min_pop_centroids = min_pop_centroids[iter_index + 1],
                                             first_iter        = FALSE,
                                             distance_matrix_  = distance_matrix_,
                                             mode              = mode,
                                             connected_node    = connected_node)
              ## PRINT
              print(sprintf(paste0("ITERATION CRITERIA BEFORE: \n - Population in partition:",
                             " %d \n - Localities in partition: %d \n - Iter index: ",
                             " %d"),
                      sum(partitioned_data$pob),
                      nrow(partitioned_data),
                      iter_index
                      ))
              ## Get length of network
              if (length(intermediate_data[[2]]) > 1) {
                  tree                   <- prim(intermediate_data[[4]])
                  cluster_plot           <- add_tree_plot(cluster_plot,intermediate_data[[1]],tree)
                  length_net[iter_index] <- sum(tree$p) * n_partitions
                  ## Save results for
                  all_trees[[iter_index]] <- tree
              }else {
                  ## Cluster with one centroid
                  ## The node was connected.
                  cluster_plot       <- add_tree_plot(cluster_plot,connected_node,only_one_point = TRUE)
                  break
              }
              ## Get Coverage
              coverage               <- get_coverage(centers = intermediate_data[[2]],
                                                    data    = intermediate_data[[1]],
                                                    ## Otro hiperparámetro que podría ser un arreglo
                                                    radius  = 100)
              covered_locs           <- coverage[[2]]
              covered_pop            <- coverage[[1]]
              ## Add pop
              total_pob[iter_index]  <- sum(covered_pop) *n_partitions
              ## Update data (don't know if this is correct????)
              ## min_pop_cirterion could be an (TRUE, FALSE, FALSE,....) sequence
              partitioned_data <- get_partition(intermediate_data[[1]],
                                               min_pop_criterion)
              ## Connected_node
              connected_node   <- intermediate_data[[2]][unique(partitioned_data$cluster), ]
              ## Get Nearest Locality
              connected_node   <- get_nearest_point(connected_node, partitioned_data)
              ## Partition loop
              iter_index       <- iter_index + 1
              ## N partitions
              n_partitions     <- length(unique(intermediate_data[[1]]$cluster))
              ## PRINT
              print(sprintf(paste0("ITERATION CRITERIA AFTER: \n - Population in partition:",
                             " %d \n - Localities in partition: %d \n - Iter index: ",
                             " %d"),
                            sum(partitioned_data$pob),
                            nrow(partitioned_data),
                            iter_index
                            ))

    }
    ## Result
    list('pop' = total_pob, 'net' = length_net, 'trees' = all_trees, 'plot'= cluster_plot)
}

