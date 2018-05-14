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
}


##-------------------------------------
## Get Nearest Point
##-------------------------------------
get_nearest_point <- function(point, data){
   if (length(point) >2) {
     point <- point[,1:2]
   }
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
                     road_hash_,
                     mode,
                     centroids,
                     connected_node,
                     road = FALSE,
                     build_with_road,
                     alpha = .7,
                     with_real_distance=TRUE){
    results <- list()
    if (centroids > 1) {
        if(!road){
            if (build_with_road) {
              ## Get Clusters (of all points, with euclidean distance)
              dist_road <- distances_to_road(data[,2:1], road_hash_) 
              clusts   <- flexclust::kcca(data[,1:2],
                                          k       = centroids,
                                          weights = (alpha * data$pob/sum(data$pob) +
                                                  ((1 - alpha) * ( 1 - dist_road/sum(dist_road)))
                                          )
                                       )
            } else {
              clusts   <- flexclust::kcca(data[,1:2],
                                          k       = centroids,
                                          weights = data$pob/sum(data$pob))
            }
            clusters <- as.factor(clusts@cluster)
            centers  <- clusts@centers ## Too many centers
        } else {
            clusts   <- vanilla_k_means(data[,1:3],
                                       n_centers = centroids,
                                       mode      = 'driving',
                                       distance_matrix_,
                                       max_iter = 100)
            clusters <- as.factor(clusts[[2]])
            centers  <- clusts[[1]][,2:1]
        }
        ## Connected_node from the previous iteration
        cluster_data         <- data
        cluster_data$cluster <- clusters
        total_centers        <- length(unique(clusters))
        cluster_name         <- unique(clusters)
        ## Only work with valid points
        ## Last point is connected_node 
        centers              <- lapply(1:total_centers, function(idx){
                              #if ( 
                              #    nrow(dplyr::filter(dplyr::filter(cluster_data, cluster == cluster_name[idx]),
                              #    lat == connected_node$lat & lon == connected_node$lon)) ==1){
                              #  return (connected_node[c("lon","lat")])
                              #}
                              #else {
                                return (get_nearest_point(centers[idx,1:2],
                               dplyr::filter(cluster_data, cluster == cluster_name[idx])))
                              #}
                            }
            )
        #centers <- rbind(do.call(rbind, centers) , connected_node)
        centers <- do.call(rbind, centers)
        ## Distance matrix of centroids!!!!
        ## Need to solve population problem
        dist_tree    <- get_distance_matrix(points =data.frame(centers),
                                           distance_matrix_ =  distance_matrix_,
                                           mode=mode,
                                           with_real_distance=with_real_distance)
        
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
                      road_hash_,
                      mode       = 'driving',
                      connected_node = c(0, 0),
                      road = FALSE,
                      build_with_road, 
                      with_real_distance = TRUE){
    ## Para evitar que haya tantos clusters como puntos
    min_pop_centroids <- min(min_pop_centroids, sum(data$pob)/2) 
    ## Número de clusters para empezar la iteración
    centroids    <- floor((nrow(data) * .1) + 1) 
    cluster_data <- data.table(data)
    centers      <- c()
    dist_m       <- list()
    tree_m       <- list()
    results      <- list()
    repeat{
        if(first_iter){
            cclusters <- flexclust::kcca(data[,c(1,2)],
                                        k       = centroids,
                                        weights = data$pob/sum(data$pob))
            
            clusters  <- cclusters@cluster
            centers   <- cclusters@centers
        } else {
            ## Non Euclidean Clustering
            non_euc_res <- build_net(data,
                                    distance_matrix_,
                                    road_hash_,
                                    mode,
                                    centroids,
                                    connected_node,
                                    road,
                                    build_with_road = build_with_road,
                                    with_real_distance = with_real_distance)
            ## Res
            dist_m      <- non_euc_res[[1]]
            tree_m      <- non_euc_res[[2]]
            clusters    <- non_euc_res[[3]]
            centers     <- join(non_euc_res[[4]],data,by=c("lat","lon"))
        }
        ## Check condition
        cluster_data$cluster <- clusters
        ## Hacer clusters más grandes -> más poblados
        centroids_next       <- max(floor(centroids / 2), 2)
        ## Sacar población de cluster menos poblado
        min_pop_clust        <- min(cluster_data[,sum(pob), by = cluster]$V1)
        ## Ver si se cumple el criterio poblacional y si tenemos
        ## al menos dos clusters
        if(min_pop_clust >= min_pop_centroids  || centroids_next <= 2){
            print(sprintf('Min Pop Clust = %i, Centroids = %i, Criterion pob =%f',
                          min_pop_clust,
                          centroids,
                          min_pop_centroids
                          ))
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





aux_iterative_clustering  <- function ( iter_index,
                                        cluster_plot,
                                        history_plot,
                                        total_pob,
                                        length_net,
                                        connected_node,
                                        partitioned_data,
                                        min_pop_centroids,
                                        distance_matrix_,
                                        mode,
                                        road_hash_,
                                        build_with_road,
                                        with_real_distance,
                                        explore_all = TRUE,
                                        centers
                                        ) {
  if (!(sum(partitioned_data$pob) > min_pop_centroids[length(min_pop_centroids)] &&
        nrow(partitioned_data)    > 1 &&
        iter_index + 1 <= length(min_pop_centroids) )){
    print ("Condicion no valida")
    return ()
  }
  ## Clusterize Data
  intermediate_data <- clusterize(data              = partitioned_data,
                                  min_pop_centroids = min_pop_centroids[iter_index + 1],
                                  first_iter        = FALSE,
                                  distance_matrix_  = distance_matrix_,
                                  road_hash_        = road_hash_,
                                  mode              = mode,
                                  connected_node    = connected_node,
                                  build_with_road   = build_with_road,
                                  with_real_distance = with_real_distance)
  
  ## Get length of network
  if (nrow(intermediate_data[[1]]) > 1 && length(intermediate_data) == 4) {
    tree                   <- prim(intermediate_data[[4]])
    cluster_plot           <- add_tree_plot(cluster_plot,intermediate_data[[1]],tree, iter_index = iter_index, with_labels = plot_with_labels)
    cluster_plot           <- add_tree_plot(cluster_plot,connected_node,only_one_point = TRUE)
    history_plot[[length(history_plot)+1]] <- cluster_plot
    
    length_net[length(length_net)+1] <- sum(tree$p)
    
    ## Save results for
    all_trees[[length(all_trees)+1]] <- tree
    
    
    
  }else {
    ## Cluster with one centroid
    ## The node was connected.
    print ("SOLO UN NODO")
    cluster_plot       <- add_tree_plot(cluster_plot,connected_node,only_one_point = TRUE, with_labels = plot_with_labels)
    history_plot[[length(history_plot)+1]] <- cluster_plot
    centers[[length(centers)+1]]     <- connected_node
    return (list('centers' = centers, 'plot'= cluster_plot ))
  }
  ## Get Coverage
  coverage               <- get_coverage(centers = intermediate_data[[2]],
                                         data    = intermediate_data[[1]],
                                         ## Otro hiperparámetro que podría ser un arreglo
                                         radius  = 1000)
  covered_locs           <- coverage[[2]]
  covered_pop            <- coverage[[1]]
  ## Add pop
  total_pob[length(total_pob)+1]  <- sum(covered_pop) ## * n_partitions // Idea de Ante
  ## Update data (don't know if this is correct????)
  ## min_pop_cirterion could be an (TRUE, FALSE, FALSE,....) sequence
  if (explore_all) {
    found_center     <- 0
    for(i in unique(intermediate_data[[1]]$cluster)){
      partitioned_data         <-  dplyr::filter(intermediate_data[[1]], cluster == i)
      connected_node    <- dplyr::filter(partitioned_data, (lat %in%tree$x & lon %in% tree$y) | (lat %in%tree$xend & lon %in% tree$yend) )[1,]
      centers[[length(centers)+1]]     <- connected_node
      partition     <- aux_iterative_clustering(
                                  iter_index +1,
                                  cluster_plot,
                                  history_plot,
                                  total_pob,
                                  length_net,
                                  connected_node,
                                  partitioned_data,
                                  min_pop_centroids,
                                  distance_matrix_,
                                  mode,
                                  road_hash_,
                                  build_with_road,
                                  with_real_distance,
                                  explore_all,
                                  centers
                               )
      if (length(partition) ==0 ){
        #cluster_plot     <- mark_as_connected_plot(cluster_plot,tree) 
        return (list('centers' = centers, 'plot'= cluster_plot ))
      }else {
        cluster_plot      <-       partition$plot
        centers           <-       partition$centers
      }
    }
   }else {
      partitioned_data <- get_partition(intermediate_data[[1]],
                                        min_pop_criterion[min(length(min_pop_criterion),iter_index+1)])
      connected_node    <- dplyr::filter(partitioned_data, lat %in%tree$x & lon %in% tree$y)[1,]
      centers[[length(centers)+1]]     <- connected_node
      
      partition     <- aux_iterative_clustering(
        iter_index +1,
        cluster_plot,
        history_plot,
        total_pob,
        length_net,
        connected_node,
        partitioned_data,
        min_pop_centroids,
        distance_matrix_,
        mode,
        road_hash_,
        build_with_road,
        with_real_distance,
        explore_all,
        centers
      )
      if (length(partition) ==0 ){
        #cluster_plot     <- mark_as_connected_plot(cluster_plot,tree) 
        return (list('centers' = centers, 'plot'= cluster_plot ))
      }else {
        cluster_plot      <-       partition$plot
        centers           <-       partition$centers
        
      }
  }
  
  ## Connected_node
  #connected_node   <- intermediate_data[[2]][unique(partitioned_data$cluster), ]
  ## Get Nearest Locality
  #connected_node   <- get_nearest_point(connected_node, partitioned_data)
  ## Partition loop
  iter_index       <- iter_index + 1
  ## N partitions
  n_partitions     <- length(unique(intermediate_data[[1]]$cluster))
  
  return (list('centers' = centers, 'plot'= cluster_plot ))  
}








##-------------------------------------
## iterative clustering
##-------------------------------------
iterative_clustering <- function(data,
                                distance_matrix_,
                                road_hash_,
                                ## Población mínima por cluster en cada iteración. 
                                min_pop_centroids = c(5000,2500,725,300), 
                                ## Si se va a usar este criterio o no... actualmente alternativa es max pop
                                ## podría ser también el cluster más disperso o el menos disperso o
                                ## mezclas y ver cómo cambia...
                                min_pop_criterion = c(TRUE, TRUE, FALSE, FALSE),
                                mode = 'driving',
                                build_with_road   = FALSE,
                                plot_with_labels = FALSE, # Parametro para plotear con nombre de las localidades
                                show_history_plot = TRUE, # Parametro para regresar una lista con el historico de las graficas
                                with_real_distance = TRUE #Parametro para determinar si la matriz de distancia se hace con distancia carretera
                                ){
    ## ------------------------------
    ## Initial solution
    ## ------------------------------
    ## Data should be (lon, lat, pob)!!!
    ## First iteration return a partition with euclidian distance
    history_plot <- list()
    clustered_res <- clusterize(data,
                                   min_pop_centroids[1],
                                   first_iter       = TRUE,
                                   distance_matrix_ = distance_matrix_,
                                   road_hash_ = road_hash_,
                                   with_real_distance = with_real_distance)
    centers          <- clustered_res[[2]]
    clustered_data   <- clustered_res[[1]]
    ## First partition
    partitioned_data <- get_partition(clustered_data,
                                     min_pop_criterion[1])
    #First iteration always use puerto carlos as reference 
    #aux <- clustered_data$cluster[which(clustered_data$nom_loc == "Puerto Carlos")]
    #partitioned_data <-  dplyr::filter(clustered_data, cluster == aux[1])
    
    ## Connected_node
    connected_node   <- centers[unique(partitioned_data$cluster), ]
    ## Get Nearest Locality
    connected_node   <- get_nearest_point(connected_node, partitioned_data)
    cluster_plot     <- plot_init_cluster(clustered_res[[1]])
    cluster_plot     <- add_tree_plot(cluster_plot,connected_node,only_one_point = TRUE, with_labels=plot_with_labels)
    

    ## ------------------------------
    ## Iterative Network Construction
    ## ------------------------------
    all_trees        <- list()
    iter_index       <- 1
    length_net       <- c()
    total_pob        <- c()
    n_partitions     <- length(unique(clustered_data$cluster))
    history_plot[[iter_index]] <- cluster_plot
    
    centers   <- c()
    centers      [[iter_index]] <-  dplyr::filter(partitioned_data, lat== connected_node[["lat"]]  & lon ==connected_node[["lon"]] )[1,]
    response <- aux_iterative_clustering ( iter_index,
                                            cluster_plot,
                                            history_plot,
                                            total_pob,
                                            length_net,
                                            connected_node,
                                            partitioned_data,
                                            min_pop_centroids,
                                            distance_matrix_,
                                            mode,
                                            road_hash_,
                                            build_with_road,
                                            with_real_distance,
                                            explore_all = TRUE,
                                           centers)
    
    centroids <- do.call(rbind.data.frame, response$centers)
    dist_tree    <- get_distance_matrix(points =centroids,
                                        distance_matrix_ =  distance_matrix_,
                                        mode=mode,
                                        with_real_distance=with_real_distance)
    cluster_plot <- response$plot
    cluster_plot     <- mark_as_connected_plot(cluster_plot,prim(dist_tree[[2]])) 
   
    ## Result
    if (show_history_plot){
      return (list('pop' = total_pob, 'net' = length_net, 'trees' = all_trees, 'plot'= cluster_plot ))
    } 
    return (list('pop' = total_pob, 'net' = length_net, 'trees' = all_trees, 'plot'= cluster_plot))
    
}

