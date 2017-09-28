
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
## get clusts
##-------------------------------------
get_clusts <- function(points, distance_matrix_, nclusts = 2,  mode = 'driving', with_map=FALSE){
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
    names(points) <- c('lat', 'lon', 'pob')
    ## Distance & Tree matrices
    dist_tree <- get_distance_matrix(points, distance_matrix_, mode)
    dist_m    <- dist_tree[[1]]
    tree_m    <- dist_tree[[2]]
    ## Clusters
    if (nclusts > 1) {
        clusts         <- wcKMedoids(dist_m,
                                    k = nclusts,
                                    weights = points$pob)
        points$cluster <- as.factor(clusts$clustering)
        clustering     <- clusts$clustering
    } else {
        points$cluster <- as.factor(1)
        clustering     <- 1
    }
    ## plot
    if (with_map) {
      base_map = get_map(location = c(min(points$lon),min(points$lat),
                                      max(points$lon),max(points$lat)),
                        zoom=11, maptype="roadmap")
      aux_map = ggmap(base_map)
      clust_plot <- aux_map +
                    geom_point(data=points, aes(x = lon, y = lat,col = cluster))
    } else {
      clust_plot <- ggplot(data=points, aes(x = lon, y = lat,col = cluster))
    }

    clust_plot <- clust_plot +
                  geom_point(size = 2, alpha = .7) +
                  theme(panel.background = element_blank(),
                        axis.title   = element_text(face = "bold",color = "#1972b9"),
                        legend.title = element_text(face = "bold",color = "#424242"),
                        panel.grid.major = element_line(colour = "#BDBDBD",
                                                        linetype = "dotted"),
                        panel.grid.minor = element_line(colour = "#E0E0E0",
                                                        linetype = "dotted")) +
                  ylab("Lat")+xlab("Lon")+ scale_colour_discrete(name = "Clusters")

    ## Add Trees
    tree_cluster_filter <- get_tree_clust(tree_m, points)
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
    ## Result
    result <- list()
    result[[1]] <- dist_m
    result[[2]] <- clustering
    result[[3]] <- clust_plot
    result[[4]] <- trees_length
    ## Return
    result
}

##-------------------------------------
## euc voronoi
##-------------------------------------
get_euclidean_vor <- function(data, coord_cols= 2:1, centroids = 20){
    ## Results
    results     <- list()
    ## Kmeans with euclidean distance
    clusts      <- kmeans(data[, coord_cols],
                         centers = centroids)
    data$clusts <- as.factor(clusts$cluster)
    ## Voronoi around centroids of k-means
    voronoi     <- deldir(clusts$centers[,1],
                         clusts$centers[,2])
    ## Plot
    voro_plot   <- ggplot(data = data,
                         aes(x    = lon,
                             y    = lat,
                             size = pob,
                             col  = clusts)) +
        geom_point() +
        geom_point(data  = data.frame(clusts$centers),
                   aes(x = lon, y = lat),
                   col   = 'black',
                   alpha = .8,
                   size  = 1) +
        geom_segment(aes(x    = x1,
                         y    = y1,
                         xend = x2,
                         yend = y2),
                     size     = 1,
                     data     = voronoi$dirsgs,
                     linetype = 1,
                     color    = "#9E9E9E") +
    theme(panel.background = element_blank(),
          axis.title = element_text(face  = "bold",
                                    color = "#1972b9"),
          legend.position  = 'none',
          panel.grid.major = element_line(colour   = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour   = "#E0E0E0",
                                          linetype = "dotted")) +
        ylab("V2") + xlab("V1")
    print(voro_plot)
    result      <- list()
    result[[1]] <- voro_plot
    result[[2]] <- data
    ## Add plot to results
    result
}

get_optimal_cluster <- function(data, distance_matrix_) {
    ## ------------------------------
    ## Per Cluster
    ## ------------------------------
    ## Get optimal number of clusters
    n_clusts   <- c()
    all_clusts <- list()
    for(clust in unique(data$clusts)){
        clust_data      <- data[data$clusts == clust,
                               c(2,1,3)]
        n_clusts[clust] <- tryCatch({
            pamk(clust_data)$nc
        },error   = function(e){
            1 ## floor(nrow(clust_data)/2)
        },warning = function(w){
            1 ## floor(nrow(clust_data)/2)
        }
        )
        if (length(clust_data[[1]]) > 1 ) {
        all_clusts[[clust]] <- get_clusts(clust_data,
                                             distance_matrix_,
                                             n_clusts[clust],
                                             mode = 'driving')
        }
    }
    ## Add all_clusts to results
    all_clusts
}

##-------------------------------------
## get cluster voronoi
##-------------------------------------
get_cluster_voronoi <- function(data, distance_matrix_, coord_cols = 2:1, centroids){
  result      <- get_euclidean_vor(data, coord_cols, centroids)
  result[[2]] <- get_optimal_cluster(result[[2]], distance_matrix_)
  for (i in 1:length(result[[2]])){
    print(result[[2]][[i]][[4]])
  }
  result
}

##################################################
##################################################
############## ITERATIVE CLUSTERING ##############
##################################################
##################################################

##-------------------------------------
## Build Network
##-------------------------------------
build_net <- function(data, distance_matrix_, mode, centroids, connected_node){
    results <- list()
    ## Get Clusters (of all points, with euclidean distance)
    if (centroids > 1) {
    clusts       <- flexclust::kcca(data,
                                   k       = centroids,
                                   weights = data$pob)
    clusters     <- as.factor(clusts@cluster)
    ## Connected_node from the previous iteration
    centers      <- rbind(clusts@centers, connected_node[2:1])
    ## Distance matrix of centroids!!!!
    ## Need to solve population problem
    dist_tree    <- get_distance_matrix(data.frame(centers),
                                       distance_matrix_,
                                       mode)
    results[[1]] <- dist_tree[[1]]
    results[[2]] <- dist_tree[[2]]
    } else {
        clusters <- as.factor(1)
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
                      euc  = FALSE,
                      distance_matrix_,
                      mode = 'driving',
                      connected_node = c(0, 0)){
    ## Adjust min_pop_centroids
    min_pop_centroids <- min(min_pop_centroids, sum(data$pob)/2)
    print(min_pop_centroids)
    ##
    centroids    <- min(100, nrow(data)/2) ## Minimum number of centroids
    cluster_data <- data.table(data)
    centers      <- c()
    dist_m       <- list()
    tree_m       <- list()
    results      <- list()
    repeat{
        if(euc){
            cclusters <- kmeans(cluster_data[, 2:1], centroids)
            clusters  <- cclusters$cluster
            centers   <- cclusters$centers
        } else {
            ## Non Euclidean Clustering
            non_euc_res <- build_net(data,
                                    distance_matrix_,
                                    mode,
                                    centroids,
                                    connected_node)
            ## Res
            dist_m      <- non_euc_res[[1]]
            tree_m      <- non_euc_res[[2]]
            clusters    <- non_euc_res[[3]]
            centers     <- non_euc_res[[4]]
        }
        cluster_data$cluster <- clusters
        centroids            <- max(floor(centroids/2), 2)
        min_pop_clust        <- min(cluster_data[,sum(pob), by = cluster]$V1)
        print(min_pop_clust)
        if(min_pop_clust > min_pop_centroids || centroids == 2){
            print(sprintf('Min Pop Clust = %i',
                          min_pop_clust))
            break
        }
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
    pops_centroid <- data[,sum(pob), by = cluster]
    ## Min pop centroid
    min_cluster <- pops_centroid$cluster[which(pops_centroid$V1 == min(pops_centroid$V1))]
    ## Max pop centroid
    max_cluster <- pops_centroid$cluster[which(pops_centroid$V1 == max(pops_centroid$V1))]
    if(min_pop_criterion){
        ans <-  dplyr::filter(data, cluster == min_cluster)
    }else{
        ans <-  dplyr::filter(data, cluster == max_cluster)
    }
    ans
}

##-------------------------------------
## iterative clustering
##-------------------------------------
iterative_clustering <- function(data,
                                distance_matrix_,
                                min_pop_centroids = seq(1000, 100, by = -100),
                                min_pop_criterion = TRUE,
                                mode = 'driving'){
    ## ------------------------------
    ## Initial solution
    ## ------------------------------
    clustered_res    <- clusterize(data,
                                  min_pop_centroids[1],
                                  euc = TRUE,
                                  distance_matrix_ = distance_matrix_)
    centers          <- clustered_res[[2]]
    clustered_data   <- clustered_res[[1]]
    ## First partition
    partitioned_data <- get_partition(clustered_data,
                                     min_pop_criterion)
    ## Connected_node
    connected_node   <- centers[unique(partitioned_data$cluster), ]
    ## ------------------------------
    ## Iterative Network Construction
    ## ------------------------------
    partition_loop   <- 2
    while(sum(partitioned_data$pob) > min_pop_centroids[length(min_pop_centroids)] &&
          nrow(partitioned_data)    > 1){
              ## Clusterize Data
              intermediate_data <- clusterize(data              = partitioned_data,
                                             min_pop_centroids = min_pop_centroids[partition_loop],
                                             euc                = FALSE,
                                             distance_matrix_   = distance_matrix_,
                                             mode               = mode,
                                             connected_node     = connected_node)
              partitioned_data <- get_partition(intermediate_data[[1]],
                                               min_pop_criterion)
              ## Right now we are connecting points
              ## but we need to connect centroids, not points!
    }
}

