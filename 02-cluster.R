
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
get_clusts <- function(points,distance_matrix,nclusts = 2,  mode = 'driving', with_map=FALSE){
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
    dist_tree <- get_distance_matrix(points, distance_matrix, mode)
    dist_m    <- dist_tree[[1]]
    tree_m    <- dist_tree[[2]]
    ## Clusters
    #clusts <- pam(dist_m, diss = TRUE, k = nclusts)
    clusts  <- wcKMedoids(dist_m, k = nclusts, weights=points$pob)
    ## plot
    points$cluster <- as.factor(clusts$clustering)
    if (with_map) {
      base_map = get_map(location = c(min(points$lon),min(points$lat),
                                      max(points$lon),max(points$lat)),
                        zoom=11, maptype="roadmap")
      aux_map = ggmap(base_map)
      clust_plot <- aux_map +
                    ggplot(data=points, aes(x = lon, y = lat,col = cluster))
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
    result[[2]] <- clusts$clustering
    result[[3]] <- clust_plot
    result[[4]] <- trees_length
    ## Return
    result
}

##-------------------------------------
## euc voronoi
##-------------------------------------
get_euclidean_vor <- function(data,coord_cols= 1:2,prop = .1){
    ## Results
    results   <- list()
    ## Number of centroids
    centroids <- floor(nrow(data)*prop)
    ## Kmeans with euclidean distance
    clusts    <- kmeans(data[,coord_cols],centers = centroids)
    data$clusts <- as.factor(clusts$cluster)
    ## Voronoi around centroids of k-means
    voronoi   <- deldir(clusts$centers[,1],
                       clusts$centers[,2])
    ## Plot
    voro_plot <- ggplot(data = data, aes(x = lon,y = lat,size = pob,col=clusts)) +
                 geom_point() +
                 geom_point(data  = data.frame(clusts$centers),
                           aes(x = lon,y = lat),col   = 'black',alpha = .8,
                              size  = 1) +
                 geom_segment(aes(x = x1,y = y1, xend = x2, yend = y2),
                              size= 1, data = voronoi$dirsgs,
                              linetype = 1,color = "#9E9E9E") +
                 theme(panel.background = element_blank(),
                       axis.title = element_text(face = "bold",
                                                 color = "#1972b9"),
                      legend.position  = 'none',
                      panel.grid.major = element_line(colour = "#BDBDBD",
                                                      linetype = "dotted"),
                      panel.grid.minor = element_line(colour = "#E0E0E0",
                                                      linetype = "dotted")) +
                 ylab("V2") + xlab("V1")
    print(voro_plot)
    result <- list()
    result[[1]] <- voro_plot
    result[[2]] <- data
    ## Add plot to results
    result
}

get_optimal_cluster <- function(data, distance_matrix) {
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
            floor(nrow(clust_data)/2)
        },warning = function(w){
            floor(nrow(clust_data)/2)
        }
        )
        print(n_clusts[clust])
        ##
        all_clusts[[clust]] <- get_clusts(clust_data,distance_matrix
                                         n_clusts[clust],
                                         mode = 'driving')
    }
    ## Add all_clusts to results
    all_clusts
}



##-------------------------------------
## get cluster voronoi
##-------------------------------------
get_cluster_voronoi <- function(data,distance_matrix,coord_cols= 1:2,prop = .1){
  result <- get_euclidean_vor(data, coord_cols, prop)
  result[[2]] <- get_optimal_cluster(result[[2]], distance_matrix)
  result
}
