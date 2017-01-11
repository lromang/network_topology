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
########################################
## Functions
########################################

##-------------------------------------
## get_directions
##-------------------------------------
get_directions <- function(origen, destino, mode = "driving"){
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
## get_distance
##-------------------------------------
get_distance <- function(origin, destiny){
    ##-------------------------------------
    ## This function uses Google's API "directions" to
    ## calculate the driving distance between two points given.
    ## origin  = geografic point in (latitude, longitude) format
    ## destiny = geografic point in (latitude, longitude) format
    ##-------------------------------------
    base        <- "https://maps.googleapis.com/maps/api/distancematrix/json?"
    origin      <- paste0("origins=",
                         paste(origin, collapse = ",")
                         )
    destiny     <- paste0("destinations=",
                         paste(destiny, collapse = ",")
                         )
    key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
    query       <- paste(base, origin, destiny, key, sep = "&")
    results     <- fromJSON(getURL(query))
    distance    <- results$rows[[1]]$elements[[1]]$distance$text
    duration    <- results$rows[[1]]$elements[[1]]$duration$text
    list("distance" = distance, "duration" = duration)
}


##-------------------------------------
## get_connect
##-------------------------------------
get_connect <- function(lat, lon, dist = 20, net = 3){
    base       <- "http://api.opensignal.com/v2/networkstats.json?"
    key        <- "apikey=ca887c76265ad8d9268df0a9cc2de523"
    network    <- paste0("network_type=",net)
    format     <- "json_format=2"
    distance   <- paste0("distance=",dist)
    data <- list()
    ## Get network parameters for each of the coordinates
    for(i in 1:length(lat)){
        latitude  <- paste0("lat=", lat[i])
        longitud  <- paste0("lng=", lon[i])
        url       <- paste(base,
                          latitude,
                          longitud,
                          distance,
                          format,
                          network,
                          key,
                          sep = "&")
        data[[i]]   <- fromJSON(getURL(url))
        Sys.sleep(6.5)
    }
    data
}

##-------------------------------------
## Essau Wiliams
##-------------------------------------
## G = [x, y, xend, yend, p]


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

## Gen graph
gen_graph <- function(n){
    lon  <- rnorm(n, 0, 10)
    lat  <- rnorm(n, 0, 10)
    x    <- c()
    y    <- c()
    xend <- c()
    yend <- c()
    p    <- c()
    k    <- 1
    for(j in 1:n){
        for(i in 1:n){
            x[k] <- lon[j]
            y[k] <- lat[j]
            k <- k + 1
        }
    }
    xend <- rep(lon,n)
    yend <- rep(lat,n)
    p    <- rnorm(n*n)^2
    ## get all together
    data.frame(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        p = p
    )
}

## plot_graph
plot_graph <- function(n){
    graph  <- gen_graph(n)
    tree   <- prim(graph)
ggplot(data = graph, aes(x = x, y = y)) +
    geom_point(size = 5, col = "orange") +
    geom_segment(
        data = graph,
        aes(x = x, y = y, xend = xend, yend = yend),
        col = "blue",
        alpha = .05
    ) +
    geom_segment(
        data = tree,
        aes(x = x, y = y, xend = xend, yend = yend),
        col = "orange",
    ) +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          text = element_blank())
}



##-------------------------------------
## simulate_points
##-------------------------------------
simulate_points <- function(latitude, longitud,
                           n_big = 20, n_small = 10,
                           big_var = 10, small_var = 1){
    ##-------------------------------------
    ## This function generates points
    ## with varying levels of population
    ## according to a normal distribution
    ## with specified mean and variance
    ## latitude = latitude of the center
    ## longitud = longitud of the center
    ## spread = spread of the population generation
    ## n_big = number of simulated instances of
    ## large populations
    ## n_small = number of simulated instances of
    ## small populations
    ## of highly populated towns.
    ## of lowly populated towns.
    ##-------------------------------------
    ## big towns
    big_lat   <- rnorm(n_big, latitude, big_var)
    big_lon   <- rnorm(n_big, longitud, big_var)
    big_pop  <- rnorm(n_big, 10000, 1000)
    big_towns <- list(lat = big_lat, lon = big_lon, pop = big_pop)
    ## small towns
    small_lat <- list()
    small_lon <- list()
    small_pop <- list()
    for(i in 1:n_big){
        small_lat[[i]] <- rnorm(n_small, big_lat[i], small_var)
        small_lon[[i]] <- rnorm(n_small, big_lon[i], small_var)
        small_pop[[i]] <- rnorm(n_big, 600, 100)
    }
    small_towns <- list(lat = small_lat, lon = small_lon, pop = small_pop)
    ## results
    results <- list(big_towns = big_towns,
                   small_towns = small_towns)
    results
}

############################################
############################################
############################################
################# Tests ####################
############################################
############################################
############################################

## Read in data
data <- read.csv("./pob/data/dataCenso.csv", stringsAsFactors = FALSE)
data[,1] <- NULL
names(data) <- c("ent",
                "mun",
                "nom_mun",
                "nom_loc",
                "lon",
                "lat",
                "pob")

## Filter data according to entities
## Baja California 2
## Yucatán 31
## Puebla 21
## Morelos 17
selec <- dplyr::filter(data, ent %in% c("7"))


## Generate data_big
big_boys   <- filter(selec, pob > 10000)
data_big   <- filter(selec, pob > 1000 & pob < 10000)
data_small <- filter(selec, pob > 100 & pob < 1000)

## Generate cumulated data
big_boys$class   <- rep("Core Network", nrow(big_boys))
data_big$class   <- rep("Secondary Network", nrow(data_big))
data_small$class <- rep("Terciary Network", nrow(data_small))
class_data <- rbind(big_boys, data_big, data_small)
write.csv(class_data, "data_tess.csv", row.names = FALSE)



#######################################
##-------------------------------------
#######################################
## Connectivity data: big_boys
connect_data   <- get_connect(big_boys$lat, big_boys$lon)
filter_connect <- llply(connect_data, function(t)t <- t$networkRank[[1]])
filter_connect <- filter_connect[!laply(filter_connect,is.null)]
filter_connect <- filter_connect[laply(filter_connect, length) >= 3]
connect_relia  <- llply(filter_connect, function(t) t <- t[[3]])
connect_relia  <- connect_relia[laply(connect_relia, length) >= 11]
relia <- extract_numeric(laply(connect_relia, function(t) t <- t[10]))
## save results

connect_big_boys <- toJSON(connect_data)
write(connect_big_boys, "connect_big_boys.json")
connect_big_boys <- RJSONIO::fromJSON("connect_big_boys.json")

## Connectivity data: data_big
connect_data   <- get_connect(data_big$lat, data_big$lon)
filter_connect <- llply(connect_data, function(t)t <- t$networkRank[[1]])
filter_connect <- filter_connect[!laply(filter_connect,is.null)]
filter_connect <- filter_connect[laply(filter_connect, length) >= 3]
connect_relia  <- llply(filter_connect, function(t) t <- t[[3]])
connect_relia  <- connect_relia[laply(connect_relia, length) >= 11]
relia <- extract_numeric(laply(connect_relia, function(t) t <- t[10]))
## save results
connect_data_big <- toJSON(connect_data)
write(connect_data_big, "connect_data_big.json")

#######################################
##-------------------------------------
#######################################
### Tesselation BIG BOYS ##############
tess <- deldir(big_boys$lon, big_boys$lat)
vor  <- tess$dirsgs

#######################################
#######################################
#######################################
######## Generate Shape file ##########
#######################################
w <- tile.list(tess)

polys <- vector(mode="list", length=length(w))

for (i in seq(along=polys)) {
   pcrds <- cbind(w[[i]]$x, w[[i]]$y)
   pcrds <- rbind(pcrds, pcrds[1,])
   polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}

SP <- SpatialPolygons(polys)
SPDF <- SpatialPolygonsDataFrame(SP,
                                data=data.frame(x=big_boys$lon,
                                                y=big_boys$lat,
                                                row.names=row.names(SP)))
plot(SPDF)
writeOGR(obj=SPDF, dsn="SPDF", layer="SPDF",driver="ESRI Shapefile")
#######################################
##-------------------------------------
#######################################

#######################################
#######################################
#######################################
### specific set
### We are going to focus on this region
### due to lack of connectivity
######################################
vor_s <- dplyr::filter(vor, ind2 == "2")
## Poligon
poly  <-  cbind(c(vor_s$x1, vor_s$x2),
               c(vor_s$y1, vor_s$y2))

### points in set
## data big
pnts  <- data_big[,c(5,6)]
#pnts  <- big_boys[,c(5,6)]

## check
in_pol <- pnt.in.poly(pnts, poly)
data_big_pol <- data_big[in_pol[,3] == 1,]
#data_big_pol <- big_boys[in_pol[,3] == 1,]

####
## data small
pnts  <- data_small[,c(5,6)]
## check
in_pol <- pnt.in.poly(pnts, poly)
data_small_pol <- data_small[in_pol[,3] == 1,]

#######################################
##-------------------------------------
#######################################
### Tesselation BIG DATA ##############
### Use previous result
#######################
tess <- deldir(data_big_pol$lon, data_big_pol$lat)
vor  <- tess$dirsgs


#######################################
#######################################
#######################################
######## Generate Shape file ##########
#######################################
w <- tile.list(tess)

polys <- vector(mode="list", length=length(w))

for (i in seq(along=polys)) {
   pcrds <- cbind(w[[i]]$x, w[[i]]$y)
   pcrds <- rbind(pcrds, pcrds[1,])
   polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}

SP <- SpatialPolygons(polys)
SPDF <- SpatialPolygonsDataFrame(SP,
                                data=data.frame(x=data_big_pol$lon,
                                                y=data_big_pol$lat,
                                                row.names=row.names(SP)))
plot(SPDF)
writeOGR(obj=SPDF, dsn="SPDFs", layer="SPDFs",driver="ESRI Shapefile")
#######################################
##-------------------------------------
#######################################


############################################################################
####### Conectivity in region
############################################################################
## Connectivity data: data_big
connect_data   <- get_connect(data_big_pol$lat, data_big_pol$lon)
##
filter_connect <- llply(connect_data, function(t)t <- t$networkRank[[1]])
index_data     <- which(laply(filter_connect, length) >= 3)

filter_connect <- filter_connect[!laply(filter_connect,is.null)]
filter_connect <- filter_connect[laply(filter_connect, length) >= 3]
connect_relia  <- llply(filter_connect, function(t) t <- t[[3]])
## connect_relia  <- connect_relia[laply(connect_relia, length) >= 11]
RSSI_ASU <- extract_numeric(laply(connect_relia, function(t) t <- t[4]))

connect_data_coords_big <- ldply(connect_data, function(t)t <- c(t$latitude,
                                                           t$longitude))
connect_data_coords_big <- connect_data_coords_big[index_data,]
connect_data_coords_big$RSSI_ASU <- RSSI_ASU
connect_data_coords <- rbind(connect_data_coords, connect_data_coords_big)
write.csv(connect_data_coords, "connect_rssi_big.csv", row.names = FALSE)
## save results
##connect_data_big <- toJSON(connect_data)
##write(connect_data_big, "connect_data_big.json")
############################################################################

################################################
## GRAPH TESSELATION
################################################
ggplot(data = data_big_pol, aes(x = lon, y = lat, size = pob)) +
    geom_point(col = "blue",
               alpha = .5
               ) +
    geom_point(data = data_small_pol, aes(x = lon, y = lat),
               col = "orange",
               alpha = .7,
               size = 1) +
    geom_segment(
        data = vor_s,
        aes(
            x = x1,
            y = y1,
            xend = x2,
            yend = y2
        ),inherit.aes=FALSE,
        lwd = 1,
        col = "pink",
        alpha = .6
    ) +
        geom_segment(
        data = vor_s,
        aes(
            x = x1,
            y = y1,
            xend = x2,
            yend = y2
        ),inherit.aes=FALSE,
        lwd = 1,
        col = "pink",
        alpha = .6
    ) +
    theme(panel.background = element_blank())

################################################
## GRAPH Prim
################################################
#######################################
##-------------------------------------
#######################################
### Tesselation BIG DATA ##############
### Use previous result  ##############
##############################################
tess <- deldir(data_big_pol$lon, data_big_pol$lat)
vor  <- tess$dirsgs

#######################################
#######################################
#######################################
### specific set
### We are going to focus on this region
### due to lack of connectivity
######################################
full_tree <- list()
full_tess <- tile.list(tess)
for(trees in 1:length(full_tess)){
    ## vor_s <- dplyr::filter(vor, ind2 == "2")
    ## Poligon
    ##poly  <-  cbind(c(vor_s$x1, vor_s$x2),
    ##               c(vor_s$y1, vor_s$y2))
    poly <- cbind(full_tess[[trees]]$x,
                 full_tess[[trees]]$y)
    ## points in set
    ## data small
    pnts  <- data_big[,c(5,6)]

    ## check
    in_pol       <- pnt.in.poly(pnts, poly)
    data_big_pol <- data_big[in_pol[,3] == 1,]

    ## data small
    pnts  <- data_small[,c(5,6)]

    ## check
    in_pol <- pnt.in.poly(pnts, poly)
    data_small_pol <- data_small[in_pol[,3] == 1,]

#############################################
## Data prim
#############################################
    data_prim <- rbind(data_small_pol, data_big_pol)

    ## cost distance
    x <- c()
    y <- c()
    pob <- c()
    n <- nrow(data_prim)
    k <- 1
    for(j in 1:n){
        for(i in 1:n){
            x[k] <- data_prim$lon[j]
            y[k] <- data_prim$lat[j]
            pob[k] <- data_prim$pob[j]
            k <- k + 1
        }
    }
    xend <- rep(data_prim$lon,n)
    yend <- rep(data_prim$lat,n)
    pob  <- pob + rep(data_prim$pob,n)
    ## data_cost
    data_cost <- data.frame(x = x,
                           y = y,
                           xend = xend,
                           yend = yend,
                           pob  = pob )
    o_id <- paste0(data_cost[,1],data_cost[,2])
    d_id <- paste0(data_cost[,3],data_cost[,4])
    data_cost <- data_cost[o_id != d_id,]
    o_id <- paste0(data_cost[,1],data_cost[,2])
    d_id <- paste0(data_cost[,3],data_cost[,4])
    calculated <- c()
    cost       <- c()
    for(i in 1:nrow(data_cost)){
        id_in_1 <- paste0(o_id[i], d_id[i])
        id_in_2 <- paste0(d_id[i], o_id[i])
        if(!(id_in_1 %in% calculated & id_in_2  %in% calculated)){
            dist <- get_distance(
                c(data_cost$y[i],
                  data_cost$x[i]),
                c(data_cost$yend[i],
                  data_cost$xend[i])
            )
            cost[i] <- dist$distance
            calculated <- c(calculated, id_in_1, id_in_2)
        }else{
            cost[i] <- cost[
                which(calculated == id_in_1 |
                      calculated == id_in_2)
            ]
        }
    }
    ## The nodes with no roads are marked with
    ## a distance equal to 1e10
    data_cost$dist <- cost
    data_cost$dist[is.na(data_cost$dist)] <- 1e10
    data_cost$cost <- extract_numeric(data_cost$dist)/data_cost$pob
    full_tree[[trees]] <- data_cost
    print(trees)
}

mstree <- full_tree[[8]][,c(1,2,3,4,7)]
names(mstree) <- c("x","y","xend","yend","p")
MStree <- prim(mstree)

## save results
full_tree_json <- toJSON(full_tree)
write(full_tree_json, "full_tree.json")

## make lines shapes
all_trees <- ldply(full_tree, function(t) t <- t)
all_prims <- ldply(full_tree, function(t) t <- {t <- t[,c(1,2,3,4,7)];names(t) <- c("x","y","xend","yend","p");prim(t)})

## Plot results
png("plot_net.png")
ggplot(data = all_trees,
       aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_segment(col = "purple", alpha = .1) +
    geom_segment(data = all_prims,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 col = "#ffc107", alpha = .8) +
    geom_point(data = all_prims,
               aes(x = xend, y = yend),
               col = "#ffc107",size = 3, alpha = .8
               ) +
    geom_segment(data = big_tree,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 col = "#1F78B4", lwd = 1, alpha =.8
                 )+
    geom_point(data = big_tree,
               aes(x = x, y = y),
               col = "#1F78B4", size = 8, alpha =.7
               ) +
    theme(panel.background =  element_rect(fill = '#050112'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          text = element_blank())


dev.off()

## escribimos shape
lines_graph <- list()
for(i in 1:nrow(all_data)){
    lines_graph[[i]] <-  Lines(Line(rbind(all_data[i,1:2], setNames(all_data[i,3:4], c("x","y")))),ID = i)
}

coorddf <-  SpatialLinesDataFrame(Lines(lines_graph))

writeOGR(coorddf, dsn = '.', layer = 'poly', driver = "ESRI Shapefile")


##################
## Plan B approach
##################
cost <- c()
for(i in 1:nrow(data_cost)){
    dist <- euc(
        c(data_cost$x[i], data_cost$y[i]),
        c(data_cost$xend[i], data_cost$yend[i])
                    )
    cost[i] <- dist
}


### Plan C
euc <- function(x1, x2){
    sqrt((x1[1] - x2[1])^2 + (x1[2]-x2[2])^2)
}

data_cost$cost <- cost/data_cost$pob
names(data_cost) <- c("x","y","xend","yend","pob","p")
data_big_prim <- data_cost[,-5]
big_tree <- prim(data_cost[,-5])
