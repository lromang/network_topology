##########################################
##
## Luis Manuel Román García
## luis.roangarci@gmail.com
##
## ---------------------------------------
##
## Libraries for general geo-statistical
## manipulation.
##########################################

## JSON manipulatino
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(RJSONIO))
## Urls manipulation
suppressPackageStartupMessages(library(RCurl))
## Manejo de arreglos
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
## Manejo de cadenas de caracteres
suppressPackageStartupMessages(library(stringr))
## Manejo de data frames
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(foreign))
## Predicción
suppressPackageStartupMessages(library(caret))
## Geoespacial
suppressPackageStartupMessages(library(geosphere))
suppressPackageStartupMessages(library(maps))
suppressPackageStartupMessages(library(maptools))
suppressPackageStartupMessages(library(spatstat))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(rgdal))
## Gráficas
suppressPackageStartupMessages(library(ggplot2))
## Otros
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(deldir))
suppressPackageStartupMessages(library(rje))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(SDMTools))
suppressPackageStartupMessages(library(PBSmapping))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(prevR))
suppressPackageStartupMessages(library(foreign))

########################################
## Functions
########################################

##-------------------------------------
## get_orthonormal
##-------------------------------------
get_orthonormal <- function(point1, point2){
    dir  <- point2 - point1
    if(dir[1] == dir[2] & dir[2] == 0){
        orth <- dir
    }else{
        orth <- c(-dir[2], dir[1])/sqrt(dir[1]^2 + dir[2]^2)
    }
    orth
}

##-------------------------------------
## gen_inter_point
##-------------------------------------
gen_inter_point  <- function(point1, point2, step = 20){
    dir          <- point2 - point1
    ## Save first point
    inter_points <- data.frame("x" = point1[1],
                              "y" = point1[2])
    if((dir[1]^2 + dir[2]^2) > 0){
        dir      <- dir / sqrt(dir[1]^2 + dir[2]^2)
        ## Get Distance between points
        dist     <- distCosine(point1, point2)
        iters    <- floor(dist / step)
        ## Generate points
        step_point   <- point1
        for(i in 1:iters){
            step_point   <- step_point + dir*((step / r_earth) * (180 / pi))
            inter_points <- rbind(inter_points, step_point)
        }
        inter_points <- rbind(inter_points, point2)
    }
    ## Return
    inter_points
}

##-------------------------------------
## gen_multi_inter_point
##-------------------------------------
gen_multi_inter_point <- function(points){
    all_points <- c()
    for(i in 1:(nrow(points) - 1)){
        all_points <- rbind(all_points, gen_inter_point(points[i,],
                                                       points[i + 1,]))
        print(i)
    }
    ## Add id
    all_points$id <- 1:nrow(all_points)
    ## Return points
    all_points
}

##-------------------------------------
## gen_multi_squares
##-------------------------------------
gen_multi_squares <- function(points){
    points <- gen_multi_inter_point(points)
    ## Gen Squares
    all_squares <- c()
    for(i in 1:(nrow(points) - 1)){
        all_squares <- rbind(all_squares,
                            orthonormal_square(c(points$x[i], points$y[i]),
                                               c(points$x[i + 1], points$y[i + 1]),
                                               dist = 10,
                                               id   = points$id[i])
                            )
        print(i)
    }
    ## PLOT
    print(ggplot(data = all_squares,
       aes(x = x, y = y, col = id)) + geom_polygon(alpha = .3) +
    theme(legend.position = "none"))

    ## RETURN
    all_squares
}

##-------------------------------------
## orthonormal_square
##-------------------------------------
orthonormal_square <- function(point1, point2, dist, id = 1,
                              r_earth = 6378137, plot = FALSE){
    dir      <- get_orthonormal(point1, point2)
    ## Left point
    pointul   <- point1  + dir*((dist / r_earth) * (180 / pi))
    pointll   <- point1  - dir*((dist / r_earth) * (180 / pi))
    ## Lat 1
    pointur   <- point2  + dir*((dist / r_earth) * (180 / pi))
    pointlr   <- point2  - dir*((dist / r_earth) * (180 / pi))
    ## Points as Data Frame
    points <- data.frame("x" = c(pointul[1],
                                pointll[1],
                                pointlr[1],
                                pointur[1]),
                        "y" = c(pointul[2],
                                pointll[2],
                                pointlr[2],
                                pointur[2]),
                        "id" = rep(paste0(id), 4))
    if(plot){
        print(ggplot(data = points, aes(x = x, y = y)) + geom_polygon(alpha = .3))
    }
    points
}

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
## get_address
##-------------------------------------
get_cp <- function(point){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving directions between two given points.
    ## point  = geografic point in (longitude, latitude) format
    ##-------------------------------------
    addr   <- revgeocode(point, output = "all")
    result <- tryCatch({
        addr$results[[1]]$address_components[[9]]$short_name
    }, warning = function(w) {
        NA
    }, error = function(e) {
        NA
    })
    result
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
                         paste(origin[,2:1], collapse = ",")
                         )
    destiny     <- paste0("destinations=",
                         paste(destiny[,2:1], collapse = ",")
                         )
    key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
    query       <- paste(base, origin, destiny, key, sep = "&")
    results     <- fromJSON(getURL(query))
    distance    <- results$rows[[1]]$elements[[1]]$distance$value
    distance
}

##-------------------------------------
## distance_matrix
##-------------------------------------
distance_matrix <- function(origin, destiny){
    ##-------------------------------------
    ## origin and destiny are arrays
    ##-------------------------------------
    all_dist_m <- data.frame(matrix(0, nrow(origin), nrow(destiny)))
    for(i in 1:nrow(origin)){
        for(j in 1:nrow(destiny)){
            all_dist_m[i, j] <- get_distance(origin[i, ],
                                            destiny[j, ]
                                            )
        }
    }
    all_dist_m
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
## get_altitude
##-------------------------------------
get_altitude <- function(locations){
    ##-------------------------------------
    ## This function uses Google's API "elevation" to
    ## calculate the altitude of the  points given.
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
    ## key         <- "key=AIzaSyB_Y84utrZHkMK6LMu4Wy0lAPRQdjE-MdE"
    ## key         <- "key=AIzaSyDGpP7-Zq7XMiuoeL2tgG6mjof4PmKeyVY"
    ## key         <- "key=AIzaSyCuk-zf-TSwoXy_VJO-CKyiLp6fuc_wd7c"
    ## key         <- "key=AIzaSyDI8dbWHa6LcYeLpi7WLn0Xko_A0WsBoa8"
    ## key         <- "key=AIzaSyDsOCeoZK5dnah0EaThfDoZlip4kzTgtAc"
    ## key         <- "key=AIzaSyCSzRpseAAEASuzDKLRYC50FQlx6x5psxU"
    ## key         <- "key=AIzaSyAkxFdycMr8COtHKfh667INihNC1yM9_Mg"
    query       <- paste(locations, key, sep = "&")
    fromJSON(getURL(query))
}

get_altitudes_matrix <- function(locations){
    ##
    ## Locations in a lat long format
    ##
    base      <- "https://maps.googleapis.com/maps/api/elevation/json?locations="
    locations <- paste(
        paste(locations[,1], locations[,2], sep = ","),
        collapse = "|"
    )
    key         <- "your key"
    query       <- paste0(base, locations)
    query       <- paste(query, key, sep = "&")
    fromJSON(getURL(query))
}

##-------------------------------------
## transform coord
##-------------------------------------
trans_coord <- function(coord, pow = 1){
    length <- str_length(coord)
    sec    <- str_sub(coord, length - 1, length)
    min    <- str_sub(coord, length - 3, length - 2)
    deg    <- str_sub(coord, 1, length - 4)
    ## Default transform lon.
    (-1)^pow * (extract_numeric(deg) + extract_numeric(min) / 60 +
      extract_numeric(sec) / 3600)
}

##-------------------------------------
## tesselate
##-------------------------------------
## Coordenadas de bloque de interés.
tesselate <- function(grids,
                     map          = NULL,
                     alpha        = .3,
                     top_left     = c(-117.1276, 32.7225),
                     bottom_left  = c(-117.1276, 30.8636),
                     top_right    = c(-114.7012, 32.7225),
                     bottom_right = c(-114.7012, 30.8636)){
  results    <- list()
  intercepts <- ceiling(sqrt(grids+1))
  h_lines    <- data.frame(x    = rep(top_left[1], intercepts),
                          y    = seq(top_left[2], bottom_left[2],
                                     length = intercepts),
                          xend = rep(top_right[1], intercepts),
                          yend = seq(top_right[2], bottom_right[2],
                                      length = intercepts))
  v_lines <- data.frame(x = seq(top_left[1], top_right[1],
                               length = intercepts),
                        y = rep(top_left[2], intercepts),
                       xend = seq(bottom_left[1], bottom_right[1],
                                  length = intercepts),
                        yend = rep(bottom_right[2],intercepts))
  if (! is.null(map)){
      map    <- map +  geom_hline(data = h_lines, aes(yintercept = y),
                                 col = "red",
                                 alpha = alpha)
      map    <- map +  geom_vline(data = v_lines, aes(xintercept = x),
                                 col = "red",
                                 alpha = alpha)
}
  x <- seq(top_left[1], top_right[1], length = intercepts)
  y <- seq(top_right[2], bottom_right[2],length = intercepts)
  k = 1
  xcoord <- c()
  ycoord <- c()
  for (i in 1:(length(y)-1)){
    for(j in 1:(length(x)-1)){
      xcoord[k] <- (x[j] + x[j + 1])/2
      ycoord[k] <- (y[i] + y[i + 1])/2
      k = k + 1
    }
  }
  points <- data.frame(x = xcoord, y = ycoord)
  results[[1]] <- map
  # ordenadas
  results[[2]] <- seq(top_left[2], bottom_left[2],length = intercepts)
  # abscisas
  results[[3]] <- seq(bottom_left[1],bottom_right[1],length = intercepts)
  # area aproximada de la celda
  results[[4]] <- (abs((bottom_right[1]-top_left[1]))*
                  abs((top_left[2]-bottom_right[2])))/grids
  # centro de la celda
  results[[5]] <- points

  return(results)
}


##-------------------------------------
## blocks
##-------------------------------------
blocks <- function(ordinates, abscises){
  block <- list()
  k        <- 1
  for (i in 1:(length(ordinates)-1)){
    for ( j in 1:(length(abscises)-1)){
      entry      <- list()
      entry[[1]] <- c(abscises[j], ordinates[i])  # upper left
      entry[[2]] <- c(abscises[j + 1], ordinates[i + 1])  # bottom right
      block[[k]] <- entry
      k = k + 1
    }
  }
  block
}



##-------------------------------------
## in.block
##-------------------------------------
in.block <- function(block, pop){
  pop.block <- list()
  for ( i in 1:length(block)){
  print(i)
  list         <- block[[i]]
  pop.xreduce  <- c()
  pop.xyreduce <- c()
  # Obtiene localidades cuyas coordenadas caigan dentro de la celda i
  pop.xreduce  <- pop[pop[, 1] > as.numeric(list[[1]][1]), ]
  pop.xreduce  <- pop.xreduce[pop.xreduce[, 1] < as.numeric(list[[2]][1]), ]
  pop.xyreduce <- pop.xreduce[pop.xreduce[, 2] > as.numeric(list[[2]][2]), ]
  pop.xyreduce <- pop.xyreduce[pop.xyreduce[, 2] < as.numeric(list[[1]][2]), ]
  ## Una vez seleccionadas las localidades obtiene datos relevantes.
  data.block         <- list()
  data.block[[1]]    <- list                          ## coordenadas extremas de cada celda
  pop.xyreduce$celda <- rep(i,nrow(pop.xyreduce))
  data.block[[2]]    <- pop.xyreduce                  ## datos de las localidades
  data.block[[3]]    <- sum(pop.xyreduce[, 3])        ## población por celda
  data.block[[4]]    <-  areaPolygon(matrix(
      c(data.block[[1]][[1]][1],data.block[[1]][[1]][2],
        data.block[[1]][[2]][1],data.block[[1]][[1]][2],
        data.block[[1]][[2]][1],data.block[[1]][[2]][2],
        data.block[[1]][[1]][1],data.block[[1]][[2]][2] ),
      nrow = 4,
      ncol = 2,
      byrow = TRUE ) )/1e6 ## area de cada celda km^²
  ## data.block[[5]] <- sd(pop.xyreduce[, 4])              # desviación estandar de las altitudes
  pop.block[[i]]  <- data.block
  }
  pop.block
}

## Datos más específicos por celda
in.block.fac <- function(block, pop){
  pop.block <- list()
  for ( i in 1:length(block)){
    print(i)
    list                  <- block[[i]]
    pop.xreduce  <- c()
    pop.xyreduce <- c()
    ## Obtiene localidades cuyas coordenadas caigan dentro de la celda i
    pop.xreduce  <- pop[pop[, 1] > as.numeric(list[[1]][1]), ]
    pop.xreduce  <- pop.xreduce[pop.xreduce[, 1] < as.numeric(list[[2]][1]), ]
    pop.xyreduce <- pop.xreduce[pop.xreduce[, 2] > as.numeric(list[[2]][2]), ]
    pop.xyreduce <- pop.xyreduce[pop.xyreduce[, 2] < as.numeric(list[[1]][2]), ]
    ## Una vez seleccionadas las localidades obtiene datos relevantes.
    data.block         <- list()
    data.block[[1]] <- list                    ## coordenadas extremas de cada celda
    pop.xyreduce$celda <- rep(i,nrow(pop.xyreduce))
    data.block[[2]] <- pop.xyreduce            ## datos por celda
    data.block[[3]] <- nrow(pop.xyreduce)      ## total apariciones por celda
    data.block[[4]] <-  areaPolygon(matrix(
        c(data.block[[1]][[1]][1],data.block[[1]][[1]][2],
          data.block[[1]][[2]][1],data.block[[1]][[1]][2],
          data.block[[1]][[2]][1],data.block[[1]][[2]][2],
          data.block[[1]][[1]][1],data.block[[1]][[2]][2] ),
        nrow = 4,
        ncol = 2,
        byrow = TRUE ) )/1e6 ## area de cada celda m^²
    pop.block[[i]]   <- data.block
  }
  pop.block
}


##-------------------------------------
## coordExtract
##-------------------------------------
coordExtract <- function(shape){
    coords     <- list()
    polygons   <- shape@polygons
    for(i in 1:length(polygons)){
        coords[[i]]       <- polygons[[i]]@Polygons[[1]]@coords
        ## coords[[i]]       <- coords[[i]][,c(1,2)]
    }
    coords
}

resRelevant <- function(list){
    ldply(list, function(t){
    result    <- list()
    result[[1]] <-  t[[1]]                  ## Coordenadas de cada celda.
    result[[2]] <-  nrow(t[[2]])            ## Número de localidades por celda.
    result[[3]] <-  t[[3]]                  ## Población por celda.
    result[[4]] <-  t[[5]]                  ## Área por cada celda.
    result
    })
}
