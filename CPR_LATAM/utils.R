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
google_keys <-  readLines('./google_keys.key')
this_key    <- 1

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
        return(RJSONIO::fromJSON(resp))
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
    resp  <- handle_queries(query)
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
## get distance
##-------------------------------------
get_num_distance <- function(origin,
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
