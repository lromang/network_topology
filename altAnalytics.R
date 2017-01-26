source('./tessFunc.R')

## --------------------------------
## Blocks
## Read in blocks
## --------------------------------
## Do it block by block!!!!
all_centers <- c()
for(i in 2:36){
    blocks               <- readOGR(paste0("../data/tramos/", i,"/"),
                                   paste0(i))
    block_centroids      <- gCentroid(blocks, byid=TRUE)
    id_centroids         <- as.data.frame(block_centroids@coords)
    id_centroids$id      <- as.array(blocks@data$id)
    id_centroids$chunkID <- rep(i, nrow(id_centroids))
    all_centers          <- rbind(all_centers, id_centroids)
    print(i)
}
all_centers[, 1:2]      <- all_centers[, 2:1]
names(all_centers)[1:2] <- names(all_centers)[2:1]

## -------------------------------------
## Get Distances
## -------------------------------------
distances    <- c()
distances[1] <- 0
for(i in 2:nrow(all_centers)){
    distances[i] <- distCosine(all_centers[i, 2:1],
                              all_centers[i - 1, 2:1])
}
all_centers$distance <- distances

## -------------------------------------
## Get Heights
## -------------------------------------

h.centers <- get_all_altitudes(all_centers, keys)

## -------------------------------------
## Get gradient
## -------------------------------------
grad    <- c()
grad[1] <- 0
grad    <- c(grad, abs(diff(h.centers$altitudes)/diff(cumsum(h.centers$distance))))
h.centers$gradient <- grad

## -------------------------------------
## Save data
## -------------------------------------
write.csv(h.centers,
          "../data/output/IDorthoAltitudes/heights16080.csv",
          row.names = FALSE)

## -------------------------------------
## Plot results
## -------------------------------------

## Elevation
elev.plot <- ggplot(data = h.centers,
       aes(x = cumsum(distance)/1000, y = altitudes)) + geom_line(color="#1972b9",
                                              alpha=.7) +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#1972b9")) +
    ylab("Elevation") + xlab("Kilometers")
ggsave("../graphs/elevationsOrtho.png", elev.plot, width = 18, height = 10)

## Gradient
grad.plot <- ggplot(data = h.centers,
       aes(x = cumsum(distance)/1000, y = gradient)) + geom_line(color="#1972b9",
                                              alpha=.7) +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#1972b9")) +
ylab("Gradient") + xlab("Meters")
ggsave("../graphs/gradientOrtho.png", grad.plot, width = 18, height = 10)

######################################################################
######################################################################
######################################################################
######################################################################
######################################################################

## blocks_data <- read.dbf("../data/tramos/1/1.dbf")
## Get centers
## centers <- centralPoints(blocks_data)

## -----------------------------------------------------
## Esto es lo que hay que paralelizar!!!!
## Get Altitudes
## centers <- block_centroids@coords[, 2:1]
## values  <- seq(6001, 8001, 200)
## for(i in 1:(length(values) - 1)){
##    altitudes <- get_altitudes_matrix(
##        centers[values[i]:min(values[i + 1], nrow(centers)),])
    ## Only Altitudes
##    only.altitudes <- laply(altitudes[[1]], function(t)t <- t$elevation)
    ## Aquí se acaba la paralelización!!!!
##    write.csv(only.altitudes,
##              paste0("../data/output/orthoAltitudes/altitudes9991",
##                     i, ".csv")
##              )
##    Sys.sleep(5)
##    print(i)
## }
## -----------------------------------------------------

## Read in all altitudes
files <- list.files("../data/output/orthoAltitudes/.")
## Get altitudes
all.altitudes  <- llply(files, function(t)t <- read.csv(paste0("../data/output/orthoAltitudes/", t)))
all.altitudes.data <- c()
for(i in 1:length(all.altitudes)){
    all.altitudes.data <- c(all.altitudes.data, all.altitudes[[i]]$x)
}
## Save altitudes
data.altitudes <- data.frame("block" = blocks@data$id,
                            "elevation" = all.altitudes.data[1:nrow(centers)],
                            "lon" = centers[,2],
                            "lat" = centers[,1])
ggplot(data = data.altitudes,
       aes(x = block, y = elevation)) + geom_line()

## -------------------------------------
## Save data
## -------------------------------------
## array.centers <- ldply(centers, function(t) t <- t[2:1])
## names(array.centers) <- c("longitude", "latitude")
## data.altitudes.c <- cbind(na.omit(data.altitudes), array.centers)
write.csv(data.altitudes, "../data/output/blocksAltitudeortho.csv", row.names = FALSE)

## PLOT
ggplot(data = data.altitudes.c,
       aes(x = longitude, y = latitude, color = elevation)) + geom_point()


## -------------------------------------
## Add Data to blocks
## -------------------------------------
blocks@data$elevation <- elev.data$elevation


## writeOGR(zip_test,
##          "../data/output/zip_nl",
##         "new_nl",
##         driver = "ESRI Shapefile")

## -------------------------------------
## Working with The data points
## -------------------------------------
b.points   <- read.csv("../data/borderPoints/nodes_reloaded-buena-clean.csv",
                      stringsAsFactors = FALSE)
coords     <- laply(b.points$wkt_geom, function(t)t <- str_split(t, " "))
b.points$x <- laply(coords, function(t) t <- readr::parse_number(t[1]))
b.points$y <- laply(coords, function(t) t <- readr::parse_number(t[2]))
ggplot(data = b.points, aes(x = x, y = y)) + geom_point()
points     <- data.frame("x" = b.points$x,
                        "y" = b.points$y)
## Gen Squares
all_squares <- gen_multi_squares(points)
## Bind together orthonormal_squares
all_squares <- c()
## b.points <- b.points[order(b.points$OBJECTID, decreasing = TRUE), ]
for(i in 1:(nrow(b.points) - 1)){
    all_squares <- rbind(all_squares,
                        orthonormal_square(c(b.points$x[i], b.points$y[i]),
                                           c(b.points$x[i + 1], b.points$y[i + 1]),
                                           dist = 10,
                                           id   = b.points$id_bueno[i])
                        )
    print(i)
}


ggplot(data = all_squares,
       aes(x = x, y = y, col = id)) + geom_polygon(alpha = .3) +
    theme(legend.position = "none")


ggplot(data = test, aes(x = x, y = y, col = id)) + geom_point()


### ------------------------------
### ------------------------------
### Distance Between Points and
### border.
### ------------------------------
### ------------------------------
new_border <- readOGR("../data/new_border/",
                     "new_border")

denue     <- readOGR("../data/denue/",
                    "denue")

censo     <- readOGR("../data/censo_60/",
                    "censo_60")

pnts.denue <- denue@data[,c(39,40)]
pnts.censo <- censo@data[,c(8, 9)]

shortest.dists <- numeric(nrow(pnts.censo))
for(i in 1:nrow(pnts.censo)){
    shortest.dists[i] <- gDistance(censo[i, ], new_border)
    print(i)
}

pnts.censo$dist <- shortest.dists * 43.496

censo@data$dist <- shortest.dists * 43.496


writeOGR(censo,
         "../data/output/dist60/",
         "censo",
         driver = "ESRI Shapefile")

denue@data$dist <- pnts$dist

writeOGR(denue,
         "../data/output/dist60/",
         "denue",
         driver = "ESRI Shapefile")


write.csv(pnts.censo,
          "../data/output/dist60/dist_border_censo.csv",
          row.names = FALSE)

ggplot(denue@data,
       aes(x = longitud,
           y = latitud,
           col = dist)) + geom_point()
