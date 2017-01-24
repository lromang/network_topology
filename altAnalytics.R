source('./tessFunc.R')

## ---------------------------------
## MAP
## ---------------------------------
map         <- get_map(location = "Nogales",
                      zoom     = 6,
                      maptype  = "roadmap")
map.plot  <- ggmap(map)

## ---------------------------------
## Read Data
## ---------------------------------

## Denue
denue        <- read.dbf("../data/denue/denue_front.dbf")
denue.filter <- denue[,c(40, 39)]

## Censo
censo <- read.dbf("../data/censo/censo_front.dbf")
censo.filter <- censo[,c(8,9,10)]

## ---------------------------------
## Tesselate
## ---------------------------------
grid     <- 40000                                    # Number of cells
tes      <- tesselate(grid,  map.plot, alpha = .05)  # Partition
block    <- blocks(tes[[2]], tes[[3]])               # Cell creation

## Partition Denue
cell_den <- in.block.fac(block,  denue.filter)       # Cell characteristics

## Partition Censo
cell_cen <- in.block.fac(block, censo.filter)        # Cell characteristics

## ------------------------------
## Analysis
## ------------------------------

## Área por celda
area_den <- laply(cell_den, function(t)t <- t[[4]])

## Observaciones por celda
obs_den  <- laply(cell_den, function(t)t <- t[[3]])

## ------------------------------
## Data to Shape
## ------------------------------
all_blocks_shp <- list()

## Build shape
for(i in 1:length(block)){
    cell      <- block[[i]]
    block_shp <- data.frame(
        x = c(rep(cell[[1]][1], 2),
              rep(cell[[2]][1], 2)
              ),
        y = c(rep(c(cell[[1]][2],
                    cell[[2]][2]), 2))
    )
    block_shp[c(3,4), ] <- block_shp[c(4,3), ]

    ## Convertir a Polígono
    block_shp           <- Polygon(block_shp)
    block_shp           <- Polygons(list(block_shp), paste0(i))
    all_blocks_shp[[i]] <- block_shp
}

## Aquí van a ir todos.
block_shp <- SpatialPolygons(all_blocks_shp)

poly      <- SpatialPolygonsDataFrame(
    block_shp,
    data.frame(
        PIDS      = paste(seq(1, length(block), 1), sep = "\n"),
        econUnit  = obs_den,
        area      = area_den
    )
)

## Escribir resultados
writeOGR(block_centroids,
         "../data/",
         "test_centroids",
         driver = "ESRI Shapefile")



## ------------------------------
## Change Zip Code
## ------------------------------
## zip_code_nl    <- readOGR("../data/zip_code/cp_mon",
##                         "CP_mon")
## proj4string(zip_code_nl) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
## zip_test <- spTransform(zip_code_nl, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
## writeOGR(zip_test,
##          "../data/output/zip_nl",
##         "new_nl",
##         driver = "ESRI Shapefile")

## Get central points
centralPoints <- function(b.data, n.points = 1){
    points <- list()
    for(i in 1:nrow(b.data)){
        points[[i]] <- gcIntermediate(c(b.data$X_MAX[i],
                                       b.data$Y_MAX[i]),
                                     c(b.data$X_MIN[i],
                                       b.data$Y_MIN[i]),
                                     n.points)[2:1]
    }
    points
}


## --------------------------------
## Blocks
## Here is where we start executing
## for getting the altitudes
## --------------------------------
all_centers <- c()
for(i in 1:36){
    blocks          <- readOGR(paste0("../data/tramos/",i,"/"),
                          paste0(i))
    block_centroids <- gCentroid(blocks, byid=TRUE)
    all_centers     <- rbind(all_centers, block_centroids@coords)
    print(i)
}

## blocks_data <- read.dbf("../data/tramos/1/1.dbf")
## Get centers
## centers <- centralPoints(blocks_data)

## -----------------------------------------------------
## Esto es lo que hay que paralelizar!!!!
## Get Altitudes
centers <- block_centroids@coords[, 2:1]
values  <- seq(10001, 12001, 200)
for(i in 1:(length(values) - 1)){
    altitudes <- get_altitudes_matrix(
        centers[values[i]:min(values[i + 1], nrow(centers)),])
    ## Only Altitudes
    only.altitudes <- laply(altitudes[[1]], function(t)t <- t$elevation)
    ## Aquí se acaba la paralelización!!!!
    write.csv(only.altitudes,
              paste0("../data/output/orthoAltitudes/altitudes6",
                     i, ".csv")
              )
    Sys.sleep(5)
    print(i)
}
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
## Read data
## -------------------------------------
elev.data <- read.csv("../data/output/blocksAltitudeortho.csv",
                     stringsAsFactors = FALSE)

## PLOT
elev.data$meters <- elev.data$block * 20

data.grad <- data.frame("meters" = elev.data$meters[2:nrow(elev.data)],
                       "grad"   =abs(diff(elev.data$elevation)/diff(elev.data$meters)))

## Elevation
elev.plot <- ggplot(data = elev.data,
       aes(x = meters, y = elevation)) + geom_line(color="#1972b9",
                                              alpha=.7) +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#1972b9")) +
    ylab("Elevation") + xlab("Meters")
ggsave("../graphs/elevationsOrtho.png", elev.plot, width = 18, height = 10)
## Gradient
grad.plot <- ggplot(data = data.grad,
       aes(x = meters, y = grad)) + geom_line(color="#1972b9",
                                              alpha=.7) +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#1972b9")) +
ylab("Gradient") + xlab("Meters")
ggsave("../graphs/gradientOrtho.png", grad.plot, width = 18, height = 10)

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
