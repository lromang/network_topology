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
writeOGR(poly,
         "../data/output/blocks/",
         "first_frag",
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

## --------------------------------
## Blocks
## --------------------------------
