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
grid     <- 200000                                   # Number of cells
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
