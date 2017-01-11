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


## ---------------------------------
## Tesselate
## ---------------------------------

## Partition
grid      <- 40000                                    # Number of cells
tes       <- tesselate(grid,  map.plot, alpha = .05)  # Partition
block     <- blocks(tes[[2]], tes[[3]])               # Cell creation
cell_feat <- in.block(block,  pts_denu_nl)            # Cell characteristics

## Data population
data_pop  <- in.block(block, data)
allpop_1  <- laply(data_pop, function(t)t <- t[[3]])

## ------------------------------
## Analysis
## ------------------------------

## Área por celda
areatest_1 <- laply(cell_feat, function(t)t <- t[[4]])

## Observaciones por celda
obs_1      <- laply(cell_feat, function(t)t <- nrow(t[[2]]))
