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
keys <- c(
)
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
write.csv(h.all,
          "../data/output/IDorthoAltitudes/heights30150.csv",
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
grad.plot <- ggplot(data = test,
       aes(x = cumsum(distance)/1000, y = gradient)) + geom_line(color="#1972b9",
                                              alpha=.7) +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#1972b9")) +
ylab("Gradient") + xlab("Kilometers")
ggsave("../graphs/gradientOrtho.png", grad.plot, width = 18, height = 10)
