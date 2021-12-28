# Link to a playlist with most (if not all) xmas songs by Michael Buble:
# https://open.spotify.com/playlist/4F1LJJA4a5qEZdtK3rczD2?si=6cb249e85c5a4793
# and his artist id: 1GxkXlMwML1oSg5eLPiAz3

library(spotifyr)
library(tidyverse)
library(ggrepel)

GeomChristmasTree <- ggproto("GeomChristmasTree", Geom,
                             required_aes = c("x", "y"),
                             default_aes = aes(shape = 19, colour = "black",
                                               fill = "green4", size = 3,
                                               linetype = 1, alpha = 1,
                                               fontsize = 1),
                             draw_key = draw_key_polygon,

                             draw_panel = function(data, panel_scales, coord) {
                                 coords <- coord$transform(data, panel_scales)

                                 # each tree has 4*branches + 3 points
                                 if (length(coords$size) == 1) {
                                     tsize <- rep(pmax(1, round(coords$size)), length(coords$x))
                                     theight <- rep(pmax(0, round(coords$size)), length(coords$x))
                                 } else {
                                     tsize <- pmax(1, round(coords$size))
                                     theight <- pmax(0, coords$size)
                                 }
                                 # scale factors
                                 r01x <- diff(range(coords$x))/100
                                 r01y <- diff(range(coords$y))/100

                                 # coords
                                 longx <- unlist(lapply(seq_along(coords$x), function(i) {
                                     if (tsize[i] == 1) {
                                         dx <- -c(0.3, 0.3, 1.2, 0, -1.2, -0.3, -0.3)
                                     } else {
                                         dx <- -c(0.3, 0.3, rep(c(1.2,0.3), tsize[i]-1), 1.2, 0, -1.2, rep(c(-0.3,-1.2), tsize[i]-1), -0.3, -0.3)
                                     }
                                     r01x*dx + coords$x[i]
                                 }))
                                 longy <- unlist(lapply(seq_along(coords$y), function(i) {
                                     if (tsize[i] == 1) {
                                         dy <- c(-0.5, 0, 0, theight[i], 0, 0, -0.5)
                                     } else {
                                         dy <- c(-0.5, 0, 0, rep(1:(tsize[i]-1), each=2), theight[i], rep((tsize[i]-1):1, each=2), 0, 0, -0.5)
                                     }
                                     r01y*dy + coords$y[i]
                                 }))
                                 longid <- unlist(sapply(seq_along(coords$y), function(i) {
                                     rep(i, each=4*tsize[i]+3)
                                 }))

                                 grid::polygonGrob(
                                     longx,
                                     longy,
                                     id = longid,
                                     gp = grid::gpar(col = coords[,"colour"],
                                                     fill = coords[,"fill"],
                                                     fontsize = 10)
                                 )
                             }
)
geom_christmas_tree <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
    layer(
        geom = GeomChristmasTree, mapping = mapping,  data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}


buble_id <- "1GxkXlMwML1oSg5eLPiAz3"
buble_xmas_playlist_id <- "4F1LJJA4a5qEZdtK3rczD2"

#### Used this section of code, before finding the `get_playlist_audio_features()` function :D
# buble_tracks <- get_playlist_tracks(buble_xmas_playlist_id, fields = NULL, limit = 100,
#                                     offset = 0, market = NULL,
#                                     authorization = get_spotify_access_token(),
#                                     include_meta_info = FALSE)
# buble_audio_features <- get_track_audio_features(as.vector(buble_tracks$track.id))

buble_playlist_features <- get_playlist_audio_features("michaelbubleofficial", buble_xmas_playlist_id) %>%
        # Just selecting the variables I think I'll need - don't need all the URLs for example
        select(track.id,track.name,track.album.name,track.popularity,track.explicit,track.duration_ms,track.artists,
           time_signature,key_name,mode_name,key_mode,danceability,energy,key,loudness,mode,speechiness,acousticness,
           instrumentalness,liveness,valence,tempo) %>%
    # Found a couple of rows of NAs, not sure why? - removed them anyway
    na.exclude()


#Removing some variables - track.explicit for example is always false
buble_playlist_features <- buble_playlist_features %>%
    select(track.id,track.name,track.album.name,track.popularity,track.duration_ms,track.artists,
           time_signature,key,key_name,mode,mode_name,key_mode,acousticness,danceability,energy,
           instrumentalness,liveness,loudness,speechiness,tempo,valence) %>%
    na.exclude()

ggplot(buble_playlist_features, mapping=aes(x=valence, y=tempo)) +
    geom_point() +
    geom_smooth() +
    geom_text_repel(aes(label=track.name))

audio_features <- buble_playlist_features %>%
    select(track.popularity,track.duration_ms,
           time_signature,key,mode,acousticness,danceability,energy,
           instrumentalness,liveness,loudness,speechiness,tempo,valence) %>%
    na.exclude()

audio_features %>%
    keep(is.numeric) %>%                     # Keep only numeric columns
    gather() %>%                             # Convert to key-value pairs
    ggplot(aes(value)) +                     # Plot the values
    facet_wrap(~ key, scales = "free") +   # In separate panels
    #geom_histogram() +
    geom_density()


summarize(buble_playlist_features)

audio_features1 <- buble_playlist_features %>%
    filter(!track.id %in% c("4MBnlMCIyooQzCq457jKMu","7f4zVC7NpraLjXjFx9DNDt","47XK2pz3LTQ5fE6YP9Vhjn","6QixSlgfhcMRoDDRQYYevd")) %>%
    select(acousticness,danceability,energy,
           instrumentalness,liveness,loudness,speechiness,tempo,valence) %>%
    na.exclude()

ggplot(data=buble_playlist_features, mapping=aes(x=speechiness,y=instrumentalness))+
    geom_point() +
    geom_text_repel(aes(label=track.name))

ggplot(data=buble_playlist_features, mapping=aes(x=energy,y=loudness,fill=valence))+
    scale_fill_gradient(low='green1', high='green4')+
    geom_smooth(aes(colour="red"))+
    geom_christmas_tree(size=3)+
    geom_text_repel(aes(label=track.name))



colours <- c(rgb(251,66,66,maxColorValue=255),rgb(212,22,22,maxColorValue=255),
             rgb(48,133,5,maxColorValue = 255),rgb(11,94,21,maxColorValue = 255),
             rgb(23,51,5,maxColorValue = 255))

