#Just messing around trying to get to grips
library(spotifyr)
dodie_playlist <- get_playlist_tracks("37i9dQZF1DZ06evO19h0GO", fields = NULL, limit = 100,
                    offset = 0, market = NULL,
                    authorization = get_spotify_access_token(),
                    include_meta_info = FALSE)

dodie_tracks_audio_features <- get_track_audio_features(as.vector(dodie_playlist$track.id))

library(dplyr)
dodie_data <- merge(dodie_playlist, dodie_tracks_audio_features, by.x="track.id", by.y="id") %>%
    select(track.id, track.duration_ms, track.name,track.popularity, danceability, energy, key, loudness, mode,speechiness,acousticness,instrumentalness,liveness, valence)

plot(dodie_data$danceability,dodie_data$energy)

library(ggplot2)
library(ggrepel)
ggplot(data=dodie_data, mapping=aes(x=danceability,y=energy)) +
    geom_point() +
    geom_text_repel(aes(label=track.name))


library(geniusr)






#Christmas stuff begins here!
library(dplyr)
christmas_playlists <- get_category_playlists("holidays", limit = 50) %>%
    select(name,description,id,tracks.total)    # select columns we want
                                                # and reorder them
