#Just messing around trying to get to grips

dodie_playlist <- get_playlist_tracks("37i9dQZF1DZ06evO19h0GO", fields = NULL, limit = 100,
                    offset = 0, market = NULL,
                    authorization = get_spotify_access_token(),
                    include_meta_info = FALSE)

dodie_tracks_audio_features <- get_track_audio_features(as.vector(dodie_playlist$track.id))


#Christmas stuff begins here!
library(dplyr)
christmas_playlists <- get_category_playlists("holidays", limit = 50) %>%
    select(name,description,id,tracks.total)    # select columns we want
                                                # and reorder them

