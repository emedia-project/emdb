-record(movie, {
    id,
    source,
    title,
    original_title,
    adult,
    date,
    poster,
    backdrop,
    genres, 
    tagline,
    overview,
    distance
  }).

-record(tv, {
    id,
    source,
    title,
    original_title,
    adult,
    date,
    poster,
    backdrop,
    genres, 
    tagline,
    overview,
    networks,
    seasons,
    episodes,
    distance
  }).

-record(season, {
    id,
    tv_id,
    season,
    source,
    title,
    original_title,
    date,
    poster,
    backdrop,
    overview,
    episodes
  }).

-record(episode, {
    id,
    tv_id,
    season,
    episode,
    source,
    title,
    original_title,
    date,
    poster,
    backdrop,
    overview
  }).
