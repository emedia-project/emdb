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
    source,
    title,
    original_title,
    date,
    poster,
    backdrop,
    overview,
    number,
    episodes
  }).

-record(episode, {
    id,
    season_id,
    tv_id,
    source,
    title,
    original_title,
    date,
    poster,
    backdrop,
    overview,
    number,
    season
  }).
