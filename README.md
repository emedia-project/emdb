# emdb

Erlang Media DataBase

## emdbd

Erlang Media DataBase Driver

## emdbi

Erlang Media Database Interface :

* http://www.themoviedb.org/
* http://thetvdb.com/
* http://anidb.net/
* http://www.tvrage.com/
* http://www.serienjunkies.de/
* OMDB (http://www.omdbapi.com/)
* http://acoustid.org/
* http://developer.rottentomatoes.com/docs/read/Home

### The movie database

### OMDB

### AniDB

### TV Rage

[acces](http://api.tvrage.com/)



### Rotten Tomatoes

[form](http://www.rottentomatoes.com/help_desk/licensing/#usageRequestForm)

## Usage

```
rr("include/emdb.hrl").
emdb:start().

% The Movie Database
emdb:search({movie, {name, "The party"}}, [{only, [themoviedb]}, {language, fr}]).
emdb:search({movie, {id, 10794}}, [{only, [themoviedb]}]).

% OMDB
emdb:search({movie, {name, "The party"}}, [{only, [omdb]}]).
emdb:search({movie, {id, "tt0063415"}}, [{only, [omdb]}]).

% Both
emdb:search({movie, {name, "The party"}}, [{only, [themoviedb, omdb]}, {language, fr}]).
```

## Licence

emdb is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014 Grégoire Lejeune <<gregoire.lejeune@free.fr>>

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

