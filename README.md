# Forecast

This is a command-line utility that accepts an address and hits a few APIs: [positionstack](https://positionstack.com) to get address coordinates, and [api.weather.gov](https://www.weather.gov/documentation/services-web-api) to get a forecast for the coordinates. It dumps out the forecast for the next few days in a table format. It tries to use a cached copy of the forecast JSON stored for the given address if it exists and we're still within the timestamp window, otherwise it will try to look the forecast data up by hitting external APIs. This often succeeds.


## Configuration

You'll need to get an API access key from [positionstack](https://positionstack.com/product), which is free for the most basic level, but does require an account. For weather.gov they require that you pass them your email address in the header, which this app takes care of if you provide it with your email address.

The configuration is in `config.dhall`, and hopefully is pretty obvious: the positionstack.com API key goes in `positionStackAccessKey`, and your email address goes in `emailAddress`.


## Development Notes

This is not really meant to be the utility which you reach for when you need to check the weather, although it works...fine for that.

Rather, I wrote this to try to explore a few different concepts in Haskell, among them how to cleanly manage a bunch of effectful code--but at a small scale that is easy to wrap your head around. So I hope it is also useful for others as a simple example of how to do some basic, useful things in Haskell.

I've tried to push commits that both work independently as well as illustrate different stages of development. Right now here's what I've done:

1. [The first commit](https://github.com/ddellacosta/forecast/commit/b2af1a99305748b2bdc4bf1bd519096d73982e75) is a single file, uses the [`Req`](https://hackage.haskell.org/package/req-3.5.0/docs/Network-HTTP-Req.html#t:Req) Monad provided with the [`req`](https://hackage.haskell.org/package/req) library as its base, and is pretty hacky (compared to the still-hacky later versions at least), with a lot of coupled up logic, partial functions, and generally messy code.

2. [Version 2](https://github.com/ddellacosta/forecast/commit/3f49f331f03140d69e3e3893f8307af9348c2870) is largely just a refactoring of the first version to clean up the main application logic. It also improves somewhat on the caching logic (it's still pretty basic though).

3. [Version 3](https://github.com/ddellacosta/forecast/commit/396a9a910ce39a025d749948c83f13fda6e68ae8) splits the single file logic up into modules to better isolate concerns, adds a separate configuration file via [dhall](https://hackage.haskell.org/package/dhall), and introduces a custom app monad instead of using the `Req` monad. This last change was done so that we can include the configuration via `MonadReader`, rather than hard-coding values into functions (in particular the API request calls which require configuration). It also opens the door to configuring stuff like the cache file and cache settings, retry settings for API calls that fail (_cough_ weather.gov _cough_), and more.

I hope to continue modifying this code, augmenting it, and improving it. In particular the `Forecast.Request` module needs some work to eliminate partial functions and tighten things up a bit. There should be some exception handling in place as well. I'd also like to try other approaches to overall structure than the "vanilla mtl" represented here, like the [handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html) as well as an effect system library like [fused-effects](https://hackage.haskell.org/package/fused-effects). So, there is more to come.

Feedback is welcome!

#### Copyright 2020 Dave Della Costa, [BSD 3-Clause "New" License](LICENSE)
