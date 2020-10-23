rockthemes
================

# <img src="man/figures/logo.png" width="160px" align="right" />

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/johnmackintosh/rockthemes.svg?branch=main)](https://travis-ci.com/johnmackintosh/rockthemes)

![R-CMD-check](https://github.com/johnmackintosh/rockthemes/workflows/R-CMD-check/badge.svg)

![Render
README](https://github.com/johnmackintosh/rockthemes/workflows/Render%20README/badge.svg)

![pkgdown](https://github.com/johnmackintosh/metallicaRt/workflows/pkgdown/badge.svg)

<!-- badges: end -->

## What?

This is a collection of colour palettes based on classic rock album
covers.

Not all of the artists are ‘rock’, but they appeared in lists of classic
rock album covers and the internet is never wrong, is it ;)

The albums were chosen either for their striking covers (in terms of
colour), or simply, because they are bona fide rock classics.

Your job is to guess which is which.

## Why?

Because [this repo of Metallica inspired
palettes](https://github.com/johnmackintosh/metallicaRt) has been
received quite well on various social media platforms, and I figured
that there were other albums with interesting covers that might provide
more scope for data visualisation purposes.

## Installation

This will probably not go to CRAN, so please install using the remotes
package.

``` r
#library(remotes)
#remotes::install_github("johnmackintosh/rockthemes")
library(rockthemes)
library(ggplot2)
library(dplyr)
library(scales)
library(gapminder)
```

# Palettes and Themes

``` r
rock_palette("californication")
rock_palette("coltrane")
rock_palette("deelite")
```

<img src="man/figures/README-californication-1.png" width="33%" /><img src="man/figures/README-californication-2.png" width="33%" /><img src="man/figures/README-californication-3.png" width="33%" />

``` r
rock_palette("electric")
rock_palette("faithnomore")
rock_palette("gogo")
```

<img src="man/figures/README-electric-1.png" width="33%" /><img src="man/figures/README-electric-2.png" width="33%" /><img src="man/figures/README-electric-3.png" width="33%" />

``` r
rock_palette("gunsnroses")
rock_palette("harvey")
rock_palette("heep")
```

<img src="man/figures/README-gnr-1.png" width="33%" /><img src="man/figures/README-gnr-2.png" width="33%" /><img src="man/figures/README-gnr-3.png" width="33%" />

``` r
rock_palette("hellawaits")
rock_palette("husker")
rock_palette("janelle")
```

<img src="man/figures/README-slayer-1.png" width="33%" /><img src="man/figures/README-slayer-2.png" width="33%" /><img src="man/figures/README-slayer-3.png" width="33%" />

``` r
rock_palette("maiden")
rock_palette("melloncollie")
rock_palette("metallica")
```

<img src="man/figures/README-maiden-1.png" width="33%" /><img src="man/figures/README-maiden-2.png" width="33%" /><img src="man/figures/README-maiden-3.png" width="33%" />

``` r
rock_palette("miles")
rock_palette("muse")
rock_palette("nevermind")
```

<img src="man/figures/README-muse-1.png" width="33%" /><img src="man/figures/README-muse-2.png" width="33%" /><img src="man/figures/README-muse-3.png" width="33%" />

``` r
rock_palette("nodoubt")
rock_palette("oasis")
rock_palette("peacesells")
```

<img src="man/figures/README-oasis-1.png" width="33%" /><img src="man/figures/README-oasis-2.png" width="33%" /><img src="man/figures/README-oasis-3.png" width="33%" />

``` r
rock_palette("swift")
rock_palette("tencc")
```

<img src="man/figures/README-swift-1.png" width="50%" /><img src="man/figures/README-swift-2.png" width="50%" />

## Longer colour palettes, more suited for ggplot2 use

The following palettes share the same inspirations, but there are more
colours, which hopefully increases their utility for data visualisation.

<img src="man/figures/README-unnamed-chunk-2-1.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-2.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-3.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-4.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-5.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-6.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-7.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-8.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-9.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-10.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-11.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-12.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-13.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-14.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-15.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-16.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-17.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-18.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-19.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-20.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-21.png" width="25%" /><img src="man/figures/README-unnamed-chunk-2-22.png" width="25%" />

## Credit

[Thanks to Ryo for the tvthemes
package](https://github.com/Ryo-N7/tvthemes) which helped me get this
off the ground quickly

## Code of Conduct

Please note that the rockthemes project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project
you agree to abide by its terms.

## Contributing

See the [Contribution guide](.github/CONTRIBUTING.md)
