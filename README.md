
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openwater <img src="man/figures/openwater.png" align="right" width="125px" />

<!-- badges: start -->

[![Under
Development](https://img.shields.io/badge/status-under%20development-red.svg)](https://github.com/Saint-Louis-University/openwater)
[![Last
Commit](https://img.shields.io/github/last-commit/Saint-Louis-University/openwater.svg)](https://github.com/Saint-Louis-University/openwater/commits/master)
<!-- badges: end -->

## Overview

The goal of `openwater` is to provide R bindings for the
[OpenWater](https://help.getopenwater.com/en/articles/3110614-openwater-rest-api)
[Application programming interface
(API)](https://en.wikipedia.org/wiki/Application_programming_interface)

<br />

## Installation

You can install `openwater` from
[GitHub](https://github.com/saint-louis-university/openwater) with:

``` r
remotes::install_github("Saint-Louis-University/openwater")
```

There are two pieces of information necessary to access and use the API:

1.  API Key
    1.  per user API authentication credentials
    2.  Can be generated by an OpenWater admin user under OpenWater \>
        System Settings \> Permissions
    3.  copy to your .Renviron file under the variable name
        `OPENWATER_API_KEY`
2.  Domain
    1.  the domain of your specific OpenWater instance
    2.  for example “demo.secure-platform.com”
    3.  copy to your .Renviron file under the variable name
        `OPENWATER_DOMAIN`

<br />

## About

### Saint Louis University <img src="man/figures/edu.slu.marcom-logowithyear_rgb.png" align="right" width="125px" />

Founded in 1818, [Saint Louis University](https://www.slu.edu) is one of
the nation’s oldest and most prestigious Catholic institutions. Rooted
in Jesuit values and its pioneering history as the first university west
of the Mississippi River, SLU offers nearly 13,000 students a rigorous,
transformative education of the whole person. At the core of the
University’s diverse community of scholars is SLU’s service-focused
mission, which challenges and prepares students to make the world a
better, more just place.
