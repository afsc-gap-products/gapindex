## Welcome 

This R package generates the standard design-based indices of biomass, 
abundance, size composition, and age composition from NOAA-NMFS-AFSC-RACE-GAP 
bottom trawl survey data. Survey regions include: Gulf of Alaska (from 1993), 
Aleutian Islands (from 1991), Eastern Bering Sea Shelf (from 1982), 
Eastern Bering Sea Slope (from 2002), and Northern Bering Sea Shelf (from 2010).

## Installation Instructions

Make sure you have installed R packages `devtools`, `RODBC`, and `getPass` 
and are connected to the AFSC network or VPN while using this package.

```
library(devtools)
devtools::install_github("afsc-gap-products/gapindex")
```

## Collaborators
The gapindex R package is a product of two AFSC-RACE-GAP working groups 
regarding GAP data processes and index computation. Many thanks to those who 
participated in those working groups:

**Data Processes Working Group**|**Index Computation Working Group**|**Supervisors **
:-----:|:-----:|:-----:
Alexandra Dowlin (AlexandraDowlin-NOAA)|Zack Oyafuso (zoyafuso-NOAA)*|Stan Kotwicki (StanKotwicki-NOAA)
Emily Markowitz (EmilyMarkowitz-NOAA)|Margaret Siple (MargaretSiple-NOAA)|Duane Stevenson (Duane-Stevenson-NOAA)
Liz Dawson (liz-dawson-NOAA)|Rebecca Haehn (RebeccaHaehn-NOAA)|Ned Laman (Ned-Laman-NOAA)
Sarah Friedman (SarahFriedman-NOAA)|Lukas DeFilippo (Lukas-DeFilippo-NOAA)| 
Christopher Anderson (ChrisAnderson-NOAA)|Paul von Szalay (vszalay)| 
Nancy Roberson (NancyRoberson)|Thaddaeus Buser (ThaddaeusBuser-NOAA)| 
 |*package maintainer| 

## Legacy
Here is an non-exhaustive list of people who provided the foundation for many 
of the functions in this package:

AI-GOA: Michael Martin, Peter Munro, Ned Laman

Bering Sea: REM, Jason Conner, Jerry Hoff, Rebecca Haehn 

Many of the index calculations are from Wakabayashi et al. (1985):

Wakabayashi, K., R. G. Bakkala, and M. S. Alton. 1985. Methods of the 
     U.S.-Japan demersal trawl surveys, p. 7-29. In R. G. Bakkala and K. 
     Wakabayashi (editors), Results of cooperative U.S.-Japan groundfish 
     investigations in the Bering Sea during May-August 1979. Int. North Pac. 
     Fish. Comm. Bull. 44.

## Acronymns
NOAA: National Oceanic and Atmospheric Administration

NMFS: National Marine Fisheries Service

AFSC: Alaska Fisheries Science Center

RACE: Resource Assessment and Conservation Engineering Division

GAP: Groundfish Assessment Program

## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
