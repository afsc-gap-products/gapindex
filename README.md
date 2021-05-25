# design-based-indices

Code to generate design-based indices of abundance from survey data. Very generally, the code takes tables from RACEBASE and GOA to calculate CPUE for each species.


## Calculation of total CPUE
CPUE for each tow is calculated as the total catch (in kg) divided by the effort (area swept in km^2). The quick calculation for this from the RACEBASE tables is `effort = distance_fished * (0.001 * net_width)`. 

Mean CPUE is calculated for each stratum, then the total CPUE is the weighted sum of those across all strata, weighted by the total area of each stratum (equation 4 in Wakabayashi et al. 1985). The overall mean CPUE for the entire survey area is CPUE_Tk:

![eqn four](https://github.com/MargaretSiple-NOAA/design-based-indices/blob/master/img/eqn4.PNG)


$$
\bar{CPUE}_{Tk} = \frac{\sum_{i}{\bar{CPUE_{ik}}}\cdot A_i}{A_T}
$$

Where **k** is the species, **i** is the stratum, **A_i** is the stratum area, and **A_T** is the total survey area.

## Legal disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.