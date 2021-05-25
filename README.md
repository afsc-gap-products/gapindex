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

