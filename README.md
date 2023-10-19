---
# Global Irrigation Expansion - ISEA3H Grid 

---


[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10012830.svg)](https://doi.org/10.5281/zenodo.10012830)


## About
This repository contains R Code to conduct a simple statistical analysis that models global irrigation expansion in the 20th century as a function of biophysical and socioeconomic drivers. The methodological framework is based on stacked random forest models and aims to explore the role of the discrete global grid choice by comparing the standard longitude-latitude grid to the geodesic ISEA3H grid.   


## Data
The data reflects the fraction of area irrigated in each grid cell between 1902 and 2005 and is based on the global Historical Irrigation Dataset (HID) developed by Siebert et al. (2015).


 ![irrigation.gif](irrigation.gif)



## Code
The code folder contains functions to build the random forest models, to compute variable importance and partial dependence measures, to plot the predicted global irrigation patterns  and compare the two discrete global grids in regards to predictive accuracy. Finally, you can find code to produce descriptive plots and tables of the data. 

Please find the detailed README file in the Code folder to get instructions on how to get the code up and running. 

## References 
Siebert, S., Kummu, M., Porkka, M., Döll, P., Ramankutty, N., and Scanlon, B. R.: A global data set of the extent of irrigated land from 1900 to 2005, Hydrol. Earth Syst. Sci., 19, 1521–1545, https://doi.org/10.5194/hess-19-1521-2015, 2015.






