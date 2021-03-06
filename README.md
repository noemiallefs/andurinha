# andurinha

<!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/andurinha)](https://CRAN.R-project.org/package=andurinha)
  [![R build status](https://github.com/noemiallefs/andurinha/workflows/R-CMD-check/badge.svg)](https://github.com/noemiallefs/andurinha/actions)
  [![codecov](https://codecov.io/gh/noemiallefs/andurinha/branch/master/graph/badge.svg)](https://codecov.io/gh/noemiallefs/andurinha)
<!-- badges: end -->

## Overview
The aim of andurinha is provides tools to make spectroscopic data processing easier and faster. It allows to find and select peaks based on the second derivative or absorbance sum spectrum. Furthermore, it supplies functions for graphic support, which makes the workflow more user friendly.

## Installation
```r
# The easiest way to get andurinha
install.packages("andurinha")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("noemiallefs/andurinha")
```

## Usage
###  Import

There are two common situations when importing the spectroscopic data.

  1. **In the first case**, data may be in the same file with the structure: 
  
      + **First column**: wave numbers.
      
      + **The following columns**: the samples absorbances. 
      
      In that case, import the data with the most suitable function.
  
  2. **In the second case**, data may be in separated files with the structure:
  
      + **First column**: wave numbers.
      
      + **Second column**: sample absorbance.
    
In that case, the function `importSpectra()` can be used; to do so the files extension should be `.csv` and they must be in the same directory - this folder must not contain any other file. 

```r
spectra <- importSpectra("/path/to/your/spectraFiles/", sep = ";")
head(spectra)

#>    WN     A     B     C
#> 1 399 0.011 0.008 0.009
#> 2 401 0.008 0.006 0.006
#> 3 403 0.006 0.005 0.006
#> 4 405 0.005 0.005 0.005
#> 5 407 0.005 0.005 0.003
#> 6 409 0.003 0.004 0.002
```

### Find peaks

The function `findPeaks()` verifies the spectra quality, finds peaks (*surprise!*) and allows to select the most relevant ones based on the absorbance or second derivative sum spectrum. To use it the data must be in the appropriate format, it means that the object class must be a `data frame` with the structure:

  + **First column**: wave numbers.
  
  + **The following columns**: samples absorbances.
  
This function has five arguments: 
  
  1. `resolution`: the equipment measurement resolution, which is by default 4 cm^-1^.
  
  2. `minAbs`: the cut off value to check spectra quality, which is by default a spectrum absorbance maximum of 0.1.
  
  3. `cutOff`: the second derivative or absorbance sum spectrum cut off to reduce the raw peaks table, which is by default `NULL`.
  
  4. `scale`: by default is `TRUE` and data is scaled as Z-scores. `FALSE` should be used in case you do not want to scale it.
  
  5. `ndd`:  by default is `TRUE` and peaks are searched based on the second derivative sum spectrum. `FALSE` should be used in case you want to search them based on absorbance sum spectrum.
  
This function - with all the arguments by default - returns a `list` with four `data frames`:

  1. **dataZ**: the standardised data by Z-scores.
  
  2. **secondDerivative**: the second derivative values of the data.
  
  3. **sumSpectrum_peaksTable**: the peaks wave numbers and their second derivative or absorbance sum spectrum values.
  
  4. **peaksTable**: the peaks wave numbers and their absorbance for each spectrum.

By default, if there is any spectrum with a maximum absorbance lower than 0.1 a warning will be returned; in case this shows up and you want to continue, you should modify the `minAbs` value. Once the quality control has been passed, by default the data is scaled - to skip it use `scale = FASLE` - the next steps will depend on the selected method for finding peaks:

  1. **Absorbance sum spectrum**: in this case the absorbance sum spectrum is calculated and the peaks are searched based on it. 
  
  2. **Second derivative sum spectrum**: in this case the second derivative of the absorbance data is calculated and then the peaks are searched based on the sum spectrum. 


```r
# Search peaks based on absorbance sum spectrum
# with standarised absorbance data
fp.abs <- findPeaks(andurinhaData, ndd = FALSE)
summary(fp.abs)
dim(fp.abs$sumSpectrum_peaksTable)

# Search peaks based on second derivative sum spectrum
# with standarised absorbance data
fp.ndd <- findPeaks(andurinhaData)
summary(fp.ndd)
dim(fp.ndd$sumSpectrum_peaksTable)

# Search peaks based on second derivative sum spectrum
# with no standarised absorbance data
fp.nZs <- findPeaks(andurinhaData, scale = FALSE)
summary(fp.nZs)
dim(fp.nZs$sumSpectrum_peaksTable)
```

### Visualisation

To visualised both the raw data and the processed data by `findPeaks()`; the functions `gOverview()` and `plotPeaks()` may be applied.

##### `gOverview()`: 

Gives a graphic summary of the data. This function has the arguments:

  1. `data_abs`: to provide a data frame with the absorbance data. The structure should be: wave numbers in the first column and samples absorbance in the following columns.
  
  2. `data_ndd`: to provide a data frame with the second derivative data. The structure should be: wave numbers in the first column and samples second derivative values in the following columns.
  
  3. `fontFamily`: to change the plot font. 
  
```r
# Graphic overview of the raw data
gOverview(andurinhaData)
```

```r
# Graphic overview of the processed data
# Peaks searched based on the second derivative sum spectrum
# with standarised absorbance data
gOverview(fp.ndd$dataZ, fp.ndd$secondDerivative)
```

##### `plotPeaks()`:

Makes a graphic representation of the peaks that have been found over the second derivative or absorbance sum spectra. This plot, together with the **sumSpectrum_peaksTable**, allows to choose the desired `cutOff` value to reduce the peaks table by running again `findPeaks()`. This function has the arguments:

  1. `peaksWN`: to provide a vector with the peaks wave numbers.
  
  2. `data_abs`: to provide a data frame with the absorbance data. The structure should be: wave numbers in the first column and samples absorbance in the following columns.
  
  3. `data_ndd`: to provide a data frame with the second derivative data. The structure should be: wave numbers in the first column and samples second derivative values in the following columns.
  
  4. `fontFamily`: to change the plot font. 
  
```r 
# Peaks searched based on absorbance sum spectrum
plotPeaks(fp.abs[[3]]$WN, 
          data_abs = fp.abs$dataZ)
```

```r 
# Peaks searched based on the second derivative sum spectrum
plotPeaks(fp.ndd[[4]]$WN, 
          data_abs = fp.ndd$dataZ, 
          data_ndd = fp.ndd$secondDerivative)
```

### Peaks selection

To reduce the peaks table a cut off must be selected; this may be based on the second derivative or on the absorbance sum spectra values. Therefore, the `sumSpectrum_peaksTable` must be kept in mind; to make the choice easier it may be ordered and then filtered. The function `plotPeaks()` may be very useful to make the choice.

When the peaks search is made based on the absorbance sum spectrum, the number of peaks found will be lower than that obtained when using the second derivative sum spectrum. Due to the smaller number of peaks selection may be not needed. This makes the workflow easier, but this method might not find all relevant peaks. Owing to the search based on absorbance sum spectrum is less efficient than the search based on the second derivative sum spectrum, we recommend the second method. But this will need a little bit more user work. The following examples show the differences between both methods:

```r
# Select cutOff based on absorbance sum spectrum 
# to clean your peaks table
round(fp.abs$sumSpectrum_peaksTable, 2) %>% 
   arrange(desc(sumSpectrum))
# In that case cleaning may not be necesary

# Select cutOff based on second derivative sum spectrum 
# to clean your peaks table
round(fp.ndd$sumSpectrum_peaksTable, 2) %>% 
   arrange(desc(sumSpectrum)) %>%
   filter(sumSpectrum > 0.18)
# In that case a cut off of 0.25 my be selected
```

When the cut off has been chosen then run `findPeaks()` changing the `cutOff` value to the desired one.

```r
# Run finPeaks() with the new cutOff
# based on the second derivative sum spectrum
fp.ndd2 <- findPeaks(andurinhaData, cutOff = 0.25)
```

Let's see the result!

```r 
# plotPeaks
# based on absorbance sum spectrum
# no cleaning needed
plotPeaks(fp.ndd2[[3]]$WN,
          data_abs = fp.ndd2$dataZ)
```

```r
# plotPeaks
# based on the second derivative sum spectrum
plotPeaks(fp.ndd2[[4]]$WN,
          data_abs = fp.ndd2$dataZ,
          data_ndd = fp.ndd2$secondDerivative)
```

## Cite

Noemi Alvarez Fernandez and Antonio Martinez Cortizas (2020). andurinha: Make Spectroscopic Data Processing Easier. R package version 0.0.2. https://CRAN.R-project.org/package=andurinha
