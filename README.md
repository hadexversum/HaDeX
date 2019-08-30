[![Build Status](https://api.travis-ci.org/hadexversum/HaDeX.png)](https://travis-ci.org/hadexversum/HaDeX)
[![Coverage status](https://codecov.io/gh/hadexversum/hadex/branch/master/graph/badge.svg)](https://codecov.io/github/hadexversum/hadex?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/HaDeX)](https://cran.r-project.org/package=HaDeX)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/HaDeX)](https://cran.r-project.org/package=HaDeX)

<p>
  <img src="https://raw.githubusercontent.com/hadexversum/HaDeX/inst/HaDeX/HaDeX.png" width="100">
</p>

## HaDeX: Analysis and Visualisation of Hydrogen/Deuterium Exchange Mass Spectrometry Data 

Hydrogen/Deuterium eXchange Mass Spectrometry (HDX-MS) is a staple technology in structural proteomics. HaDeX provides a analytic workflow for HDX-MS data available as a standalone GUI (https://sourceforge.net/projects/HaDeX/), web server (http://mslab-ibb.pl/shiny/HaDeX/) and the **R** package available on CRAN (https://cran.r-project.org/web/packages/HaDeX/index.html). 

### Local instance of HaDeX GUI

To run HaDeX GUI locally on Windows, install it using the following binary file: https://sourceforge.net/projects/HaDeX/files/HaDeX_setup.exe/download

### Local instance of HaDeX package

You can install the latest development version of the package:

```R
source("https://install-github.me/hadexversum/HaDeX")
```
or the latest version available on CRAN:

```R
install.packages("HaDeX")
```

After installation GUI can be accessed locally:

```R
library(HaDeX)
HaDeX_gui()
```

### Online manual

The HaDeX documentation is available [online](https://HaDeXversum.github.io/HaDeX/).

### Citation

Puchala W, Burdukiewicz M, Kistowski M, Dabrowska KA, Badaczewska-Dawid AE, Cysewski D and Dadlez M (2019). HaDeX: Analysis and Visualisation of Hydrogen/Deuterium Exchange Mass Spectrometry Data. R package version 1.0.

### Funding  

This work is supported by Foundation of Polish Science (TEAM TECH CORE FACILITY/2016-2/2 *Mass Spectrometry of Biopharmaceuticals - improved methodologies for qualitative, quantitative and structural characterization of drugs, proteinaceous drug targets and diagnostic molecules)*.
