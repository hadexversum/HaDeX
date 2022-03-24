[![published in: Bioinformatics](https://img.shields.io/badge/published%20in-Bioinformatics-green.svg)](http://dx.doi.org/10.1093/bioinformatics/btaa587)
[![R build status](https://github.com/hadexversum/HaDeX/workflows/R-CMD-check/badge.svg)](https://github.com/hadexversum/HaDeX/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/HaDeX)](https://cran.r-project.org/package=HaDeX)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/HaDeX)](https://cran.r-project.org/package=HaDeX)


<p>
  <img src="https://raw.githubusercontent.com/hadexversum/HaDeX/master/inst/HaDeX/HaDeX.png" width="100">
</p>

## HaDeX: Analysis and Visualisation of Hydrogen/Deuterium Exchange Mass Spectrometry Data 

Hydrogen/Deuterium eXchange Mass Spectrometry (HDX-MS) is a staple technology in structural proteomics. HaDeX provides a analytic workflow for HDX-MS data available as a standalone GUI (https://sourceforge.net/projects/HaDeX/), web server (https://hadex.mslab-ibb.pl/) and the **R** package available on CRAN (https://cran.r-project.org/web/packages/HaDeX/index.html). 

### Local instance of HaDeX GUI

To run HaDeX GUI locally on Windows, install it using the following binary file: https://sourceforge.net/projects/HaDeX/files/HaDeX_setup.exe/download

### Local instance of HaDeX package

You can install the latest development version of the package:

```R
devtools::install_github("hadexversum/HaDeX")
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

Puchala W, Burdukiewicz M, Kistowski M, Dabrowska KA, Badaczewska-Dawid AE, Cysewski D and Dadlez M (2020). HaDeX: Analysis and Visualisation of Hydrogen/Deuterium Exchange Mass Spectrometry Data. Bioinformatics, 10.1093/bioinformatics/btaa587.

### Funding  

This work is supported by Foundation of Polish Science (TEAM TECH CORE FACILITY/2016-2/2 *Mass Spectrometry of Biopharmaceuticals - improved methodologies for qualitative, quantitative and structural characterization of drugs, proteinaceous drug targets and diagnostic molecules)* and Narodowe Centrum Nauki (Preludium Bis 1 2019/35/O/NZ2/03745 *Wysokorozdzielcza analiza danych z eksperymentów wymiany proton-deuter monitorowanych spektrometrią mas)*.
