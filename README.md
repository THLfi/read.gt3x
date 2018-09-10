# read.gt3x

read.gt3x is an R package which implements a high performance C++ parser for actigraphs .gt3x data format. Read the binary accelerometer data (.gt3x) into an R data frame in a few seconds.


## Actigraph accelerometers

Actigraph's wearable [accelerometer devices](https://en.wikipedia.org/wiki/Accelerometer) (e.g. GT9X Link) are used by both individuals and reasearchers to track movement. The devices measures [proper acceloration](https://en.wikipedia.org/wiki/Proper_acceleration) in three directions:  X (right-left), Y (forward-backward), Z (up-down). The measurement unit is the gravitational unit, $g = 9.81 m / s^2$

Data from the wearable Actigraph devices is usually extracted and analyzed via a software called *ActiLife*.  When data is extracted from the wearable Actigraph device, it is saved as a .gt3x file. A gt3x file is a zip archive with two files: 
- info.txt  
- log.bin  

The log.bin file is a binary file which includes the raw activity samples, written to the wearable device during usage. The format of the binary file is described in detail in the [GT3X github repository](https://github.com/actigraph/GT3X-File-Format). info.txt is a simple text file with meta information related to the device.


## Motivation for the package

ActiLife software provides a "Raw to Raw" import option, which reads the activity samples from a .gt3x file and writes them to a .csv file. However, this can be slow and the csv files can be large compared to the binary .gt3x format. Also, according to actigraphs customer support, "A raw file exported via ActiLife is run through a proprietary band pass filter that will exclude movement considered outside of the human spectrum", which might not be desirable for a researcher. 

This package makes it easier and faster to read the raw accelerometer samples into R after extracting the data from the wearable device. No modification is done to the raw data. The package implements an efficient C++ parser which reads activity samples directly from the binary log.bin file inside the .gt3x archive. This allows for

- Storing of the data in original binary format to reserve space  
- Fast access to the accelerometer's measurements  
- Circumvent ActiLifes filtering algorithms  

## Installation

You can install the read.gt3x package from GitHub, using the devtools-package (available in CRAN).

```
devtools::install_github("THLfi/read.gt3x")
```


## Basic usage

The read.gt3x package includes two sample .gt3x files which can be used to demonstrate reading the data. 

```{r}
library(read.gt3x)
```

First we need the path to a single gt3x file.


```{r}
gt3xfile <- gt3x_datapath(1)
```

The `readGT3X()` function can take as input a path to a single .gt3x file and will then read activity samples as an R matrix with three columns: X,Y,Z.

```{r}
X <- read.gt3x(gt3xfile)
```

You can also convert the matrix to a data.frame with 4 columns: X,Y,Z,time

```{r}
df <- as.data.frame(X)
```

## Documentation

[Documentation](https://thlfi.github.io/read.gt3x/), hosted by GitHub pages.

