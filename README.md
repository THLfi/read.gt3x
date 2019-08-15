
<!-- README.md is generated from README.Rmd. Please edit that file -->

# read.gt3x

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/muschellij2/read.gt3x.svg?branch=master)](https://travis-ci.com/muschellij2/read.gt3x)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/muschellij2/read.gt3x?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/read.gt3x)
<!-- badges: end -->

The `read.gt3x` R package implements a high performance C++ parser for
actigraphs `.gt3x` data format. Read the binary accelerometer data
(.gt3x) into an R data frame in a few seconds.

## Actigraph accelerometers

Actigraph’s wearable [accelerometer
devices](https://en.wikipedia.org/wiki/Accelerometer) (e.g. GT9X Link)
are used by both individuals and reasearchers to track movement. The
devices measures [proper
acceloration](https://en.wikipedia.org/wiki/Proper_acceleration) in
three directions: X (right-left), Y (forward-backward), Z (up-down). The
measurement unit is the gravitational unit, \(g = 9.81 m / s^2\)

Data from the wearable Actigraph devices is usually extracted and
analyzed via a software called *ActiLife*. When data is extracted from
the wearable Actigraph device, it is saved as a .gt3x file. A gt3x file
is a zip archive with two files: - info.txt  
\- log.bin

The log.bin file is a binary file which includes the raw activity
samples, written to the wearable device during usage. The format of the
binary file is described in detail in the [GT3X github
repository](https://github.com/actigraph/GT3X-File-Format). info.txt is
a simple text file with meta information related to the device.

## Motivation for the package

ActiLife software provides a “Raw to Raw” import option, which reads the
activity samples from a .gt3x file and writes them to a .csv file.
However, this can be slow and the csv files can be large compared to the
binary .gt3x format. Also, according to actigraphs customer support, “A
raw file exported via ActiLife is run through a proprietary band pass
filter that will exclude movement considered outside of the human
spectrum”, which might not be desirable for a researcher.

This package makes it easier and faster to read the raw accelerometer
samples into R after extracting the data from the wearable device. No
modification is done to the raw data. The package implements an
efficient C++ parser which reads activity samples directly from the
binary log.bin file inside the .gt3x archive. This allows for

  - Storing of the data in original binary format to reserve space  
  - Fast access to the accelerometer’s measurements  
  - Circumvent ActiLifes filtering algorithms

## Installation

You can install the read.gt3x package from GitHub, using the
devtools-package (available in CRAN).

    devtools::install_github("THLfi/read.gt3x")

## Basic usage

The read.gt3x package includes two sample .gt3x files which can be used
to demonstrate reading the data.

``` r
library(read.gt3x)
```

First we need the path to a single gt3x file.

``` r
gt3xfile <- gt3x_datapath(1)
#> Downloading gt3x sample data from https://github.com/THLfi/read.gt3x/releases/download/v1.0/EE_left_29.5.2017-05-30.gt3x.zip
```

The `read.gt3x()` function can take as input a path to a single .gt3x
file and will then read activity samples as an R matrix with three
columns: X,Y,Z.

``` r
X <- read.gt3x(gt3xfile)
#> Input is a .gt3x file, unzipping to a temporary location first...
#> Unzipping gt3x data to /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//Rtmp11q9Ka
#> 1/1
#> Unzipping /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//Rtmp11q9Ka/.read.gt3x-data/EE_left_29.5.2017-05-30.gt3x
#>  === info.txt and log.bin extracted to /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//Rtmp11q9Ka/EE_left_29.5.2017-05-30
#> Parsing GT3X data via CPP.. expected sample size: 60692900
#> Sample size: 60684100
#> Done (in 16.2557809352875 seconds)
head(X)
#>           X     Y      Z
#> [1,] -0.164 0.820 -0.637
#> [2,] -0.145 0.766 -0.594
#> [3,] -0.121 0.766 -0.527
#> [4,] -0.094 0.777 -0.441
#> [5,] -0.090 0.777 -0.422
#> [6,] -0.098 0.754 -0.465
head(attributes(X)$time_index)
#> [1] 100 101 102 103 104 105
attributes(X)[setdiff(names(attributes(X)), c("dim", "dimnames", "time_index"))]
#> $missingness
#>                           time n_missing
#> 1495549060 2017-05-23 14:17:40       100
#> 1495965995 2017-05-28 10:06:35       100
#> 1496137744 2017-05-30 09:49:04       100
#> 
#> $start_time_log
#> [1] 1495530900
#> 
#> $sample_rate
#> [1] 100
#> 
#> $start_time
#> [1] "2017-05-23 09:15:00 GMT"
#> 
#> $stop_time
#> [1] "0001-01-01 GMT"
#> 
#> $subject_name
#> [1] "EE_left_29.5."
#> 
#> $time_zone
#> [1] "03:00:00"
#> 
#> $class
#> [1] "activity" "matrix"
```

You can also convert the matrix to a data.frame with 4 columns:
X,Y,Z,time

``` r
df <- as.data.frame(X)
#> Converting to a data.frame ...
#> Done
head(df)
#>        X     Y      Z                time
#> 1 -0.164 0.820 -0.637 2017-05-23 09:15:01
#> 2 -0.145 0.766 -0.594 2017-05-23 09:15:01
#> 3 -0.121 0.766 -0.527 2017-05-23 09:15:01
#> 4 -0.094 0.777 -0.441 2017-05-23 09:15:01
#> 5 -0.090 0.777 -0.422 2017-05-23 09:15:01
#> 6 -0.098 0.754 -0.465 2017-05-23 09:15:01
attributes(df)[setdiff(names(attributes(df)), c("names", "row.names"))]
#> $class
#> [1] "activity_df" "data.frame" 
#> 
#> $subject_name
#> [1] "EE_left_29.5."
#> 
#> $time_zone
#> [1] "03:00:00"
#> 
#> $missingness
#>                           time n_missing
#> 1495549060 2017-05-23 14:17:40       100
#> 1495965995 2017-05-28 10:06:35       100
#> 1496137744 2017-05-30 09:49:04       100
```

## Documentation

[Documentation](https://thlfi.github.io/read.gt3x/), hosted by GitHub
pages.
