
<!-- README.md is generated from README.Rmd. Please edit that file -->

# read.gt3x

<!-- badges: start -->

[![R-CMD-check](https://github.com/THLfi/read.gt3x/workflows/R-CMD-check/badge.svg)](https://github.com/THLfi/read.gt3x/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/read.gt3x)](https://CRAN.R-project.org/package=read.gt3x)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/read.gt3x?color=blue)](https://r-pkg.org/pkg/read.gt3x)
<!-- badges: end -->

The `read.gt3x` R package implements a high performance C++ parser for
ActiGraph’s `.gt3x` data format. Read the binary accelerometer data
(.gt3x) into an R data frame in a few seconds.

## ActiGraph accelerometers

ActiGraph’s wearable [accelerometer
devices](https://en.wikipedia.org/wiki/Accelerometer) (e.g. GT9X Link)
are used by both individuals and researchers to track movement. The
devices measures [proper
acceleration](https://en.wikipedia.org/wiki/Proper_acceleration) in
three directions: X (right-left), Y (forward-backward), Z (up-down). The
measurement unit is the gravitational unit,
*g* = 9.81*m*/*s*<sup>2</sup>

Data from the wearable ActiGraph devices is usually extracted and
analyzed via a software called *ActiLife*. When data is extracted from
the wearable ActiGraph device, it is saved as a .gt3x file. A gt3x file
is a zip archive with two files: - info.txt  
- log.bin

The log.bin file is a binary file which includes the raw activity
samples, written to the wearable device during usage. The format of the
binary file is described in detail in the [GT3X github
repository](https://github.com/actigraph/GT3X-File-Format). info.txt is
a simple text file with meta information related to the device.

## Motivation for the package

ActiLife software provides a “Raw to Raw” import option, which reads the
activity samples from a .gt3x file and writes them to a .csv file.
However, this can be slow and the csv files can be large compared to the
binary .gt3x format. Also, according to ActiGraph’s customer support, “A
raw file exported via ActiLife is run through a proprietary band pass
filter that will exclude movement considered outside of the human
spectrum”, which might not be desirable for a researcher.

This package makes it easier and faster to read the raw accelerometer
samples into R after extracting the data from the wearable device. No
modification is done to the raw data. The package implements an
efficient C++ parser which reads activity samples directly from the
binary log.bin file inside the .gt3x archive. This allows for

-   Storing of the data in original binary format to reserve space  
-   Fast access to the accelerometer’s measurements  
-   Circumvent ActiLife’s filtering algorithms

## Installation

You can install the read.gt3x package from GitHub, using the `remotes`
package (available in CRAN).

``` r
remotes::install_github("THLfi/read.gt3x")
```

## Basic usage

The read.gt3x package includes two sample .gt3x files which can be used
to demonstrate reading the data.

``` r
library(read.gt3x)
```

First we need the path to a single gt3x file. We have one file included
in the package:

``` r
gt3xfile <-
  system.file(
    "extdata", "TAS1H30182785_2019-09-17.gt3x",
    package = "read.gt3x")
```

And you can download larger and more extensive `gt3x` files if desired:

``` r
gt3xfile <- gt3x_datapath(1)
```

The `read.gt3x()` function can take as input a path to a single .gt3x
file and will then read activity samples as an R matrix with three
columns: X,Y,Z.

``` r
X <- read.gt3x(gt3xfile)
head(X)
#> Sampling Rate: 100Hz
#> Firmware Version: 1.7.2
#> Serial Number Prefix: TAS
#>          X      Y     Z
#> [1,] 0.000  0.008 0.996
#> [2,] 0.016  0.000 1.008
#> [3,] 0.020 -0.008 1.004
#> [4,] 0.016 -0.012 1.012
#> [5,] 0.016 -0.008 1.008
#> [6,] 0.008 -0.008 1.008
head(attributes(X)$time_index)
#> [1] 0 1 2 3 4 5
attributes(X)[setdiff(names(attributes(X)), c("dim", "dimnames", "time_index"))]
#> $missingness
#>                           time n_missing
#> 1568745610 2019-09-17 18:40:10       400
#> 1568745861 2019-09-17 18:44:21     10500
#> 1568745977 2019-09-17 18:46:17     55400
#> 1568746545 2019-09-17 18:55:45    112600
#> 1568747697 2019-09-17 19:14:57      3300
#> 1568747740 2019-09-17 19:15:40       100
#> 1568747741 2019-09-17 19:15:41       100
#> 1568747742 2019-09-17 19:15:42       500
#> 1568747759 2019-09-17 19:15:59       100
#> 1568747760 2019-09-17 19:16:00     24500
#> 
#> $total_records
#> [1] 33000
#> 
#> $start_time_param
#> [1] 1568745600
#> 
#> $features
#> [1] "sleep mode"
#> 
#> $start_time_info
#> [1] 1568745600
#> 
#> $sample_rate
#> [1] 100
#> 
#> $impute_zeroes
#> [1] FALSE
#> 
#> $add_light
#> [1] FALSE
#> 
#> $start_time
#> [1] "2019-09-17 18:40:00 GMT"
#> 
#> $stop_time
#> [1] "2019-09-18 19:00:00 GMT"
#> 
#> $last_sample_time
#> [1] "2019-09-17 19:20:05 GMT"
#> 
#> $subject_name
#> [1] "suffix_85"
#> 
#> $time_zone
#> [1] "-04:00:00"
#> 
#> $firmware
#> [1] "1.7.2"
#> 
#> $serial_prefix
#> [1] "TAS"
#> 
#> $acceleration_min
#> [1] "-8.0"
#> 
#> $acceleration_max
#> [1] "8.0"
#> 
#> $bad_samples
#> [1] FALSE
#> 
#> $old_version
#> [1] FALSE
#> 
#> $header
#> GT3X information
#>  $ Serial Number     :"TAS1H30182785"
#>  $ Device Type       :"Link"
#>  $ Firmware          :"1.7.2"
#>  $ Battery Voltage   :"4.18"
#>  $ Sample Rate       :100
#>  $ Start Date        : POSIXct, format: "2019-09-17 18:40:00"
#>  $ Stop Date         : POSIXct, format: "2019-09-18 19:00:00"
#>  $ Last Sample Time  : POSIXct, format: "2019-09-17 19:20:05"
#>  $ TimeZone          :"-04:00:00"
#>  $ Download Date     : POSIXct, format: "2019-09-17 19:20:05"
#>  $ Board Revision    :"8"
#>  $ Unexpected Resets :"0"
#>  $ Acceleration Scale:256
#>  $ Acceleration Min  :"-8.0"
#>  $ Acceleration Max  :"8.0"
#>  $ Subject Name      :"suffix_85"
#>  $ Serial Prefix     :"TAS"
#> 
#> $class
#> [1] "activity" "matrix"   "array"
```

You can also convert the matrix to a data.frame with 4 columns:
X,Y,Z,time

``` r
df <- as.data.frame(X)
head(df)
#> Sampling Rate: 100Hz
#> Firmware Version: 1.7.2
#> Serial Number Prefix: TAS
#>                     time     X      Y     Z
#> 1 2019-09-17 18:40:00.00 0.000  0.008 0.996
#> 2 2019-09-17 18:40:00.00 0.016  0.000 1.008
#> 3 2019-09-17 18:40:00.01 0.020 -0.008 1.004
#> 4 2019-09-17 18:40:00.02 0.016 -0.012 1.012
#> 5 2019-09-17 18:40:00.03 0.016 -0.008 1.008
#> 6 2019-09-17 18:40:00.04 0.008 -0.008 1.008
attributes(df)[setdiff(names(attributes(df)), c("names", "row.names"))]
#> $class
#> [1] "activity_df" "data.frame" 
#> 
#> $subject_name
#> [1] "suffix_85"
#> 
#> $time_zone
#> [1] "-04:00:00"
#> 
#> $missingness
#>                           time n_missing
#> 1568745610 2019-09-17 18:40:10       400
#> 1568745861 2019-09-17 18:44:21     10500
#> 1568745977 2019-09-17 18:46:17     55400
#> 1568746545 2019-09-17 18:55:45    112600
#> 1568747697 2019-09-17 19:14:57      3300
#> 1568747740 2019-09-17 19:15:40       100
#> 1568747741 2019-09-17 19:15:41       100
#> 1568747742 2019-09-17 19:15:42       500
#> 1568747759 2019-09-17 19:15:59       100
#> 1568747760 2019-09-17 19:16:00     24500
#> 
#> $old_version
#> [1] FALSE
#> 
#> $firmware
#> [1] "1.7.2"
#> 
#> $last_sample_time
#> [1] "2019-09-17 19:20:05 GMT"
#> 
#> $serial_prefix
#> [1] "TAS"
#> 
#> $sample_rate
#> [1] 100
#> 
#> $acceleration_min
#> [1] "-8.0"
#> 
#> $acceleration_max
#> [1] "8.0"
#> 
#> $header
#> GT3X information
#>  $ Serial Number     :"TAS1H30182785"
#>  $ Device Type       :"Link"
#>  $ Firmware          :"1.7.2"
#>  $ Battery Voltage   :"4.18"
#>  $ Sample Rate       :100
#>  $ Start Date        : POSIXct, format: "2019-09-17 18:40:00"
#>  $ Stop Date         : POSIXct, format: "2019-09-18 19:00:00"
#>  $ Last Sample Time  : POSIXct, format: "2019-09-17 19:20:05"
#>  $ TimeZone          :"-04:00:00"
#>  $ Download Date     : POSIXct, format: "2019-09-17 19:20:05"
#>  $ Board Revision    :"8"
#>  $ Unexpected Resets :"0"
#>  $ Acceleration Scale:256
#>  $ Acceleration Min  :"-8.0"
#>  $ Acceleration Max  :"8.0"
#>  $ Subject Name      :"suffix_85"
#>  $ Serial Prefix     :"TAS"
#> 
#> $start_time
#> [1] "2019-09-17 18:40:00 GMT"
#> 
#> $stop_time
#> [1] "2019-09-18 19:00:00 GMT"
#> 
#> $total_records
#> [1] 33000
#> 
#> $bad_samples
#> [1] FALSE
#> 
#> $features
#> [1] "sleep mode"
```

## Documentation

[Documentation](https://thlfi.github.io/read.gt3x/), hosted by GitHub
pages.
