# Accelerometer

accelerometer is an R package which implements a high performance C++ parser for actigraphs .gt3x data format. 

## Installation

```
devtools::install_github("accelerometer", "TuomoNieminen")
```

## Actigraph accelerometers

Actigraph's wearable [accelerometer devices](https://en.wikipedia.org/wiki/Accelerometer) (e.g. GT9X Link) are used by both individuals and reasearchers to track movement. The devices measures [proper acceloration](https://en.wikipedia.org/wiki/Proper_acceleration) in three directions:  X (right-left), Y (forward-backward), Z (up-down). The measurement unit is the gravitational unit, $g = 9.81 m / s^2$

## GT3X data format

Data from the wearable devices is extracted and often analyzed via a software called ActiLife. When data is extracted from the device, it is saved as a .gt3x file. A gt3x file is a zip archive with two files: 

- info.txt  
- log.bin  

The log.bin file is a binary file which includes the raw activity samples, written to the wearable device during usage. The format of the binary file is described in detail in the [GT3X github repository](https://github.com/actigraph/GT3X-File-Format).

## Motivation for the package

ActiLife software provides a "Raw to Raw" import option, which reads the activity samples from a .gt3x file and writes them to a .csv file. However, this can be slow and the csv files can be large compared to the binary .gt3x format. Also, according to actigraphs customer support, "A raw file exported via ActiLife is run through a proprietary band pass filter that will exclude movement considered outside of the human spectrum", which might not be desirable for a researcher.

This package implements an efficient C++ parser which reads activity samples directly from the binary log.bin file inside the .gt3x archive. This allows for

- Fast access to the accelerometer's measurements
- Circumvent ActiLifes filtering algorithms
- Storing of the data in original binary format to reserve space

