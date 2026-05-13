# read.gt3x 1.3.0

* Fixed CRAN checks by removing test-time network dependencies.
* Added tests for `fill_zeros()` and `fill_zeroes()`.
* Updated package metadata for CRAN submission.

# read.gt3x 1.2.1

* Added filling in zeroes function.
* Added ability to give non-rounded data back in `read.gt3x`.

# read.gt3x 1.2.0

* Added batch-reading functionality, see #40

# read.gt3x 1.1.1

* Added extraction of features.
* Added `extract_gt3x_info` for allowing of passing of connections for info text files.


# read.gt3x 1.0.2

* Added timeout for `gt3x_datapath` downloading.
* Released to CRAN

# read.gt3x 1.0.1

* Exported `datetime2ticks` and `ticks2datetime`.
* Changed options export with `readGT3X`.
* Released to CRAN.

# read.gt3x 1.0.0

* Released to CRAN on 2021-01-07.
* dev note: release SHA commit f23eda5

# read.gt3x 0.4.2

* Memory issues with Windows cloud systems is solved for now.

# read.gt3x 0.4.1

* Fixing the time stamp problem with 30 Hz.


# read.gt3x 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Checked and ensured that imputation worked in specific cases.
* Reads in old (NHANES) and newer versions of `gt3x` files.
* First CRAN release candidate.
