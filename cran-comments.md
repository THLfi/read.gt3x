* Release prep for `read.gt3x` 1.3.0.
* Fixed test-time network dependencies and refreshed CRAN-facing metadata.

## Test environments

* macOS 15.4.1, R 4.4.0

## R CMD check results

`R CMD check --as-cran read.gt3x_1.3.0.tar.gz`

0 errors | 0 warnings | 3 notes

Notes were limited to the local environment:

* CRAN incoming checks could not be queried because this sandbox has no network access.
* The check could not verify external URLs due lack of network access.
* `xcrun_db` was present in the macOS temp directory during check cleanup.
