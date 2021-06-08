# recordSwapping 1.0.0

* included parameters `int &count_swapped_records` and `int &count_swapped_hid` `std::string log_file_name` to the cpp-function `recordSwap()` which count the number of swapped records and swapped households. These parameters are convenience parameters for mu-Argus.
* included parameter `std::string log_file_name` and `log_file_name` in the cpp-function `recordSwap()` and R-function `recordSwap()` respectively. 


# recordSwapping 0.4.0

* Made `recordSwap()` an S3 Method, which can also accept an `sdcMicro`-object as input.
* included unit tests for `recordSwap()` when using `sdcMicro`-objects
* inlcuded unit tests for `infoLoss()`
* cleaned up code and unit tests such that `R CMD check` runs without notes, warnings and errors
* fixed warnings when compiling c++ code like `comparing unsigned integer ...` or `unused variable ...`
* fixed minor typos in README, help pages and vignettes

# recordSwapping 0.3.3

* included function `infoLoss()` to calculated various information loss measures after `recordSwap()` was applied. Function `infoLoss()` is heavily inspired by function `cellKey::ck_cnt_measures()` but accepts the micro data as input instead of frequency or magnitude counts.
* changed variable names and content of dummy data created by `createDat`.
* updated documentation and fixed some typos.

# recordSwapping 0.3.2

* extended unit tests
* Argument `data` for R-function `recordSwap()` does no longer need to have only integer values. Only variables needed for underlying C++ function defined through parameter `hid`, `hierarchy`, `risk_variables`, `similar`, `carry_along` need to be integer type.


# recordSwapping 0.3.1

* Fixed bug where less than households than indicated through the swaprate were drawn
* added version number to header file
* developed different method for distributing number of draws over all geographic hierarchies

# recordSwapping 0.3.0

* Changed parameter order in `R` and `C++` function `recordSwap()` to have a more consistent documentation. Function call to `C++` function changed to 

```r
std::vector< std::vector<int> > recordSwap(std::vector< std::vector<int> > data, int hid,
                                           std::vector<int> hierarchy, 
                                           std::vector< std::vector<int> > similar,
                                           double swaprate,
                                           std::vector< std::vector<double> > risk, double risk_threshold,
                                           int k_anonymity, std::vector<int> risk_variables,  
                                           std::vector<int> carry_along,
                                           int seed = 123456)
```

* Improved interface of `R` function `recordSwap()`. Now, parameters `hid`, `hierarchy`, `similar` and `risk_variables` can be used with column indices or column names of `data`. Please note that indices in `R` start with 1 but in `C++` they start with `0`. The `R` function `recordSwap()` which is basically just a wrapper for the `C++` function converts column names or indices into the correct format for the `C++` function. So the call for `recordSwap()` from `R` expects indices starting from 1.

* added parameter `std::vector<int> carry_along` to `C++` function `recordSwap()`. Like the paramter `hierarchy`, `carry_along` expects column indices of `data` and swaps these values in addition to the ones defined in `hierarchy`. These variables do however not interfere with risk calculation, sampling or finding a donor.

* added parameter `carry_along` to `R` function `recordSwap()`, expects either column indices or column names

* added parameter `return_swapped_id` to `R` function `recordSwap()`. If `return_swapped_id` is `TRUE` an additional column will be returned which holds the `hid` with which each record was swapped with. If this new column has the same value as `hid` the record was not swapped.

* Improved documentation and vignette

# recordSwapping 0.2.0

* Some parameter changes to the `C++` function `recordSwap()`:

    + `similar` is now a vector of vectors allowing for multiple similarity profiles
    + changed `std::vector<int> risk` to `std::vector<int> risk_variables` for a more descriptive name
    + changed `int th` to `int k_anonymity` for a more descriptive name
    + added parameter `double risk_threshold` which can be used to set a custom risk threshold. household whith a risk **greater or equal** `risk_threshold` is set as risky household (**not yet supported by `R` wrapper**)
    + added parameter `std::vector<std::vector<double>> risk` which can be used as custom risks for each household in each hierarchy level (**not yet supported by `R` wrapper**)

```r
recordSwap(std::vector< std::vector<int> > data, std::vector<std::vector<int>> similar,
                                           std::vector<int> hierarchy, std::vector<int> risk_variables, int hid, 
                                           int k_anonymity, double swaprate, double risk_threshold,
                                           std::vector<std::vector<double>> risk, int seed = 123456)
```
                                           
* Fixed some bugs concerning the application of `swaprate`
* `data` as well as `risk` are used internally such that `data[0]` or `risk[0]` contain the micro data or risk over all hierarchies for the first individual,
`data[1]` or `risk[1]` contain the micro data or risk, over all hierarchies, for the second individual, and so on.
* Documentation and parameter descriptions have been updated in the corresponding help files.

# recordSwapping 0.1.0

* First prototype version of record swapping 
* Contains the function `recordSwap()` as the main function of this package
* `recordSwap()` is an `R`-wrapper which calls an underlying `Rcpp` function where at the bottom a call to the **C++** function `recordSwap()` is made.

```r
recordSwap(std::vector< std::vector<int>> data, std::vector<int> similar,
                                           std::vector<int> hierarchy, std::vector<int> risk, int hid, 
                                           int th, double swaprate, int seed = 123456)
```

* The parameter `data` contains the household data and is used internally such that `data[0]` contains values for each individual for the first column of the data,
`data[1]` contains the values for each individual for the second column of the data. This also implies that `data[0][0]` addresses the first value of the first column and so on.
* The procedure expects the data to be **ordered** by household ID (column `hid`). Ordering inside each household is irrelevant. 
* Various internal help functions are included. These are however not intended for flexible use and are only called from withing `recordSwap()`                                           


