# Biodiversity Application

<!-- badges: start -->

<!-- badges: end -->

The goal of this R Shiny Biodiversity Application is to produce a Biodiversity Analysis Dashboard using Shiny moduler framework. The 
data which has been used is available in a .csv file however I have created a .RDS format and kept in project folder to 
avoid reimport of CSV.

## Access

The Biodiversity Application is deployed to shinyapps.io. Link to Application is provided below.

``` r
https://cdiscdata.shinyapps.io/biodiversity/
```

The Biodiversity Application package could be accessed locally by cloning the [GitHub repository](https://github.com/saini1397/biodiversity

## Steps to Run the Application locally after downloading zip codes from Github Repository


-   navigate to the root directory of the `{GitHub}` package
-   select and open app.R file
-   Run app.R


# General Work Process

## Packages Used

-   I have used `shiny`, `usethis`, `tidyverse` packages for dashboard development.
-   Few testcase have been written to test the functions. Test Cases are available in `{testthat folder}`
-   Modules has been created for UI and Server and all modules are called from app.R file.
-   Functions has been written and tested using testthat.
-   Used CSS for styling of Buttons etc.
-   All required packages are called from global.R file.
