# R Shinyapp Sample size

This shinyapp will calculate the sample size needed for a given power.

# Usage

- The sample size shinyapp is browser-based, and thus, no installation is needed
- The sample size shinyapp can be accessed in this link xxxx

More details are available in the documentation: [DOCS.md](https://github.com/solislemuslab/sample-size-shinyapp/blob/master/DOCS.md)


# Source Code
Sample size shinyapp is an [open source](http://opensource.org) project, and the source code is available at in this repository with the main web app code in `app.R`.


### Running the web apps locally

Users with strong programming skills might like to modify the existing R code and run a version of the web apps locally. 

1. The first step is to download the code. You can do this with git:

```git clone https://github.com/solislemuslab/sample-size-shinyapp.git```


2. Make sure you have the dependencies installed. You can use the following command in R to install all the package dependencies:

```r
list.of.packages <- c(
  "shiny", 
  "ggplot2", 
  "bslib",
  "DT",
  "tidyverse",
  "greekLetters",
  "thematic",
  "shinyhelper",
  "shinyBS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
```

3. Within R, you can run the web app with the following command:

```shiny::runApp("/path/to/sample-size-shinyapp")```

# Contributions

Users interested in expanding functionalities in the sample size shiny app are welcome to do so.
See details on how to contribute in [CONTRIBUTING.md](https://github.com/solislemuslab/sample-size-shinyapp/blob/master/CONTRIBUTING.md)

# License
The sample size shiny app is licensed under the [MIT](https://opensource.org/licenses/MIT) licence. &copy; Solis-Lemus Lab (2021)


# Feedback, issues and questions

Issues reports are encouraged through the [GitHub issue tracker](https://github.com/solislemuslab/sample-size-shinyapp/issues)



