if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# renv::init()

requiredPackages <- c(
  "abind",
  "agridat",
  "broom",
  "BsMD",
  "crosstable",
  "DiagrammeR",
  "emmeans",
  "FrF2",
  "gridExtra",
  "janitor",
  "kableExtra",
  "knitr",
  "latex2exp",
  "magic",
  "patchwork",
  "pwr",
  "reticulate",
  "scidesignR",
  "tidyverse"
)
install.packages(requiredPackages)

for (pkg in requiredPackages) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

# Helper Functions
is_installed <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

is_loaded <- function(pkg) {
  pkg %in% loadedNamespaces()
}

package_version <- function(pkg) {
  if (is_installed(pkg)) as.character(packageVersion(pkg)) else NA_character_
}

# Build status table
package_status <- data.frame(
  Package = requiredPackages,
  Version = vapply(requiredPackages, package_version, character(1)),
  Installed = vapply(requiredPackages, is_installed, logical(1)),
  Loaded = vapply(requiredPackages, is_loaded, logical(1))
)

kbl(package_status) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
  



# renv::snapshot()
```

## Jupyter Notebook Setup

```{r}
# Reproducibility / Display Defaults
options(
  stringAsFactors = FALSE,
  repos = c(CRAN = "http://cloud.r-project.org"),
  warn = 1
)
set.seed(123)
```

## Welcome

```{r}
# List of all packages used
requiredPackages <- c(
  "tidyverse",
  "knitr",
  "kableExtra",
  "reticulate",
  "janitor",
  "latex2exp",
  "gridExtra",
  "broom",
  "patchwork",
  "crosstable",
  "agridat",
  "FrF2",
  "pwr",
  "emmeans",
  "DiagrammeR",
  "abind",
  "magic",
  "BsMD",
  "scidesignR"
)

# Sort all the packages alphabetically
requiredPackages <- sort(unique(requiredPackages))

# Display the current status of all the packages: Package Name | Installed Status | Installed Version | Loaded Status -- first sort the packages in Alphabetical Order (A --> Z)



# Check if package is already installed, if it doesn't, it gets installed. 
# TODO: Create a "report" that has the list of all packages that are required and if they are installed before starting. Then, update it after they are installed to confirm they are installed, then output the table: Package | Version | Install Status Before | Install Status After

# Loads all required packages
# TODO: Create a report similar to the installation one, but now it would be Package | Version | Load Status Before | Load Status After
