# shiny_ibm
Oscillation Hypothesis - Individual Based Model

This is a shiny application to visualize and explore the results from a Individual Based Model that explores the evolution of host range and diversification process on a spatial context.

Before running the app, please make sure that all the required packages are installed on your computer. You can use the following command:

```R
ipak <- function(pkg){

  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)>0){ 
    install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)} else {
      print("All packages already installed")
    }
  
}

# usage
packages <- c("shiny", "animation", "scales", "viridis", 
              "dplyr", "data.table", "ggthemes", "plotrix", 
              "ggplot2", "DT", "htmlwidgets", "shinyjs")
ipak(packages)

```

If all packages are already installed, you can run the shiny app using the following commands:

```R
library(shiny)
runGitHub("shiny_ibm", "MarceloB87")
```

In case you decide to download the files to your computer, you can run the app following these steps:
1. Download the files and unzip them to a folder in your computer.
2. Run RStudio.
3. Open the file "app.R".
4. Press "Run" or the CTRL+SHIFT+ENTER command.

Note that depending on the capacity of your computer, it may take a few moments in order to the app resources load completely.
In case your find any error or have any doubts, contact me at:
meborges.b [at] gmail.com



