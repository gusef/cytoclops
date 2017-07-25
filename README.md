# cytoclops 👁
A workspace for cytometry data.



How to install in R:

## Generate a github token to access the private repositories

https://github.com/settings/tokens

##install pacakges from cran

```{r}
install.packages(c('devtools','shiny','shinyBS','Rtsne','shinythemes','V8','data.table','shinyjs','sp')
```

##install pacakges from bioconductor

```{r}
source("https://bioconductor.org/biocLite.R")
biocLite("flowCore")
``` 
 
###load devtools and install the package

```{r}
library(devtools)
install_github("gusef/cio-shiny",auth_token = 'COPY_PASTE_YOUR_TOKEN_HERE')
install_github("gusef/cytoclops",auth_token = 'COPY_PASTE_YOUR_TOKEN_HERE')
```

### Launch the shiny server

```{r}
library('cytoclops')
cytoclops()
```
