# cytoclops 👁
A workspace for cytometry data.



How to install in R:

### Generate a github token to access the private repositories
https://github.com/settings/tokens

###install devtools from cran
install.packages('devtools')
 
###load devtools and install the package
library(devtools)
install_github("gusef/cio-shiny",auth_token = 'COPY_PASTE_YOUR_TOKEN_HERE')
install_github("gusef/cytoclops",auth_token = 'COPY_PASTE_YOUR_TOKEN_HERE')

### Launch the shiny server

```{r}
library('cytoclops')
cytoclops()
```
