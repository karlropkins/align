# Contributing to **align**

This outlines how to propose a change to align. The text is based on the template provided by the folks who developed the tidyverse packages, but 
we are not as strict as them regarding coding style.

Please see the [**tidy contributing guide**](https://rstd.io/tidy-contrib) for further details regarding contributions to tidyverse packages, and 
good practices for those contributing to open software, datasets and archives more generally. 

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface or as a pull request (see below).

Documentation is generated using [roxygen2](https://roxygen2.r-lib.org/articles/roxygen2.html), so you will need to edit comments in the rocxygen comments 
in the .R relevant script, rather than the .Rd file. (That gets built from the .R using devtools::document())   

## Bigger changes and code changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 

Likewise, if you’ve found a bug, please file an issue. We would like you to illustrate the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) example and we may ask you to do that if we have problems replicated any non-reprex bug reports, but 
we would appreciate a 'head-up' regardless of the format if you think anything is not behaving...

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, **tidyverse** folks recommend using 
    `usethis::create_from_github("karlropkins/align", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to get in contact before continuing. 

*   Create a Git branch for your pull request (PR). Here the **tidyverse** recommendation is to use `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR (**tidyverse** recommendation `usethis::pr_push()` then follow the prompts in your browser).
    Ideally, the title of your PR should briefly describe the change, and the body of your PR should contain `Fixes #issue-number`.

*   For user-facing changes, please include a suggestion for a `NEWS.Rmd` bullet. 

### Code style

*   Please feel free to follow [tidyverse style guide](https://style.tidyverse.org) regarding coding practices and format, but if it works and 
    we can understand it, we are not going to waste anyone's time enforcing specific coding practices.  

*   We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), 
    for documentation. This we require you to also do.  

*   We currently do NOT use [testthat](https://cran.r-project.org/package=testthat) for unit tests. We would appreciate worked examples demonstrating a code modification 
    but, please, no testthat.  

## Code of Conduct

Please note that the align project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
