# The Project Cache

The REDCapSync package benefits from storing/caching key information
about your different projects. The two most important pieces of
information is the `short_name` and `directory` where it’s stored. This
is enough information to be able to find where you have chosen to
securely store your files, load what has already been collected, and
then communicate with R to fetch any new updates. REDCapSync’s ability
to store your projects in a standardized directory is what allows for
powerful pipeline tasks.

``` r
library(REDCapSync) # don't forget to load the package
```

## The Cache

Using the `hoadr` package, R finds the standard location where R
typically stores cahced package data. For example, on Mac the location
might look like, “/Users/yourmacname/Library/Caches/R/REDCapSync”. Your
exact path can be found with
[`cache_path()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_path.md)
At the time writing this article, the only thing that REDCapSync stores
in this cache is the projects data.frame, which can be loaded with
`projects <- get_projects()`.

``` r
cache_path()
```

## Projects

``` r
projects <- get_projects()
print.data.frame(projects)
```
