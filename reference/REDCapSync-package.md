# REDCapSync: Encapsulated REDCap projects for data pipelines in R

Provides tools to download, organize, and synchronize data from REDCap
projects through the REDCap application programming interface (API). The
package constructs a standardized project object that stores data,
metadata, logs, and configuration details in a user-defined directory.
It supports incremental updates by checking the REDCap log and
retrieving only modified data. Full updates are only triggered by major
metadata changes. User can define subsets, derived fields, and
transformations, which are stored in the list object and passed down to
output objects to be used for data pipelines and downstream
applications, such as the R shiny app RosyREDCap.

## See also

Useful links:

- <https://github.com/thecodingdocs/REDCapSync>

- <https://thecodingdocs.github.io/REDCapSync/>

- Report bugs at <https://github.com/thecodingdocs/REDCapSync/issues>

## Author

**Maintainer**: Brandon Rose <thecodingdocs@gmail.com>
([ORCID](https://orcid.org/0009-0009-7813-1960))

Other contributors:

- Natalie Goulett \[contributor\]

- Chris Hilsinger-Pate \[contributor\]

- Nikola Susic \[contributor\]

- Mason Thornton \[contributor\]

- Sydney Stern ([ORCID](https://orcid.org/0000-0002-1479-605X))
  \[contributor\]
