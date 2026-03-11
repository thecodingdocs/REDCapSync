# REDCapSync: Encapsulated REDCap Project Objects for Syncronized Data Pipelines

Wraps dozens of REDCap API endpoints into a standardized R6 object.
Research Electronic Data Capture (REDCap) is a survey and database web
application software maintained by Vanderbilt University. It has a
robust application programming interface (API) utilized by several R
packages. 'REDCapSync' uses 'redcapAPI' and 'REDCapR' behind-the-scenes
to retrieve all metadata, data, and log details for a project. To
minimize unneccesary server calls, the interim REDCap log is analyzed
and used to update necessary records. Furthermore, the user can define
subsets and transformations, such as merges and deidentification, that
save to a user-defined directory. Those subsets, called summaries by the
package, also continue to refresh when data is updated. Having a
standarized, API-effecient, project-agnostic R object streamlines
downstream use in scripts, functions, and shiny applications.

## See also

Useful links:

- <https://github.com/thecodingdocs/REDCapSync>

- <https://thecodingdocs.github.io/REDCapSync/>

- Report bugs at <https://github.com/thecodingdocs/REDCapSync/issues>

## Author

**Maintainer**: Brandon Rose <thecodingdocs@gmail.com>
([ORCID](https://orcid.org/0009-0009-7813-1960)) \[copyright holder\]

Other contributors:

- Natalie Goulett \[contributor\]

- Chris Hilsinger-Pate \[contributor\]

- Nikola Susic \[contributor\]

- Mason Thornton \[contributor\]

- Sydney Stern ([ORCID](https://orcid.org/0000-0002-1479-605X))
  \[contributor\]
