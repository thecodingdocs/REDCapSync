# Uploads

``` r

library(REDCapSync)
```

Another unique feature of REDCapSync is the ability to upload labelled
data. Currently, you can use R to modify data and then upload back to
REDCap. For classic REDCap projects, without events or repeating
instruments, uploading data is straightforward. For more complex data,
you must follow the same rules as the REDCap data import tool (using
`redcap_event_name`, `redcap_repeat_instrument`, and
`redcap_repeat_instance`).

``` r

project <- load_project("TEST_CLASSIC")$sync()

new_branching <- sample(c("Yes", "No"), size = 50, replace = TRUE)

# add 50 records
upload_this <- data.frame(record_id = as.character(51:100),
                          var_branching = new_branching,
                          ecog_at_diagnosis = "0")

project$upload(upload_this) # will also trigger sync/update
```

Future versions may have comparisons/checks and derived fields using R
code. For now, project accepts data.frames or named lists of
data.frames. Use with caution at it will modify REDCap data. We are also
considering developing a pathway to reimport saved excel files to send
back to REDCap!
