tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar("REDCAPSYNC_CACHE_OVERRIDE" = tempdir_file)
