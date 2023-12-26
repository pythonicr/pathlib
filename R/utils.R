.is_absolute <- function(paths, invert = FALSE) {
  if (isTRUE(invert)) {
    !base::grep("^(/+|[A-Za-z]:)", .tidy(paths))
  } else {
    base::grep("^(/+|[A-Za-z]:)", .tidy(paths))
  }
}

.test_type <- function(paths, type_name) {
  type_id <- fs:::file_types[[type_name]]
  # %in% treats NAs as valid objects, so comparisons don't pass NAs
  # along
  return(base::.Call("fs_stat_", paths, TRUE, PACKAGE = "fs")$type %in% type_id)
}

.tidy <- function(paths) {
  if (!base::inherits(paths, "fs_path")) {
    paths <- fs::path_tidy(paths)
  }
  return(paths)
}