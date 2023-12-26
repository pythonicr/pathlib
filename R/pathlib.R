#' Get the absolute path of the provided path.
#'
#' This function returns the absolute path of the input path, similar to the
#' behavior of Python's `pathlib.Path.resolve()` or `pathlib.Path.absolute()`
#' methods.
#'
#' @param paths A character vector of file paths.
#'
#' @return A character vector containing the absolute paths.
#'
#' @aliases Path_resolve
#' @export
#' @examples
#' \dontrun{
#' Path_absolute("relative/path/to/file.txt")
#' }
Path_absolute <- function(paths) {
  is_not_abs <- .is_absolute(paths, invert = TRUE)
  paths[is_not_abs] <- fs::path(base::getwd(), paths[is_not_abs])
  return(paths)
}

#' Change the mode (permissions) of a file or directory.
#'
#' This function changes the mode (permissions) of the specified file or
#' directory to the provided `mode`.
# Similar to Python's `pathlib.Path.chmod()` method.
#'
#' @param paths A character vector of file paths.
#' @param mode  The mode (permissions) to set for the file or directory.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_chmod("file.txt", "755")
#' }
Path_chmod <- function(paths, mode) {
  fs::file_chmod(paths, mode)
}

#' Get the current working directory.
#'
#' This function returns the current working directory as a character vector,
#' similar to Python's `pathlib.Path.cwd()` method.
#'
#' @return A character vector containing the current working directory.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_cwd()
#' }
Path_cwd <- function() {
  base::getwd()
}

#' Check if the provided paths exist.
#'
#' This function checks if the specified files or directories exist and returns
#' a logical vector indicating their existence, similar to Python's
#' `pathlib.Path.exists()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating the existence of each path.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_exists("file.txt")
#' }
Path_exists <- function(paths) {
  fs::file_access(paths, mode = "exists")
}

#' Expand the tilde (~) in file paths to the user's home directory.
#'
#' This function replaces the tilde (~) character with the user's home directory
#' path in the provided paths, similar to Python's `pathlib.Path.expanduser()`
#' method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A character vector with expanded tilde paths.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_expanduser("~/Documents/file.txt")
#' }
Path_expanduser <- function(paths) {
  fs::path_expand(paths)
}

#' Glob for files matching a pattern.
#'
#' This function searches for files matching the specified patterns and returns
#' their paths as a character vector, similar to Python's `pathlib.Path.glob()`
#' method.
#'
#' @param patterns A character vector of file patterns to match.
#'
#' @return A character vector containing the paths of matching files.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_glob("*.txt")
#' }
Path_glob <- function(patterns) {
  .tidy(base::Sys.glob(patterns, dirmark = FALSE))
}

#' Get the group owner of the specified files or directories.
#'
#' This function returns the group owner of the provided paths as a character
#' vector, similar to Python's `pathlib.Path.group()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A character vector containing the group owner of each path.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_group("file.txt")
#' }
Path_group <- function(paths) {
  fs::file_info(paths)$group
}

#' Create a hard link from the paths to the target paths.
#'
#' This function creates hard links from the specified paths to the target
#' paths, similar to Python's `pathlib.Path.link_to()` method.
#'
#' @param paths   A character vector of source file paths.
#' @param targets A character vector of target file paths.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_hardlink_to("file.txt", "hardlink.txt")
#' }
Path_hardlink_to <- function(paths, targets) {
  fs::link_create(paths, targets, symbolic = FALSE)
}

#' Get the user's home directory.
#'
#' This function returns the user's home directory as a character vector,
#' similar to Python's `pathlib.Path.home()` method.
#'
#' @return A character vector containing the user's home directory path.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_home()
#' }
Path_home <- function() {
  fs::path_expand("~")
}

#' Check if the provided paths are absolute paths.
#'
#' This function checks if the specified paths are absolute paths and returns a
#' logical vector indicating the result. An absolute path is a path that starts
#' from the root directory, e.g., "/path/to/file.txt", "C:\\path\\to\\file.txt"
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating whether each path is an absolute path.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_is_absolute("/path/to/file.txt")
#' }
Path_is_absolute <- function(paths) {
  .is_absolute(paths)
}

#' Check if the provided paths are block devices.
#'
#' This function checks if the specified paths are block devices and returns a
#' logical vector indicating the result. Similar to Python's
#' `pathlib.Path.is_block_device()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating whether each path is a block device.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_is_block_device("file.txt")
#' }
Path_is_block_device <- function(paths) {
  .test_type(paths, "block_device")
}

#' Check if the provided paths are character devices.
#'
#' This function checks if the specified paths are character devices and returns
#' a logical vector indicating the result. Similar to Python's
#' `pathlib.Path.is_char_device()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating whether each path is a character device.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_is_char_device("file.txt")
#' }
Path_is_char_device <- function(paths) {
  .test_type(paths, "character_device")
}

#' Check if the provided paths are directories.
#'
#' This function checks if the specified paths are directories and returns a
#' logical vector indicating the result. Similar to Python's
#' `pathlib.Path.is_dir()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating whether each path is a directory.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_is_dir("directory")
#' }
Path_is_dir <- function(paths) {
  .test_type(paths, "directory")
}

#' Check if the provided paths are FIFOs (named pipes).
#'
#' This function checks if the specified paths are FIFOs (named pipes) and
#' returns a logical vector indicating the result. Similar to Python's
#' `pathlib.Path.is_fifo()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating whether each path is a FIFO.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_is_fifo("fifo_pipe")
#' }
Path_is_fifo <- function(paths) {
  .test_type(paths, "FIFO")
}

#' Check if the provided paths are regular files.
#'
#' This function checks if the specified paths are regular files and returns a
#' logical vector indicating the result. Similar to Python's
#' `pathlib.Path.is_file()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating whether each path is a regular file.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_is_file("file.txt")
#' }
Path_is_file <- function(paths) {
  .test_type(paths, "file")
}

#' Check if the provided paths are sockets.
#'
#' This function checks if the specified paths are sockets and returns a logical
#' vector indicating the result. Similar to Python's `pathlib.Path.is_socket()`
#' method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating whether each path is a socket.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_is_socket("socket_file")
#' }
Path_is_socket <- function(paths) {
  .test_type(paths, "socket")
}

#' Check if the provided paths are symbolic links.
#'
#' This function checks if the specified paths are symbolic links and returns a
#' logical vector indicating the result. Similar to Python's
#' `pathlib.Path.is_symlink()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A logical vector indicating whether each path is a symbolic link.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_is_symlink("symbolic_link")
#' }
Path_is_symlink <- function(paths) {
  .test_type(paths, "symlink")
}


#' Get file information without following symbolic links.
#'
#' This function retrieves file information for the provided paths without
#' following symbolic links. Similar to Python's `pathlib.Path.lstat()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A list containing file information for each path.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_lstat("file.txt")
#' }
Path_lstat <- function(paths) {
  fs::file_info(paths, fail = TRUE, follow = FALSE)
}

#' Create directories at the specified paths.
#'
#' This function creates directories at the specified paths. If the `parents`
#' argument is set to TRUE, it creates parent directories as needed. Similar to
#' Python's `pathlib.Path.mkdir()` method.
#'
#' @param paths    A character vector of directory paths to create.
#' @param parents  Logical. If TRUE, create parent directories as needed.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_mkdir("new_directory")
#' }
Path_mkdir <- function(paths, parents = FALSE) {
  fs::dir_create(paths, recurse = parents)
}

#' Get the owner of the specified files or directories.
#'
#' This function returns the owner of the provided paths as a character vector,
#' similar to Python's `pathlib.Path.owner()` method.
#'
#' @param paths A character vector of file paths.
#'
#' @return A character vector containing the owner of each path.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_owner("file.txt")
#' }
Path_owner <- function(paths) {
  fs::file_info(paths)$user
}

#' Read the content of a file as text.
#'
#' This function reads the content of a file at the specified path as text,
#' using the specified encoding.
#'
#' @param path     A character vector specifying the path to the file.
#' @param encoding The character encoding to use for reading the file
#' ("unknown", "UTF-8", or "latin1").
#'
#' @return A character vector containing the content of the file.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_read_text("file.txt", encoding = "UTF-8")
#' }
Path_read_text <- function(path, encoding = c("unknown", "UTF-8", "latin1")) {
  checkmate::qassert(path, "S1")
  enc <- base::match.arg(encoding)
  base::paste(base::readLines(path, encoding = enc), collapse = "\n")
}

#' Get the target path of a symbolic link.
#'
#' This function returns the target path of the specified symbolic links,
#' similar to Python's `pathlib.Path.readlink()` method.
#'
#' @param paths A character vector of symbolic link paths.
#'
#' @return A character vector containing the target path of each symbolic link.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_readlink("symbolic_link")
#' }
Path_readlink <- function(paths) {
  fs::link_path(paths)
}

#' Rename files or directories to new paths.
#'
#' This function renames files or directories from their current paths to new
#' paths. Similar to Python's `pathlib.Path.rename()` method.
#'
#' @param paths     A character vector of source file paths.
#' @param new_paths A character vector of target file paths.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_rename("old_file.txt", "new_file.txt")
#' }
Path_rename <- function(paths, new_paths) {
  fs::file_move(paths, new_paths)
}

#' Remove directories at the specified paths.
#'
#' This function removes directories at the specified paths. It first checks if
#' the directories exist and are empty. Similar to Python's
#' `pathlib.Path.rmdir()` method.
#'
#' @param paths A character vector of directory paths to remove.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_rmdir("empty_directory")
#' }
Path_rmdir <- function(paths) {
  checkmate::assert_directory_exists(paths)
  if (base::length(fs::dir_map(paths, base::identity)) != 0L) {
    base::stop("All `paths` must be empty.")
  }

  fs::dir_delete(paths)
}

#' @inherit Path_absolute
#' @export
Path_resolve <- Path_absolute

#' Get file information for the provided paths.
#'
#' This function retrieves file information for the provided paths, allowing an
#' optional argument `follow_symlinks` to specify whether symbolic links should
#' be followed when obtaining file information. Similar to Python's
#' `pathlib.Path.stat()` method.
#'
#' @param paths          A character vector of file paths.
#' @param ...            Additional arguments to pass to `fs::file_info()`.
#' @param follow_symlinks Logical. If TRUE, follow symbolic links to obtain file
#' information.
#'
#' @return A list containing file information for each path.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_stat("file.txt", follow_symlinks = TRUE)
#' }
Path_stat <- function(paths, ..., follow_symlinks = TRUE) {
  fs::file_info(paths, fail = TRUE, follow = follow_symlinks)
}

#' Create symbolic links from the paths to the target paths.
#'
#' This function creates symbolic links from the specified paths to the target
#' paths. Similar to Python's `pathlib.Path.symlink_to()` method.
#'
#' @param paths   A character vector of source file paths.
#' @param targets A character vector of target file paths.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_symlink_to("source_file.txt", "target_file.txt")
#' }
Path_symlink_to <- function(paths, targets) {
  fs::link_create(paths, targets, symbolic = TRUE)
}

#' Touch files at the specified paths.
#'
#' This function creates the files if they do not exist and updates the access
#' and modification times if they do exist. Similar to the Unix `touch` command.
#'
#' @param paths A character vector of file paths to touch.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_touch("file.txt")
#' }
Path_touch <- function(paths) {
  checkmate::qassert(paths, "S")
  now <- base::as.POSIXct(base::Sys.time())
  base::.Call("fs_create_", paths, 420L, PACKAGE = "fs")
  base::.Call("fs_touch_", paths, now, now, PACKAGE = "fs")
}

#' Unlink (delete) files at the specified paths.
#'
#' This function unlinks (deletes) files at the specified paths. It does not
#' remove directories. If the `missing_ok` argument is set to FALSE, it checks
#' if the files exist before attempting to unlink them. Similar to Python's
#' `pathlib.Path.unlink()` method.
#'
#' @param paths      A character vector of file paths to unlink.
#' @param missing_ok Logical. If TRUE, allow unlinking of non-existent files.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_unlink("file.txt")
#' }
Path_unlink <- function(paths, missing_ok = FALSE) {
  checkmate::qassert(paths, "S")
  checkmate::qassert(missing_ok, "B1")
  # Cannot use fs::file_delete because it deletes directories recursively
  if (base::isFALSE(missing_ok)) {
    if (!base::all(fs::file_exists(paths))) {
      base::stop("When `missing_ok` is FALSE, all `paths` must exist.")
    }
  }

  if (base::any(fs::is_dir(paths, follow = FALSE))) {
    base::stop("To remove directories, use `Path_rmdir` instead.")
  }

  base::.Call("fs_unlink_", paths, PACKAGE = "fs")
}

#' Write text to a file at the specified path.
#'
#' This function writes the provided text to a file at the specified path.
# Similar to Python's `pathlib.Path.write_text()` method.
#'
#' @param path A character vector specifying the path to the file.
#' @param text The text to write to the file.
#'
#' @export
#' @examples
#' \dontrun{
#' Path_write_text("file.txt", "Hello, World!")
#' }
Path_write_text <- function(path, text) {
  checkmate::qassert(path, "S1")
  base::writeLines(text, con = path)
}
