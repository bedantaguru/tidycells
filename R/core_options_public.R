
#' Set Package Options
#'
#' This function sets user-specific options for the package, such as whether to
#' derive nice attribute names or enable Excel-specific optimization etc..
#'
#' @param derive_nice_names_for_attributes Logical. Should the package derive
#'   user-friendly names for attributes? Default is \code{FALSE}.
#' @param excel_specific_optimization Logical. Should Excel-specific
#'   optimization be enabled? Default is \code{FALSE}.
#'
#' @details This function stores the options in the package cache under the
#' "user" namespace, which can affect package behavior such as naming
#' conventions and optimization for Excel based cells etc.
#'
#' @return No return value; called for its side effects.
#' @export
set_options <- function(
    derive_nice_names_for_attributes = FALSE,
    excel_specific_optimization = FALSE) {
  # This function sets options for the package.
  util_pkg_cache(
    derive_nice_names_for_attributes = derive_nice_names_for_attributes,
    excel_specific_optimization = excel_specific_optimization,
    pkg_cache_head_name = "user")
}


# Specific get option method as system (by this package) and user both can set
# options. Priority is given to user options. Also a default value can be set
# (in case not defined by system or user).
#
# Ideally wherever we use this function (core_opt_get) all unique options should
# be listed to users through (set_options) with proper documentation.
core_opt_get <- function(key, default = NULL) {

  user_opt_exists <-
    util_pkg_cache(key, pkg_cache_head_name = "user", exists = TRUE)
  user_opt_val <- util_pkg_cache(key, pkg_cache_head_name = "user")

  sys_opt_exists <-
    util_pkg_cache(key, pkg_cache_head_name = "system", exists = TRUE)
  sys_opt_val <- util_pkg_cache(key, pkg_cache_head_name = "system")

  if (user_opt_exists) {
    return(user_opt_val)
  } else if (sys_opt_exists) {
    return(sys_opt_val)
  } else {
    return(default)
  }

}
