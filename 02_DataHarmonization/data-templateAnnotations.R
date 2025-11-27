#' Template for data annotations or data key
#'
#' An empty template with the desired headers. Note that colums are seperated by ';' not ',' ...
#'
#' @format ## `who`
#' An empty data frame with no observations and 6 columns.
#' When using this template all data should be read in as characters:
#' \describe{
#'   \item{study_id}{Project wide unique study identifier}
#' of_variable; is_type; with_entry
#'   \item{table_id}{Study wide unique table identifier (should match table names)}
#'   \item{column_id}{Table wide unique column identifier (should match column names)}
#'   \item{of_variable}{Variable that the column is linked to, vocabulary is study specific and should be self documented in the annotation table}
#'   \item{is_type}{The type of information that is in the with-entry colun or contained in the associated data table}
#'   \item{with_entry}{The entry of specified information or an empty cell coded as '--' signifying the value is in the specified table.}
#' }
#' 
#' @source Data annotations compiled by Kathe Todd-Brown <ktoddbrown@ufl.edu> last updated on 15 January 2024