#' Generate Edge List from Grouped Data
#'
#' This function creates an edge list from a dataframe where each row represents an edge. The edges are formed between items within the same group.
#'
#' @param df A dataframe containing at least two columns: one for the group identifier and one for the items.
#' @param group_col The name of the column in the dataframe that identifies the group.
#' @param item_col The name of the column that contains the items.
#' @param remove_duplicates Logical; if TRUE, duplicate edges are removed, considering that an edge between two items is undirected.
#' @return A dataframe representing the edge list, with 'source' and 'target' columns.
#' @export
#' @examples
#' \dontrun{
#' # Example dataframe with 'group' and 'item' columns
#' df <- data.frame(
#'   group = c("A", "A", "B", "B", "B"),
#'   item = c(1, 2, 1, 3, 4),
#'   stringsAsFactors = FALSE
#' )
#' # Generate edge list without removing duplicates
#' edge_list <- edgelist_from_group(df, "group", "item", remove_duplicates = FALSE)
#' }
edgelist_from_group <- function(df, group_col, item_col, remove_duplicates = FALSE) {
  # Ensure the input is a dataframe
  if (!is.data.frame(df)) {
    stop("The input df must be a data frame.")
  }

  # Ensure the specified columns exist in the dataframe
  if (!all(c(group_col, item_col) %in% names(df))) {
    stop("group_col and/or item_col do not exist in df.")
  }

  # Split the dataframe into groups based on the group_col
  groups <- split(df[[item_col]], df[[group_col]])

  # Initialize an empty list to store the edges
  edges <- list()

  # Iterate through each group to create edges
  for (group in groups) {
    # Skip groups with fewer than two items as no edges can be formed
    if (length(group) < 2) {
      next
    }
    # Generate all possible pairings within a group
    group_combinations <- expand.grid(group, group)
    # Remove self-paired combinations
    group_combinations <- subset(group_combinations, Var1 != Var2)
    # Remove duplicate edges if specified
    if (remove_duplicates) {
      group_combinations <- group_combinations[!duplicated(t(apply(group_combinations, 1, sort))), ]
    }
    # Add the combinations to the list of edges
    edges <- c(edges, list(group_combinations))
  }

  # Combine edges from all groups into a single dataframe
  edge_list <- do.call(rbind, edges)
  # Remove any incomplete cases
  edge_list <- na.omit(edge_list)

  # Set the column names to 'source' and 'target'
  colnames(edge_list) <- c("source", "target")

  # Reset the row names to ensure they are sequential
  rownames(edge_list) <- NULL

  # Return the final edge list
  return(edge_list)
}
