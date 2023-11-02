#' Create an edge list from a dataframe based on groups
#'
#' This function takes a dataframe and constructs an edge list where each item within the same group is connected.
#' It can optionally remove duplicate edges.
#'
#' @param df A dataframe with at least two columns.
#' @param group_col The name of the column that contains the grouping variable.
#' @param item_col The name of the column that contains the items to be connected within groups.
#' @param remove_duplicates Logical; if TRUE, removes duplicate edges.
#'
#' @return A dataframe with two columns representing the source and target of each edge.
#' @examples
#' df <- data.frame(
#'   Group = c('A', 'A', 'B', 'B', 'B', 'C'),
#'   Item = c('Item1', 'Item2', 'Item1', 'Item2', 'Item3', 'Item1')
#' )
#' edge_list <- edgelist_from_group(df, 'Group', 'Item', remove_duplicates = TRUE)
#' print(edge_list)
#'
#' @export
edgelist_from_group  <- function(df, group_col, item_col, remove_duplicates = FALSE) {

  # Dummy assignment to avoid R CMD check notes
  Var1 <- Var2 <- NULL

  # Split the dataframe by group
  groups <- split(df[[item_col]], df[[group_col]])

  # Initialize list to store edge list
  edges <- lapply(groups, function(group) {
    if(length(group) < 2) return(NULL) # If there is only one item, there is no edge
    # Create all combinations of item pairs within the group
    combinations <- expand.grid(group, group)
    # Remove pairs with the same items
    combinations <- subset(combinations, Var1 != Var2)
    if(remove_duplicates) {
      # Remove duplicates by comparing after sorting
      combinations <- combinations[!duplicated(t(apply(combinations, 1, sort))),]
    }
    return(combinations)
  })

  # Combine all edge lists from each group and remove empty rows
  edge_list <- do.call(rbind, edges)
  edge_list <- edge_list[stats::complete.cases(edge_list), ]

  # Remove row indices
  rownames(edge_list) <- NULL

  # Change column names
  colnames(edge_list) <- c("source", "target")

  return(edge_list)
}
