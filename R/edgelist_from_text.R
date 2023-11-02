#' Create an edge list from a text using a sliding window
#'
#' This function takes a string of text and creates an edge list based on a sliding window.
#' It can optionally remove duplicate edges and exclude specified stopwords.
#'
#' @param text A character string from which to create the edge list.
#' @param window_size An integer defining the size of the sliding window.
#' @param remove_duplicates Logical; if TRUE, removes duplicate edges.
#' @param stopwords A character vector of words to be removed from the text.
#'
#' @return A data frame with two columns representing the source and target of each edge.
#' @examples
#' text <- "I like you in hate"
#' stopwords <- c("in")
#' edge_list <- edgelist_from_text(text, 3, remove_duplicates = TRUE, stopwords = stopwords)
#' print(edge_list)
#'
#' @export
edgelist_from_text <- function(text = NULL, window_size = 1, remove_duplicates = FALSE, stopwords = character(0)) {
  # Convert text to lowercase to ignore case
  text <- tolower(text)
  # Convert stopwords to lowercase to ignore case
  stopwords <- tolower(stopwords)

  # Split text into words and remove stopwords
  words <- unlist(strsplit(text, " "))
  words <- words[!words %in% stopwords]
  len <- length(words)

  # Initialize a list to store edges
  edge_list <- vector("list", length = len * (window_size * 2))

  # Variable to keep track of the index in the list
  edge_index <- 1

  # Loop through each word
  for (i in 1:len) {
    # Define the boundaries for the context window
    window_indices <- (max(1, i - window_size)):(min(len, i + window_size))

    # Remove the index that is the same as the current word
    window_indices <- window_indices[window_indices != i]

    # Add pairs to the list
    for (j in window_indices) {
      edge_list[[edge_index]] <- list(source = words[i], target = words[j])
      edge_index <- edge_index + 1
    }
  }

  # Remove NULL elements and convert to a data frame
  edge_list <- do.call(rbind.data.frame, edge_list)

  if (remove_duplicates) {
    edge_list <- edge_list[!duplicated(t(apply(edge_list, 1, sort))), ]
  }

  # Set the correct column names
  names(edge_list) <- c("source", "target")

  return(edge_list)
}
