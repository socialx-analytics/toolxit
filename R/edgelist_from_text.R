#' Generate Edge List from Text with Context Window
#'
#' Processes a string of text and generates an edge list where each edge represents a pair of words that appear within a certain distance defined by the window size. The function allows for the removal of stopwords and duplicates to reduce noise in the edge list.
#'
#' @param text A string containing the text to be processed.
#' @param window_size A positive integer indicating the size of the context window around each word for edge formation (default is 1).
#' @param remove_duplicates Logical; if TRUE, duplicate edges are removed, considering that an edge between two words is undirected.
#' @param stopwords A character vector of stopwords to be removed from the text (default is an empty character vector).
#' @return A dataframe with two columns 'source' and 'target', each row representing an edge between two words in the text within the context window.
#' @export
#' @examples
#' \dontrun{
#' sample_text <- "I like programming in R"
#' sample_stopwords <- c("in")
#' # Generate edge list with a window size of 4 and remove duplicates
#' edge_list <- edgelist_from_text(sample_text, 4, TRUE, sample_stopwords)
#' print(edge_list)
#' }
edgelist_from_texts <- function(texts = NULL, window_size = 2, remove_duplicates = FALSE, stopwords = character(0)) {
  # Validate input parameters
  if (is.null(texts) || !is.character(texts) || length(texts) == 0) {
    stop("Text must be a non-empty character vector.")
  }

  if (!is.numeric(window_size) || window_size < 1) {
    stop("Window size must be a positive integer.")
  }

  # Initialize an empty data frame to store edges
  edge_list <- data.frame(source = character(), target = character(), stringsAsFactors = FALSE)

  # Iterate over each text string
  for (text in texts) {
    # Convert text to lowercase
    text <- tolower(text)

    # Split text into words, remove stopwords, and define word length
    words <- unlist(strsplit(text, "\\W+"))
    words <- words[!words %in% stopwords] # Remove stopwords
    len <- length(words) # Get the number of words after stopwords removal

    # Generate edges based on the context window
    for (i in 1:len) {
      window_start <- max(1, i - window_size)
      window_end <- min(len, i + window_size)
      window_indices <- window_start:window_end
      window_indices <- window_indices[window_indices != i]

      # Create edges within the context window
      for (j in window_indices) {
        edge_list <- rbind(edge_list, data.frame(source = words[i], target = words[j]))
      }
    }
  }

  # Remove duplicates if requested
  if (remove_duplicates) {
    edge_list <- edge_list[!duplicated(t(apply(edge_list, 1, sort))), ]
  }

  # Reset row names to ensure clean output
  row.names(edge_list) <- NULL

  return(edge_list)
}
