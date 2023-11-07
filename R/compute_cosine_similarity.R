#' Compute Cosine Similarity
#'
#' This function calculates the cosine similarity between documents in a dataframe.
#' The output can be returned as either a matrix or an edge list.
#'
#' @param dataframe A dataframe containing the documents and content.
#' @param doc_col The name of the column in the dataframe that contains document identifiers.
#' @param content_col The name of the column that contains the document content.
#' @param output_type The type of output required: "matrix" or "edgelist".
#' @return A matrix or edge list representing the cosine similarities between documents.
#' @export
#' @examples
#' \dontrun{
#' # Example dataframe with 'id' and 'text' columns
#' df <- data.frame(id = c(1, 2, 3), text = c("text one", "text two", "text three"))
#' # Compute cosine similarity and return a matrix
#' similarity_matrix <- compute_cosine_similarity(df, "id", "text", "matrix")
#' }
compute_cosine_similarity <- function(dataframe, doc_col, content_col, output_type = "matrix") {
  # Validate the output_type argument
  if (!output_type %in% c("matrix", "edgelist")) {
    stop("Output type must be either 'matrix' or 'edgelist'.")
  }

  # Create an iterator over the documents
  it <- text2vec::itoken(dataframe[[content_col]], tokenizer = text2vec::word_tokenizer, ids = dataframe[[doc_col]])

  # Build the vocabulary and prune it
  v <- text2vec::create_vocabulary(it) |>
    text2vec::prune_vocabulary(term_count_min = 1, doc_proportion_max = 1.0)
  vectorizer <- text2vec::vocab_vectorizer(v)

  # Create a Document-Term Matrix (DTM)
  dtm <- text2vec::create_dtm(it, vectorizer)

  # Compute the cosine similarity from the DTM
  similarity_matrix <- text2vec::sim2(as(dtm, "CsparseMatrix"), method = "cosine")

  # Select the requested output type
  if (output_type == "matrix") {
    # Convert to a dataframe matrix
    return(as.data.frame(as.matrix(similarity_matrix)))
  } else {
    # Create an edge list without the diagonal and duplicates
    sim_list <- summary(Matrix::triu(similarity_matrix, diag = FALSE))
    edge_list <- data.frame(
      source = sim_list$i,
      target = sim_list$j,
      weight = sim_list$x,
      stringsAsFactors = FALSE
    )
    # Filter out entries where source is equal to target if they slipped through
    edge_list <- edge_list[edge_list$source != edge_list$target, ]
    return(edge_list)
  }
}
