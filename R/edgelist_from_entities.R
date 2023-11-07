#' Generate Edge List or Matrix from Entity Pairs in Text
#'
#' Processes a dataframe containing text content and identifiers, as well as a list of entities,
#' to create an edge list or matrix based on the co-occurrence of entities within the same document.
#'
#' @param df A dataframe with text documents and their identifiers.
#' @param text_var The column in df containing text content.
#' @param id_var The column in df containing document identifiers.
#' @param entities_list A named list where each element is a vector of synonyms for an entity.
#' @param output_type The desired output: "edgelist" (default) or "matrix".
#' @return If output_type is "edgelist", a dataframe with columns for document_id, source entity, and target entity.
#'         If output_type is "matrix", a matrix of co-occurrence counts.
#' @export
#' @examples
#' \dontrun{
#' text_df <- data.frame(
#'   id = 1:3,
#'   text_content = c(
#'     "Acset Indonusa and Adhi Commuter Property",
#'     "ABM and ACE",
#'     "Astra Agro Lestari and Ace Hardware Indonesia collaborate"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' entities_list <- list(
#'   AALI = c("Astra Agro Lestari", "Astra Agro"),
#'   ABMM = c("ABM Investama", "ABM"),
#'   ACES = c("Ace Hardware Indonesia", "Ace Hardware", "Ace"),
#'   ACST = c("Acset Indonusa", "Acset"),
#'   ADCP = c("Adhi Commuter Properti", "Adhi")
#' )
#'
#' edges <- edgelist_from_entities(text_df, "text_content", "id", entities_list, "edgelist")
#' matrix <- edgelist_from_entities(text_df, "text_content", "id", entities_list, "matrix")
#' print(edges)
#' print(matrix)
#' }
edgelist_from_entities <- function(df, text_var, id_var, entities_list, output_type = "edgelist") {
  # Check if the input dataframe is valid
  if (!"data.frame" %in% class(df)) {
    stop("The 'df' argument must be a dataframe.")
  }

  # Check if text_var and id_var columns exist in the dataframe
  if (!text_var %in% names(df) || !id_var %in% names(df)) {
    stop("The specified 'text_var' or 'id_var' do not exist in the dataframe.")
  }

  # Initialize an empty dataframe for edge list and a matrix for co-occurrence
  edgelist <- data.frame(document_id = integer(), source = character(), target = character(), stringsAsFactors = FALSE)
  entities <- names(entities_list)
  matrix <- matrix(0, nrow = length(entities), ncol = length(entities), dimnames = list(entities, entities))

  # Extract the columns based on text_var and id_var
  text_var <- df[[text_var]]
  id_var <- df[[id_var]]

  # Iterate through each document
  for (i in seq_along(text_var)) {
    current_text <- tolower(text_var[i]) # Convert text to lowercase
    document_id <- id_var[i] # Get current document ID

    # Detect entities within the text
    detected_entities <- sapply(entities_list, function(synonyms) {
      any(sapply(synonyms, function(synonym) {
        grepl(paste0("\\b", tolower(synonym), "\\b"), current_text) # Match synonyms using word boundaries
      }))
    })

    # Filter and name the detected entities
    names(detected_entities) <- entities
    detected_entities <- names(detected_entities[detected_entities])

    # Generate combinations of detected entities if more than one entity is detected
    if (length(detected_entities) > 1) {
      detected_combinations <- expand.grid(source = detected_entities, target = detected_entities)
      detected_combinations <- detected_combinations[detected_combinations$source != detected_combinations$target, ]

      # If output is an edge list, append the combinations to the edgelist dataframe
      if (output_type == "edgelist") {
        detected_combinations$document_id <- document_id
        edgelist <- rbind(edgelist, detected_combinations)
      } else if (output_type == "matrix") {
        # Corrected matrix population
        combinations <- t(combn(detected_entities, 2))
        for (k in seq_len(nrow(combinations))) {
          matrix[combinations[k, 1], combinations[k, 2]] <- matrix[combinations[k, 1], combinations[k, 2]] + 1
          matrix[combinations[k, 2], combinations[k, 1]] <- matrix[combinations[k, 2], combinations[k, 1]] + 1
        }
      }
    }
  }

  # Finalize the edge list or matrix before returning
  if (output_type == "edgelist") {
    # Remove duplicate edges and sort the edgelist
    edgelist <- edgelist[!duplicated(t(apply(edgelist[, c("source", "target")], 1, sort))), ]
    edgelist <- edgelist[order(edgelist$document_id, edgelist$source, edgelist$target), ]
    row.names(edgelist) <- NULL # Reset row indices for a clean output
    return(edgelist) # Return the edge list
  } else if (output_type == "matrix") {
    return(matrix) # Return the matrix
  } else {
    stop("Invalid output type. Please choose 'edgelist' or 'matrix'.") # Handle invalid output types
  }
}
