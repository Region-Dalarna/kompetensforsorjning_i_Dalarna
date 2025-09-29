# Funktion som hjälper till att matcha kolumner mellan två dataframes

build_program_map <- function(from, to,
                              custom = NULL,        # e.g. c("Handels- och administration"="Försäljnings- ...")
                              ignore_case = TRUE,
                              max_dist = 8,         # maximum allowed edit distance for auto-matches
                              drop_unmatched = TRUE) {
  from <- unique(from); to <- unique(to)
  
  # normalise for matching (case-insensitive if wanted)
  norm <- function(x) if (ignore_case) tolower(x) else x
  f_n <- norm(from); t_n <- norm(to)
  
  # distance matrix using base R (Levenshtein)
  dmat <- adist(f_n, t_n, partial = FALSE, ignore.case = FALSE)
  best_j <- max.col(-dmat, ties.method = "first")      # index of best match in 'to' per 'from'
  best_to <- to[best_j]
  best_dist <- dmat[cbind(seq_along(from), best_j)]
  
  # drop too-distant matches
  best_to[best_dist > max_dist] <- NA_character_
  
  # apply manual overrides (force specific mappings)
  if (!is.null(custom)) {
    for (nm in intersect(names(custom), from)) {
      best_to[which(from == nm)] <- unname(custom[[nm]])
    }
  }
  
  # build named vector: names = short (from), values = canonical (to)
  map <- best_to
  names(map) <- from
  
  if (drop_unmatched) map <- map[!is.na(map)]
  return(map)
}