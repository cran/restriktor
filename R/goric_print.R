print.con_goric <- function(x, digits = max(3, getOption("digits") - 4), ...) {
  
  type <- x$type
  comparison <- x$comparison
  dig <- paste0("%6.", digits, "f")
  x2 <- lapply(x$result[-1], sprintf, fmt = dig)
  df <- data.frame(model = x$result$model, x2)
  
  cat(sprintf("restriktor (%s): ", packageDescription("restriktor", fields = "Version")))
  
  if (type == "goric") {
    cat("generalized order-restricted information criterion: \n")
  } else if (type == "gorica") {
    cat("generalized order-restricted information criterion approximation:\n")
  } else if (type == "goricc") {
    cat("small sample generalized order-restricted information criterion:\n")
  } else if (type == "goricac") {
    cat("small sample generalized order-restricted information criterion approximation:\n")
  }
  
  wt_bar <- sapply(x$objectList, function(x) attr(x$wt.bar, "method") == "boot")
  # we need a check if the hypothesis is equalities only
  ceq_only <- sapply(x$objectList, function(x) nrow(x$constraints) == x$neq)
  wt_bar <- as.logical(wt_bar * !ceq_only)
  
  if (sum(wt_bar) > 0) {
    wt_method_boot <- x$objectList[wt_bar]
    wt_bootstrap_draws  <- sapply(wt_method_boot, function(x) attr(x$wt.bar, "mix.bootstrap"))
    wt_bootstrap_errors <- sapply(wt_method_boot, function(x) attr(x$wt.bar, "error.idx"))
    max_nchar <- max(nchar(names(wt_method_boot)))
    len <- length(wt_method_boot)
    if (len > 0) {
      cat("\n")
      wt_bootstrap_draws_succesful <- wt_bootstrap_draws[1] - sapply(wt_bootstrap_errors, length)
      
      if (!is.null(x$messages$mix_weights)) {
        text1 <- paste("Note: Since the constraint matrix for hypotheses", paste0(sQuote(names(wt_method_boot)), collapse = " and "), 
                "is not full row-rank, we used the 'boot' method for calculating", 
                "the penalty term value. For additional details, see '?goric' or the Vignette.")
      }
      
      if (any(sapply(wt_bootstrap_errors, function(x) length(x) > 0))) { 
        if (!all(wt_bootstrap_draws_succesful == wt_bootstrap_draws[1])) {
          cat("Bootstrap-based penalty term calculation:\n")
          cat("  Number of requested bootstrap draws", wt_bootstrap_draws[1], "\n")
          for (i in 1:len) {
            cat(paste0("  Number of successful bootstrap draws for ", sprintf(paste0("%", max_nchar, "s"), names(wt_method_boot)[i]), ": ", (wt_bootstrap_draws[1] - length(wt_bootstrap_errors[[i]])), "\n"))
          }
        } 
        text2 <- paste("Advise: If a substantial number of bootstrap draws fail to converge,", 
                "the resulting penalty term may become unreliable. In such cases, it is advisable", 
                "to increase the number of bootstrap draws (e.g.,", paste0("mix.bootstrap > ", wt_bootstrap_draws[1]), ".")
      }
    }
  }
  
  cat("\nResults:\n")
  print(format(df, digits = digits, scientific = FALSE), 
        print.gap = 2, quote = FALSE)
  #cat("---\n")
  
  if (exists("text1")) message("---\n", text1)
  if (exists("text2")) message("---\n", text2)
  
  # Calculate the absolute difference between the logliks
  loglik <- x$result$loglik
  #-2 * (as.numeric(A)-as.numeric(B)))
  loglik_diff <- as.matrix(dist(loglik, diag = TRUE))
  # we only need the lower or upper part
  loglik_diff[upper.tri(loglik_diff)] <- NA
  diag(loglik_diff) <- NA
  # Create a matrix with the logical vector and assign dimension names
  loglik_diff_mat <- matrix(loglik_diff, nrow(loglik_diff), ncol(loglik_diff), 
                            dimnames = list(rownames(x$ratio.gw), colnames(x$ratio.gw)))
  # which hypotheses overlap, i.e., have equal likelihood values
  loglik_overlap <- which(loglik_diff_mat == 0, arr.ind = TRUE)
  # get unique combination from row- and columnnames
  overlap_unique_combinations <- apply(loglik_overlap, 1, function(x) {
    get_names <- c(rownames(loglik_diff_mat)[x[1]], gsub("vs. ", "", colnames(loglik_diff_mat)[x[2]]))
    paste(get_names, collapse = " vs. ")
  })
  
  # # which hypotheses have nearly equal likelihood values
  # # Create a logical vector based on the threshold
  # loglik_diff_bound <- loglik_diff_mat > 0 & loglik_diff_mat < threshold
  # # which indices are TRUE
  # true_indices <- which(loglik_diff_bound == TRUE, arr.ind = TRUE)
  # # get unique combination from row- and columnnames
  # bound_unique_combinations <- apply(true_indices, 1, function(x) {
  #   get_names <- c(rownames(loglik_diff_mat)[x[1]], gsub("vs. ", "", colnames(loglik_diff_mat)[x[2]]))
  #   paste(get_names, collapse = " vs. ")
  # })
  
  # Create a function to sort the elements in each string
  sort_combination <- function(combination) {
    split_combination <- strsplit(combination, " vs. ")[[1]]
    #sorted_combination <- sort(split_combination)
    # Check if the combination includes "complement"
    if ("complement" %in% split_combination) {
      # Find the other element that is not "complement"
      other_element <- split_combination[split_combination != "complement"]
      sorted_combination <- c(other_element, "complement")
    } else if ("unconstrained" %in% split_combination) {
      # Find the other element that is not "unconstrained"
      other_element <- split_combination[split_combination != "unconstrained"]
      sorted_combination <- c(other_element, "unconstrained")
    } else {
      sorted_combination <- sort(split_combination)
    }
    paste(sorted_combination, collapse = " vs. ")
  }

  overlap_sorted_vector <- sapply(overlap_unique_combinations, sort_combination)
  overlap_unique_combinations <- unique(overlap_sorted_vector)
  
  #bound_sorted_vector <- sapply(bound_unique_combinations, sort_combination)
  #bound_unique_combinations <- unique(bound_sorted_vector)
  
  # check if the likelihood of models are equal, this means that the hypotheses overlap (i.e., subset)
  if (length(overlap_unique_combinations) > 0) {
    # message("Note: The hypotheses ", paste(sQuote(overlap_unique_combinations), collapse = ", "), 
    #         " overlap with one another (i.e., they have equal likelihood values).", 
    #         " Consequently, the GORIC(A) (ratio) weights are solely influenced by fluctuations in penalty values",
    #         " and therefore have an upper limit. If any of the overlapping hypotheses proves to be the most", 
    #         " convincing (i.e., has the lowest GORIC(A) value) in the full set,",
    #         " we recommend further evaluation by comparing it to its complement.\n"
    # )
    message("---\nNote: Hypotheses ", paste(sQuote(overlap_unique_combinations), collapse = ", "), 
            " overlap (equal likelihood values). GORIC(A) weights, influenced solely by penalty value fluctuations,",
            " have an upper limit. If any overlap is most convincing (lowest GORIC(A) value in the full set),",
            " further evaluation against its complement is recommended.")
  }


  # # check if loglik are nearly equal, 
  # if (length(bound_unique_combinations) > 0) {
  #   text4 <- paste("Note: Some hypotheses yield nearly identical log-likelihoods, making GORIC(A)", 
  #           "weights sensitive to penalty fluctuations. For a more robust comparison, examine", 
  #           "how hypothesized and unconstrained model parameters differ:")
  #   message(text4)
  #   cat(paste("  -", bound_unique_combinations, collapse = "\n"), "\n\n")
  #   cat("Hypothesized model parameters:\n")
  #   print(coef(x))
  #   cat("\n")
  #   
  #   # haal penalty weg.
  #   # voeg toe: dat ze de model par kunnen gebruiken voor niuwe hypo op te stellen.
  # }
  
  if (comparison == "unconstrained" && length(df$model) == 2) {
    message("---\nAdvise: Are you certain you wish to assess the order-restricted hypothesis", 
            "in comparison to the unconstrained one, rather than its complement?")
  }
  
  if (comparison == "complement" && length(overlap_unique_combinations) == 0) {# && length(bound_unique_combinations) == 0) { 
    objectnames <- as.character(df$model)
    class(x$ratio.gw) <- "numeric"
    cat("---\nThe order-restricted hypothesis", sQuote(objectnames[1]), "has", sprintf("%.3f", x$ratio.gw[1,2]), "times more support than its complement.\n\n")
  } else if (comparison == "unconstrained" && length(overlap_unique_combinations) == 0 && length(df$model) == 2) { #&& length(bound_unique_combinations) == 0) {
    objectnames <- as.character(df$model)
    formatted_numbers <- sprintf("%.3f %.3f", x$result[[7]][1], x$result[[7]][2])
    numbers <- strsplit(formatted_numbers, " ")[[1]]
    if (as.numeric(numbers[1]) / as.numeric(numbers[2]) > 1) {
      result <- paste(numbers[1], "/", numbers[2], "> 1", sep = " ")
      cat("---\nThe order-restricted hypothesis", sQuote(objectnames[1]), "has", result, "times more support than the unconstrained.\n\n")
    } else if (as.numeric(numbers[1]) / as.numeric(numbers[2]) < 1) {
      result <- paste(numbers[1], "/", numbers[2], "< 1", sep = " ")
      cat("---\nThe order-restricted hypothesis", sQuote(objectnames[1]), "has", result, "times more support than the unconstrained.\n\n")      
    } else {
      result <- paste(numbers[1], "/", numbers[2], "= 1", sep = " ")
      cat("---\nThe order-restricted hypothesis", sQuote(objectnames[1]), "and the unconstrained have equal support:", result, "\n\n")      
    }
  }
  #invisible(x)
}
