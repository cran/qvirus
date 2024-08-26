#' Simulate Entanglement Evolution
#'
#' This function simulates the evolution of entanglement between two quantum states x1 and x2 using the CNOT gate.
#'
#' @param x1 Quantum state for qubit 1, represented as a qstate object.
#' @param x2 Quantum state for qubit 2, represented as a qstate object.
#' @param iterations Number of iterations for the entanglement process.
#' @param angle Rotation angle for applying Rx gate.
#' @param verbose If TRUE, prints detailed information to the console.
#'
#' @return A list containing the entangled quantum state x2 after each iteration and other relevant information.
#' @export
#'
#' @examples
#' library(qsimulatR)
#' library(qvirus)
#' x1 <- qstate(1, coefs = as.complex(c(0.8, 0.6)))
#' x2 <- qstate(1, coefs = as.complex(c(0.38, 0.92)))
#' results <- simulate_entanglement(x1, x2, iterations = 3, angle = pi/4, verbose = TRUE)
#' print(results)
simulate_entanglement <- function(x1, x2, iterations, angle, verbose = FALSE) {
  results <- list()  # Initialize a list to store results
  
  for (i in 1:iterations) {
    if (verbose) {
      message("Iteration: ", i)
      message("x1:")
      print(qvirus::normalize_check(x1, probs = TRUE))
      message("x2:")
      print(qvirus::normalize_check(x2, probs = TRUE))
    }
    
    # Apply the CNOT gate to entangle x1 and x2
    res <- qsimulatR::CNOT(c(2,1)) * qsimulatR::qstate(2, coefs = as.complex(kronecker(x1@coefs, x2@coefs)))
    probs <- qvirus::normalize_check(res, probs = TRUE)
    
    # Update x2 based on entanglement results
    x2 <- qsimulatR::qstate(nbits = 1, 
                            coefs = as.complex(c(sqrt(probs[1] + probs[3]), 
                                                 sqrt(probs[2] + probs[4]))))
    
    if (verbose) {
      message("x2 update:")
      print(qvirus::normalize_check(x2, probs = TRUE))
    }
    
    # Apply the Rx gate
    entangled_state <- qsimulatR::Rx(1, theta = angle) * res
    probsr <- qvirus::normalize_check(entangled_state, probs = TRUE)
    
    # Update x2 based on Rx results
    x2r <- qsimulatR::qstate(nbits = 1, 
                             coefs = as.complex(c(sqrt(probsr[1] + probsr[3]), 
                                                  sqrt(probsr[2] + probsr[4]))))
    
    if (verbose) {
      message("x2r update:")
      print(qvirus::normalize_check(x2r, probs = TRUE))
    }
    
    # Store the result for this iteration
    results[[i]] <- list(
      iteration = i,
      x1 = qvirus::normalize_check(x1, probs = TRUE),
      x2_before = qvirus::normalize_check(x2, probs = TRUE),
      x2_after = qvirus::normalize_check(x2r, probs = TRUE),
      entangled_state = entangled_state
    )
  }
  
  return(results)
}