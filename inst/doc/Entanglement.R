## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ste1---------------------------------------------------------------------
library(qsimulatR)
library(qvirus)

# Create qubits
x1 <- qstate(nbits = 2)
x1

# Apply gates
x1_sup <- H(1)*x1
x1_sup

x1_entangled <- CNOT(c(1,2))*(x1_sup)
x1_entangled

truth.table(CNOT, nbits = 2)

## ----ste2---------------------------------------------------------------------
# Print probabilities
normalize_check(x1_sup, probs = TRUE)

normalize_check(x1_entangled, probs = TRUE)

## ----ste3---------------------------------------------------------------------
# Plot entangled states
qsimulatR::plot(x1_sup, qubitnames = c("|0>"))
qsimulatR::plot(x1_entangled, qubitnames = c("|0>"))

## ----ste4---------------------------------------------------------------------
a <- qstate(1) # |0>
b <- X(1)*a    # |1>
kronecker(a@coefs, b@coefs)
ab <- qstate(2, coefs = kronecker(a@coefs, b@coefs))
ab

## ----ste5, eval=FALSE, echo=FALSE---------------------------------------------
#  # qubit |x1>
#  x1 <- qstate(1, coefs = as.complex(c(.8, .6)))
#  x1
#  normalize_check(x1, probs = TRUE)
#  # qubit |x2>
#  x2 <- qstate(1, coefs = as.complex(c(5/13, 12/13)))
#  x2
#  normalize_check(x2, probs = TRUE)

## ----ste6, eval=FALSE, echo=FALSE---------------------------------------------
#  # quantum state |x1x2>
#  x1x2 <- qstate(2, coefs = as.complex(kronecker(x1@coefs, x2@coefs)))
#  x1x2

## ----ste7, eval=FALSE, echo=FALSE---------------------------------------------
#  # First CNOT : CNOT(|x1x2>)
#  # x1 is the control bit thus bit 2
#  # x2 is the target bit thus bit 1
#  res <- CNOT(c(2,1))*x1x2
#  res
#  probs <- normalize_check(res, probs = TRUE)
#  probs
#  # Prob(x1=|0>) => unchanged
#  as.numeric(probs[1]+probs[2])
#  # Prob(x1=|1>) => unchanged
#  as.numeric(probs[3]+probs[4])
#  # Prob(x2=|0>) => updated to 0.4014201 > 0.147929
#  as.numeric(probs[1]+probs[3])
#  # Prob(x2=|1>) => updated to 0.5985799 < 0.852071
#  as.numeric(probs[2]+probs[4])

## ----ste8, eval=FALSE, echo=FALSE---------------------------------------------
#  # Updated |x2>
#  x2 <- qstate(nbits = 1,
#                 coefs = as.complex(c(sqrt(probs[1]+probs[3]),
#                                      sqrt(probs[2]+probs[4]))))
#  x2
#  # Thus, quantum state |x1x2> is now
#  x1x2 <- qstate(2, coefs = as.complex(kronecker(x1@coefs, x2@coefs)))
#  x1x2

## ----ste9, eval=FALSE, echo=FALSE---------------------------------------------
#  # Second CNOT : CNOT(|x1x2>)
#  # x1 is the control bit thus bit 2
#  # x2 is the target bit thus bit 1
#  res <- CNOT(c(2,1))*x1x2
#  res
#  probs <- normalize_check(res, probs = TRUE)
#  probs
#  # Prob(x1=|0>) => unchanged
#  as.numeric(probs[1]+probs[2])
#  
#  # Prob(x1=|1>) => unchanged
#  as.numeric(probs[3]+probs[4])
#  
#  # Prob(x2=|0>) => updated to 0.4723976 > 0.4014201 > 0.147929
#  as.numeric(probs[1]+probs[3])
#  
#  # Prob(x2=|1>) => updated to 0.5276024 < 0.5985799 < 0.852071
#  as.numeric(probs[2]+probs[4])

## ----ste10, eval=FALSE, echo=FALSE--------------------------------------------
#  # updated |x2>
#  x2

## ----ste11--------------------------------------------------------------------
x1 <- qstate(1, coefs = as.complex(c(0.8, 0.6)))
x2 <- qstate(1, coefs = as.complex(c(0.38, 0.92)))
simulate_entanglement(x1, x2, iterations = 2, angle = pi/4)

## ----ayb----------------------------------------------------------------------
# Create qubits
AlBo <- qstate(nbits = 2)
x1

# Apply gates
H_AlBo <- H(1)*AlBo

Entangled <- CNOT(c(1,2))*(H_AlBo)
# Display the entangled state
Entangled

# Check normalization with probabilities
normalize_check(Entangled, probs = TRUE)

## ----ayb1, eval=FALSE, echo=FALSE---------------------------------------------
#  # Create qubit Alice in state |0>
#  AlBo <- qstate(nbits = 2)

## ----ayb2,  eval=FALSE, echo=FALSE--------------------------------------------
#  # Apply Hadamard gate to Alice
#  H_AlBo <- H(1) * AlBo
#  
#  # Apply CNOT gate to entangle Alice and Bob
#  Entangled <- CNOT(c(1, 2)) * (H_AlBo)
#  
#  # Display the entangled state
#  Entangled

## ----ayb3, eval=FALSE, echo=FALSE---------------------------------------------
#  # Check normalization with probabilities
#  normalize_check(Entangled, probs = TRUE)

## ----avp----------------------------------------------------------------------
# Application in drug design simulation
# Simulate entanglement to improve accuracy
# Quantum computer can handle complex states efficiently
# Suppose we're simulating a drug-protein interaction
# Here's a simplified example

# Quantum representation of drug and protein states
Bind <- qstate(nbits = 2)

# Entangle Drug and Protein states
Entangled_Drug_Protein <- CNOT(c(1, 2)) * (H(1)*Bind)

# Perform simulation operations using entangled states
# Measure and analyze results for drug-protein binding
# Quantum computer allows for more accurate simulations
Entangled_Drug_Protein

# Now, let's check the normalization of the entangled state
normalize_check(Entangled_Drug_Protein, probs = TRUE)

## ----avp1, eval=FALSE, echo=FALSE---------------------------------------------
#  # Perform a measurement on Alice
#  result <- measure(Entangled)
#  result
#  
#  
#  # Application in drug design simulation
#  # Simulate entanglement to improve accuracy
#  # Quantum computer can handle complex states efficiently
#  # Suppose we're simulating a drug-protein interaction
#  # Here's a simplified example
#  
#  # Quantum representation of drug and protein states
#  Bind <- qstate(nbits = 2)
#  
#  # Entangle Drug and Protein states
#  Entangled_Drug_Protein <- CNOT(c(1, 2)) * (H(1)*Bind)
#  
#  # Perform simulation operations using entangled states
#  # Measure and analyze results for drug-protein binding
#  # Quantum computer allows for more accurate simulations
#  Entangled_Drug_Protein
#  
#  # Now, let's check the normalization of the entangled state
#  normalize_check(Entangled_Drug_Protein, probs = TRUE)

## ----avpr---------------------------------------------------------------------
# Quantum representation of drug and protein states
Drug_State <- qstate(1, coefs = as.complex(c(1, 0)))  # State |0>
Protein_State <- qstate(1, coefs = as.complex(c(1, 0)))  # State |0>
# Entangle Drug and Protein states
# Perform simulation operations using entangled states
# Measure and analyze results for drug-protein binding
simulate_entanglement(Drug_State, Protein_State, 3, angle = pi/4)

## ----avp2, eval=FALSE, echo=FALSE---------------------------------------------
#  # Quantum representation of drug and protein states
#  Drug_State <- qstate(1, coefs = as.complex(c(1, 0)))  # State |0>
#  Protein_State <- qstate(1, coefs = as.complex(c(1, 0)))  # State |0>
#  
#  Separable_Drug_Protein <- qstate(2, coefs = kronecker(Drug_State@coefs, Protein_State@coefs))
#  
#  # Entangle Drug and Protein states
#  Entangled_Drug_Protein <- CNOT(c(2, 1)) * Separable_Drug_Protein
#  
#  angle <- pi/4  # Rotation angle
#  Entangled_Drug_Protein <- Rx(1, theta = angle) * Entangled_Drug_Protein
#  
#  # Perform simulation operations using entangled states
#  # Measure and analyze results for drug-protein binding
#  Entangled_Drug_Protein
#  
#  # Measure the entangled state
#  measured_state <- measure(Entangled_Drug_Protein)
#  
#  # Check the normalization of the entangled state with probabilities
#  normalize_check(Entangled_Drug_Protein, probs = TRUE)

## ----avp3, eval=FALSE, echo=FALSE---------------------------------------------
#  # Second CNOT : CNOT(|x1x2>)
#  # x1 is the control bit thus bit 2
#  # x2 is the target bit thus bit 1
#  res <- CNOT(c(2,1))*Entangled_Drug_Protein
#  res
#  probs <- normalize_check(Entangled_Drug_Protein, probs = TRUE)
#  probs
#  as.numeric(probs[1]+probs[2])
#  # Prob(x1=|1>) => unchanged
#  as.numeric(probs[3]+probs[4])
#  # Prob(x2=|0>) => updated to 0.4723976 > 0.4014201 > 0.147929
#  as.numeric(probs[1]+probs[3])
#  # Prob(x2=|1>) => updated to 0.5276024 < 0.5985799 < 0.852071
#  as.numeric(probs[2]+probs[4])

## ----avp4, eval=FALSE, echo=FALSE---------------------------------------------
#  # Updated Protein_State
#  Protein_State <- qstate(nbits = 1,
#                 coefs = as.complex(c(sqrt(probs[1]+probs[3]),
#                                      sqrt(probs[2]+probs[4]))))
#  Protein_State
#  
#  # Thus, quantum entangled state is now
#  Entangled_Drug_Protein <- qstate(2, coefs = kronecker(Drug_State@coefs, Protein_State@coefs))
#  
#  # Entangled Drug and Protein states
#  Entangled_Drug_Protein
#  
#  normalize_check(Entangled_Drug_Protein, probs = TRUE)

