## ----setup--------------------------------------------------------------------
library(qsimulatR)
ket0 <- qstate(nbits = 1)
ket0

## ----str----------------------------------------------------------------------
ket0 |> str()

## ----ket1---------------------------------------------------------------------
ket1 <- qstate(nbits = 1, coefs = c(0, 1))
ket1

## ----str1---------------------------------------------------------------------
ket1 |> str()

## ----ket0---------------------------------------------------------------------
ket1 <- X(1)*ket0
ket1

## ----str0---------------------------------------------------------------------
ket1 |> str()

## ----sum----------------------------------------------------------------------
Mod(ket0@coefs)^2
sum(Mod(ket0@coefs)^2)

## ----norm---------------------------------------------------------------------
library(qvirus)
normalize_check(ket0)
normalize_check(ket1)

## ----compl--------------------------------------------------------------------
complex_check(ket0)
complex_check(ket1)

## ----spos---------------------------------------------------------------------
psi <- qstate(nbits = 1, coefs = c((1+2i)/3, -2/3))
psi |> str()

## ----mspq---------------------------------------------------------------------
# Define coefficients for the superposition states
alpha_00 <- 1 / sqrt(2)  # Coefficient for |00⟩
alpha_01 <- 1 / sqrt(2)  # Coefficient for |01⟩
alpha_10 <- 1 / sqrt(2)  # Coefficient for |10⟩
alpha_11 <- 1 / sqrt(2)  # Coefficient for |11⟩

# Create quantum state objects for the superposition states
ket00 <- qstate(nbits = 2, coefs = as.complex(c(alpha_00, 0, 0, 0)), basis = "|00>")
ket01 <- qstate(nbits = 2, coefs = as.complex(c(0, alpha_01, 0, 0)), basis = "|01>")
ket10 <- qstate(nbits = 2, coefs = as.complex(c(0, 0, alpha_10, 0)), basis = "|10>")
ket11 <- qstate(nbits = 2, coefs = as.complex(c(0, 0, 0, alpha_11)), basis = "|11>")
str(c(ket00,ket10,ket01,ket11))

## ----spck---------------------------------------------------------------------
complex_check(psi) 
normalize_check(psi)

## ----mqbt---------------------------------------------------------------------
# Validation checks
complex_check(ket00)  # Check coefficients of ket00 are complex
normalize_check(ket00)  # Check ket00 is normalized

complex_check(ket01)  # Check coefficients of ket01 are complex
normalize_check(ket01)  # Check ket01 is normalized

complex_check(ket10)  # Check coefficients of ket10 are complex
normalize_check(ket10)  # Check ket10 is normalized

complex_check(ket11)  # Check coefficients of ket11 are complex
normalize_check(ket11)  # Check ket11 is normalized

## ----npqs1--------------------------------------------------------------------
# Define the complex coefficients for the superpositioned state
coefficients <- c(0.5+0.5i, 0.5-0.5i, -0.5+0.5i, -0.5-0.5i)

# Create the two-qubit state with the defined coefficients and basis states
ket_superposition <- qstate(nbits = 2, coefs = coefficients, basis = c("|00>", "|01>", "|10>", "|11>"))

# Display the structure of the created state
ket_superposition |> str()

## ----npqs2--------------------------------------------------------------------
# Perform complex check on the superpositioned state
complex_check(ket_superposition)
# Perform normalize check on the superpositioned state
normalize_check(ket_superposition)

## ----npqs3--------------------------------------------------------------------
# Define the parameters
theta <- pi/4
phi <- pi/6

# Create the quantum state
psi_qubit1 <- pure_qubit1(theta, phi)
psi_qubit1 |> str()

# Validation checks
normalize_check(psi_qubit1)
complex_check(psi_qubit1)

## ----npqs4--------------------------------------------------------------------
# Define the parameters
theta1 <- pi/3
theta2 <- pi/4
phi1 <- pi/6
phi2 <- pi/5

# Create the quantum state
psi_qubit2 <- pure_qubit2(theta1, theta2, phi1, phi2)
psi_qubit2 |> str()

# Validation checks
normalize_check(psi_qubit2)
complex_check(psi_qubit2)

## ----mqso---------------------------------------------------------------------
normalize_check(psi, probs = TRUE)

## ----zass---------------------------------------------------------------------
six_state(1)[[1]] |> str()
six_state(2)[[1]] |> str()

## ----xass---------------------------------------------------------------------
six_state(3)[[1]] |> str()
six_state(4)[[1]] |> str()

## ----yass---------------------------------------------------------------------
six_state(5)[[1]] |> str()
six_state(6)[[1]] |> str()

## ----eigz---------------------------------------------------------------------
Z(1)*six_state(1)[[1]]
Z(1)*six_state(2)[[1]]

## ----eigx---------------------------------------------------------------------
X(1)*six_state(3)[[1]]
X(1)*six_state(4)[[1]]

## ----eigy---------------------------------------------------------------------
Y(1)*six_state(5)[[1]]
Y(1)*six_state(6)[[1]]

## ----braz---------------------------------------------------------------------
# <0| is a row vector
bra0 <- six_state(1)[[1]] |> conjugate_transpose()
bra0
# ∣1> is a col vector
bra0 %*% six_state(2)[[1]]@coefs 
# <1| is a row vector
bra1 <- six_state(2)[[1]] |> conjugate_transpose()
bra1
# ∣0> is a col vector
bra1 %*% six_state(1)[[1]]@coefs
# equal vectors
bra0 %*% six_state(1)[[1]]@coefs
bra1 %*% six_state(2)[[1]]@coefs

## ----brax---------------------------------------------------------------------
# <+| is a row vector
brax_plus <- six_state(3)[[1]] |> conjugate_transpose()
brax_plus
# ∣-> is a col vector
brax_plus %*% six_state(4)[[1]]@coefs 
# <-| is a row vector
brax_minus <- six_state(4)[[1]] |> conjugate_transpose()
brax_minus
# ∣+> is a col vector
brax_minus %*% six_state(3)[[1]]@coefs
# equal vectors
brax_plus %*% six_state(3)[[1]]@coefs
brax_minus %*% six_state(4)[[1]]@coefs

## ----bray---------------------------------------------------------------------
# <i| is a row vector
bray_iplus <- six_state(5)[[1]] |> conjugate_transpose()
bray_iplus
# ∣-i> is a col vector
bray_iplus %*% six_state(6)[[1]]@coefs 
# <-i| is a row vector
bray_iminus <- six_state(6)[[1]] |> conjugate_transpose()
bray_iminus
# ∣i> is a col vector
bray_iminus %*% six_state(5)[[1]]@coefs
# equal vectors
bray_iplus %*% six_state(6)[[1]]@coefs
bray_iminus %*% six_state(5)[[1]]@coefs

## ----szax---------------------------------------------------------------------
X(1)*six_state(1)[[1]]
X(1)*six_state(2)[[1]]
Y(1)*six_state(1)[[1]]
Y(1)*six_state(2)[[1]]
Z(1)*six_state(1)[[1]]
Z(1)*six_state(2)[[1]]

## ----czax---------------------------------------------------------------------
bra0 %*% (X(1)*six_state(1)[[1]])@coefs
bra0 %*% (Y(1)*six_state(1)[[1]])@coefs
bra0 %*% (Z(1)*six_state(1)[[1]])@coefs
bra1 %*% (X(1)*six_state(2)[[1]])@coefs
bra1 %*% (Y(1)*six_state(2)[[1]])@coefs
bra1 %*% (Z(1)*six_state(2)[[1]])@coefs

## ----sxax---------------------------------------------------------------------
X(1)*six_state(3)[[1]]
X(1)*six_state(4)[[1]]
Y(1)*six_state(3)[[1]]
Y(1)*six_state(4)[[1]]
Z(1)*six_state(3)[[1]]
Z(1)*six_state(4)[[1]]

## ----cxax---------------------------------------------------------------------
brax_plus %*% (X(1)*six_state(3)[[1]])@coefs
brax_plus %*% (Y(1)*six_state(3)[[1]])@coefs
brax_plus %*% (Z(1)*six_state(3)[[1]])@coefs
brax_minus %*% (X(1)*six_state(4)[[1]])@coefs
brax_minus %*% (Y(1)*six_state(4)[[1]])@coefs
brax_minus %*% (Z(1)*six_state(4)[[1]])@coefs

## ----syax---------------------------------------------------------------------
X(1)*six_state(5)[[1]]
X(1)*six_state(6)[[1]]
Y(1)*six_state(5)[[1]]
Y(1)*six_state(6)[[1]]
Z(1)*six_state(5)[[1]]
Z(1)*six_state(6)[[1]]

## ----cyax---------------------------------------------------------------------
bray_iplus %*% (X(1)*six_state(5)[[1]])@coefs
bray_iplus %*% (Y(1)*six_state(5)[[1]])@coefs
bray_iplus %*% (Z(1)*six_state(5)[[1]])@coefs
bray_iminus %*% (X(1)*six_state(6)[[1]])@coefs
bray_iminus %*% (Y(1)*six_state(6)[[1]])@coefs
bray_iminus %*% (Z(1)*six_state(6)[[1]])@coefs

## ----bsph---------------------------------------------------------------------
# Define |0>
theta <- 0
phi <- 0

# Create the point on +z-axis in Bloch sphere
pure_qubit1(theta, phi, spherical = TRUE)

## ----xmat---------------------------------------------------------------------
X(1)@M

## ----xplo---------------------------------------------------------------------
(X(1)*six_state(1)[[1]]) |> plot(qubitnames = "|0>")

## ----ymat---------------------------------------------------------------------
Y(1)@M

## ----yplo---------------------------------------------------------------------
(Y(1)*six_state(1)[[1]]) |> plot(qubitnames = "|0>")

## ----zmat---------------------------------------------------------------------
Z(1)@M

## ----zplo---------------------------------------------------------------------
(Z(1)*six_state(1)[[1]]) |> plot(qubitnames = "|0>")

## ----hmat---------------------------------------------------------------------
H(1)@M

## ----hplo---------------------------------------------------------------------
# H.|0>
(H(1)*six_state(1)[[1]]) |> plot(qubitnames = "|0>")

## ----splo---------------------------------------------------------------------
(S(1)*six_state(1)[[1]]) |> plot(qubitnames = "|0>")

