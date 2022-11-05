
# Functions
eq_select <- function() {
  rm(list = ls())
  f1 <- function(x) {
    cos(x) - x * exp(x)
  }
  f2 <- function(x) {
    x * log10(x) - 1.2
  }
  f3 <- function(x) {
    x - exp(-x)
  }
  f4 <- function(x) {
    x^4 - x - 10
  }
  f5 <- function(x) {
    x - 0.2 * exp(x)
  }
  f6 <- function(x) {
    x^3 - 4 * x + 9
  }
  f7 <- function(x) {
    x * sin(x) - 1
  }
  f8 <- function(x) {
    cos(x) - 3 * x + 1
  }
  f9 <- function(x) {
    3 * x + sin(x) - exp(x)
  }
  f10 <- function(x) {
    x^3 - x^2 - x - 1
  }

  # Derivatives
  fd1 <- function(x) {
    -sin(x) - exp(x) - x * exp(x)
  }
  fd2 <- function(x) {
    log10(x) + log10(exp(1))
  }
  fd3 <- function(x) {
    1 + exp(-x)
  }
  fd4 <- function(x) {
    4 * x^3 - 1
  }
  fd5 <- function(x) {
    1 - 0.2 * exp(x)
  }
  fd6 <- function(x) {
    3 * x^2 - 4
  }
  fd7 <- function(x) {
    sin(x) + x * cos(x)
  }
  fd8 <- function(x) {
    -sin(x) - 3
  }
  fd9 <- function(x) {
    3 + cos(x) - exp(x)
  }
  fd10 <- function(x) {
    3 * x^2 - 2 * x - 1
  }

  # Second Derivatives

  fdd1 <- function(x) {
    -cos(x) - 2 * exp(x) - x * exp(x)
  }
  fdd2 <- function(x) {
    (1 / x) * log10(exp(1))
  }
  fdd3 <- function(x) {
    -exp(-x)
  }
  fdd4 <- function(x) {
    12 * x^2
  }
  fdd5 <- function(x) {
    -0.2 * exp(x)
  }
  fdd6 <- function(x) {
    6 * x
  }
  fdd7 <- function(x) {
    2 * cos(x) - x * sin(x)
  }
  fdd8 <- function(x) {
    -cos(x)
  }
  fdd9 <- function(x) {
    -sin(x) - exp(x)
  }
  fdd10 <- function(x) {
    6 * x - 2
  }

  # phi

  phi1 <- function(x) {
    exp(-x) * cos(x)
  }
  phi2 <- function(x) {
    1.2 / log10(x)
  }
  phi3 <- function(x) {
    exp(-x)
  }
  phi4 <- function(x) {
    x^4 - 10
  }
  phi5 <- function(x) {
    0.2 * exp(x)
  }
  phi6 <- function(x) {
    (x^3 + 9) / 4
  }
  phi7 <- function(x) {
    1/(sin(x))
  }
  phi8 <- function(x) {
    (1 + cos(x)) / 3
  }
  phi9 <- function(x) {
    (exp(x) - sin(x)) / 3
  }
  phi10 <- function(x) {
    x^3 - x^2 - 1
  }

  # Table of equations
  while (TRUE) {
    eqns <- c("cos(x) - x*exp(x)", "x*log10(x) - 1.2", "x - exp(-x)", "x^4 - x -10", "x - (1/5)*exp(x)", "x^3 - 4*x + 9", "x*sin(x) - 1", "cos(x) - 3*x + 1", "3*x + sin(x) - exp(x)", "x^3 - x^2 - x - 1")
    cat("--------------------------------------------\n")
    cat("Eqn No.", "\t", "Equation\n")
    cat("--------------------------------------------\n")
    for (i in 1:10) {
      cat(i, "\t", "f(x) =", eqns[i], "= 0", "\n")
    }
    eqnum <- as.numeric(readline("Enter Eqn No. to solve: "))
    if (eqnum == 1) {
      f <- f1
      fd <- fd1
      fdd <- fdd1
      phi <- phi1
      curve(cos(x) - x * exp(x), xlim = c(-15, 5), ylim = c(-5, 5), col = "blue4", lwd = 2, main = "f(x) = cos(x) - x*exp(x)")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 2) {
      f <- f2
      fd <- fd2
      fdd <- fdd2
      phi <- phi2
      curve(x*log10(x)-1.2, xlim = c(1, 10), ylim = c(-3, 5), col = "blue4", lwd = 2, main = "f(x) = x*log10(x) - 1.2")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 3) {
      f <- f3
      fd <- fd3
      fdd <- fdd3
      phi <- phi3
      curve(x - exp(-x), xlim = c(-2, 5), ylim = c(-3, 5), col = "blue4", lwd = 2, main = "f(x) = x - exp(-x)")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 4) {
      f <- f4
      fd <- fd4
      fdd <- fdd4
      phi <- phi4
      curve(x^4 - x - 10, xlim = c(-5, 5), ylim = c(-12, 7), col = "blue4", lwd = 2, main = "f(x) = x^4 - x -10")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 5) {
      f <- f5
      fd <- fd5
      fdd <- fdd5
      phi <- phi5
      curve(x - (1/5)*exp(x), xlim = c(-5, 5), ylim = c(-5, 5), col = "blue4", lwd = 2, main = "f(x) = x - (1/5)*exp(x)")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 6) {
      f <- f6
      fd <- fd6
      fdd <- fdd6
      phi <- phi6
      curve(x^3 - 4*x + 9, xlim = c(-5, 5), ylim = c(-20, 20), col = "blue4", lwd = 2, main = "f(x) = x^3 - 4*x + 9")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 7) {
      f <- f7
      fd <- fd7
      fdd <- fdd7
      phi <- phi7
      curve(x*sin(x) - 1, xlim = c(-5, 5), ylim = c(-10, 10), col = "blue4", lwd = 2, main = "f(x) = x*sin(x) - 1")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 8) {
      f <- f8
      fd <- fd8
      fdd <- fdd8
      phi <- phi8
      curve(cos(x) - 3*x + 1, xlim = c(-5, 5), ylim = c(-15, 15), col = "blue4", lwd = 2, main = "f(x) = cos(x) - 3*x + 1")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 9) {
      f <- f9
      fd <- fd9
      fdd <- fdd9
      phi <- phi9
      curve(3*x + sin(x) - exp(x), xlim = c(-3.5, 3.5), ylim = c(-4, 4), col = "blue4", lwd = 2, main = "f(x) = 3*x + sin(x) - exp(x)")
      abline(h = 0, col = "red")
      break
    } else if (eqnum == 10) {
      f <- f10
      fd <- fd10
      fdd <- fdd10
      phi <- phi10
      curve(x^3 - x^2 - x - 1, xlim = c(-5, 5), ylim = c(-5, 5), col = "blue4", lwd = 2, main = "f(x) = x^3 - x^2 - x - 1")
      abline(h = 0, col = "red")
      break
    }
  }
  list(f, fd, fdd, phi)
}

intervals <- function(f) {
  g <- function(x) {
    x * log10(x) - 1.2
  }
  if ( all(f(2:10)==g(2:10)) ) {
    xi <- 2
    xii <- 3
    # Possible Initial Approximations
    print("Possible Intervals are: ")
    dfb <- data.frame(x0 = xi, x1 = xii)
    print(dfb)
  } else {
    cat("\n Enter range and increment: ")
    r <- scan(quiet = TRUE, nmax = 3)

    xi <- c()
    xii <- c()
    for (i in r[1]:r[2]) {
      if (f(i) * f(i + r[3]) < 0) {
        xi <- c(xi, as.numeric(i))
        xii <- c(xii, i + r[3])
      }
    }
    # Possible Initial Approximations
    print("Possible Intervals are: ")
    dfb <- data.frame(x0 = xi, x1 = xii)
    print(dfb)
  }
}