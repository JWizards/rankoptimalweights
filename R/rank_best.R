write_CIP <- function(table, comp, path = "", bounds = c(0, 5)) {
  nComp <- nrow(table)
  nInd <- ncol(table)
  
  
  # setup for generating CIP
  nVars <- 2 * (nComp - 1) + nInd
  nConsrt <- 2 * nComp - 1
  
  # Problem Statement Strings
  varStr <- str_c(
    "Variables : ",
    as.character(nVars),
    " (",
    as.character(nComp - 1),
    " binary, ",
    as.character(nInd),
    " integer, 0 implicit integer, ",
    as.character(nComp - 1),
    " continuous)"
  )
  
  constStr <- str_c(
    "Constraints: ",
    as.character(nConsrt),
    " initial, ",
    as.character(nConsrt),
    " maximal"
  )
  
  # Strings to generate variables
  boolStr <- function(i) {
    str_c("[binary] <z", as.character(i), ">: obj=1, original bounds=[0,1]")
  }
  
  weightStr <- function(i) {
    str_c(
      "[integer] <w", as.character(i), ">: obj=0, original bounds=[",
      bounds[1],
      ",",
      bounds[2],
      "]"
    )
  }
  
  dummyStr <- function(i) {
    str_c("[continuous] <s", as.character(i), ">: obj=0, original bounds=[-inf,+inf]")
  }
  
  minWtStr <- function(i) {
    str_c(
      "[linear] <c0>: ",
      str_flatten(str_c("<w", as.character(2:i - 1), "> + ")),
      "<w", as.character(i), "> ",
      " >= 1;"
    )
  }
  
  # Strings to setup Constraints
  
  # two helper functions for calcCompStr use
  as.charSigned <- Vectorize(function(x) {
    if (x >= 0) {
      str_c("+", as.character(x))
    } else {
      as.character(x)
    }
  })
  
  # This one is used when calling calcCompStr
  rowas.vector <- function(x, data, r = 6) {
    round(as.numeric(as.vector(data[x, ])), digits = r)
  }
  
  # nth lines, i data values (as vector)
  calcCompStr <- function(n, i) {
    str_c(
      "[linear] <c", as.character(n),
      ">: <s", as.character(n), ">",
      str_flatten(str_c(" ", as.charSigned(i), "<w", as.character(1:length(i)), ">")),
      " == 0;"
    )
  }
  
  
  mainCnstStr <- function(i) {
    str_c("[nonlinear] <qc", as.character(i), ">: (<s", as.character(i), "> * <z", as.character(i), ">) >= 0;")
  }
  
  # j main country
  # i comparing against
  cmpCompStr <- function(i, j) {
    counter <- 0
    l <- str_c("[linear] <c", i, ">:", "<s", i, ">")
    
    for (ind in 1:nInd) {
      diff <- table[j, ind] - table[i, ind]
      if (diff > 0) {
        append(l, " +")
      } else {
        append(l, " ")
      }
      append(l, as.character(diff))
      append(l, "<w")
      append(l, as.character(ind))
      append(l, ">")
    }
    
    append(l, " == 0;")
    
    str_c(l)
  }
  
  
  
  #--------- Generating CIP file
  f <- file(str_c(path, "comp_", as.character(comp), ".cip"))
  
  
  l <- c(
    "STATISTICS",
    "Problem name : solve",
    varStr,
    constStr,
    "OBJECTIVE",
    "Sense : maximize",
    "VARIABLES"
  )
  
  for (i in 1:(nComp - 1)) {
    l <- append(l, boolStr(i))
  }
  
  
  for (i in 1:(nInd)) {
    l <- append(l, weightStr(i))
  }
  
  for (i in 1:(nComp - 1)) {
    l <- append(l, dummyStr(i))
  }
  
  l <- append(l, "CONSTRAINTS")
  l <- append(l, minWtStr(nInd))
  
  counter <- 0
  for (ocomp in 1:nComp) {
    if (ocomp == comp) {
      next
    }
    counter <- counter + 1
    l <- append(l, calcCompStr(counter, rowas.vector(ocomp, table) - rowas.vector(comp, table)))
  }
  
  l <- append(l, mainCnstStr(c(2:nComp - 1)))
  
  l <- append(l, "END")
  
  l <- str_c(l)
  
  writeLines(l, f)
  close(f)
}


# --------------------------------------------------------------------------------------------------------



#' Rank Best Optimal Weights
#'
#' @param table Table of indicators
#' @param bounds Bounds for integer weights (default 0-5)
#' @param cleanup Flag to remove .cip and .sol files (default TRUE)
#'
#' @return A table of weights in order of competitors
#' @export
#'
#' @examples
rank_best <- function(table, bounds = c(0, 5), cleanup = TRUE) {
  nComp <- nrow(table)
  nInd <- ncol(table)
  
  
  if (!dir.exists("temp")) {
    dir.create("temp")
  }
  
  for (i in 1:nComp) {
    write_CIP(table, i, "temp/", bounds)
  }
  
  # solve using SCIP
  for (i in 1:nComp) {
    system(str_c(
      "scip -c \"read temp/comp_",
      as.character(i),
      ".cip\" -c \"optimize\" -c \"write solution temp/comp_",
      as.character(i),
      ".sol\""
    ))
  }
  
  lsols <- list()
  for (i in 1:nComp) {
    f <- file(str_c("temp/comp_", as.character(i), ".sol"))
    reads <- readLines(f)
    sols <- Filter(length, str_extract_all(reads, "w[0-9]+[ ]+[0-9]+"))
    
    sols <- Filter(length, str_match_all(unlist(sols), "([0-9]+)[ ]+([0-9]+)"))
    sol_vec <- integer(nInd)
    for (j in 1:length(sols)) {
      sol_vec[as.numeric(sols[[j]][1, 2])] <- as.numeric(sols[[j]][1, 3])
    }
    lsols[[i]] <- sol_vec
    
    close(f)
  }
  
  if (cleanup) {
    unlink("temp", recursive = TRUE)
  }
  lsols
}
