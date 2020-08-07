#################################################
# Autograder tests for PH142 HW 10
# This is the sanity check

###############################################
sol_path <- "setup/sol/"
source("setup/autograder_setup.R")
#source("../common/setup/autograder_setup.R")

# Replace with number of problems
setup_autograder(22)

# --------------------------------------------

check_problem1 = function() {
  problem_num <- 1 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p1)=="numeric", 
             correct_message = "Your answer is numeric",
             error_message = "Is your answer numeric?")
  
  checkpoint(checkpoint_number = 2,
             test = p1 > 0,
             correct_message = "Your answer is positive",
             error_message = "Is your answer a positive number?")
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem2 = function() {
  problem_num <- 2 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p2)=="numeric", 
             correct_message = "Your answer is numeric",
             error_message = "Is your answer numeric?")
  
  checkpoint(checkpoint_number = 2,
             test = p2 > 0,
             correct_message = "Your answer is positive",
             error_message = "Is your answer a positive number?")
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}
# --------------------------------------------

check_problem3 = function() {
  problem_num <- 3 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p3) == "numeric", 
             correct_message = "Passed this checkpoint",
             error_message = "Did you store your CI as a numeric object?")
  
  checkpoint(checkpoint_number = 2,
             test = length(p3) == 2,
             correct_message = "Passed this checkpoint",
             error_message = "Did you store 2 elements in your object?")
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}
# --------------------------------------------

check_problem4 = function() {
  problem_num <- 4 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p4)== "numeric", 
             correct_message = "Passed this checkpoint",
             error_message = "Did you store your CI as a numeric object?")
  
  checkpoint(checkpoint_number = 2,
             test = length(p4) == 2,
             correct_message = "Passed this checkpoint",
             error_message = "Did you store 2 elements in your object?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem5 = function() {
  problem_num <- 5 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p5)== "numeric", 
             correct_message = "Passed this checkpoint",
             error_message = "Did you store your CI as a numeric object?")
  
  checkpoint(checkpoint_number = 2,
             test = length(p5) == 2,
             correct_message = "Passed this checkpoint",
             error_message = "Did you store 2 elements in your object?")
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------
check_problem6 = function() {
  problem_num <- 6 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p6)== "numeric", 
             correct_message = "Passed this checkpoint",
             error_message = "Did you store your CI as a numeric object?")
  
  checkpoint(checkpoint_number = 2,
             test = length(p6) == 2,
             correct_message = "Passed this checkpoint",
             error_message = "Did you store 2 elements in your object?")
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem7 = function() {
  problem_num <- 7 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = identical(p7$data, ci_methods), 
             correct_message = "The correct dataset was used",
             error_message = "Did you use the correct dataset?")
  
  checkpoint(checkpoint_number = 2,
             test = "ggplot" %in% class(p7),
             correct_message = "A ggplot has been defined",
             error_message = "Did you define a ggplot?")
  
  checkpoint(checkpoint_number = 3,
             test = "GeomPoint" %in% class(p7$layers[[1]]$geom), 
             correct_message = "Point estimates are plotted",
             error_message = "Did you plot the point estimates?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomSegment" %in% class(p7$layers[[2]]$geom), 
             correct_message = "CI segments are plotted",
             error_message = "Did you plot the CI segments?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem8 = function() {
  problem_num <- 8 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem9 = function() {
  problem_num <- 9 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p9),
             correct_message = "A ggplot has been defined",
             error_message = "Did you define a ggplot?")
  
  checkpoint(checkpoint_number = 2,
             test = "GeomPoint" %in% class(p9$layers[[1]]$geom), 
             correct_message = "A scatterplot has been defined",
             error_message = "Did you define a scatterplot?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem10 = function() {
  problem_num <- 10 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(p10),
             correct_message = "Your object is a dataframe",
             error_message = "Is your object a dataframe?")
  
  checkpoint(checkpoint_number = 2,
             test = nrow(p10) == 2,
             correct_message = "Your dataframe contains two observations",
             error_message = "Did you filter your dataframe correctly?")
  
  checkpoint(checkpoint_number = 3,
             test = ncol(p10) == 4,
             correct_message = "Your dataframe is not missing any columns",
             error_message = "Is your dataframe missing any columns?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem11 = function() {
  problem_num <- 11 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(p11),
             correct_message = "Your object is a dataframe",
             error_message = "Is your object a dataframe?")
  
  checkpoint(checkpoint_number = 2,
             test = nrow(p11) == nrow(cases_CA),
             correct_message = "Your dataframe contains the correct number of observations",
             error_message = "Did you modify the correct dataframe?")
  
  checkpoint(checkpoint_number = 3,
             test = ncol(p11) == ncol(cases_CA) + 2,
             correct_message = "Your dataframe is not missing any columns",
             error_message = "Did you create two additional columns?")
  
  checkpoint(checkpoint_number = 4,
             test = c("log_total_confirmed", "total_deaths") %in% colnames(p11),
             correct_message = "You have named your new columns correctly",
             error_message = "Did you name your columns correctly?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem12 = function() {
  problem_num <- 12 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = identical(p12$data, p11), 
             correct_message = "The correct dataset was used",
             error_message = "Did you use the correct dataset?")
  
  checkpoint(checkpoint_number = 2,
             test = "ggplot" %in% class(p12),
             correct_message = "A ggplot has been defined",
             error_message = "Did not define a ggplot?")
  
  checkpoint(checkpoint_number = 3,
             test = "GeomPoint" %in% class(p12$layers[[1]]$geom), 
             correct_message = "A scatterplot has been defined",
             error_message = "Did you define a scatterplot?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem13 = function() {
  problem_num <- 13 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 1 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "lm" %in% class(lm), 
             correct_message = "You made a linear model",
             error_message = "Did you make a linear model?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem14 = function() {
  problem_num <- 14 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = identical(p14$data, p11), 
             correct_message = "The correct dataset was used",
             error_message = "Did you use the correct dataset?")
  
  checkpoint(checkpoint_number = 2,
             test = "ggplot" %in% class(p14),
             correct_message = "A ggplot has been defined",
             error_message = "Did you define a ggplot?")
  
  checkpoint(checkpoint_number = 3,
             test = "GeomPoint" %in% class(p14$layers[[1]]$geom), 
             correct_message = "A scatterplot has been defined",
             error_message = "Did you define a scatterplot?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomAbline" %in% class(p14$layers[[2]]$geom),
             correct_message = "Regression line has been plotted",
             error_message = "Did you plot regression line using the correct function?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem15 = function() {
  problem_num <- 15 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(r)=="numeric", 
             correct_message = "Your correlation coefficient is numeric",
             error_message = "Is your correlation coefficient numeric?")

  checkpoint(checkpoint_number = 2,
             test = r > -1 & r < 1,
             correct_message = "Your correlation coefficient is in the correct range",
             error_message = "Your correlation coefficient is not within a reasonable range")
  
  checkpoint(checkpoint_number = 3,
             test = class(r_sq)=="numeric", 
             correct_message = "Your coefficient of determination (r-squared) is numeric",
             error_message = "Is your coefficient of determination numeric?")
  
  checkpoint(checkpoint_number = 4,
             test = r_sq > 0 & r_sq < 1,
             correct_message = "Your coefficient of determination (r-squared) is in the correct range",
             error_message = "Your coefficient of determination is not within a reasonable range")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}


# --------------------------------------------

check_problem16 = function() {
  problem_num <- 16 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem17 = function() {
  problem_num <- 17 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = identical(p17$data, augment_lm), 
             correct_message = "The correct dataset was used",
             error_message = "Did you use the correct dataset?")
  
  checkpoint(checkpoint_number = 2,
             test = "ggplot" %in% class(p17),
             correct_message = "A ggplot has been defined",
             error_message = "Did you define a ggplot?")
  
  checkpoint(checkpoint_number = 3,
             test = "GeomPoint" %in% class(p17$layers[[1]]$geom), 
             correct_message = "A scatterplot has been defined",
             error_message = "Did you define a scatterplot?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomSmooth" %in% class(p17$layers[[2]]$geom), 
             correct_message = "A regression line is present",
             error_message = "Did you plot the regression line?")
  
  checkpoint(checkpoint_number = 5,
             test = "GeomSegment" %in% class(p17$layers[[3]]$geom), 
             correct_message = "Residual lines are present",
             error_message = "Did you plot the residual lines?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem18 = function() {
  problem_num <- 18 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = identical(p18$data, augment_lm), 
             correct_message = "The correct dataset was used",
             error_message = "Did you use the correct dataset?")
  
  checkpoint(checkpoint_number = 2,
             test = "ggplot" %in% class(p18),
             correct_message = "A ggplot has been defined",
             error_message = "Did you define a ggplot?")
  
  checkpoint(checkpoint_number = 3,
             test = "GeomPoint" %in% class(p18$layers[[1]]$geom), 
             correct_message = "Points are plotted",
             error_message = "Did you plot datapoints in your graph?")
  
  checkpoint(checkpoint_number = 4,
             test = "StatQq" %in% class(p18$layers[[1]]$stat),
             correct_message = "A QQ plot has been defined",
             error_message = "Did you define a QQ plot?")
  
  checkpoint(checkpoint_number = 5,
             test = "StatQqLine" %in% class(p18$layers[[2]]$stat),
             correct_message = "A QQ line is present",
             error_message = "Did you plot a QQ line?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem19 = function() {
  problem_num <- 19 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 4 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = identical(p19$data, augment_lm), 
             correct_message = "The correct dataset was used",
             error_message = "Did you use the correct dataset?")
  
  checkpoint(checkpoint_number = 2,
             test = "ggplot" %in% class(p19),
             correct_message = "A ggplot has been defined",
             error_message = "Did you define a ggplot?")
  
  checkpoint(checkpoint_number = 3,
             test = "GeomPoint" %in% class(p19$layers[[1]]$geom), 
             correct_message = "A scatterplot has been defined",
             error_message = "Did you define a scatterplot?")
  
  checkpoint(checkpoint_number = 4,
             test = "GeomHline" %in% class(p19$layers[[2]]$geom),
             correct_message = "A horizontal line is present",
             error_message = "Did you plot a horizontal line?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem20 = function() {
  problem_num <- 20 # problem number
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = identical(p20$data, augment_lm_gather), 
             correct_message = "The correct dataset was used",
             error_message = "Did you use the correct dataset?")
  
  checkpoint(checkpoint_number = 2,
             test = "ggplot" %in% class(p20),
             correct_message = "A ggplot has been defined",
             error_message = "Did you define a ggplot?")
  
  checkpoint(checkpoint_number = 3,
             test = "GeomBoxplot" %in% class(p20$layers[[1]]$geom), 
             correct_message = "A boxplot has been defined",
             error_message = "Did you define a boxplot?")
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem21 = function() {
  problem_num <- 21 # problem number
  max_scores[problem_num] <<- 4 # total pts possible
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

check_problem22 = function() {
  problem_num <- 22 # problem number
  max_scores[problem_num] <<- 0 # total pts possible
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  
  # Assign appropriate score to problem depending on tests passed/failed
  
  if (tests_failed == 0 && problem_types[problem_num] != "free-response"){
    scores[problem_num] <<- max_scores[problem_num]
  } else {
    scores[problem_num] <<- 0
  }
  
  assert_that(tests_failed <= num_tests, tests_failed >= 0,
              msg = sprintf("Did you set your num_test correctly for problem %d?", problem_num))
  return_score(problem_num, num_tests, tests_failed)
}

# --------------------------------------------

capture.output(check_problem1(), file='NUL')
capture.output(check_problem2(), file='NUL')
capture.output(check_problem3(), file='NUL')
capture.output(check_problem4(), file='NUL')
capture.output(check_problem5(), file='NUL')
capture.output(check_problem6(), file='NUL')
capture.output(check_problem7(), file='NUL')
capture.output(check_problem8(), file='NUL')
capture.output(check_problem9(), file='NUL')
capture.output(check_problem10(), file='NUL')
capture.output(check_problem11(), file='NUL')
capture.output(check_problem12(), file='NUL')
capture.output(check_problem13(), file='NUL')
capture.output(check_problem14(), file='NUL')
capture.output(check_problem15(), file='NUL')
capture.output(check_problem16(), file='NUL')
capture.output(check_problem17(), file='NUL')
capture.output(check_problem18(), file='NUL')
capture.output(check_problem19(), file='NUL')
capture.output(check_problem20(), file='NUL')
capture.output(check_problem21(), file='NUL')
capture.output(check_problem22(), file='NUL')
