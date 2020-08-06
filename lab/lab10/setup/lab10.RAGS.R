#################################################
# Autograder tests for PH142 lab10-sol
#
###############################################
sol_path <- "setup/src/"
source("setup/autograder_setup.R")

#source("../common/setup/autograder_setup.R")


# Replace with number of problems
setup_autograder(15)

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
             test = class(p1) %in% c("lm", "glm"),
             correct_message = "Correct!",
             error_message = "Is p1 a linear model (lm) object?")
  
  checkpoint(checkpoint_number = 2,
             test = round(p1$coefficients[2],2) == 1.09 ,
             correct_message = "Correct!",
             error_message = "Did you run the correct model? Check your response and explanatory variables.")
  
  
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
             test = class(p2) == "numeric",
             correct_message = "Correct!",
             error_message = "Is p2 a number?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(round(p2, 2), 1.09) ,
             correct_message = "Correct!",
             error_message = "Your slope is not correct")
  
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = length(p3) == 2,
             correct_message = "Correct!",
             error_message = "Is p3 a vector of 2 values?")
  
  checkpoint(checkpoint_number = 2,
             test = round(p3[1],1) == 0.7 ,
             correct_message = "Correct!",
             error_message = "Your lower bound (first value) is wrong.")
  
  checkpoint(checkpoint_number = 3,
             test = round(p3[2],1) == 1.5 ,
             correct_message = "Correct!",
             error_message = "Your upper bound (second value) is wrong.")
  
  
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
             test = class(p4) == "numeric",
             correct_message = "Correct!",
             error_message = "Is p4 a number?")
  
  checkpoint(checkpoint_number = 2,
             test = all.equal(round(p4, 2), 0.06) ,
             correct_message = "Correct!",
             error_message = "Your R^2 is not correct")
  
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "ggplot" %in% class(p5),
             correct_message = "Correct!",
             error_message = "Is p5 a ggplot?")
  
  checkpoint(checkpoint_number = 2,
             test = length(p5$layers) > 2,
             correct_message = "Correct!",
             error_message = "Did you add a scatterplot, regression line, and horizontal line?")
  
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 1 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = grepl("qq", p6, ignore.case = TRUE),
             correct_message = "Correct!",
             error_message = "Provide a character string of the first two letters of the plot type")
  
  
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
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = class(p7) == "numeric" & 
               length(p7) == 4,
             correct_message = "Correct!",
             error_message = "Is p7 vector with four numbers?")
  
  checkpoint(checkpoint_number = 2,
             test = round(p7[1],0) %in% c(4, 20),
             correct_message = "Correct!",
             error_message = "Is this the lower bound for the dis = 2.5 C.I?")
  
  checkpoint(checkpoint_number = 3,
             test = round(p7[2],0) %in% c(6, 23),
             correct_message = "Correct!",
             error_message = "Is this the lower bound for the dis = 5 C.I?")
  
  checkpoint(checkpoint_number = 4,
             test = round(p7[3],0) %in% c(9, 25),
             correct_message = "Correct!",
             error_message = "Is this the lower bound for the dis = 7.5 C.I?")
  
  checkpoint(checkpoint_number = 5,
             test = round(p7[4],0) %in% c(12, 27),
             correct_message = "Correct!",
             error_message = "Is this the lower bound for the dis = 10 C.I?")
  
  
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
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  
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
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "numeric" %in% class(p10),
             correct_message = "A numeric vector is made.",
             error_message = "You did not make a numeric vector.")
  
  checkpoint(checkpoint_number = 2,
             test = length(p10) == 2,
             correct_message = "Correct number of elements",
             error_message = "Your list has the wrong number of elements")
  
  checkpoint(checkpoint_number = 3,
             test = p10[1] < p10[2],
             correct_message = "Correct order for elements",
             error_message = "Your bounds are not stored in the correct order.")
  
  checkpoint(checkpoint_number = 4,
             test = 0.21 < p10[1] & p10[1] < 0.22,
             correct_message = "Correct lower bound!",
             error_message = "Your lower bound has a wrong number.")
  
  checkpoint(checkpoint_number = 5,
             test = 0.52 < p10[2] & p10[2] < 0.53,
             correct_message = "Correct upper bound!",
             error_message = "Your upper bound is wrong.")
  
  
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
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "numeric" %in% class(p11),
             correct_message = "A numeric vector is made.",
             error_message = "You did not make a numeric vector.")
  
  checkpoint(checkpoint_number = 2,
             test = length(p11) == 2,
             correct_message = "Correct number of elements",
             error_message = "Your list has the wrong number of elements")
  
  checkpoint(checkpoint_number = 3,
             test = p11[1] < p11[2],
             correct_message = "Correct order for elements",
             error_message = "Your bounds are not stored in the correct order.")
  
  checkpoint(checkpoint_number = 4,
             test = 0.22 < p11[1] & p11[1] < 0.23,
             correct_message = "Correct lower bound!",
             error_message = "Your lower bound has a wrong number.")
  
  checkpoint(checkpoint_number = 5,
             test = 0.54 < p11[2] & p11[2] < 0.55,
             correct_message = "Correct upper bound!",
             error_message = "Your upper bound is wrong.")
  
  
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
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "numeric" %in% class(p12),
             correct_message = "A numeric vector is made.",
             error_message = "You did not make a numeric vector.")
  
  checkpoint(checkpoint_number = 2,
             test = length(p12) == 2,
             correct_message = "Correct number of elements",
             error_message = "Your list has the wrong number of elements")
  
  checkpoint(checkpoint_number = 3,
             test = p12[1] < p12[2],
             correct_message = "Correct order for elements",
             error_message = "Your bounds are not stored in the correct order.")
  
  checkpoint(checkpoint_number = 4,
             test = 0.23 < p12[1] & p12[1] < 0.24,
             correct_message = "Correct lower bound!",
             error_message = "Your lower bound is wrong.")
  
  checkpoint(checkpoint_number = 5,
             test = 0.52 < p12[2] & p12[2] < 0.53,
             correct_message = "Correct upper bound!",
             error_message = "Your upper bound is wrong.")
  
  
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
  num_tests <<- 5 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = "numeric" %in% class(p13),
             correct_message = "A numeric vector is made.",
             error_message = "You did not make a numeric vector.")
  
  checkpoint(checkpoint_number = 2,
             test = length(p13) == 2,
             correct_message = "Correct number of elements",
             error_message = "Your list has the wrong number of elements")
  
  checkpoint(checkpoint_number = 3,
             test = p13[1] < p13[2],
             correct_message = "Correct order for elements",
             error_message = "Your bounds are not stored in the correct order.")
  
  checkpoint(checkpoint_number = 4,
             test = 0.21 < p13[1] & p13[1] < 0.22,
             correct_message = "Correct lower bound!",
             error_message = "Your lower bound is wrong.")
  
  checkpoint(checkpoint_number = 5,
             test = 0.54 < p13[2] & p13[2] < 0.55,
             correct_message = "Correct upper bound!",
             error_message = "Your upper bound is wrong.")
  
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  
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
  num_tests <<- 0 # num of checkpoints
  
  problem_types[problem_num] <<- "free-response" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  
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
