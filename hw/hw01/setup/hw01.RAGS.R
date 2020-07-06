#################################################
# Public autograder tests for PH142 hw01-data-manipulation-dplyr

###############################################
sol_path <- "setup/src/"
source("autograder_setup.R")

# Replace with number of problems
setup_autograder(16)

# --------------------------------------------
check_problem1 = function() {
  problem_num <- 1 # problem number
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = length(p1) == 4, 
             correct_message = "Correct!",
             error_message = "You should list 4 functions just like in the fruits example above")
  
  checkpoint(checkpoint_number = 2,
             test = all(p1 %in% c("rename", 
                                  "select", 
                                  "arrange", 
                                  "filter", 
                                  "mutate", 
                                  "group_by", 
                                  "summarize",
                                  "dim",
                                  "head",
                                  "names",
                                  "str")), 
             correct_message = "Correct!",
             error_message = "Incorrect. Check your spelling and review lecture notes!")
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 2 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(birthwt_small), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birthwt_small) == 3,
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
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
             test = is.data.frame(birthwt_small_colon), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birthwt_small_colon) == 3,
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
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
             test = is.data.frame(birthwt_no_ht), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birthwt_no_ht) == 8,
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
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
  num_tests <<- 1 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = p5 == "returns all columns that start with wt", 
             correct_message = "Correct!",
             error_message = "Incorrect. Try again!")
  
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
             test = is.data.frame(birthwt_wt), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birthwt_wt) == 2,
             correct_message = "Correct!",
             error_message = "Did you subset your columns correctly?")
  
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
  max_scores[problem_num] <<- 1 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(birthwt_low), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birthwt_low) == 9,
             correct_message = "Correct!",
             error_message = "Did you subset from the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(birthwt_low) == 59,
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(birthwt_prem), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birthwt_prem) == 9,
             correct_message = "Correct!",
             error_message = "Did you subset from the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(birthwt_prem) == 18,
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(birth_hosp), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birth_hosp) == 9,
             correct_message = "Correct!",
             error_message = "Did you subset from the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(birth_hosp) == 81,
             correct_message = "Correct!",
             error_message = "Did you subset your rows correctly?")
  
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
             test = is.data.frame(birth_order), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birth_order) == 9,
             correct_message = "Correct!",
             error_message = "Did you arrange the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(birth_order) == 81,
             correct_message = "Correct!",
             error_message = "You should have the same number of rows as birth_hosp.")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(birth_rev), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birth_rev) == 9,
             correct_message = "Correct!",
             error_message = "Did you arrange the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(birth_rev) == 81,
             correct_message = "Correct!",
             error_message = "You should keep all observations.")
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(birthwt_smoke), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birthwt_smoke) == 9,
             correct_message = "Correct!",
             error_message = "Did you arrange the right data frame?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(birthwt_smoke) == 189,
             correct_message = "Correct!",
             error_message = "You should keep all observations.")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(birthwt_lbs), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(birthwt_lbs) == 10,
             correct_message = "Correct!",
             error_message = "Do you have the correct number of columns?")
  
  checkpoint(checkpoint_number = 3,
             test = nrow(birthwt_lbs) == 189,
             correct_message = "Correct!",
             error_message = "You should keep all observations.")
  
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
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(avg_birthwt), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(avg_birthwt) == 1 &&
               nrow(avg_birthwt) == 1,
             correct_message = "Correct!",
             error_message = "Did you use the correct dplyr function?")
  
  checkpoint(checkpoint_number = 3,
             test = is.numeric(avg_birthwt$bw_avg),
             correct_message = "Correct!",
             error_message = "Did you calculate the mean?")
  
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
  max_scores[problem_num] <<- 2 # total pts possible
  num_tests <<- 3 # num of checkpoints
  
  problem_types[problem_num] <<- "autograded" # choices: autograded, free-response
  problem_names[problem_num] <<- sprintf("Problem %d", problem_num)
  
  tests_failed <<- num_tests
  
  # Test cases here:
  
  checkpoint(checkpoint_number = 1,
             test = is.data.frame(avg_by_smoker), 
             correct_message = "Correct!",
             error_message = "Make sure your final answer is a data frame.")
  
  checkpoint(checkpoint_number = 2,
             test = ncol(avg_by_smoker) == 2 &&
               nrow(avg_by_smoker) == 2,
             correct_message = "Correct!",
             error_message = "Did you use the group_by function?")
  
  checkpoint(checkpoint_number = 3,
             test = identical(names(avg_by_smoker), c("smoke", "bw_avg")),
             correct_message = "Correct!",
             error_message = "Did you use the summarize function?")
  
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