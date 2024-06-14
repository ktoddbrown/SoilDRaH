test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

badHeaders <- 
'column_name;table_name;of_variable;is_type;with_entry
;;;;'

test_that('checking bad headers', {
  expect_error(checkAnnotations(badHeaders))
  })

# check is_type elements
badIsType <-
'column_name;table_name;of_variable;is_type;with_entry
;;;identifer;invalid'

test_that('checking bad is_type', {
  expect_error(checkAnnotations(badIsType))
  })

# check that only table-columns have the '--'
#badRef <-
#'column_name;table_name;of_variable;is_type;with_entry
# Dev Note: Note sure what is being tested. Do we have an example file?
