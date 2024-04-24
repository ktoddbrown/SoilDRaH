test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

badHeaders <- 
'column_name;table_name;of_variable;is_type;with_entry
;;;;'

test_that('checking bad headers', {
  expect_error(checkAnnotations(badHeaders))
  })


