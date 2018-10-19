add_executable( buggy
  main.f90
  vectors.f90
)
add_test(test-buggy buggy)
set_tests_properties(test-buggy PROPERTIES PASS_REGULAR_EXPRESSION "Test passed.")
