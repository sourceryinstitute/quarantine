add_executable( buggy
  hzetrn_trn.f90
  total_mod.f90
)
add_test(test-buggy buggy)
set_tests_properties(test-buggy PROPERTIES PASS_REGULAR_EXPRESSION "Test passed.")
