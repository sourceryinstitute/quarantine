add_executable( buggy
  hzetrn_trn.f90
  Parameters_Mod.f90
  Read_Write_Basic_Mod.f90
  Read_Write_TRNINPUT_Mod.f90
  Read_Write_Target_Material_Mod.f90
  Type_CArray_1D_Mod.f90
  Type_Target_Material_Mod.f90
)
add_test(test-buggy buggy)
set_tests_properties(test-buggy PROPERTIES PASS_REGULAR_EXPRESSION "Test passed.")
