    Implicit None
    Call Read_TRNINPUT
Contains
  Subroutine Read_TRNINPUT()
    Use total_mod
    Type (vector), Target :: a
    Class(*), Pointer :: a_ptr
    a_ptr => a
    Call Read_elements ( a_ptr )
    Select Type ( a_ptr )
    Type is ( vector )
      print *, a%upper(1), '<-- should be 3'
      print *, trim(a%elements(1)), '<--- seg fault'
    End Select
  End Subroutine
End
