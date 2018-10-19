Module total_mod
  Implicit None
  Type vector
      Integer, Allocatable, Dimension(:) :: upper
      Character(LEN=1024), Allocatable, Dimension(:) :: elements
  End Type
Contains
  Subroutine Read_elements ( input )
    Class(*), Pointer, intent(in) :: input
    Open (unit=1,file='trninput.dat',status='old',form='formatted')
    Select Type ( input )
        Type is ( vector )
          Allocate ( input%upper(1) )
          Read (1,*) input%upper(1)
          Allocate ( input%elements(1:input%upper(1)) )
          Read (1,*) input%elements
    End Select
  End Subroutine
  Subroutine Read_TRNINPUT()
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
End Module
