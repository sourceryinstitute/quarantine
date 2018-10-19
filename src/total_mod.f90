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
          Allocate ( input%elements(input%upper(1)) )
          Read (1,*) input%elements
    End Select
  End Subroutine
End Module
