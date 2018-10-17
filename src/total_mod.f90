Module total_mod
  Implicit None
  Type CArray_1D
      Integer :: Dimen=1
      Integer, Allocatable, Dimension(:) :: lower, upper
      Character(LEN=1024), Allocatable, Dimension(:) :: Data
  End Type
Contains
  Subroutine Read_Data ( Data_up )
    Class(*), Pointer,          intent(in)    :: Data_up
    Integer                   :: LU
    Open ( newunit=LU, file=trim('trninput.dat'), status='old', form=trim('formatted'), action='read' )
    Select Type ( Data_up )
        Type is ( CArray_1D )
            Call Read_CArray_1D ( LU, Data_up )
    End Select
    Close ( unit=LU )
  End Subroutine
  Subroutine Read_TRNINPUT ( )
    Integer                                                                  :: i
    Type (CArray_1D),         Target                                         :: A_t
    Class(*),                 Pointer                                        :: Data_up
    Data_up => A_t
    Call Read_Data ( Data_up )
    Select Type ( Data_up )
    Type is ( CArray_1D )
      print *, 'Read_Write_TRNINPUT: A_t%upper   = ', A_t%upper(1), '    ---    SHOULD BE 3 but is not'
      do i = 1, A_t%upper(1)
        print *, 'Read_Write_TRNINPUT: A_t%Data(i) = ', i, trim(A_t%Data(i)), '    ---    SEGFAULT'
      end do
    End Select
  End Subroutine
  Subroutine Read_CArray_1D ( LU, D )
    Integer, intent(in) :: LU
    Type ( CArray_1D ), intent(out) :: D
    Integer :: i
    Allocate ( D%lower(d%Dimen), D%upper(d%Dimen) )
    Read ( unit=LU, fmt=* ) (D%lower(i), D%upper(i), i=1,D%Dimen)
    Allocate ( D%Data(D%lower(D%Dimen):D%upper(D%Dimen)) )
    Read ( unit=LU, fmt=* ) D%Data
  End Subroutine
End Module
