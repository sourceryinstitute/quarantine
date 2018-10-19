Module total_mod
  Implicit None
  Type CArray_1D
      Integer, Allocatable, Dimension(:) :: lower, upper
      Character(LEN=1024), Allocatable, Dimension(:) :: Data
  End Type
Contains
  Subroutine Read_Data ( Data_up )
    Class(*), Pointer,          intent(in)    :: Data_up
    Integer                   :: LU
    Open ( newunit=LU, file='trninput.dat', status='old', form='formatted', action='read' )
    Select Type ( Data_up )
        Type is ( CArray_1D )
              Allocate ( Data_up%lower(1), Data_up%upper(1) )
              Read ( unit=LU, fmt=* ) Data_up%lower(1), Data_up%upper(1)
              Allocate ( Data_up%Data(Data_up%lower(1):Data_up%upper(1)) )
              Read ( unit=LU, fmt=* ) Data_up%Data
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
End Module
