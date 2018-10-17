Module Read_Write_TRNINPUT_Mod
  Use Parameters_Mod, only : CLEN_SIZE
  Implicit None
Contains
  Subroutine Read_TRNINPUT ( FILE, u, C, ACCESS)
    Use Read_Write_Basic_Mod, only : Read_Data
    Use Type_CArray_1D_Mod, only : CArray_1D
    Implicit None
    Integer,                  Target,                            intent(out) :: u
    Character(LEN=CLEN_SIZE), Target, Allocatable, Dimension(:), intent(out) :: C
    Character(Len=*),                                            intent(in)  :: FILE
    Character(Len=3), Optional,                                  intent(in)  :: ACCESS
    Integer                                                                  :: i
    Type (CArray_1D),         Target                                         :: A_t
    Class(*),                 Pointer                                        :: Data_up
    Data_up => A_t
    Call Read_Data ( FILE, Data_up, ACCESS )
    Select Type ( Data_up )
    Type is ( CArray_1D )
print *, 'Read_Write_TRNINPUT: A_t%upper   = ', A_t%upper(1), '    ---    SHOULD BE 3 but is not'
do i = 1, A_t%upper(1)
print *, 'Read_Write_TRNINPUT: A_t%Data(i) = ', i, trim(A_t%Data(i)), '    ---    SEGFAULT'
end do
    End Select
  End Subroutine
End Module
