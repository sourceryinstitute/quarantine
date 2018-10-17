Module total_mod
    Implicit None
    Type :: CArray_1D
        Integer :: Dimen=1
        Integer, Allocatable, Dimension(:) :: lower, upper
        Character(LEN=1024), Allocatable, Dimension(:) :: Data
    End Type CArray_1D
Contains
    Subroutine Read_Data ( FILE, Data_up )
        Implicit None
        Class(*), Pointer,          intent(in)    :: Data_up
        Character(Len=*),           intent(in)    :: FILE
        Integer                   :: i, LU
        Character(LEN=11)         :: FORM
        Logical                   :: Binary
        FORM = 'formatted'
        Binary = .false.
        Open ( newunit=LU, file=trim(FILE), status='old', form=trim(FORM), action='read' )
DUPtyp: Select Type ( Data_up )
            Type is ( CArray_1D )
                Call Read_CArray_1D ( LU, Binary, Data_up )
print *, 'Read_Write_Basic_Mod: Data_up%upper(1) = ', Data_up%upper(1)
do i = 1, Data_up%upper(1)
print *, '                      Data_up%Data(i)  = ', i, trim(Data_up%Data(i))
enddo
        End Select DUPTyp
        Close ( unit=LU )
    End Subroutine Read_Data
  Subroutine Read_TRNINPUT ( FILE)
    Implicit None
    Character(Len=*),                                            intent(in)  :: FILE
    Integer                                                                  :: i
    Type (CArray_1D),         Target                                         :: A_t
    Class(*),                 Pointer                                        :: Data_up
    Data_up => A_t
    Call Read_Data ( FILE, Data_up )
    Select Type ( Data_up )
    Type is ( CArray_1D )
print *, 'Read_Write_TRNINPUT: A_t%upper   = ', A_t%upper(1), '    ---    SHOULD BE 3 but is not'
do i = 1, A_t%upper(1)
print *, 'Read_Write_TRNINPUT: A_t%Data(i) = ', i, trim(A_t%Data(i)), '    ---    SEGFAULT'
end do
    End Select
  End Subroutine
    Subroutine Read_CArray_1D ( LU, Binary, D )
        Implicit None
        Integer, intent(in) :: LU
        Logical, intent(in) :: Binary
        Type ( CArray_1D ), intent(out) :: D
        Integer :: i
        Allocate ( D%lower(d%Dimen), D%upper(d%Dimen) )
        Read ( unit=LU, fmt=* ) (D%lower(i), D%upper(i), i=1,D%Dimen)
        Allocate ( D%Data(D%lower(D%Dimen):D%upper(D%Dimen)) )
        Read ( unit=LU, fmt=* ) D%Data
print *, 'Type_CArray_1D_Mod: D%upper(1) = ', D%upper(1)
do i = 1, D%upper(1)
print *, 'Type_CArray_1D_Mod: D%Data(i)  = ', i, trim(D%Data(i))
enddo
    End Subroutine Read_CArray_1D
End Module
