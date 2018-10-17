Module Read_Write_Basic_Mod
    Use Parameters_Mod
    Implicit None
Contains
    Subroutine Read_Data ( FILE, Data_up, ACCESS)
      Use Type_CArray_1D_Mod
      Use Type_Target_Material_Mod
        Class(*), Pointer,          intent(in)    :: Data_up
        Character(Len=*),           intent(in)    :: FILE
        Character(Len=3), Optional, intent(in)    :: ACCESS
        Integer                   :: i, LU, IOSTAT
        Character(LEN=IOMSG_SIZE) :: IOMSG
        Character(LEN=CLEN_SIZE)  :: NAM
        Character(LEN=11)         :: FORM
        Logical                   :: Speak, Binary, OPENED, RW = .true.
        Speak = .true.
DACif:  If ( .not. Present(ACCESS) ) Then
            Binary = G_BINARY
BINIf:      If ( Binary ) Then
                FORM = 'unformatted'
            Else
                FORM = 'formatted'
            End If BINIf
        Else
ACif:       If ( ACCESS(1:3) .eq. 'BIN' ) Then
                FORM = 'unformatted' ; Binary = .true.
            Else If ( ACCESS(1:3) .eq. 'ASC' ) Then
                FORM = 'formatted' ; Binary = .false.
            Else
                Write (*,*) 'Optional ACCESS present does not equal "BIN" or "ASC"'
                Write (*,*) 'ACCESS = ', ACCESS(1:3)
                Stop 'Abort Read_Data'
            End If ACif
        End If DACif
        Open ( newunit=LU, file=trim(FILE), status='old', form=trim(FORM), action='read', iostat=IOSTAT, iomsg=IOMSG )
OPerr:  If ( IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'File Open Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_Data'
        End If OPerr
DUPtyp: Select Type ( Data_up )
            Type is ( CArray_1D )
                Call Read_CArray_1D ( LU, Binary, Data_up )
Q_C1D:          If ( Speak ) Then
                End If Q_C1D
print *, 'Read_Write_Basic_Mod: Data_up%upper(1) = ', Data_up%upper(1)
do i = 1, Data_up%upper(1)
print *, '                      Data_up%Data(i)  = ', i, trim(Data_up%Data(i))
enddo
            Type is ( Target_Material_Pointer )
                Call Read_Target_Material ( LU, Binary, Data_up )
            Class Default
                Write (*,'(a,i0,1x,a)') 'The class of the data passed to Read_Data was not recognized'
                Stop 'Abort Read_Data'
        End Select DUPTyp
CLerr:  If ( IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'File Close Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_Data'
        End If CLerr
        Return
    End Subroutine Read_Data
End Module Read_Write_Basic_Mod
