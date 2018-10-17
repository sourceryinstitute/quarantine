Module Read_Write_Basic_Mod
    Use Parameters_Mod
    Implicit None
Contains
    Subroutine Read_Data ( FILE, Data_up, ACCESS, OBytes, Opt_Int, Quite )
      Use Type_CArray_1D_Mod
      Use Type_Target_Material_Mod
        Class(*), Pointer,          intent(in)    :: Data_up
        Character(Len=*),           intent(in)    :: FILE
        Character(Len=3), Optional, intent(in)    :: ACCESS
        Integer, Optional,          intent(out)   :: OBytes
        Integer, Optional,          intent(inout) :: Opt_Int
        Integer, Optional,          intent(in)    :: Quite
        Integer                   :: i, LU, IOSTAT
        Integer                   :: NOBytes
        Character(LEN=IOMSG_SIZE) :: IOMSG
        Character(LEN=CLEN_SIZE)  :: NAM
        Character(LEN=11)         :: FORM
        Logical                   :: Speak, Binary, OPENED, RW = .true.
        Speak = .true.
Q:      If ( Present(Quite) ) Then
           Speak = .false.
        End If Q
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
WObytes_C1D:        If ( present(OBytes) ) Then
                        OBytes = Output_Bytes ( RW, FILE, Binary, Data_up%Dimen, Data_up%lower, Data_up%upper )
                    Else
                        NObytes = Output_Bytes ( RW, FILE, Binary, Data_up%Dimen, Data_up%lower, Data_up%upper )
                    End If WOBytes_C1D
                End If Q_C1D
print *, 'Read_Write_Basic_Mod: Data_up%upper(1) = ', Data_up%upper(1)
do i = 1, Data_up%upper(1)
print *, '                      Data_up%Data(i)  = ', i, trim(Data_up%Data(i))
enddo
            Type is ( Target_Material_Pointer )
                Call Read_Target_Material ( LU, Binary, Data_up )
Q_Targ:         If ( Speak ) Then
                    Write (*,*) 'Read ', trim(FILE), ' - no size data available'
                End If Q_Targ
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
    Function Output_Bytes ( Read_flag, FILE, Binary, Dimen, lower, upper ) Result ( OBytes )
        Implicit None
        Character(Len=*), intent(in) :: FILE
        Logical, intent(in) :: Read_Flag, Binary
        Integer, intent(in) :: Dimen, lower(1:Dimen), upper(1:Dimen)
        Integer :: OBytes
        Real :: Data_size
        Character(Len=2) :: unit, punit(5) = [ 'B ', 'kB', 'MB', 'GB', 'TB' ]
        Integer :: i
IFBin:  If ( Binary ) Then
            Data_size = 1.0d0
DoBsize:    Do i = 1, Dimen
                Data_size = Data_size * ( upper(i)-lower(i)+1 )
            End Do DoBSize
            Data_size = Data_size * kind(Data_size)
            Data_size = Data_size + ( 2*Dimen*kind(upper(1)) )
        Else
            Data_size = 1.0d0
DoAsize:    Do i = 1, Dimen
                Data_size = Data_size * ( upper(i)-lower(i)+1 )
            End Do DoASize
            Data_size = Data_size * ASCII_DATA_SIZE
            Data_size = Data_size + ( 2*Dimen*ASCII_BOUND_SIZE )
        End If IFBin
        OBytes = Int ( Data_size )
        unit = 'ER'
Dounit: Do i = 1, 5
            If ( Data_size .ge. 1.0d0  .and.  Data_size .lt. 1024.0d0 ) Then
                unit = punit(i)
                exit
            End If
            Data_size = Data_size / 1024.0d0
        End Do Dounit
IfRDF:  If ( Read_flag ) Then
            Write (*,'(1x,f14.7,1x,a2,a,a)') Data_size, unit, ' read from ', trim(FILE)
        Else
            Write (*,'(1x,f14.7,1x,a2,a,a)') Data_size, unit, ' written to ', trim(FILE)
        End If IfRDF
        Return
    End Function Output_Bytes
End Module Read_Write_Basic_Mod
