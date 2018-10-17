Module Read_Write_Basic_Mod
!
!**********************************************************************************************************************************
! This module defines the raw reads/writes of derived data types to/from a data file.  The access can be ASCII or Binary with the default being Binary.
!     This module contains 1D through 7D routines but can define array data types to 15D, the 2008 Standard (Intel allows 31D, PGI is unknown).
!     This module can handle any derived data type to be read/written to a file.  Just add the derived data type and a corresponding Read_<type> and Write_<type>
!     subrotuine to the module.  Make it provate and it cannot be intercepted by any other routine Use'ing this module
!**********************************************************************************************************************************
!
    Use Parameters_Mod
!
!**********************************************************************************************************************************
! No implicit typing
!**********************************************************************************************************************************
!
    Implicit None
!
!**********************************************************************************************************************************
! All names are private except as noted below
!**********************************************************************************************************************************
!
    Private
!
!**********************************************************************************************************************************
! Make public only the subroutine names and the data types
!**********************************************************************************************************************************
!
    Public :: Read_Data
!
!**********************************************************************************************************************************
! The subrotines are define below.  The Read_Data and Write_Data subroutines are all that are public.  All other routines are private
!**********************************************************************************************************************************
!
Contains
!
!==================================================================================================================================
! Read Data - Public
!==================================================================================================================================
!
    Subroutine Read_Data ( FILE, Data_up, ACCESS, OBytes, Opt_Int, Quite )
!
!**********************************************************************************************************************************
! This routine reads data into the data type passed by Data_up and passes it back to the calling routine.
!       The data type is queried below and the proper set of reads are used.
!       Any data type can be read with this routine.
!**********************************************************************************************************************************
! Use needed modules for data definitions
!**********************************************************************************************************************************
!
      Use Type_CArray_1D_Mod
      Use Type_Target_Material_Mod
!
!**********************************************************************************************************************************
! No implicit data typing
!**********************************************************************************************************************************
!
        Implicit None
!
!**********************************************************************************************************************************
! Data_up - An unlimited polymorphic Pointer that points to the data straucture to be read
! FILE - The file name to open and connect to the LU
! ACCESS - Optional value of BIN or ASC to open the file formatted ro unformatted manually.
!               The default is in the global variable G_Binary
! OBytes - Optional output for the output number of bytes read
! Opt_Int - Optional Integer to pass to type routine
! Quite - Optional Integer argument to not have data read output if defined, value unimportant
!**********************************************************************************************************************************
!
        Class(*), Pointer,          intent(in)    :: Data_up
        Character(Len=*),           intent(in)    :: FILE
        Character(Len=3), Optional, intent(in)    :: ACCESS
        Integer, Optional,          intent(out)   :: OBytes
        Integer, Optional,          intent(inout) :: Opt_Int
        Integer, Optional,          intent(in)    :: Quite
!
!**********************************************************************************************************************************
! Local variables
!   i - do loop index
!   LU - The Logical Unit number to read
!   IOSTAT - Message buffer from IO errors
!   NOBytes - The function to determine BYTES read
!   IOMSG - Message buffer from memory managment errors
!   NAM - Return from Inquire Function that contains the name of the opened data file
!   FORM - The string used to open the file formatted or unformatted
!   Binary - Flag for Binary (unformatted - true) or ASCII (formatted - false) data access
!   OPENED - Flag for opened data file or not from the Inquire function
!   RW - Flag to determine if reading or writting to Output_Bytes
!**********************************************************************************************************************************
!
        Integer                   :: i, LU, IOSTAT
        Integer                   :: NOBytes
        Character(LEN=IOMSG_SIZE) :: IOMSG
        Character(LEN=CLEN_SIZE)  :: NAM
        Character(LEN=11)         :: FORM
        Logical                   :: Speak, Binary, OPENED, RW = .true.
!
!**********************************************************************************************************************************
! Determine if Quite is defined
!**********************************************************************************************************************************
!
        Speak = .true.
Q:      If ( Present(Quite) ) Then
           Speak = .false.
        End If Q
!
!**********************************************************************************************************************************
! Determine access for file open (formatted or unformatted)
!**********************************************************************************************************************************
!
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
!
!**********************************************************************************************************************************
! Open the data file for read
!**********************************************************************************************************************************
!
        Open ( newunit=LU, file=trim(FILE), status='old', form=trim(FORM), action='read', iostat=IOSTAT, iomsg=IOMSG )
OPerr:  If ( IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'File Open Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_Data'
        End If OPerr
!
!**********************************************************************************************************************************
! Select code execution on the data type being read
!**********************************************************************************************************************************
!
DUPtyp: Select Type ( Data_up )
!
!**********************************************************************************************************************************
! Type: CArray_1D - read data and tell user how many bytes read
!**********************************************************************************************************************************
!
            Type is ( CArray_1D )
!
                Call Read_CArray_1D ( LU, Binary, Data_up )
Q_C1D:          If ( Speak ) Then
WObytes_C1D:        If ( present(OBytes) ) Then
!                        OBytes = Output_Bytes ( RW, FILE, Binary, Data_up%Dimen, Data_up%lower_p, Data_up%upper_p )
                        OBytes = Output_Bytes ( RW, FILE, Binary, Data_up%Dimen, Data_up%lower, Data_up%upper )
                    Else
!                        NObytes = Output_Bytes ( RW, FILE, Binary, Data_up%Dimen, Data_up%lower_p, Data_up%upper_p )
                        NObytes = Output_Bytes ( RW, FILE, Binary, Data_up%Dimen, Data_up%lower, Data_up%upper )
                    End If WOBytes_C1D
                End If Q_C1D
print *, 'Read_Write_Basic_Mod: Data_up%upper(1) = ', Data_up%upper(1)
do i = 1, Data_up%upper(1)
print *, '                      Data_up%Data(i)  = ', i, trim(Data_up%Data(i))
enddo
!
!**********************************************************************************************************************************
! Type Target_Material - read data
!**********************************************************************************************************************************
!
            Type is ( Target_Material_Pointer )
!
                Call Read_Target_Material ( LU, Binary, Data_up )
Q_Targ:         If ( Speak ) Then
                    Write (*,*) 'Read ', trim(FILE), ' - no size data available'
                End If Q_Targ
!
!
!**********************************************************************************************************************************
! Class Default - Error out
!**********************************************************************************************************************************
!
            Class Default
!
                Write (*,'(a,i0,1x,a)') 'The class of the data passed to Read_Data was not recognized'
                Stop 'Abort Read_Data'
!
!**********************************************************************************************************************************
! End Select Type on Data_up
!**********************************************************************************************************************************
!
        End Select DUPTyp
!
!**********************************************************************************************************************************
! Close the Data File
!**********************************************************************************************************************************
!
        Close ( unit=LU, iostat=IOSTAT, iomsg=IOMSG )
CLerr:  If ( IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'File Close Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_Data'
        End If CLerr
!
!**********************************************************************************************************************************
! Return to the calling program
!**********************************************************************************************************************************
!
        Return
!
!**********************************************************************************************************************************
! End of Subroutine Read_Data
!**********************************************************************************************************************************
!
    End Subroutine Read_Data
!
!==================================================================================================================================
! Routine to write out the number of Bytes read/written - Private
!==================================================================================================================================
!
    Function Output_Bytes ( Read_flag, FILE, Binary, Dimen, lower, upper ) Result ( OBytes )
!
!**********************************************************************************************************************************
! No implicit typing
!**********************************************************************************************************************************
!
        Implicit None
!
!**********************************************************************************************************************************
! FILE - The file name just read/written
! Read_Flag - .true. if reading; .false. if writting
! Binary - .true if binary and .false. if ASCII
! Dimen - The dimension of the array being read/written
! lower - The lower bounds of the array indicies
! upper - The upper bounds of the array indicies
! OBytes - The number of BYTES read/written to/from a file (needs to be I*8)
!**********************************************************************************************************************************
!
        Character(Len=*), intent(in) :: FILE
        Logical, intent(in) :: Read_Flag, Binary
        Integer, intent(in) :: Dimen, lower(1:Dimen), upper(1:Dimen)
        Integer :: OBytes
!
!**********************************************************************************************************************************
! Local Varaibles
!   Data_size - The size in B/kB/MB/GB/TB of the data
!   unit - The unit used in the write statement
!   punit - The possible sizes allowed
!   i - Do loop index
!**********************************************************************************************************************************
!
        Real :: Data_size
        Character(Len=2) :: unit, punit(5) = [ 'B ', 'kB', 'MB', 'GB', 'TB' ]
        Integer :: i
!
!**********************************************************************************************************************************
! Determine the size in Bytes of a binary data file
!**********************************************************************************************************************************
!
IFBin:  If ( Binary ) Then
            Data_size = 1.0d0
DoBsize:    Do i = 1, Dimen
                Data_size = Data_size * ( upper(i)-lower(i)+1 )
            End Do DoBSize
            Data_size = Data_size * kind(Data_size)
            Data_size = Data_size + ( 2*Dimen*kind(upper(1)) )
!
!**********************************************************************************************************************************
! Determine the size in Bytes of an ASCII data file
!**********************************************************************************************************************************
!
        Else
            Data_size = 1.0d0
DoAsize:    Do i = 1, Dimen
                Data_size = Data_size * ( upper(i)-lower(i)+1 )
            End Do DoASize
            Data_size = Data_size * ASCII_DATA_SIZE
            Data_size = Data_size + ( 2*Dimen*ASCII_BOUND_SIZE )
!
!**********************************************************************************************************************************
! End of IF block on data size determination
!**********************************************************************************************************************************
!
        End If IFBin
!
!**********************************************************************************************************************************
! Set the function result to an integer of size - use default fortran 2003 conversion
!   This is in BYTES
!**********************************************************************************************************************************
!
        OBytes = Int ( Data_size )
!
!**********************************************************************************************************************************
! Find the 1024 byte scale that brings the size between >=1 and <1024 then set unit to the two letter code for the scale factor
!**********************************************************************************************************************************
!
        unit = 'ER'
Dounit: Do i = 1, 5
            If ( Data_size .ge. 1.0d0  .and.  Data_size .lt. 1024.0d0 ) Then
                unit = punit(i)
                exit
            End If
            Data_size = Data_size / 1024.0d0
        End Do Dounit
!
!**********************************************************************************************************************************
! Tell the user - whether a read or write
!**********************************************************************************************************************************
!
IfRDF:  If ( Read_flag ) Then
            Write (*,'(1x,f14.7,1x,a2,a,a)') Data_size, unit, ' read from ', trim(FILE)
        Else
            Write (*,'(1x,f14.7,1x,a2,a,a)') Data_size, unit, ' written to ', trim(FILE)
        End If IfRDF
!
!**********************************************************************************************************************************
! Return to calling program
!**********************************************************************************************************************************
!
        Return
!
!**********************************************************************************************************************************
! End of function Output_Bytes
!**********************************************************************************************************************************
!
    End Function Output_Bytes
!
End Module Read_Write_Basic_Mod
