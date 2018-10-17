Module Type_CArray_1D_Mod
!
    Use Parameters_Mod
!
    Implicit None
!
    Private
    Public :: Read_CArray_1D
    Public :: CArray_1D
!
    Type :: CArray_1D
        Integer :: Dimen=1
        Integer, Allocatable, Dimension(:) :: lower, upper
        Character(LEN=CLEN_SIZE), Allocatable, Dimension(:) :: Data
    End Type CArray_1D
!
Contains
!
    Subroutine Read_CArray_1D ( LU, Binary, D )
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Called by Read_Write_Basic_Mod.f90
!
!**********************************************************************************************************************************
! This subroutine is for the CArray_1D derived type data read
!**********************************************************************************************************************************
!
        Implicit None
!
!**********************************************************************************************************************************
! LU - The logical unit for input
! Binary - Flag for binary or ASCII input
! D - The data to pass back and is of type CArray_1D
!**********************************************************************************************************************************
!
        Integer, intent(in) :: LU
        Logical, intent(in) :: Binary
        Type ( CArray_1D ), intent(out) :: D
!
!**********************************************************************************************************************************
! Local Varaibles
!   IOSTAT - THe status of he IO statement
!   IOMSG - The associate message with an error in the IO statement
!   i - do loop variable
!**********************************************************************************************************************************
!
        Integer :: IOSTAT, i
        Character(Len=IOMSG_SIZE) :: IOMSG
!
!**********************************************************************************************************************************
! Make sure the lower pointer is not allocated
!**********************************************************************************************************************************
!
DMAlcl: If ( Allocated ( D%lower ) ) Then
            Write (*,'(a)') 'Lower already Allocated - Deallocate'
            Deallocate ( D%lower )
         End If DMAlcl
!
!**********************************************************************************************************************************
! Make sure the upper pointer is not allocated
!**********************************************************************************************************************************
!
DMAlcu: If ( Allocated ( D%upper ) ) Then
            Write (*,'(a)') 'Upper already Allocated - Deallocate'
            Deallocate ( D%upper )
         End If DMAlcu
!
!**********************************************************************************************************************************
! Make sure the data pointer is not allocated
!**********************************************************************************************************************************
!
DAlc:   If ( Allocated ( D%Data ) ) Then
            Write (*,'(a)') 'Data already Allocated - Deallocate'
            Deallocate ( D%Data )
        End If DAlc
!
!**********************************************************************************************************************************
! Allocate the lower and upper array boundaries
!**********************************************************************************************************************************
!
        Allocate ( D%lower(d%Dimen), D%upper(d%Dimen), stat=IOSTAT, errmsg=IOMSG )
DMerr:  If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Dimension Allocation Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_CArray_1D'
        End If DMerr
!
!**********************************************************************************************************************************
! Read the lower and upper array boundaries from the data file: Binary or ASCII
!**********************************************************************************************************************************
!
DRDif:  If ( Binary ) Then
            Read ( unit=LU,        iostat=IOSTAT, iomsg=IOMSG ) (D%lower(i), D%upper(i), i=1,D%Dimen)
        Else
            Read ( unit=LU, fmt=*, iostat=IOSTAT, iomsg=IOMSG ) (D%lower(i), D%upper(i), i=1,D%Dimen)
        End If DRDif
RDMerr: If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Dimension Read Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_CArray_1D'
        End If RDMerr
!
!**********************************************************************************************************************************
! Allocate the data array
!**********************************************************************************************************************************
!
        Allocate ( D%Data(D%lower(D%Dimen):D%upper(D%Dimen)), stat=IOSTAT, errmsg=IOMSG )        
Derr:   If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Data Allocation Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_CArray_1D'
        End If Derr
!
!**********************************************************************************************************************************
! Read the data array: Binary or ASCII
!**********************************************************************************************************************************
!
RDif:   If ( Binary ) Then
            Read ( unit=LU,        iostat=IOSTAT, iomsg=IOMSG ) D%Data
        Else
            Read ( unit=LU, fmt=*, iostat=IOSTAT, iomsg=IOMSG ) D%Data
        End If RDif
RDerr:  If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Data Read Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_CArray_1D'
        End If RDerr
print *, 'Type_CArray_1D_Mod: D%upper(1) = ', D%upper(1)
do i = 1, D%upper(1)
print *, 'Type_CArray_1D_Mod: D%Data(i)  = ', i, trim(D%Data(i))
enddo
!
!**********************************************************************************************************************************
! Return to the calling program
!**********************************************************************************************************************************
!
        Return
!
!**********************************************************************************************************************************
! End for read CArray_1D
!**********************************************************************************************************************************
!
    End Subroutine Read_CArray_1D
!
End Module Type_CArray_1D_Mod
