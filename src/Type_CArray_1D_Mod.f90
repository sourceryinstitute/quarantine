Module Type_CArray_1D_Mod
    Use Parameters_Mod
    Implicit None
    Type :: CArray_1D
        Integer :: Dimen=1
        Integer, Allocatable, Dimension(:) :: lower, upper
        Character(LEN=CLEN_SIZE), Allocatable, Dimension(:) :: Data
    End Type CArray_1D
Contains
    Subroutine Read_CArray_1D ( LU, Binary, D )
        Integer, intent(in) :: LU
        Logical, intent(in) :: Binary
        Type ( CArray_1D ), intent(out) :: D
        Integer :: IOSTAT, i
        Character(Len=IOMSG_SIZE) :: IOMSG
DMAlcl: If ( Allocated ( D%lower ) ) Then
            Write (*,'(a)') 'Lower already Allocated - Deallocate'
            Deallocate ( D%lower )
         End If DMAlcl
DMAlcu: If ( Allocated ( D%upper ) ) Then
            Write (*,'(a)') 'Upper already Allocated - Deallocate'
            Deallocate ( D%upper )
         End If DMAlcu
DAlc:   If ( Allocated ( D%Data ) ) Then
            Write (*,'(a)') 'Data already Allocated - Deallocate'
            Deallocate ( D%Data )
        End If DAlc
        Allocate ( D%lower(d%Dimen), D%upper(d%Dimen), stat=IOSTAT, errmsg=IOMSG )
DMerr:  If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Dimension Allocation Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_CArray_1D'
        End If DMerr
DRDif:  If ( Binary ) Then
            Read ( unit=LU,        iostat=IOSTAT, iomsg=IOMSG ) (D%lower(i), D%upper(i), i=1,D%Dimen)
        Else
            Read ( unit=LU, fmt=*, iostat=IOSTAT, iomsg=IOMSG ) (D%lower(i), D%upper(i), i=1,D%Dimen)
        End If DRDif
RDMerr: If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Dimension Read Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_CArray_1D'
        End If RDMerr
        Allocate ( D%Data(D%lower(D%Dimen):D%upper(D%Dimen)), stat=IOSTAT, errmsg=IOMSG )
Derr:   If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Data Allocation Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_CArray_1D'
        End If Derr
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
        Return
    End Subroutine Read_CArray_1D
End Module Type_CArray_1D_Mod
