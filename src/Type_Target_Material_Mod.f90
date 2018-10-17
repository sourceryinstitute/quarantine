Module Type_Target_Material_Mod
!
  Use Parameters_Mod
!
  Implicit None
!
  Private
  Public :: Read_Target_Material, Write_Target_Material
  Public :: Target_Material, Target_Material_Pointer
!
  Type Target_Material_Pointer
     Character(LEN=CLEN_SIZE)    :: Name
     Integer                     :: Num_Isotopes
     Real                        :: Density
     Real, Pointer, Dimension(:) :: A_Target_Material_p=>null(), Z_Target_Material_p=>null(), D_Target_Material_p=>null()
  End Type Target_Material_Pointer
!
  Type Target_Material
     Character(LEN=CLEN_SIZE)        :: Name
     Integer                         :: Num_Isotopes
     Real                            :: Density
     Real, Allocatable, Dimension(:) :: A_Target_Material, Z_Target_Material, D_Target_Material
  End Type Target_Material
!
Contains
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Called by Read_Write_Basic_Mod.f90
!
    Subroutine Read_Target_Material ( LU, Binary, D )
!
!**********************************************************************************************************************************
! This subroutine is for the Target_Material derived type data read
!**********************************************************************************************************************************
!
        Implicit None
!
!**********************************************************************************************************************************
! LU - The logical unit for input
! Binary - Flag for binary or ASCII input
! D - The data to pass back and is of type Target_Material
!**********************************************************************************************************************************
!
        Integer, intent(in) :: LU
        Logical, intent(in) :: Binary
        Type ( Target_Material_Pointer ), intent(out) :: D
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
! Make sure the A_Target_Material_p pointer is not associated
!**********************************************************************************************************************************
!
DMAlc1: If ( Associated ( D%A_Target_Material_p ) ) Then
            Write (*,'(a)') 'A_Target_Material_p already Associated - nullify'
            Nullify ( D%A_Target_Material_p )
        End If DMAlc1
!
!**********************************************************************************************************************************
! Make sure the Z_Target_Material_p pointer is not associated
!**********************************************************************************************************************************
!
DMAlc2: If ( Associated ( D%Z_Target_Material_p ) ) Then
            Write (*,'(a)') 'Z_Target_Material_p already Associated - nullify'
            Nullify ( D%Z_Target_Material_p )
        End If DMAlc2
!
!**********************************************************************************************************************************
! Make sure the D_Target_Material_p pointer is not associated
!**********************************************************************************************************************************
!
DMAlc3: If ( Associated ( D%D_Target_Material_p ) ) Then
            Write (*,'(a)') 'D_Target_Material_p already Associated - nullify'
            Nullify ( D%D_Target_Material_p )
        End If DMAlc3
!
!**********************************************************************************************************************************
! Read the file to get the Name, Num_Isotopes, and Density
!**********************************************************************************************************************************
!
bin1:   If ( Binary ) Then
            Read ( unit=LU,        iostat=IOSTAT, iomsg=IOMSG ) D%Name, D%Num_Isotopes, D%Density
        Else
            Read ( unit=LU, fmt=*, iostat=IOSTAT, iomsg=IOMSG ) D%Name, D%Num_Isotopes, D%Density
        End If bin1
R1err:  If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Constant Line Read Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_Target_Material'
        End If R1err
!
!**********************************************************************************************************************************
! Allocate Arrays
!**********************************************************************************************************************************
!
        Allocate ( D%A_Target_Material_p(1:D%Num_Isotopes), D%Z_Target_Material_p(1:D%Num_Isotopes), D%D_Target_Material_p(1:D%Num_Isotopes) )
!
!**********************************************************************************************************************************
! Read the data
!**********************************************************************************************************************************
!
        Do i = 1, D%Num_Isotopes
bin2:       If ( Binary ) Then
                Read ( unit=LU,        iostat=IOSTAT, iomsg=IOMSG ) D%A_Target_Material_p(i), D%Z_Target_Material_p(i), D%D_Target_Material_p(i)
            Else
                Read ( unit=LU, fmt=*, iostat=IOSTAT, iomsg=IOMSG ) D%A_Target_Material_p(i), D%Z_Target_Material_p(i), D%D_Target_Material_p(i)
            End If bin2
        End Do
R2err:  If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Target Data Read Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_Target_Material'
        End If R2err
!
!**********************************************************************************************************************************
! Return to calling programs
!**********************************************************************************************************************************
!
        Return
!
!**********************************************************************************************************************************
! End of Subroutine
!**********************************************************************************************************************************
!
    End Subroutine Read_Target_Material
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Called by Read_Write_Basic_Mod.f90
!
    Subroutine Write_Target_Material ( LU, Binary, D )
!
!**********************************************************************************************************************************
! This subroutine is for the Target_Material derived type data read
!**********************************************************************************************************************************
!
        Implicit None
!
!**********************************************************************************************************************************
! LU - The logical unit for input
! Binary - Flag for binary or ASCII input
! D - The data to pass back and is of type Target_Material
!**********************************************************************************************************************************
!
        Integer, intent(in) :: LU
        Logical, intent(in) :: Binary
        Type ( Target_Material_Pointer ), intent(in) :: D
!
!**********************************************************************************************************************************
! Local Varaibles
!   IOSTAT - THe status of he IO statement
!   IOMSG - The associate message with an error in the IO statement
!   i - Do loop variable
!**********************************************************************************************************************************
!
        Integer :: IOSTAT, i
        Character(Len=IOMSG_SIZE) :: IOMSG
!
!**********************************************************************************************************************************
! Write the front data
!**********************************************************************************************************************************
!
bin1:   If ( Binary ) Then
            Write ( unit=LU,                            iostat=IOSTAT, iomsg=IOMSG ) D%Name, D%Num_Isotopes, D%Density
        Else
            Write ( unit=LU, fmt='(a,/,i10,/,1pe14.6)', iostat=IOSTAT, iomsg=IOMSG ) D%Name, D%Num_Isotopes, D%Density
        End If bin1
!
!**********************************************************************************************************************************
! Write the isotope data
!**********************************************************************************************************************************
!
        Do i = 1, D%Num_Isotopes
bin2:       If ( Binary ) Then
                Write ( unit=LU,                        iostat=IOSTAT, iomsg=IOMSG ) D%A_Target_Material_p(i), D%Z_Target_Material_p(i), D%D_Target_Material_p(i)
            Else
                Write ( unit=LU, fmt='(3(1x,1pe14.6))', iostat=IOSTAT, iomsg=IOMSG ) D%A_Target_Material_p(i), D%Z_Target_Material_p(i), D%D_Target_Material_p(i)
            End If bin2
        End Do
!
!**********************************************************************************************************************************
! Return to the calling program
!**********************************************************************************************************************************
!
        Return
!
!**********************************************************************************************************************************
! End for Write_Target_Material
!**********************************************************************************************************************************
!
    End Subroutine Write_Target_Material
!
End Module Type_Target_Material_Mod
