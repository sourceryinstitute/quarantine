Module Type_Target_Material_Mod
  Use Parameters_Mod
  Implicit None
  Type Target_Material_Pointer
     Character(LEN=CLEN_SIZE)    :: Name
     Integer                     :: Num_Isotopes
     Real                        :: Density
     Real, Pointer, Dimension(:) :: A_Target_Material_p=>null(), Z_Target_Material_p=>null(), D_Target_Material_p=>null()
  End Type Target_Material_Pointer
  Type Target_Material
     Character(LEN=CLEN_SIZE)        :: Name
     Integer                         :: Num_Isotopes
     Real                            :: Density
     Real, Allocatable, Dimension(:) :: A_Target_Material, Z_Target_Material, D_Target_Material
  End Type Target_Material
Contains
    Subroutine Read_Target_Material ( LU, Binary, D )
        Integer, intent(in) :: LU
        Logical, intent(in) :: Binary
        Type ( Target_Material_Pointer ), intent(out) :: D
        Integer :: IOSTAT, i
        Character(Len=IOMSG_SIZE) :: IOMSG
DMAlc1: If ( Associated ( D%A_Target_Material_p ) ) Then
            Write (*,'(a)') 'A_Target_Material_p already Associated - nullify'
            Nullify ( D%A_Target_Material_p )
        End If DMAlc1
DMAlc2: If ( Associated ( D%Z_Target_Material_p ) ) Then
            Write (*,'(a)') 'Z_Target_Material_p already Associated - nullify'
            Nullify ( D%Z_Target_Material_p )
        End If DMAlc2
DMAlc3: If ( Associated ( D%D_Target_Material_p ) ) Then
            Write (*,'(a)') 'D_Target_Material_p already Associated - nullify'
            Nullify ( D%D_Target_Material_p )
        End If DMAlc3
bin1:   If ( Binary ) Then
            Read ( unit=LU,        iostat=IOSTAT, iomsg=IOMSG ) D%Name, D%Num_Isotopes, D%Density
        Else
            Read ( unit=LU, fmt=*, iostat=IOSTAT, iomsg=IOMSG ) D%Name, D%Num_Isotopes, D%Density
        End If bin1
R1err:  If (IOSTAT .ne. 0 ) Then
            Write (*,'(a,i0,1x,a)') 'Constant Line Read Error, IOSTAT = ', IOSTAT, trim(IOMSG)
            Stop 'Abort Read_Target_Material'
        End If R1err
        Allocate ( D%A_Target_Material_p(1:D%Num_Isotopes), D%Z_Target_Material_p(1:D%Num_Isotopes), D%D_Target_Material_p(1:D%Num_Isotopes) )
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
        Return
    End Subroutine Read_Target_Material
    Subroutine Write_Target_Material ( LU, Binary, D )
        Integer, intent(in) :: LU
        Logical, intent(in) :: Binary
        Type ( Target_Material_Pointer ), intent(in) :: D
        Integer :: IOSTAT, i
        Character(Len=IOMSG_SIZE) :: IOMSG
bin1:   If ( Binary ) Then
            Write ( unit=LU,                            iostat=IOSTAT, iomsg=IOMSG ) D%Name, D%Num_Isotopes, D%Density
        Else
            Write ( unit=LU, fmt='(a,/,i10,/,1pe14.6)', iostat=IOSTAT, iomsg=IOMSG ) D%Name, D%Num_Isotopes, D%Density
        End If bin1
        Do i = 1, D%Num_Isotopes
bin2:       If ( Binary ) Then
                Write ( unit=LU,                        iostat=IOSTAT, iomsg=IOMSG ) D%A_Target_Material_p(i), D%Z_Target_Material_p(i), D%D_Target_Material_p(i)
            Else
                Write ( unit=LU, fmt='(3(1x,1pe14.6))', iostat=IOSTAT, iomsg=IOMSG ) D%A_Target_Material_p(i), D%Z_Target_Material_p(i), D%D_Target_Material_p(i)
            End If bin2
        End Do
        Return
    End Subroutine Write_Target_Material
End Module Type_Target_Material_Mod
