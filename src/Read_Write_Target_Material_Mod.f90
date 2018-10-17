Module Read_Write_Target_Material_Mod
!
  Use Parameters_Mod
!
  Implicit None
!
  Private
!
  Public :: Read_TM
!
Contains
!
  Subroutine Read_TM ( Num_Mats, Mat_Name, TM_Mat, ACCESS )
!
    Use Read_Write_Basic_Mod, only : Read_Data
    Use Type_Target_Material_Mod, only : Target_Material, Target_Material_Pointer
!
    Implicit None
!
    Integer,                                             intent(in)  :: Num_Mats
    Character(LEN=*),                      Dimension(*), intent(in)  :: Mat_Name
    Type ( Target_Material ), Allocatable, Dimension(:), intent(out) :: TM_Mat
    Character(Len=3), Optional,                          intent(in)  :: ACCESS
!
    Type ( Target_Material_Pointer ), Pointer,     Dimension(:)      :: TM_Mat_p
!
    Integer                                                          :: i, j, k
    Character(LEN=1000)                                              :: FILE
    Type ( Target_Material_Pointer ), Target                         :: D_t
    Class(*), Pointer                                                :: Data_up=>null()
!
    Allocate ( TM_Mat_p(1:Num_Mats) )
    Do i = 1, Num_Mats
       FILE = trim(Mat_Name(i)) // '.mat'
       Data_up => D_t
       Call Read_Data ( FILE, Data_up, ACCESS )
       Select Type ( Data_up )
          Type is ( Target_Material_Pointer )
             TM_mat_p(i)%Name                =  Data_up%Name
             TM_mat_p(i)%Num_Isotopes        =  Data_up%Num_Isotopes
             TM_mat_p(i)%Density             =  Data_up%Density
             TM_mat_p(i)%A_Target_Material_p => Data_up%A_target_Material_p
             TM_mat_p(i)%Z_Target_Material_p => Data_up%Z_target_Material_p
             TM_mat_p(i)%D_Target_Material_p => Data_up%D_target_Material_p
       End Select
       Nullify ( Data_up )
    End Do
!
    Allocate ( TM_Mat(1:Num_Mats) )
    Do i = 1, Num_Mats
       TM_Mat(i)%Name         = TM_Mat_p(i)%Name
       TM_Mat(i)%Num_Isotopes = TM_Mat_p(i)%Num_Isotopes
       TM_Mat(i)%Density      = TM_Mat_p(i)%Density
       Allocate ( TM_Mat(i)%A_Target_Material(TM_Mat(i)%Num_Isotopes) )
       Allocate ( TM_Mat(i)%Z_Target_Material(TM_Mat(i)%Num_Isotopes) )
       Allocate ( TM_Mat(i)%D_Target_Material(TM_Mat(i)%Num_Isotopes) )
       TM_Mat(i)%A_Target_Material = TM_Mat_p(i)%A_Target_Material_p
       TM_Mat(i)%Z_Target_Material = TM_Mat_p(i)%Z_Target_Material_p
       TM_Mat(i)%D_Target_Material = TM_Mat_p(i)%D_Target_Material_p
    End Do
!
!print *, 'TM_Mat in Read_TM'
!do i=1,Num_Mats
!associate ( S => TM_Mat(i) )
!print *, 'Name NIso Dens: ', i, '-', trim(S%Name), '-', S%Num_Isotopes, S%Density
!print *, 'a_target_material allocated? ', allocated(S%a_target_material)
!print *, 'z_target_material allocated? ', allocated(S%z_target_material)
!print *, 'd_target_material allocated? ', allocated(S%d_target_material)
!do j=1,S%num_isotopes
!print *, '     A,Z,D: ', j, S%a_target_material(j), S%z_target_material(j), S%d_target_material(j)
!enddo
!end associate
!enddo
!
  End Subroutine Read_TM
!
End Module Read_Write_Target_Material_Mod
