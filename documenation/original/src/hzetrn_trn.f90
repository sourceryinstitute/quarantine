Program HZETRN_TRN
!
    Use Parameters_Mod
!
    Use Type_Target_Material_Mod
!
    Use Read_Write_Target_Material_Mod
    Use Read_Write_TRNINPUT_Mod
!
!================================================================================
!================================================================================
!                               Define Data
!================================================================================
!================================================================================
!
!********************************************************************************
! This program illistrates an error
!********************************************************************************
!
    Implicit None
!
!********************************************************************************
! Do loop indexes and local variables
!********************************************************************************
!
    Integer :: i, j
!
!********************************************************************************
! File I/O
!********************************************************************************
!
    Character(LEN=CLEN_SIZE) :: FILE
!
!********************************************************************************
! TRNInput
!********************************************************************************
!
    Integer                                                             :: Num_Mats_loc
    Character(LEN=CLEN_SIZE), Allocatable, Dimension(:)                 :: Mat_Name_loc
!
!********************************************************************************
! Target Material
!********************************************************************************
!
    Type ( Target_Material ), Allocatable, Dimension(:)                 :: TM_mat_loc
!
!********************************************************************************
! Read TRNINPUT
!********************************************************************************
!
    FILE = 'trninput.dat'
    Call Read_TRNINPUT ( FILE, Num_Mats_loc, Mat_Name_loc, 'ASC' )
!
    print *, 'Num_Mats_loc = ', Num_Mats_loc
    Do i = 1, Num_Mats_loc
       print *, 'Mat_Name_loc =', trim(Mat_Name_loc(i)), '-'
    End Do
!
!********************************************************************************
! Read TM
!********************************************************************************
!
       Call Read_TM ( Num_Mats_loc, Mat_Name_loc, TM_Mat_loc, 'ASC' )
!
       print *, 'TM_Mat_loc allocated? : ', Allocated(TM_Mat_loc)
       Do i = 1, Num_Mats_loc
          print *, 'p_read: TM_Mat_loc Name, Number, Density: '
          print *, trim(TM_Mat_loc(i)%Name), TM_Mat_loc(i)%Num_Isotopes, TM_Mat_loc(i)%Density
          print *, 'TM_Mat_loc(i)%A allocated? : ', Allocated(TM_Mat_loc(i)%A_Target_Material)
          print *, 'TM_Mat_loc(i)%Z allocated? : ', Allocated(TM_Mat_loc(i)%Z_Target_Material)
          print *, 'TM_Mat_loc(i)%D allocated? : ', Allocated(TM_Mat_loc(i)%D_Target_Material)
          Do j = 1, TM_Mat_loc(i)%Num_Isotopes
             print *, 'A: ', TM_Mat_loc(i)%A_Target_Material(j)
             print *, 'Z: ', TM_Mat_loc(i)%Z_Target_Material(j)
             print *, 'D: ', TM_Mat_loc(i)%D_Target_Material(j)
          End Do
       End Do
!
!********************************************************************************
! End of Program
!********************************************************************************
!
End Program HZETRN_TRN
