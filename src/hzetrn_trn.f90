    Use Parameters_Mod, only : CLEN_SIZE
    Use Read_Write_TRNINPUT_Mod, only : READ_TRNINPUT
    Implicit None
    Integer                                                             :: Num_Mats_loc
    Character(LEN=CLEN_SIZE), Allocatable, Dimension(:)                 :: Mat_Name_loc
    Call Read_TRNINPUT ( 'trninput.dat', Num_Mats_loc, Mat_Name_loc, 'ASC' )
End
