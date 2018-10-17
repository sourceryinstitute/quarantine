    Use total_mod, only : CLEN_SIZE, READ_TRNINPUT
    Implicit None
    Integer                                                             :: Num_Mats_loc
    Character(LEN=CLEN_SIZE), Allocatable, Dimension(:)                 :: Mat_Name_loc
    Call Read_TRNINPUT ('trninput.dat')
End
