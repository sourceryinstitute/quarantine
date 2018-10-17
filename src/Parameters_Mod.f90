Module Parameters_Mod
    Implicit None
    Integer, Parameter           :: ASCII_DATA_SIZE    =       1 +   14
    Integer, Parameter           :: ASCII_BOUND_SIZE   =        1 + 5
    Integer, Parameter :: CLEN_SIZE = 1024
    Integer, Parameter :: IOMSG_SIZE = 120
    Logical, Parameter :: G_BINARY = .false.        ! Default value is ASCII input/output - change to .true. to be Binary input/output
End Module
