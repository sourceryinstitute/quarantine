Module Parameters_Mod
!
    Implicit None
!
!**********************************************************************************************************************************
! Characters: Define the output format of all ASCII data written to a file.
! Integers: Defines the data used to calulate sizes of read/write file
!**********************************************************************************************************************************
!                                                         12345678901234567
    Character(Len=17), Parameter :: DATA_FORMAT_SIZE   = '(:,5(1x,1pe14.7))'
    Integer, Parameter           :: ASCII_DATA_SIZE    =       1 +   14
!                                                         123456789012
    Character(Len=12), Parameter :: CDATA_FORMAT_SIZE  = '(:,(1x,a))'
!                                                         123456789012345
    Character(Len=15), Parameter :: BOUND_FORMAT_SIZE  = '(:,10(1x,i5.5))'
    Integer, Parameter           :: ASCII_BOUND_SIZE   =        1 + 5
!
!**********************************************************************************************************************************
! Define the length of all string sizes
!**********************************************************************************************************************************
!
    Integer, Parameter :: CLEN_SIZE = 1024
    Integer, Parameter :: IOMSG_SIZE = 120
!
!**********************************************************************************************************************************
! Data for default file format
!       For Binary file formats: G_BINARY = .true.
!       For ASCII file formats:  G_BINARY = .false.
!**********************************************************************************************************************************
!
    Logical, Parameter :: G_BINARY = .false.        ! Default value is ASCII input/output - change to .true. to be Binary input/output
!
End Module Parameters_Mod
