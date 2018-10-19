module total_mod
  implicit none
  type vector
      integer, allocatable :: upper(:)
      character(LEN=99), allocatable  :: elements(:)
  end Type
Contains
  Subroutine read_elements ( input )
    class(*), pointer, intent(in) :: input
    open (1,file='input.dat',form='formatted')
    select type ( input )
      type is ( vector )
        allocate ( input%upper(1) )
        read (1,*) input%upper
    end select
  end subroutine
end module
