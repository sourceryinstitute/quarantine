module total_mod
  implicit none
  type vector
      integer, allocatable :: upper(:)
      character(LEN=99), allocatable  :: elements(:)
  end Type
Contains
  Subroutine set_elements ( input )
    class(*), pointer, intent(in) :: input
    select type ( input )
      type is ( vector )
        allocate ( input%upper(1), source=1 )
    end select
  end subroutine
end module
