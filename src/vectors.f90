module vectors
  implicit none
  type vector
    integer, allocatable :: elements(:)
  end Type
contains
  subroutine set_elements(input)
    class(*), pointer, intent(in) :: input
    select type (input)
      type is (vector)
        input%elements=[2]
    end select
  end subroutine
end module
