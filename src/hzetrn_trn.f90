  implicit none
  call read_input
contains
  subroutine read_input()
    use total_mod
    type (vector), target :: a
    class(*), pointer :: a_ptr
    a_ptr => a
    call set_elements ( a_ptr )
    select Type ( a_ptr )
    type is ( vector )
      print *, a%upper(1), '<-- should be 1'
    end select
  end subroutine
end
