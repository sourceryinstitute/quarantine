  implicit none
  call set_vector
contains
  subroutine set_vector()
    use vectors
    type (vector), target :: v
    class(*), pointer :: v_ptr
    v_ptr => v
    call set_elements ( v_ptr )
    select type ( v_ptr )
    type is ( vector )
      print *, v%elements(1), '<-- should be 2'
    end select
  end subroutine
end
