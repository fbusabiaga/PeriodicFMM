module fmm_C_interface
  use, intrinsic :: iso_c_binding
  implicit none
  public
  
  integer, parameter :: dp_c = c_double
  
contains
  
  subroutine hello(word_in)
    implicit none
    ! integer, intent(in) :: a
    character (*), optional, intent(in) :: word_in
    character (1024) :: word
    if(present(word_in)) then
       word = word_in
    else 
       word = "Hola"
    end if
    write(*,*) trim(word)
  end subroutine hello
  
        
end module fmm_C_interface
