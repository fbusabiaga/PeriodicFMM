program test
  use fmm_c_interface
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: n = 1
  real(kind=dp) :: f0
  real(kind=dp) :: src_coor(n), src_value(n), trg_coor(n), trg_value(n)
  
  call hello("Hello")


  print*, "# End"

end program test
