program test
  use libfmmwrapper
  ! use mpi
  use iso_c_binding
  implicit none
  integer :: i, j, k
  integer :: mpi_rank, mpi_size, id
  integer :: ierr
  integer, parameter :: dp = kind(1.0d0)
  real(kind=dp) :: f0
  real(kind=dp) :: xlow, xhigh, ylow, yhigh, zlow, zhigh
  type(fmm_wrapper) :: fmm
  integer :: mult_order
  integer :: max_pts
  integer :: init_depth
  integer :: pbc
  integer :: num_src, num_trg
  real(dp), allocatable :: src_coord(:), trg_coord(:)
  real(dp), allocatable :: src_value(:), trg_value(:)

  ! Call random seed
  call random_seed()

  ! Set FMM parameters
  mult_order = 8
  max_pts = 1024
  init_depth = 0
  pbc = 0
  xlow = 0
  xhigh = 1
  ylow = 0
  yhigh = 1
  zlow = 0
  zhigh = 0.25
  num_src = 10
  num_trg = 10

  ! Allocate memory for sources and targets
  allocate(src_coord(num_src * 3), trg_coord(num_trg * 3))
  allocate(src_value(num_src * 3), trg_value(num_trg * 3))

  ! Init sources and targets
  call random_number(src_coord)
  call random_number(trg_coord)
  call random_number(src_value)
  
  ! zhigh should be smaller than xhigh and yhigh
  do i=1, num_src
     src_coord(i*3) = src_coord(i*3) * 0.25
  end do
  do i=1, num_trg
     trg_coord(i*3) = trg_coord(i*3) * 0.25
  end do

  ! Init MPI
  call MPI_INIT(mpi_rank,mpi_size)     
  
  ! Create FMM object
  fmm = fmm_wrapper(mult_order, max_pts, init_depth, pbc)

  ! Set box (all the sources and targer should be inside)
  call fmm%FMM_SetBox(xlow, xhigh, ylow, yhigh, zlow, zhigh)

  ! Clear tree, in general we don't need to call this method
  call fmm%FMM_TreeClear()

  ! Clear FMM data, in general we don't need to call this method
  call fmm%FMM_DataClear()

  ! Update tree with the coordinates of targets and sources
  call fmm%FMM_UpdateTree(trg_coord, src_coord, num_trg, num_src)

  ! Evaluate FMM
  call fmm%FMM_Evaluate(trg_value, src_value, num_trg, num_src)
  
  ! Print target values
  write(*,*) "Write target values"
  write(*,*) (trg_value(i), i=1, num_trg * 3)

  ! Delete FMM method
  call fmm%delete
  
  ! Close MPI
  call MPI_FINALIZE(ierr)
  if(ierr .gt. 0) print*, "MPI_FINALIZE error = ", ierr

  ! Free memory
  deallocate(src_coord, trg_coord)
  deallocate(src_value, trg_value)
  
  print*, "# End"

end program test
