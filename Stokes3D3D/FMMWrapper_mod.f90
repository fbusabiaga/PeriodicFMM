! -----------------------------------------------------------
! Wrapper to call Periodic FMM.
! 
! Reference:
! Flexibly imposing periodicity in kernel independent FMM: A multipole-to-local operator approach,
! Wen Yan and Michael Shelley, JCP (355) 214 (2018).
! https://doi.org/10.1016/j.jcp.2017.11.012
!
! Dependencies:
! 1. pvFMM, use Wen Yan's fork: https://github.com/wenyan4work/pvfmm
! 2. Wrapper for Periodic FMM: https://github.com/wenyan4work/PeriodicFMM
! 3. All the pvFMM depencies.
!
! -----------------------------------------------------------


module libfmmwrapper
    use iso_c_binding
    private
    public :: fmm_wrapper
	integer, parameter :: dp = kind(1.0d0)

    include "FMMWrapper_C_def.f90"

    type fmm_wrapper
      private
      type(c_ptr) :: ptr ! pointer to the FMMWrapper class
	  contains
	    ! We can bind some functions to this type, allowing for a cleaner syntax.
	    procedure :: delete => delete_fmm_wrapper_polymorph 
		procedure :: FMM_SetBox => fmm_wrapper_FMM_SetBox
        procedure :: FMM_TreeClear => fmm_wrapper_FMM_TreeClear
        procedure :: FMM_DataClear => fmm_wrapper_FMM_DataClear
		procedure :: FMM_UpdateTree => fmm_wrapper_FMM_UpdateTree
        procedure :: FMM_Evaluate => fmm_wrapper_FMM_Evaluate
	end type fmm_wrapper

    ! This function will act as the constructor for fmm_wrapper type
    interface fmm_wrapper
        procedure create_fmm_wrapper
    end interface

    contains 

	! -------------------------------------------------------------------
	! Call constructor.
	!
	! Inputs:
	! mult_order: (integer) control the accuracy of the periodic FMM
    ! max_pts: (integer) maximum number of points per leaf box
    ! init_depth: (integer) ???
    ! pbc: (integer) Periodic Boundary Conditions.
    !      0 = None (unbounded)
    !      1 = PXYZ (periodic in X, Y and Z)
    !      2 = PX   (periodic in X)
    !      3 = PY
    !      4 = PZ
    !      5 = PXY  (periodic in X and Y)
    !      6 = PXZ 
    !      7 = PYZ
	!
	! Output:
	! fmm = (fmm_wrapper) FMM object.
	! -------------------------------------------------------------------
    function create_fmm_wrapper(mult_order, max_pts, init_depth, pbc)
	    use iso_c_binding
        implicit none
        type(fmm_wrapper) :: create_fmm_wrapper
        integer(c_int), intent(in) :: mult_order
        integer(c_int), intent(in) :: max_pts
        integer(c_int), intent(in) :: init_depth
        integer(c_int), intent(in) :: pbc
        create_fmm_wrapper%ptr = create_fmm_wrapper_c(mult_order, max_pts, init_depth, pbc)
		write(*,*) "FROM FMMWrapper_mod.f90 ===== create_fmm_wrapper"
	end function create_fmm_wrapper

	! -------------------------------------------------------------------
    ! Destructor
	! -------------------------------------------------------------------
    subroutine delete_fmm_wrapper(this)
        implicit none
        type(fmm_wrapper) :: this
        call delete_fmm_wrapper_c(this%ptr)
	end subroutine delete_fmm_wrapper

	! -------------------------------------------------------------------
    ! Destructor
	! -------------------------------------------------------------------
    subroutine delete_fmm_wrapper_polymorph(this)
        implicit none
        class(fmm_wrapper) :: this
        call delete_fmm_wrapper_c(this%ptr)
    end subroutine delete_fmm_wrapper_polymorph
                     
	! -------------------------------------------------------------------
    ! Set box with corners xlow, xhigh, ylow, yhigh, zlow and zhigh.
    ! All sources and targets should be inside the box even if the
    ! FMM is periodic.
	! -------------------------------------------------------------------
    subroutine fmm_wrapper_FMM_SetBox(this, xlow, xhigh, ylow, yhigh, zlow, zhigh)
	  implicit none
      class(fmm_wrapper), intent(in) :: this
      real(dp), intent(in) :: xlow, xhigh, ylow, yhigh, zlow, zhigh
	  call fmm_wrapper_FMM_SetBox_c(this%ptr, xlow, xhigh, ylow, yhigh, zlow, zhigh)
    end subroutine fmm_wrapper_FMM_SetBox

	! -------------------------------------------------------------------
    ! Clear FMM tree
	! -------------------------------------------------------------------
    subroutine fmm_wrapper_FMM_TreeClear(this)
	  implicit none
      class(fmm_wrapper), intent(in) :: this
      call fmm_wrapper_FMM_TreeClear_c(this%ptr)
	end subroutine fmm_wrapper_FMM_TreeClear

	! -------------------------------------------------------------------
    ! Clear data
	! -------------------------------------------------------------------
    subroutine fmm_wrapper_FMM_DataClear(this)
	  implicit none
      class(fmm_wrapper), intent(in) :: this
      call fmm_wrapper_FMM_DataClear_c(this%ptr)
	end subroutine fmm_wrapper_FMM_DataClear

	! -------------------------------------------------------------------
    ! Update FMM tree.
    !
    ! Inputs:
    ! trg_coord: (real(dp)(:)) array with the target coordinates. 
    ! src_coord: (real(dp)(:)) array with the source coordinates.
    ! num_trg: (integer) number of targets
    ! num_src: (integer) number of sources
	! -------------------------------------------------------------------
    subroutine fmm_wrapper_FMM_UpdateTree(this, src_coord, trg_coord, num_src, num_trg)
	  implicit none
      class(fmm_wrapper), intent(in) :: this
	  real(dp), intent(in) :: src_coord(*), trg_coord(*)
      integer, intent(in) :: num_src, num_trg
      call fmm_wrapper_FMM_UpdateTree_c(this%ptr, trg_coord, src_coord, num_trg, num_src)
	end subroutine fmm_wrapper_FMM_UpdateTree

	! -------------------------------------------------------------------
    ! Evaluate the FMM 
    !
    ! Inputs:
    ! src_values: (real(dp)(:)) array with the sources values.
    ! num_trg: (integer) number of targets
    ! num_src: (integer) number of sources
    !
    ! Output:
    ! trg_values: (real(dp)(:)) array with the target values.
	! -------------------------------------------------------------------
    subroutine fmm_wrapper_FMM_Evaluate(this, trg_value, src_value, num_trg, num_src)
	  implicit none
      class(fmm_wrapper), intent(in) :: this
	  real(dp), intent(in) :: src_value(*)
      real(dp), intent(inout) :: trg_value(*)
      integer, intent(in) :: num_src, num_trg
      call fmm_wrapper_FMM_Evaluate_c(this%ptr, trg_value, src_value, num_trg, num_src)
	end subroutine fmm_wrapper_FMM_Evaluate
end module
