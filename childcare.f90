! Licensed under a Creative Commons
! Attribution 4.0 International License:
! https://creativecommons.org/licenses/by/4.0/!
!
! Code for solving "Market Work, Housework and Childcare: A Time Use
! Approach" by Emanuela Cardia and Paul Gomme, published in Review of
! Economic Dynamics.
!
! Code by:
!    Paul Gomme
!    Department of Economics
!    Concordia University
!    Montreal, Quebec, Canada
!
! To compile:
!     mpiifort model4mpi.f90 -o model4mpi
! To run:
!     mpiexec -n 90 ./model4mpi
!
! Parameters
!   beta:    discount factor
!   gamma:   coefficient of relative risk aversion
!   omega:   weight on leisure in utility function
!   psi:     weight on market consumption in consumption aggregator
!   xi:      CES parameter in consumption aggregator
!   eta:     weight on durables in home production function
!   zeta:    CES parameter in home production function
!   delta_d: depreciation rate of durables
!   delta_k: depreciation rate of market capital
!   nu:      weight on primary child care time in child care function
!   ---epsilon: weight on daycare in childcare function
!   varphi:  CES parameter in childcare function
!   ---rho:     CES parameter in childcare function
!   alpha:   capital's share of income (market sector)
!   theta_h: weight on housework time in secondary childcare
!   theta_ell: weight on leisure time in secondary childcare
!   N_C:     number of periods of positive child care
!   T:       number of periods of life

! p: price of daycare
!
! var%q: relative price of durables

module CG_COMMON
  use PRECISION
  integer :: NGRID
  integer, parameter               :: T = 10
  real(long), parameter            :: T_endow = 680d0 ! Minutes per day (one person)
  real(long), parameter            :: gamma     = 1d0
  real(long), parameter            :: varphi    = 6.0410150606981627d-01
  real(long), parameter            :: nu        = 5.4918780047410298d-01
  real(long), parameter            :: zeta      = 0.35d0
  real(long), parameter            :: xi        = 0.429d0
  real(long), parameter            :: phi_1965  = 0.6d0
  real(long), parameter            :: phi_2005  = 0.8d0
  ! 7.5 hours/day * 60 min/hr * 5/7 ~ 320
  real(long), parameter            :: n         = 320d0
  real(long), parameter            :: theta_h   = 0.8d0
  real(long), parameter            :: theta_ell = 0.6d0
  real(long), parameter            :: alpha     = 0.33d0
  real(long), parameter            :: nm_max = n
  real(long), parameter            :: nm_min = 0d0
  real(long)                       :: delta_k, delta_d
  real(long), parameter            :: d_share = .1014d0
  real(long), parameter            :: rr_target = 1.04d0
  real(long), parameter            :: nh_target = 161.74d0
  real(long), parameter            :: nm_target = 195.7d0
  real(long)                       :: p, r, w, ratio_target
  real(long), dimension(T)         :: beta_vec, eff_m_1965, eff_f_1965, eff_m_2000, eff_f_2000
  real(long), dimension(3,3,3,3)   :: weight, weight1965, weight_census
  real(long), dimension(4,3)       :: w_atus, w1965, re_w
  integer, parameter               :: N_C = 5
  integer                          :: i1, i2, i3, i4 ! type indices
  integer                          :: root=0, NPROC=1, rank=0
  logical                          :: f_debug = .false.
  character(len=60)                :: file_profile, file_reweight, file_junk

  type cg_var
     real(long)                    :: omega, beta, psi, eta, q=1d0, phi=phi_2005, p_frac=0.635d0, terminate
     real(long), dimension(T)      :: eff_m, eff_f
  end type cg_var

  type cg_rule
     real(long), dimension(T)      :: cm, nm, nh, ch, np, s
     real(long), dimension(T)      :: ell, ns, cc
     real(long), dimension(T+1)    :: d, a
  end type cg_rule

  integer, parameter               :: NVAR = 2*T + 8
  integer, parameter               :: NRULE = 9*T + 2*(T+1)

  type(cg_var)  :: var
  type(cg_rule), dimension(3,3,3,3) :: rule
  type(cg_rule) :: a

end module CG_COMMON
!=============================================================================
module CG_ROUTINES
  interface
     subroutine AGGREGATE_RULES(f_reweight, f_final)
       use PRECISION
       logical, intent(in) :: f_reweight, f_final
     end subroutine AGGREGATE_RULES
  end interface

  interface
     function MATCH_DATA(x_in)
       use PRECISION
       real(long), dimension(:), intent(in) :: x_in
       real(long), dimension(size(x_in)) :: MATCH_DATA
     end function MATCH_DATA
  end interface

  interface
     function EP_MATCH_DATA(x_in)
       use PRECISION
       real(long), dimension(:), intent(in) :: x_in
       real(long) :: EP_MATCH_DATA
     end function EP_MATCH_DATA
  end interface

  interface G
     function G_V(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in), dimension(:) :: np, nh, ell, s, cc
       real(long), dimension(size(np))      :: G_V
     end function G_V
     function G_S(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in) :: np, nh, ell, s, cc
       real(long)    :: G_S
     end function G_S
  end interface

  interface G1
     function G1_V(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in), dimension(:) :: np, nh, ell, s, cc
       real(long), dimension(size(np))      :: G1_V
     end function G1_V
     function G1_S(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in):: np, nh, ell, s, cc
       real(long)      :: G1_s
     end function G1_S
  end interface

  interface G2
     function G2_V(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in), dimension(:) :: np, nh, ell, s, cc
       real(long), dimension(size(np))      :: G2_V
     end function G2_V
     function G2_S(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in) :: np, nh, ell, s, cc
       real(long)      :: G2_S
     end function G2_S
  end interface

  interface G3
     function G3_V(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in), dimension(:) :: np, nh, ell, s, cc
       real(long), dimension(size(np))      :: G3_V
     end function G3_V
     function G3_S(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in):: np, nh, ell, s, cc
       real(long)      :: G3_s
     end function G3_S
  end interface

  interface G4
     function G4_V(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in), dimension(:) :: np, nh, ell, s, cc
       real(long), dimension(size(np))      :: G4_V
     end function G4_V
     function G4_S(np, nh, ell, s, cc)
       use PRECISION
       real(long), intent(in) :: np, nh, ell, s, cc
       real(long)      :: G4_S
     end function G4_S
  end interface

  interface CAGG
     function CAGG_S(cm, ch)
       use PRECISION
       real(long), intent(in) :: cm, ch
       real(long) :: CAGG_S
     end function CAGG_S
     function CAGG_V(cm, ch)
       use PRECISION
       real(long), intent(in), dimension(:) :: cm, ch
       real(long), dimension(size(cm))      :: CAGG_V
     end function CAGG_V
  end interface

  interface C1
     function C1_S(cm, ch)
       use PRECISION
       real(long), intent(in) :: cm, ch
       real(long) :: C1_S
     end function C1_S
     function C1_V(cm, ch)
       use PRECISION
       real(long), intent(in), dimension(:) :: cm, ch
       real(long), dimension(size(cm))      :: C1_V
     end function C1_V
  end interface C1

  interface C2
     function C2_S(cm, ch)
       use PRECISION
       real(long), intent(in) :: cm, ch
       real(long) :: C2_S
     end function C2_S
     function C2_V(cm, ch)
       use PRECISION
       real(long), intent(in), dimension(:) :: cm, ch
       real(long), dimension(size(cm))      :: C2_V
     end function C2_V
  end interface C2

  interface F
     function F(k, nm)
       use PRECISION
       real(long), intent(in) :: k, nm
       real(long) :: F
     end function F
  end interface F

  interface F1
     function F1(k, nm)
       use PRECISION
       real(long), intent(in) :: k, nm
       real(long) :: F1
     end function F1
  end interface F1

  interface F2
     function F2(k, nm)
       use PRECISION
       real(long), intent(in) :: k, nm
       real(long) :: F2
     end function F2
  end interface F2

  interface H
     function H_S(d, nh)
       use PRECISION
       real(long), intent(in) :: d, nh
       real(long) :: H_S
     end function H_S
     function H_V(d, nh)
       use PRECISION
       real(long), intent(in), dimension(:) :: d, nh
       real(long), dimension(size(d))       :: H_V
     end function H_V
  end interface H

  interface H1
     function H1_S(d, nh)
       use PRECISION
       real(long), intent(in) :: d, nh
       real(long) :: H1_S
     end function H1_S
     function H1_V(d, nh)
       use PRECISION
       real(long), intent(in), dimension(:) :: d, nh
       real(long), dimension(size(d))       :: H1_V
     end function H1_V
  end interface H1

  interface H2
     function H2_S(d, nh)
       use PRECISION
       real(long), intent(in) :: d, nh
       real(long) :: H2_S
     end function H2_S
     function H2_V(d, nh)
       use PRECISION
       real(long), intent(in), dimension(:) :: d, nh
       real(long), dimension(size(d))       :: H2_V
     end function H2_V
  end interface H2

  interface U
     function U_S(cm, ch, ell)
       use PRECISION
       real(long), intent(in) :: cm, ch, ell
       real(long) :: U_S
     end function U_S
     function U_V(cm, ch, ell)
       use PRECISION
       real(long), intent(in), dimension(:) :: cm, ch, ell
       real(long), dimension(size(cm))      :: U_V
     end function U_V
  end interface

  interface U1
     function U1_S(cm, ch, ell)
       use PRECISION
       real(long), intent(in) :: cm, ch, ell
       real(long) :: U1_S
     end function U1_S
     function U1_V(cm, ch, ell)
       use PRECISION
       real(long), intent(in), dimension(:) :: cm, ch, ell
       real(long), dimension(size(cm))      :: U1_V
     end function U1_V
  end interface U1

  interface U2
     function U2_S(cm, ch, ell)
       use PRECISION
       real(long), intent(in) :: cm, ch, ell
       real(long) :: U2_S
     end function U2_S
     function U2_V(cm, ch, ell)
       use PRECISION
       real(long), intent(in), dimension(:) :: cm, ch, ell
       real(long), dimension(size(cm))      :: U2_V
     end function U2_V
  end interface U2

  interface U3
     function U3_S(cm, ch, ell)
       use PRECISION
       real(long), intent(in) :: cm, ch, ell
       real(long) :: U3_S
     end function U3_S
     function U3_V(cm, ch, ell)
       use PRECISION
       real(long), intent(in), dimension(:) :: cm, ch, ell
       real(long), dimension(size(cm))      :: U3_V
     end function U3_V
  end interface U3
end module CG_ROUTINES
!=============================================================================
module TOOLS
  interface AGG
     pure function AGG_V(x, alpha, sigma)
       use PRECISION
       real(long), intent(in), dimension(:) :: x, alpha
       real(long), intent(in) :: sigma
       real(long) :: AGG_V
     end function AGG_V
     pure function AGG_M(x, alpha, sigma)
       use PRECISION
       real(long), intent(in), dimension(:,:) :: x
       real(long), intent(in), dimension(:) :: alpha
       real(long), intent(in) :: sigma
       real(long), dimension(size(x,1)) :: AGG_M
     end function AGG_M
     pure function AGG_MM(x, alpha, sigma)
       use PRECISION
       real(long), intent(in), dimension(:,:) :: x
       real(long), intent(in), dimension(:,:) :: alpha
       real(long), intent(in) :: sigma
       real(long), dimension(size(x,1)) :: AGG_MM
     end function AGG_MM
  end interface

  interface DAGG
     pure function DAGG_V(x, alpha, sigma, ix)
       use PRECISION
       real(long), intent(in), dimension(:) :: x, alpha
       real(long), intent(in) :: sigma
       integer, intent(in) :: ix
       real(long) :: DAGG_V
     end function DAGG_V
     pure function DAGG_M(x, alpha, sigma, ix)
       use PRECISION
       real(long), intent(in), dimension(:,:) :: x
       real(long), intent(in), dimension(:) :: alpha
       real(long), intent(in) :: sigma
       integer, intent(in) :: ix
       real(long), dimension(size(x,1)) :: DAGG_M
     end function DAGG_M
     pure function DAGG_MM(x, alpha, sigma, ix)
       use PRECISION
       real(long), intent(in), dimension(:,:) :: x
       real(long), intent(in), dimension(:,:) :: alpha
       real(long), intent(in)  :: sigma
       integer, intent(in) :: ix
       real(long), dimension(size(x,1)) :: DAGG_MM
     end function DAGG_MM
  end interface

  interface
     recursive subroutine SECANT(FUNC, xx, ff, tolf, tol, maxit, pctptb, ptb)
       use PRECISION
       real(long), dimension(:), intent(inout) :: xx
       real(long), dimension(:), intent(out)   :: ff
       real(long), intent(in)                  :: tolf
       real(long), intent(in), optional        :: tol
       integer, intent(in), optional           :: maxit
       real(long), intent(in), optional        :: pctptb
       real(long), intent(in), optional        :: ptb

       interface
          function FUNC(x)
            use precision
            real(long), dimension(:), intent(in) :: x
            real(long), dimension(size(x)) :: FUNC
          end function FUNC
       end interface
     end subroutine SECANT
  end interface
end module TOOLS
!=============================================================================
program CARDIA_GOMME
  use CG_COMMON
  use CG_ROUTINES, only : MATCH_DATA, EP_MATCH_DATA
  use TOOLS, only : SECANT
  use MPI
  implicit none
  integer :: ierr, tag=10
  integer, dimension(MPI_STATUS_SIZE) :: status
  real(long), dimension(4) :: guess, feval
  real(long) :: junk

  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, NPROC, ierr)

  if (rank == root) print *, 'Number of processors:', NPROC
  call INITIALIZE
  open(unit=86, file='max.output', status='unknown')
  if (rank == root) then
     file_profile  = 'profile-atus.txt'
     file_junk     = 'junk.txt'
     file_reweight = 'profile-reweight-atus.txt'

     weight    = weight_census
     re_w      = w_atus
     var%eff_m = eff_m_2000
     var%eff_f = eff_f_2000

     var%q         = 1d0
     var%omega     = 0.3037d0
     var%beta      = 0.8606d0
     var%psi       = 0.7143d0
     var%eta       = 0.5480d0
     var%terminate = 1d0

     guess = [ log(var%omega), log(var%beta), log(var%psi)-log(1d0-var%psi), log(var%eta)-log(1d0-var%eta) ]
     call SECANT(MATCH_DATA, guess, feval, 1d-4)
     feval = MATCH_DATA(guess)
     call PRINT_RESULTS
     call PRINT_RULES

     print *
     print *, 'Higher price of durables.'
     var%q         = 2.8289d0

     feval         = MATCH_DATA(guess)

     file_profile  = 'profile-atus-dur.txt'
     file_junk     = 'junk-atus-dur.txt'
     file_reweight = 'profile-reweight-atus-dur.txt'

     call PRINT_RESULTS
     call PRINT_RULES
     var%q         = 1d0

     print *
     print *, '1965 Wage profile.'
     var%eff_m     = eff_m_1965
     var%eff_f     = eff_f_1965

     feval         = MATCH_DATA(guess)

     file_profile  = 'profile-atus-1965w.txt'
     file_junk     = 'junk-atus-1965w.txt'
     file_reweight = 'profile-reweight-atus-1965w.txt'

     call PRINT_RESULTS
     call PRINT_RULES
     var%eff_m     = eff_m_2000
     var%eff_f     = eff_f_2000

     print *
     print *, 'Flat Wage profile.'
     var%eff_m     = 1d0
     var%eff_f     = phi_2005

     feval         = MATCH_DATA(guess)

     file_profile  = 'profile-atus-flat-w.txt'
     file_junk     = 'junk-atus-flat-w.txt'
     file_reweight = 'profile-reweight-atus-flat-w.txt'

     call PRINT_RESULTS
     call PRINT_RULES
     var%eff_m     = eff_m_2000
     var%eff_f     = eff_f_2000

     print *
     print *, '1965 Fertility profile.'
     weight        = weight1965
     re_w          = w1965

     feval         = MATCH_DATA(guess)

     file_profile  = 'profile-atus-fertility.txt'
     file_junk     = 'junk-atus-fertility.txt'
     file_reweight = 'profile-reweight-atus-fertility.txt'

     call PRINT_RESULTS
     call PRINT_RULES
     weight        = weight_census
     re_w          = w_atus

     print *
     print *, 'Lower price of daycare.'
     var%p_frac    = .75d0*var%p_frac

     feval         = MATCH_DATA(guess)

     file_profile  = 'profile-atus-dc.txt'
     file_junk     = 'junk-atus-dc.txt'
     file_reweight = 'profile-reweight-atus-dc.txt'

     call PRINT_RESULTS
     call PRINT_RULES

     print *
     print *, '1965 run.'

     weight = weight1965
     re_w = w1965
     var%eff_m = eff_m_1965
     var%eff_f = eff_f_1965

     var%q = 2.8289d0
     var%phi = phi_1965
     var%p_frac = .635d0
     var%terminate = -1d0
     feval = MATCH_DATA(guess)

     file_profile = 'profile-1965.txt'
     file_junk = 'junk-1965.txt'
     file_reweight = 'profile-reweight-1965.txt'

     call PRINT_RESULTS
     call PRINT_RULES
  else
     call SOLVE
  endif
  close(86)
  call MPI_FINALIZE(ierr)

end program CARDIA_GOMME
!=============================================================================
subroutine INITIALIZE
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : F1, F2
  use NR_RAN, only : RAN_SEED
  implicit none
  real(long), dimension(:,:), allocatable :: x_mat
  real(long), dimension(3,3) :: cc_atus
  real(long), dimension(T) :: age = [20.5d0, 26.5d0, 32.5d0, 38.5d0, 44.5d0, 50.5d0, 56.5d0, 62.5d0, 68.5d0, 74.5d0]
  real(long), dimension(T,3) :: Xmat = 1d0
  real(long), dimension(T,5) :: XXmat = 1d0
  real(long) :: temp
  integer :: iage
  integer, dimension(3,3,3,3,6) :: n_child

  if (f_debug) print *, ">>> INITIALIZE"

  call RANDOM_SEED
  call RANDOM_NUMBER(temp)
  call RAN_SEED(INT(100000d0*temp))

  ! 1990 Census
  weight_census(1,1,1,1) = 1.496745754995190D-01
  weight_census(1,1,1,2) = 1.047152923646820D-02
  weight_census(1,1,2,1) = 2.513373963970490D-02
  weight_census(1,1,1,3) = 3.269766046169920D-03
  weight_census(1,1,2,2) = 9.405751063191330D-03
  weight_census(1,2,1,1) = 4.136874890059290D-02
  weight_census(1,1,3,1) = 1.764225034404970D-02
  weight_census(1,1,2,3) = 9.209151205984910D-04
  weight_census(1,2,1,2) = 3.600881595149160D-03
  weight_census(1,1,3,2) = 2.824829527229080D-03
  weight_census(1,2,2,1) = 4.364516829982510D-02
  weight_census(2,1,1,1) = 8.279958196661940D-02
  weight_census(1,2,1,3) = 2.897261053568290D-04
  weight_census(1,1,3,3) = 3.518102707904350D-04
  weight_census(1,2,2,2) = 3.373239655225930D-03
  weight_census(1,3,1,1) = 5.097109982099070D-02
  weight_census(1,2,3,1) = 7.594962904711150D-03
  weight_census(2,1,1,2) = 3.435323820659540D-03
  weight_census(2,1,2,1) = 1.607979884730400D-02
  weight_census(1,2,2,3) = 1.241683308672120D-04
  weight_census(1,3,1,2) = 1.965998572064200D-03
  weight_census(1,2,3,2) = 1.055430812371300D-03
  weight_census(1,3,2,1) = 1.642126175718880D-02
  weight_census(2,1,1,3) = 5.587574889024550D-04
  weight_census(2,1,2,2) = 1.862524963008180D-03
  weight_census(2,2,1,1) = 9.726519251264960D-02
  weight_census(2,1,3,1) = 3.518102707904350D-03
  weight_census(1,3,1,3) = 1.655577744896160D-04
  weight_census(1,2,3,3) = 1.138209699616110D-04
  weight_census(1,3,2,2) = 1.438283165878540D-03
  weight_census(1,3,3,1) = 2.886913692662690D-03
  weight_census(2,1,2,3) = 1.759051353952170D-04
  weight_census(2,2,1,2) = 2.493713978249850D-03
  weight_census(2,1,3,2) = 4.759786016576470D-04
  weight_census(2,2,2,1) = 1.952547002886910D-02
  weight_census(1,3,2,3) = 9.312624815040920D-05
  weight_census(1,3,3,2) = 4.138944362240410D-04
  weight_census(3,1,1,1) = 1.946855954388830D-01
  weight_census(2,2,1,3) = 2.690313835456270D-04
  weight_census(2,1,3,3) = 1.034736090560100D-05
  weight_census(2,2,2,2) = 1.272725391388930D-03
  weight_census(2,3,1,1) = 2.465776103804720D-02
  weight_census(2,2,3,1) = 2.659271752739460D-03
  weight_census(1,3,3,3) = 1.965998572064190D-04
  weight_census(3,1,1,2) = 4.356238941258030D-03
  weight_census(3,1,2,1) = 1.854247074283700D-02
  weight_census(2,2,2,3) = 1.138209699616110D-04
  weight_census(2,3,1,2) = 7.967467897312790D-04
  weight_census(2,2,3,2) = 3.000734662624300D-04
  weight_census(2,3,2,1) = 6.156679738832610D-03
  weight_census(3,1,1,3) = 4.552838798464450D-04
  weight_census(3,1,2,2) = 1.490019970406550D-03
  weight_census(3,2,1,1) = 6.979294930827890D-02
  weight_census(3,1,3,1) = 3.611228956054760D-03
  weight_census(2,3,1,3) = 5.173680452800510D-05
  weight_census(2,2,3,3) = 7.243152633920720D-05
  weight_census(2,3,2,2) = 6.104942934304600D-04
  weight_census(2,3,3,1) = 1.479672609500950D-03
  weight_census(3,1,2,3) = 1.552104135840150D-04
  weight_census(3,2,1,2) = 2.535103421872250D-03
  weight_census(3,1,3,2) = 4.966733234688490D-04
  weight_census(3,2,2,1) = 1.246856989124920D-02
  weight_census(2,3,2,3) = 1.138209699616110D-04
  weight_census(2,3,3,2) = 4.035470753184400D-04
  weight_census(3,2,1,3) = 3.000734662624300D-04
  weight_census(3,1,3,3) = 1.241683308672120D-04
  weight_census(3,2,2,2) = 1.272725391388930D-03
  weight_census(3,3,1,1) = 1.557277816292950D-02
  weight_census(3,2,3,1) = 2.379893008288240D-03
  weight_census(2,3,3,3) = 2.069472181120210D-04
  weight_census(3,2,2,3) = 1.552104135840150D-04
  weight_census(3,3,1,2) = 6.829258197696680D-04
  weight_census(3,2,3,2) = 4.035470753184400D-04
  weight_census(3,3,2,1) = 4.677007129331660D-03
  weight_census(3,3,1,3) = 9.312624815040920D-05
  weight_census(3,2,3,3) = 9.312624815040920D-05
  weight_census(3,3,2,2) = 6.518837370528650D-04
  weight_census(3,3,3,1) = 1.469325248595350D-03
  weight_census(3,3,2,3) = 9.312624815040920D-05
  weight_census(3,3,3,2) = 3.725049926016370D-04
  weight_census(3,3,3,3) = 2.897261053568290D-04
 
  ! From ATUS data

  ! Fraction of women 18-23 with 0, 1 2+ children under 6

  w_atus(1,:) = [0.401640617589d0, 0.399120709686d0, 0.199238672725d0]

  ! Fraction of women 24-29 with 0, 1 2+ children under 6

  w_atus(2,:) = [0.418531971484d0, 0.354036089976d0, 0.227431938540d0]

  ! Fraction of women 30-35 with 0, 1 2+ children under 6

  w_atus(3,:) = [0.409567020273d0, 0.369490078773d0, 0.220942900954d0]

  ! Fraction of women 36-41 with 0, 1 2+ children under 6

  w_atus(4,:) = [0.636950155462d0, 0.254273029882d0, 0.108776814656d0]

  ! From 1965 TUS data

  w1965(1,:) = [ 0.4680851d0, 0.2872340d0, 0.2446809d0]
  w1965(2,:) = [ 0.1751825d0, 0.2773723d0, 0.5474453d0]
  w1965(3,:) = [ 0.3469388d0, 0.2380952d0, 0.4149660d0]
  w1965(4,:) = [ 0.6307692d0, 0.2230769d0, 0.1461538d0]

  ! From 1960 Census

  weight1965(1,1,1,1)=1.37425849361855D-01
  weight1965(1,1,1,2)=1.11450656120798D-02
  weight1965(1,1,2,1)=2.59751932410570D-02
  weight1965(1,1,1,3)=3.86482113967284D-03
  weight1965(1,1,2,2)=1.04260291209779D-02
  weight1965(1,2,1,1)=4.72766492899515D-02
  weight1965(1,1,3,1)=1.44706093834262D-02
  weight1965(1,1,2,3)=1.70771166636707D-03
  weight1965(1,2,1,2)=6.92072622685601D-03
  weight1965(1,1,3,2)=5.57253280603991D-03
  weight1965(1,2,2,1)=4.19737551680748D-02
  weight1965(2,1,1,1)=8.53855833183534D-02
  weight1965(1,2,1,3)=5.39277368326443D-04
  weight1965(1,1,3,3)=1.34819342081611D-03
  weight1965(1,2,2,2)=8.08916052489664D-03
  weight1965(1,3,1,1)=3.64012223620349D-02
  weight1965(1,2,3,1)=1.12349451734676D-02
  weight1965(2,1,1,2)=4.67373719216250D-03
  weight1965(2,1,2,1)=1.74366349092216D-02
  weight1965(1,2,2,3)=1.25831385942837D-03
  weight1965(1,3,1,2)=4.13445982383606D-03
  weight1965(1,2,3,2)=4.31421894661154D-03
  weight1965(1,3,2,1)=1.87848283300378D-02
  weight1965(2,1,1,3)=4.49397806938702D-04
  weight1965(2,1,2,2)=2.69638684163221D-03
  weight1965(2,2,1,1)=7.44202768290491D-02
  weight1965(2,1,3,1)=4.13445982383606D-03
  weight1965(1,3,1,3)=1.07855473665289D-03
  weight1965(1,2,3,3)=1.34819342081611D-03
  weight1965(1,3,2,2)=5.48265324465217D-03
  weight1965(1,3,3,1)=5.93205105159087D-03
  weight1965(2,1,2,3)=4.49397806938702D-04
  weight1965(2,2,1,2)=6.47132841991731D-03
  weight1965(2,1,3,2)=1.79759122775481D-03
  weight1965(2,2,2,1)=2.63347114866079D-02
  weight1965(1,3,2,3)=3.59518245550962D-04
  weight1965(1,3,3,2)=2.78626640301995D-03
  weight1965(3,1,1,1)=1.09203667086105D-01
  weight1965(2,2,1,3)=1.16843429804063D-03
  weight1965(2,1,3,3)=7.19036491101923D-04
  weight1965(2,2,2,2)=5.12313499910120D-03
  weight1965(2,3,1,1)=2.43573611360777D-02
  weight1965(2,2,3,1)=7.19036491101923D-03
  weight1965(1,3,3,3)=1.43807298220385D-03
  weight1965(3,1,1,2)=3.68506201689736D-03
  weight1965(3,1,2,1)=1.55491641200791D-02
  weight1965(2,2,2,3)=9.88675175265145D-04
  weight1965(2,3,1,2)=2.96602552579543D-03
  weight1965(2,2,3,2)=1.79759122775481D-03
  weight1965(2,3,2,1)=1.10551860506921D-02
  weight1965(3,1,1,3)=8.08916052489664D-04
  weight1965(3,1,2,2)=3.86482113967284D-03
  weight1965(3,2,1,1)=5.07819521840733D-02
  weight1965(3,1,3,1)=5.03325543771346D-03
  weight1965(2,3,1,3)=3.59518245550962D-04
  weight1965(2,2,3,3)=8.08916052489664D-04
  weight1965(2,3,2,2)=2.69638684163221D-03
  weight1965(2,3,3,1)=6.20168973575409D-03
  weight1965(3,1,2,3)=4.49397806938702D-04
  weight1965(3,2,1,2)=5.93205105159087D-03
  weight1965(3,1,3,2)=1.52795254359159D-03
  weight1965(3,2,2,1)=1.94139852597519D-02
  weight1965(2,3,2,3)=1.25831385942837D-03
  weight1965(2,3,3,2)=2.60650728024447D-03
  weight1965(3,2,1,3)=5.39277368326443D-04
  weight1965(3,1,3,3)=8.08916052489664D-04
  weight1965(3,2,2,2)=5.03325543771346D-03
  weight1965(3,3,1,1)=1.85151896458745D-02
  weight1965(3,2,3,1)=6.20168973575409D-03
  weight1965(2,3,3,3)=1.25831385942837D-03
  weight1965(3,2,2,3)=7.19036491101923D-04
  weight1965(3,3,1,2)=3.59518245550962D-03
  weight1965(3,2,3,2)=2.42674815746899D-03
  weight1965(3,3,2,1)=1.26730181556714D-02
  weight1965(3,3,1,3)=6.29156929714183D-04
  weight1965(3,2,3,3)=9.88675175265145D-04
  weight1965(3,3,2,2)=4.67373719216250D-03
  weight1965(3,3,3,1)=8.26891964767212D-03
  weight1965(3,3,2,3)=1.61783210497933D-03
  weight1965(3,3,3,2)=5.93205105159087D-03
  weight1965(3,3,3,3)=5.03325543771346D-03

51 format('weight(',i,',',i,',',i,',',i,')=',f,';')

  XXmat(:,2) = age
  XXmat(:,3) = age**2
  XXmat(:,4) = age**3
  XXmat(:,5) = age**4
  
  eff_m_2000 = matmul(XXmat,[1.564d+00,1.229d-01,-4.350d-03,6.762d-05,-4.057d-07])
  eff_m_2000 = dble(T) * eff_m_2000 / sum(eff_m_2000)
  eff_f_2000 = phi_2005 * eff_m_2000
  eff_m_1965 = eff_m_2000
  eff_f_1965 = phi_1965 * eff_m_1965

  if (rank == root) then
     print *, 'F 2000', eff_f_2000
     print *, 'M 2000', eff_m_2000
     print *, sum(eff_f_2000) / sum(eff_m_2000)
     print *, 'F 1965', eff_f_1965
     print *, 'M 1965', eff_m_1965
     print *, sum(eff_f_1965) / sum(eff_m_1965)
  end if

  delta_k = 0.07d0
  delta_d = 0.2d0

  delta_k  = 1d0 - (1d0 - delta_k)**(60d0/dble(T))
  delta_d  = 1d0 !- (1d0 - delta_d)**(60d0/dble(T))

  ratio_target = ((rr_target**(60d0/dble(T)) - 1d0 + delta_k)/alpha)**(1d0/(alpha-1d0))

  r = F1(ratio_target,1d0) + 1d0 - delta_k
  w = F2(ratio_target,1d0)
  p = var%p_frac * w * var%phi

  cc_atus(:,:) = 0d0
  cc_atus(1,2) = 1.7672578230737184d+02
  cc_atus(1,3) = 1.9620960669229186d+02

  cc_atus(2,1) = 2.4512562572890863d+02
  cc_atus(2,2) = 2.3967692871281758d+02
  cc_atus(2,3) = 2.4989331549034770d+02

  cc_atus(3,1) = 2.6838310929630853d+02
  cc_atus(3,2) = 2.7340610846696165d+02
  cc_atus(3,3) = 2.7447051249405405d+02

  do i1 = 0, 2
  do i2 = 0, 2
  do i3 = 0, 2
  do i4 = 0, 2
     n_child(i1+1,i2+1,i3+1,i4+1,:) = (/ i1, i2, i3, i4, 0, 0 /)
  end do
  end do
  end do
  end do

  do i1 = 1, 3
  do i2 = 1, 3
  do i3 = 1, 3
  do i4 = 1, 3
     rule(i1,i2,i3,i4)%cc(1) = cc_atus(i1,1)
     rule(i1,i2,i3,i4)%cc(2) = cc_atus(i2,i1)
     rule(i1,i2,i3,i4)%cc(3) = cc_atus(i3,i2)
     rule(i1,i2,i3,i4)%cc(4) = cc_atus(i4,i3)
     rule(i1,i2,i3,i4)%cc(5) = cc_atus(1,i4)
  end do
  end do
  end do
  end do

  do i1 = 1, 3
  do i2 = 1, 3
  do i3 = 1, 3
  do i4 = 1, 3
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%cm)
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%ch)
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%nm)
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%nh)
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%np)
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%ell)
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%a)
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%d)
     call RANDOM_NUMBER(rule(i1,i2,i3,i4)%s)

     rule(i1,i2,i3,i4)%nm = .37d0*rule(i1,i2,i3,i4)%nm*T_endow
     rule(i1,i2,i3,i4)%nh = nh_target
     rule(i1,i2,i3,i4)%np = .1d0*rule(i1,i2,i3,i4)%np*T_endow
     rule(i1,i2,i3,i4)%ell = .6d0*rule(i1,i2,i3,i4)%ell*T_endow
     rule(i1,i2,i3,i4)%s = T_endow*rule(i1,i2,i3,i4)%s
  end do
  end do
  end do
  end do

  if (f_debug .and. rank==root) call CHECK_PARTIALS

  do i1 = 1, 3
  do i2 = 1, 3
  do i3 = 1, 3
  do i4 = 1, 3
     rule(i1,i2,i3,i4)%nm = 0d0
     rule(i1,i2,i3,i4)%nh = 0d0
     rule(i1,i2,i3,i4)%np = 0d0
     rule(i1,i2,i3,i4)%ell = 0d0
     rule(i1,i2,i3,i4)%s = 0d0
     rule(i1,i2,i3,i4)%a = 0d0
     rule(i1,i2,i3,i4)%d = 0d0
     rule(i1,i2,i3,i4)%cm = 0d0
     rule(i1,i2,i3,i4)%ch = 0d0
     
     rule(i1,i2,i3,i4)%a = 1d0
     rule(i1,i2,i3,i4)%a(1) = 0d0
     rule(i1,i2,i3,i4)%a(T+1) = 0d0
     rule(i1,i2,i3,i4)%d = .001d0
     rule(i1,i2,i3,i4)%d(1) = 0d0
  end do
  end do
  end do
  end do

  if (f_debug) print *, "<<< INITIALIZE"
end subroutine INITIALIZE
!=============================================================================
subroutine AGGREGATE_RULES(f_reweight, f_final)
  use PRECISION
  use CG_COMMON
  implicit none
  logical, intent(in) :: f_reweight, f_final
  type(cg_rule), dimension(3,3,3,3) :: w_rule
  integer :: iage


  do i1 = 1, 3
  do i2 = 1, 3
  do i3 = 1, 3
  do i4 = 1, 3
     rule(i1,i2,i3,i4)%ns = 0d0
     rule(i1,i2,i3,i4)%ns(1:N_C) = theta_ell*rule(i1,i2,i3,i4)%ell(1:N_C) &
          & + theta_h*rule(i1,i2,i3,i4)%nh(1:N_C)
  end do
  end do
  end do
  end do

  a%cm = 0d0
  a%nm = 0d0
  a%nh = 0d0
  a%ch = 0d0
  a%np = 0d0
  a%s = 0d0
  a%ell = 0d0
  a%ns = 0d0
  a%cc = 0d0
  a%d = 0d0
  a%a = 0d0

  do iage = 1, T
     w_rule%cm(iage)  = rule%cm(iage)  * weight
     w_rule%nm(iage)  = rule%nm(iage)  * weight
     w_rule%nh(iage)  = rule%nh(iage)  * weight
     w_rule%ch(iage)  = rule%ch(iage)  * weight
     w_rule%np(iage)  = rule%np(iage)  * weight
     w_rule%s(iage)   = rule%s(iage)   * weight
     w_rule%ell(iage) = rule%ell(iage) * weight
     w_rule%ns(iage)  = rule%ns(iage)  * weight
     w_rule%cc(iage)  = rule%cc(iage)  * weight
     w_rule%d(iage)   = rule%d(iage)   * weight
     w_rule%a(iage)   = rule%a(iage)   * weight
  end do

  if (f_final) then
     ! '18-23'
     do i1 = 1, 3
        a%cm(1)  = a%cm(1)  + sum(w_rule(i1,:,:,:)%cm(1)  * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%nm(1)  = a%nm(1)  + sum(w_rule(i1,:,:,:)%nm(1)  * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%nh(1)  = a%nh(1)  + sum(w_rule(i1,:,:,:)%nh(1)  * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%ch(1)  = a%ch(1)  + sum(w_rule(i1,:,:,:)%ch(1)  * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%np(1)  = a%np(1)  + sum(w_rule(i1,:,:,:)%np(1)  * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%s(1)   = a%s(1)   + sum(w_rule(i1,:,:,:)%s(1)   * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%ell(1) = a%ell(1) + sum(w_rule(i1,:,:,:)%ell(1) * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%ns(1)  = a%ns(1)  + sum(w_rule(i1,:,:,:)%ns(1)  * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%cc(1)  = a%cc(1)  + sum(w_rule(i1,:,:,:)%cc(1)  * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%d(1)   = a%d(1)   + sum(w_rule(i1,:,:,:)%d(1)   * re_w(1,i1) / sum(weight(i1,:,:,:)))
        a%a(1)   = a%a(1)   + sum(w_rule(i1,:,:,:)%a(1)   * re_w(1,i1) / sum(weight(i1,:,:,:)))
     end do

     ! '24-29'
     do i2 = 1, 3
        a%cm(2)  = a%cm(2)  + sum(w_rule(:,i2,:,:)%cm(2)  * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%nm(2)  = a%nm(2)  + sum(w_rule(:,i2,:,:)%nm(2)  * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%nh(2)  = a%nh(2)  + sum(w_rule(:,i2,:,:)%nh(2)  * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%ch(2)  = a%ch(2)  + sum(w_rule(:,i2,:,:)%ch(2)  * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%np(2)  = a%np(2)  + sum(w_rule(:,i2,:,:)%np(2)  * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%s(2)   = a%s(2)   + sum(w_rule(:,i2,:,:)%s(2)   * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%ell(2) = a%ell(2) + sum(w_rule(:,i2,:,:)%ell(2) * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%ns(2)  = a%ns(2)  + sum(w_rule(:,i2,:,:)%ns(2)  * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%cc(2)  = a%cc(2)  + sum(w_rule(:,i2,:,:)%cc(2)  * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%d(2)   = a%d(2)   + sum(w_rule(:,i2,:,:)%d(2)   * re_w(2,i2) / sum(weight(:,i2,:,:)))
        a%a(2)   = a%a(2)   + sum(w_rule(:,i2,:,:)%a(2)   * re_w(2,i2) / sum(weight(:,i2,:,:)))
     end do
     
     ! '30-35'
     do i3 = 1, 3
        a%cm(3)  = a%cm(3)  + sum(w_rule(:,:,i3,:)%cm(3)  * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%nm(3)  = a%nm(3)  + sum(w_rule(:,:,i3,:)%nm(3)  * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%nh(3)  = a%nh(3)  + sum(w_rule(:,:,i3,:)%nh(3)  * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%ch(3)  = a%ch(3)  + sum(w_rule(:,:,i3,:)%ch(3)  * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%np(3)  = a%np(3)  + sum(w_rule(:,:,i3,:)%np(3)  * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%s(3)   = a%s(3)   + sum(w_rule(:,:,i3,:)%s(3)   * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%ell(3) = a%ell(3) + sum(w_rule(:,:,i3,:)%ell(3) * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%ns(3)  = a%ns(3)  + sum(w_rule(:,:,i3,:)%ns(3)  * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%cc(3)  = a%cc(3)  + sum(w_rule(:,:,i3,:)%cc(3)  * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%d(3)   = a%d(3)   + sum(w_rule(:,:,i3,:)%d(3)   * re_w(3,i3) / sum(weight(:,:,i3,:)))
        a%a(3)   = a%a(3)   + sum(w_rule(:,:,i3,:)%a(3)   * re_w(3,i3) / sum(weight(:,:,i3,:)))
     end do

     do iage = 4, T
     do i3 = 1, 3
        a%cm(iage)  = a%cm(iage)  + sum(w_rule(:,:,:,i3)%cm(iage)  * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%nm(iage)  = a%nm(iage)  + sum(w_rule(:,:,:,i3)%nm(iage)  * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%nh(iage)  = a%nh(iage)  + sum(w_rule(:,:,:,i3)%nh(iage)  * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%ch(iage)  = a%ch(iage)  + sum(w_rule(:,:,:,i3)%ch(iage)  * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%np(iage)  = a%np(iage)  + sum(w_rule(:,:,:,i3)%np(iage)  * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%s(iage)   = a%s(iage)   + sum(w_rule(:,:,:,i3)%s(iage)   * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%ell(iage) = a%ell(iage) + sum(w_rule(:,:,:,i3)%ell(iage) * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%ns(iage)  = a%ns(iage)  + sum(w_rule(:,:,:,i3)%ns(iage)  * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%cc(iage)  = a%cc(iage)  + sum(w_rule(:,:,:,i3)%cc(iage)  * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%d(iage)   = a%d(iage)   + sum(w_rule(:,:,:,i3)%d(iage)   * re_w(4,i3) / sum(weight(:,:,:,i3)))
        a%a(iage)   = a%a(iage)   + sum(w_rule(:,:,:,i3)%a(iage)   * re_w(4,i3) / sum(weight(:,:,:,i3)))
     end do
     end do
  else
     do iage = 1, T
        a%cm(iage)  = a%cm(iage)  + sum(w_rule%cm(iage))
        a%nm(iage)  = a%nm(iage)  + sum(w_rule%nm(iage))
        a%nh(iage)  = a%nh(iage)  + sum(w_rule%nh(iage))
        a%ch(iage)  = a%ch(iage)  + sum(w_rule%ch(iage))
        a%np(iage)  = a%np(iage)  + sum(w_rule%np(iage))
        a%s(iage)   = a%s(iage)   + sum(w_rule%s(iage))
        a%ell(iage) = a%ell(iage) + sum(w_rule%ell(iage))
        a%ns(iage)  = a%ns(iage)  + sum(w_rule%ns(iage))
        a%cc(iage)  = a%cc(iage)  + sum(w_rule%cc(iage))
        a%d(iage)   = a%d(iage)   + sum(w_rule%d(iage))
        a%a(iage)   = a%a(iage)   + sum(w_rule%a(iage))
     end do
  end if
  
end subroutine AGGREGATE_RULES
!=============================================================================
subroutine PRINT_RULES
  use PRECISION
  use CG_COMMON
  implicit none
  integer :: iage

  open(unit=81, file='rules.m', status='unknown')

  do iage = 1, T
  do i1 = 1, 3
  do i2 = 1, 3
  do i3 = 1, 3
  do i4 = 1, 3
     write(81,51) i1,i2,i3,i4,iage,rule(i1,i2,i3,i4)%nm(iage)
     write(81,52) i1,i2,i3,i4,iage,rule(i1,i2,i3,i4)%nh(iage)
     write(81,53) i1,i2,i3,i4,iage,rule(i1,i2,i3,i4)%cm(iage)
     write(81,54) i1,i2,i3,i4,iage,rule(i1,i2,i3,i4)%ch(iage)
  end do
  end do
  end do
  end do
  end do

  close(81)

51 format('nm(',i,',',i,',',i,',',i,',',i,')=',f,';')
52 format('nh(',i,',',i,',',i,',',i,',',i,')=',f,';')
53 format('cm(',i,',',i,',',i,',',i,',',i,')=',f,';')
54 format('ch(',i,',',i,',',i,',',i,',',i,')=',f,';')
end subroutine PRINT_RULES
!=============================================================================
subroutine PRINT_RESULTS
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : F, G, AGGREGATE_RULES
  implicit none
  real(long) :: agg_k, agg_n
  type(cg_rule), dimension(3,3,3,3) :: w_rule
  integer :: iage

  call AGGREGATE_RULES(.false., .false.)
  
  open(unit=25, file=file_profile, status='unknown')

  do iage = 1, T
     write(25,20) iage, a%cm(iage), a%a(iage), a%d(iage+1), &
          & a%nm(iage), a%nh(iage), a%np(iage), a%cc(iage), &
          & a%ell(iage), a%ns(iage), a%s(iage), &
          & ((((rule(i1,i2,i3,i4)%cm(iage), rule(i1,i2,i3,i4)%a(iage+1), &
          & rule(i1,i2,i3,i4)%d(iage+1), rule(i1,i2,i3,i4)%nm(iage), &
          & rule(i1,i2,i3,i4)%nh(iage), rule(i1,i2,i3,i4)%np(iage), &
          & rule(i1,i2,i3,i4)%cc(iage), rule(i1,i2,i3,i4)%ell(iage), &
          & rule(i1,i2,i3,i4)%ns(iage), rule(i1,i2,i3,i4)%s(iage), &
          & i1 = 1,3), i2 = 1,3), i3 = 1,3), i4 = 1,3)
  end do
  close(25)


  open(unit=45, file=file_junk, status='unknown')
  do i1 = 1,3
  do i2 = 1,3
  do i3 = 1,3
  do i4 = 1,3
     write(45,30) i1,i2,i3,i4,rule(i1,i2,i3,i4)%np(1:N_C),rule(i1,i2,i3,i4)%cc(1:N_C),weight(i1,i2,i3,i4)
  end do
  end do
  end do
  end do

  close(45)

  call AGGREGATE_RULES(.true., .true.)
  
  open(unit=25, file=file_reweight, status='unknown')

  do iage = 1, T
     write(25,20) iage, a%cm(iage), a%a(iage), a%d(iage+1), &
          & a%nm(iage), a%nh(iage), a%np(iage), a%cc(iage), &
          & a%ell(iage), a%ns(iage), a%s(iage), &
          & ((((rule(i1,i2,i3,i4)%cm(iage), rule(i1,i2,i3,i4)%a(iage+1), &
          & rule(i1,i2,i3,i4)%d(iage+1), rule(i1,i2,i3,i4)%nm(iage), &
          & rule(i1,i2,i3,i4)%nh(iage), rule(i1,i2,i3,i4)%np(iage), &
          & rule(i1,i2,i3,i4)%cc(iage), rule(i1,i2,i3,i4)%ell(iage), &
          & rule(i1,i2,i3,i4)%ns(iage), rule(i1,i2,i3,i4)%s(iage), &
          & i1 = 1,3), i2 = 1,3), i3 = 1,3), i4 = 1,3)
  end do
  close(25)

  agg_n = dot_product(var%eff_f,a%nm) + n*sum(var%eff_m)
  agg_k = sum(a%a)
  print *, 'Durable share:', delta_d*sum(a%d) / F(agg_k,agg_n)
  print *, 'Market work:  ', sum(a%nm) / dble(T)
  print *, 'Market work (18-65):  ', sum(a%nm(1:(T-2))) / dble(T-2)
  print *, 'Housework:    ', sum(a%nh) / dble(T)
  print *, 'Leisure:    ', sum(a%ell) / dble(T)
  print *, 'K-N ratio:    ', agg_k/agg_n, ratio_target
  print *, 'Primary time (18-41): ', sum(a%np(1:4)) / 4d0
  print *, 'Primary time (18-47): ', sum(a%np(1:5)) / 5d0
  print *, 'Secondary time (18-47): ', sum(a%ns(1:5)) / 5d0

10 format(/' beta: ', t25, f16.12 / ' omega:', t25, f16.12 &
        & /' psi:  ', t25, f16.12 /' eta:  ', t25, f16.12 &
        & /' nu:  ', t25, f16.12 /' varphi:  ', t25, f16.12 /&
        & / t30, '1965', t48, '2006' &
        & /' K/L ratio:', t25, 2(1x,f18.10) &
        & /' D/Y ratio:', t25, 2(1x,f18.10) &
        & /' return to capital:', t25, 2(1x,f18.10) &
        & /' wage:', t25, 2(1x,f18.10) &
        & /' Output:', t25, 2(1x,f18.10) &
        & /' Capital:', t25, 2(1x,f18.10) &
        & /' Market Labor input:', t25, 2(1x,f18.10) &
        & /' Durables:', t25, 2(1x,f18.10) &
        & /' Durable-output ratio:', t25, 2(1x,f18.10) &
        & /' Capital-output ratio:', t25, 2(1x,f18.10) &
        & /' Price of Durables:', t25, 2(1x,f18.10) &
        & /' Price of Daycare:', t25, 2(1x,f18.10) &
        & /' Female market time:', t25, 2(1x,f18.10) &
        & /' -- (excl. ret.):', t25, 2(1x,f18.10) &
        & /' Home time: ', t25, 2(1x,f18.10) &
        & /' Average daycare: ', t25, 2(1x,f18.10)/)
11 format(' Childcare time: ', 99(t25,2(1x,f18.10)/))
12 format(' Daycare: ', 99(t25,2(1x,f18.10)/))
15 format(4(1x,f15.5))
20 format(1x, i5, 100000(1x,e22.14e3))
30 format(4(1x,i5), 100000(1x,e22.14e3))
40 format('\documentclass{article}' / &
        & '\usepackage{dcolumn}' / &
        & '\usepackage[margin=1in]{geometry}' / &
        & '\usepackage{booktabs}' / &
        & '\newcolumntype{d}[1]{D{.}{.}{#1}}' / &
        & '\begin{document}' / &
        & '\begin{table}[htbp]' / &
        & '\begin{center}' / &
        & '\caption{Benchmark Parameter Values}' / &
        & '\label{tab:param}' / &
        & '\begin{tabular}{c p{3in} d{2.4}} ' / &
        & '\toprule' / &
        & 'Parameter & Description & \multicolumn{1}{c}{Value} \\' / &
        & '\midrule' / &
        & '$\beta$ & Discount factor & ', f10.4, '\\' / &
        & '$\gamma$ & Coefficient of relative risk aversion & ', f10.4, '\\' / &
        & '$\omega$ & Weight on leisure in utility function & ', f10.4, '\\' / &
        & '$\psi$ & Weight on market consumption in consumption aggregator & ', f10.4, '\\' / &
        & '$\xi$ & CES parameter in consumption aggregator & ', f10.4, '\\' / &
        & '$\eta$ & Weight on durables in home production function & ', f10.4, '\\' / &
        & '$\zeta$ & CES parameter in home production function & ', f10.4, '\\' / &
        & '$\delta_d$ & Depreciation rate of durables & ', f10.4, '\\' / &
        & '$\delta_k$ & Depreciation rate of market capital & ', f10.4, '\\' / &
        & '$\nu$ & Weight on primary child care time in child care function & ', f10.4, '\\' / &
        & '$\varphi$ & CES parameter in child care function & ', f10.4, '\\' / &
        & '$\alpha$ & Capital''s share of income (market sector) & ', f10.4, '\\' / &
        & '$\theta_h$ & Weight on housework time in secondary childcare & ', f10.4, '\\' / &
        & '$\theta_ell$ & Weight on leisure time in secondary childcare & ', f10.4, '\\' / &
        & '$\phi_{1965}$ & Relative wage of women in 1965 & ', f10.4, '\\' / &
        & '$\phi_{2006}$ & Relative wage of women in 2006 & ', f10.4, '\\' / &
        & '$T$ & Number of model periods of life & ', i4, '\\' / &
        & ' & Length of a model period in years & ', i4, '\\' / &
        & '$TC_{min}$ & Model period in which child care starts & ', i4, '\\' / &
        & '$TC_{max}$ & Model period in which child care ends & ', i4, '\\' / &
        & '$N_C$ & Number of periods of child care & ', i4, '\\' / &
        & '\midrule' / &
        & ' & 1965 & 2006 \\' / &
        & /' K/L ratio: ', t25, 2(1x,'&',1x,f18.10), '\\' &
        & /' D/Y ratio:', t25, 2(1x,'&',1x,f18.10) , '\\' &
        & /' return to capital:', t25, 2(1x,'&',1x,f18.10) , '\\' &
        & /' wage:', t25, 2(1x,'&',1x,f18.10) , '\\' &
        & /' Output:', t25, 2(1x,'&',1x,f18.10) , '\\' &
        & /' Capital:', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Market Labor input:', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Durables:', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Durable-output ratio:', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Capital-output ratio:', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Price of Durables:', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Price of Daycare:', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Female market time:', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' -- (excl. ret.):', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Home time: ', t25, 2(1x,'&',1x,f18.10), '\\'  &
        & /' Average daycare: ', t25, 2(1x,'&',1x,f18.10), '\\' /&
        & '\bottomrule' / &
        & '\end{tabular}' / &
        & '\end{center}' / &
        & '\end{table}' / &
        & '\end{document}')
end subroutine PRINT_RESULTS
!=============================================================================
function MATCH_DATA(x_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : F, AGGREGATE_RULES
  use MPI
  implicit none
  real(long), dimension(:), intent(in) :: x_in
  real(long), dimension(size(x_in)) :: MATCH_DATA
  real(long) :: agg_k, agg_n
  integer :: iage

  var%omega = exp(x_in(1))
  var%beta = exp(x_in(2))
  var%psi = exp(x_in(3)) / (1d0 + exp(x_in(3)))
  var%eta = exp(x_in(4)) / (1d0 + exp(x_in(4)))

  write(6,10) 'P:', var%omega, var%beta, var%psi, var%eta

  call SOLVE

  call AGGREGATE_RULES(.true., .false.)
  
  agg_n = dot_product(var%eff_f,a%nm) + n*sum(var%eff_m)
  agg_k = sum(a%a)
  MATCH_DATA(1) = delta_d*sum(a%d) / F(agg_k,agg_n) / d_share - 1d0
  MATCH_DATA(2) = sum(a%nm) / nm_target / dble(T) - 1d0
  MATCH_DATA(3) = sum(a%nh) / nh_target / dble(T) - 1d0
  MATCH_DATA(4) = agg_k/agg_n / ratio_target - 1d0

  write(6,10) 'F:', MATCH_DATA

10 format(1x,a2,4(1x,f16.8))

end function MATCH_DATA
!=============================================================================
subroutine SOLVE
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : F1, F2, U, G
  use MPI
  implicit none
  integer :: iage
  integer :: ierr, status
  integer :: j, i

  if (f_debug) print *, ">>> SOLVE"

  LOOP: do

     call MPI_BCAST(var, NVAR, MPI_DOUBLE_PRECISION, root, MPI_COMM_WORLD, ierr)
     
     p = var%p_frac * w * var%phi

     beta_vec = 1d0
     do iage = 2, T
        beta_vec(iage) = beta_vec(iage-1)*var%beta
     end do
     
     do i1 = 1, 3
     do i2 = 1, 3
     do i3 = 1, 3
     do i4 = 1, 3
        i = i1 + 3*(i2-1 + 3*(i3-1 + 3*(i4-1)))
        if (mod(i,NPROC) == rank) then
           ! To maximize lifetime utility
           call VMAX
           
           ! To maximize lifetime utility, imposing dynamic Euler equations
           call VMAX_EE
        end if
     end do
     end do
     end do
     end do
     
     if (rank /= root) then
        ! Processes need to send their decision rules back to the root process.
        ! Was calling MPI_SSEND(...)
        do i1 = 1, 3
        do i2 = 1, 3
        do i3 = 1, 3
        do i4 = 1, 3
           i = i1 + 3*(i2-1 + 3*(i3-1 + 3*(i4-1)))
           if (mod(i,NPROC) == rank) then
              call MPI_SSEND(rule(i1,i2,i3,i4), NRULE, MPI_DOUBLE_PRECISION, root, &
                   & 100, MPI_COMM_WORLD, ierr)
           end if
        end do
        end do
        end do
        end do
     else
        do j = 1, NPROC-1
           do i1 = 1, 3
           do i2 = 1, 3
           do i3 = 1, 3
           do i4 = 1, 3
              i = i1 + 3*(i2-1 + 3*(i3-1 + 3*(i4-1)))
              if (mod(i,NPROC) == j) then
                 call MPI_RECV(rule(i1,i2,i3,i4), NRULE, MPI_DOUBLE_PRECISION, j, 100, &
                      & MPI_COMM_WORLD, status, ierr)
              end if
           end do
           end do
           end do
           end do
        end do
     end if

     if (rank == root) exit LOOP
     if (var%terminate < 0d0) exit LOOP
  end do LOOP

  if (f_debug) print *, "<<< SOLVE"
20  format(1x, i5, 10000(1x,e22.14e3))
end subroutine SOLVE
!=============================================================================
subroutine VMAX
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : U, G, H
  implicit none
  integer :: NG
  integer :: N_M
  integer :: N_ME
  integer :: LWA
  integer :: LKWA
  integer :: LACTIV
  real(long), dimension(:), allocatable :: x_guess
  real(long) :: acc, f_eval
  integer :: j, MAXIT, IPRINT, IOUT, IFAIL, nfunc
  real(long), dimension(:), allocatable :: g_eval
  real(long), dimension(:), allocatable :: xl, xu
  real(long), dimension(:), allocatable :: wa
  integer, dimension(:), allocatable :: KWA
  logical, dimension(:), allocatable :: ACTIVE
  logical :: f_continue
  real(long), dimension(T) :: Usave

  integer :: ijunk, iage
  real(long) :: junk

  if (f_debug) print *, ">>> VMAX"
  NG = 5*T - 1 + 2*N_C
  LKWA = NG+25
  N_ME = 0
  N_M = N_C + T + N_ME
  LWA = 3*NG*NG + N_M*NG + 45*NG + 12*N_M + 200
  LACTIV = 2*N_M + 10
  allocate(g_eval(MAX(1,N_M)), wa(LWA), ACTIVE(LACTIV), x_guess(NG), xl(NG), xu(NG), KWA(LKWA))

  x_guess = 0d0
  ! a
  j = 0
  x_guess(j+1:j+T-1) = 1d0
  xl(j+1:j+T-1) = -1d10
  xu(j+1:j+T-1) = 1d10
  j = j+T-1
  
  !d
  x_guess(j+1:j+T) = .001d0
  xl(j+1:j+T) = 0d0
  xu(j+1:j+T) = 1d10
  j = j+T
  
  !nm
  x_guess(j+1:j+T) = 100d0
  xl(j+1:j+T) = 0d0
  xu(j+1:j+T) = nm_max
  j = j+T
  
  !nh
  x_guess(j+1:j+T) = 200d0
  xl(j+1:j+T) = 0d0
  xu(j+1:j+T) = 1d10
  j = j+T
  
  !np
  x_guess(j+1:j+N_C) = 10d0
  xl(j+1:j+N_C) = 0d0
  xu(j+1:j+N_C) = 1d10
  j = j+N_C
  
  !s
  x_guess(j+1:j+N_C) = 10d0
  xl(j+1:j+N_C) = 0d0
  xu(j+1:j+N_C) = 1d10
  j = j+N_C
  
  !cm
  x_guess(j+1:j+T) = 10d0
  xl(j+1:j+T) = 0d0
  xu(j+1:j+T) = 1d10
  j = j+T

  iout = 86
  acc = 1d-8
  maxit = 50000
  iprint = 0
  ifail = 0
  nfunc = 0
  wa = 0d0
  kwa = 0
  active = .false.
  
  f_continue = .true.
  
  do while (f_continue)
     j = 0
     rule(i1,i2,i3,i4)%a(2:T) = x_guess(j+1:j+T-1)
     j = j+T-1
     rule(i1,i2,i3,i4)%d(2:T+1) = x_guess(j+1:j+T)
     j = j+T
     rule(i1,i2,i3,i4)%nm(1:T) = x_guess(j+1:j+T)
     j = j+T
     rule(i1,i2,i3,i4)%nh(1:T) = x_guess(j+1:j+T)
     j = j+T
     rule(i1,i2,i3,i4)%np(1:N_C) = x_guess(j+1:j+N_C)
     j = j+N_C
     rule(i1,i2,i3,i4)%s(1:N_C) = x_guess(j+1:j+N_C)
     j = j+N_C
     rule(i1,i2,i3,i4)%cm(1:T) = x_guess(j+1:j+T)
     j = j+T

     rule(i1,i2,i3,i4)%ell(:) = T_endow - rule(i1,i2,i3,i4)%nm(:) - rule(i1,i2,i3,i4)%nh(:) - rule(i1,i2,i3,i4)%np(:)
     rule(i1,i2,i3,i4)%ch(:) = H(rule(i1,i2,i3,i4)%d(2:T+1), rule(i1,i2,i3,i4)%nh(:))
     Usave = U(rule(i1,i2,i3,i4)%cm(:), rule(i1,i2,i3,i4)%ch(:), rule(i1,i2,i3,i4)%ell(:))

     f_eval = -dot_product(beta_vec, Usave)

     g_eval(1:N_C) = G(rule(i1,i2,i3,i4)%np(1:N_C), &
          &            rule(i1,i2,i3,i4)%nh(1:N_C), &
          &            rule(i1,i2,i3,i4)%ell(1:N_C), &
          &            rule(i1,i2,i3,i4)%s(1:N_C), &
          &            rule(i1,i2,i3,i4)%cc(1:N_C)) &
          & - rule(i1,i2,i3,i4)%cc(1:N_C)
     g_eval(N_C+1:N_C+T) = - rule(i1,i2,i3,i4)%cm(:) &
          & - rule(i1,i2,i3,i4)%a(2:T+1) &
          & - var%q * rule(i1,i2,i3,i4)%d(2:T+1) &
          & - p * rule(i1,i2,i3,i4)%s(:) &
          & + w*(var%eff_m*n + var%eff_f*rule(i1,i2,i3,i4)%nm(:)) &
          & + r*rule(i1,i2,i3,i4)%a(1:T) &
          & + var%q * (1d0-delta_d)*rule(i1,i2,i3,i4)%d(1:T)
     
     nfunc = nfunc+1
     

     ! Routine provided under license from Klauss Schittkowski; 
     ! http://www.ai7.uni-bayreuth.de/nlpqly.htm

     call NLPQLY(N_M, N_ME, NG, x_guess, f_eval, g_eval, xl, xu, acc, maxit, &
          & iprint, iout, ifail, wa, lwa, kwa, lkwa, active, lactiv)
     
     if (ifail >= 0) f_continue = .false.
  end do

  if (ifail /= 0) print *, 'VMAX: IFAIL=', ifail

  if (maxval(abs(rule(i1,i2,i3,i4)%cm(:))) < 1d-4) then
     print *, 'VMAX', i1, i2, i3, i4
  end if

  if (f_debug) then
     print *, 'g_eval'
     print *, g_eval
     print *, 'a'
     print *, rule(i1,i2,i3,i4)%a(:)
     print *, 'd'
     print *, rule(i1,i2,i3,i4)%d(:)
     print *, 'nm'
     print *, rule(i1,i2,i3,i4)%nm(:)
     print *, 'nh'
     print *, rule(i1,i2,i3,i4)%nh(:)
     print *, 'np'
     print *, rule(i1,i2,i3,i4)%np(:)
     print *, 's'
     print *, rule(i1,i2,i3,i4)%s(:)
     print *, 'ell'
     print *, rule(i1,i2,i3,i4)%ell(:)
     print *, 'ch'
     print *, rule(i1,i2,i3,i4)%ch(:)
     print *, 'cm'
     print *, rule(i1,i2,i3,i4)%cm(:)
  end if

  deallocate(g_eval, wa, active, x_guess, xl, xu, kwa)
  if (f_debug) print *, "<<< VMAX"
end subroutine VMAX
!=============================================================================
subroutine VMAX_EE
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : U, U1, U2, H, H1, G
  implicit none
  real(long), dimension(T) :: Usave, U1save, U2save, H1save
  real(long), dimension(T) :: nm_v, nh_v, np_v, ell_v, s_v, ch_v, cm_v
  real(long), dimension(T+1) :: a_v, d_v

  integer :: NG
  integer :: N_M
  integer :: N_ME
  integer :: LWA
  integer :: LKWA
  integer :: LACTIV
  real(long), dimension(:), allocatable :: x_guess
  real(long) :: acc, f_eval
  integer :: j, MAXIT, IPRINT, IOUT, IFAIL, nfunc
  real(long), dimension(:), allocatable :: g_eval
  real(long), dimension(:), allocatable :: xl, xu
  real(long), dimension(:), allocatable :: wa
  integer, dimension(:), allocatable :: KWA
  logical, dimension(:), allocatable :: ACTIVE
  logical :: f_continue

  if (f_debug) print *, ">>> VMAX_EE"
  NG = 4*T - 1 + 2*N_C
  LKWA = NG+25
  N_ME = 2*T - 1
  N_M = N_C + N_ME
  LWA = 3*NG*NG + N_M*NG + 45*NG + 12*N_M + 200
  LACTIV = 2*N_M + 10
  allocate(g_eval(MAX(1,N_M)), wa(LWA), ACTIVE(LACTIV), x_guess(NG), xl(NG), xu(NG), KWA(LKWA))

  if (f_debug) then
     print *, 'g_eval'
     print *, g_eval
     print *, 'a'
     print *, rule(i1,i2,i3,i4)%a(:)
     print *, 'd'
     print *, rule(i1,i2,i3,i4)%d(:)
     print *, 'nm'
     print *, rule(i1,i2,i3,i4)%nm(:)
     print *, 'nh'
     print *, rule(i1,i2,i3,i4)%nh(:)
     print *, 'np'
     print *, rule(i1,i2,i3,i4)%np(:)
     print *, 's'
     print *, rule(i1,i2,i3,i4)%s(:)
     print *, 'ell'
     print *, rule(i1,i2,i3,i4)%ell(:)
     print *, 'ch'
     print *, rule(i1,i2,i3,i4)%ch(:)
     print *, 'cm'
     print *, rule(i1,i2,i3,i4)%cm(:)
  end if

  x_guess = 0d0
  ! a
  j = 0
  x_guess(j+1:j+T-1) = rule(i1,i2,i3,i4)%a(2:T)
  xl(j+1:j+T-1) = -1d10
  xu(j+1:j+T-1) = 1d10
  j = j+T-1
  
  !d
  x_guess(j+1:j+T) = log(rule(i1,i2,i3,i4)%d(2:T+1))
  xl(j+1:j+T) = -1d10
  xu(j+1:j+T) = 1d10
  j = j+T
  
  !nm
  x_guess(j+1:j+T) = rule(i1,i2,i3,i4)%nm(:)
  xl(j+1:j+T) = 0d0
  xu(j+1:j+T) = nm_max
  j = j+T
  
  !nh
  x_guess(j+1:j+T) = rule(i1,i2,i3,i4)%nh(:)
  xl(j+1:j+T) = 0d0
  xu(j+1:j+T) = 1d10
  j = j+T
  
  !np
  x_guess(j+1:j+N_C) = rule(i1,i2,i3,i4)%np(1:N_C)
  xl(j+1:j+N_C) = 0d0
  xu(j+1:j+N_C) = 1d10
  j = j+N_C
  
  !s
  x_guess(j+1:j+N_C) = rule(i1,i2,i3,i4)%s(1:N_C)
  xl(j+1:j+N_C) = 0d0
  xu(j+1:j+N_C) = 1d10
  j = j+N_C
  
  iout = 86
  acc = 1d-8
  maxit = 5000
  iprint = 0
  ifail = 0
  nfunc = 0
  wa = 0d0
  kwa = 0
  active = .false.
  
  f_continue = .true.
  
  do while (f_continue)
     j = 0
     a_v = (/ 0d0, x_guess(j+1:j+T-1), 0d0 /)
     j = j+T-1
     d_v = (/ 0d0, exp(x_guess(j+1:j+T)) /)
     j = j+T
     nm_v = x_guess(j+1:j+T)
     j = j+T
     nh_v = x_guess(j+1:j+T)
     j = j+T
     np_v = 0d0
     np_v(1:N_C) = x_guess(j+1:j+N_C)
     j = j+N_C
     s_v = 0d0
     s_v(1:N_C) = x_guess(j+1:j+N_C)
     j = j+N_C
     ell_v = T_endow - nm_v - nh_v - np_v
     ch_v = H(d_v(2:T+1), nh_v)
     cm_v = w*(var%eff_m*n + var%eff_f*nm_v) + r*a_v(1:T) &
          & + var%q * (1d0-delta_d)*d_v(1:T) - a_v(2:T+1) &
          & - var%q * d_v(2:T+1) - s_v*p
     Usave = U(cm_v, ch_v, ell_v)
  
     f_eval = -dot_product(beta_vec, Usave)
     
     U1save = U1(cm_v, ch_v, ell_v)
     U2save = U2(cm_v, ch_v, ell_v)
     H1save = H1(d_v(2:T+1), nh_v)

     j = 0
     g_eval(j+1:j+T-1) = U1save(2:T)*var%beta*r - U1save(1:T-1)
     j = j+T-1
     g_eval(j+1:j+T-1) = U2save(1:T-1)*H1save(1:T-1) &
       & + var%beta*(1d0-delta_d)*U1save(2:T) * var%q &
       & - U1save(1:T-1) * var%q
     g_eval(j+T) = U2save(T)*H1save(T) - U1save(T)*var%q
     j = j+T

     g_eval(j+1:j+N_C) = G(np_v(1:N_C), nh_v(1:N_C), ell_v(1:N_C), s_v(1:N_C), rule(i1,i2,i3,i4)%cc(1:N_C)) &
          & - rule(i1,i2,i3,i4)%cc(1:N_C)
     
     nfunc = nfunc+1
     
     ! Routine provided under license from Klauss Schittkowski; 
     ! http://www.ai7.uni-bayreuth.de/nlpqly.htm

     call NLPQLY(N_M, N_ME, NG, x_guess, f_eval, g_eval, xl, xu, acc, maxit, &
          & iprint, iout, ifail, wa, lwa, kwa, lkwa, active, lactiv)
     
     if (ifail >= 0) f_continue = .false.
  end do

  if (maxval(abs(rule(i1,i2,i3,i4)%cm(:))) < 1d-4) then
     print *, 'VMAX_EE', i1, i2, i3, i4
  end if

  if (ifail /= 0) print *, 'VMAX_EE: IFAIL=',ifail
  if (ifail < 7) then
     rule(i1,i2,i3,i4)%a(:) = a_v
     rule(i1,i2,i3,i4)%d(:) = d_v
     rule(i1,i2,i3,i4)%nm(:) = nm_v
     rule(i1,i2,i3,i4)%nh(:) = nh_v
     rule(i1,i2,i3,i4)%np(:) = np_v
     rule(i1,i2,i3,i4)%s(:) = s_v
     rule(i1,i2,i3,i4)%ell(:) = ell_v
     rule(i1,i2,i3,i4)%ch(:) = ch_v
     rule(i1,i2,i3,i4)%cm(:) = cm_v
  end if
  if (f_debug) then
     print *, 'g_eval'
     print *, g_eval
     print *, 'a'
     print *, rule(i1,i2,i3,i4)%a(:)
     print *, 'd'
     print *, rule(i1,i2,i3,i4)%d(:)
     print *, 'nm'
     print *, rule(i1,i2,i3,i4)%nm(:)
     print *, 'nh'
     print *, rule(i1,i2,i3,i4)%nh(:)
     print *, 'np'
     print *, rule(i1,i2,i3,i4)%np(:)
     print *, 's'
     print *, rule(i1,i2,i3,i4)%s(:)
     print *, 'ell'
     print *, rule(i1,i2,i3,i4)%ell(:)
     print *, 'ch'
     print *, rule(i1,i2,i3,i4)%ch(:)
     print *, 'cm'
     print *, rule(i1,i2,i3,i4)%cm(:)
     f_debug = .false.
  end if

  deallocate(g_eval, wa, active, x_guess, xl, xu, kwa)
  if (f_debug) print *, "<<< VMAX_EE"
end subroutine VMAX_EE
!=============================================================================
subroutine CHECK_PARTIALS
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : U, U1, U2, U3, F, F1, F2, H, H1, H2, G, G1, G2, G3, G4
  implicit none
  real(long), dimension(:,:), allocatable :: tab
  real(long), parameter :: eps=1d-8
  integer :: j, iage

  allocate(tab(2,T))

  tab(1,:) = (U(rule(i1,i2,i3,i4)%cm(:)+eps,rule(i1,i2,i3,i4)%ch(:),rule(i1,i2,i3,i4)%ell(:))&
       & -U(rule(i1,i2,i3,i4)%cm(:),rule(i1,i2,i3,i4)%ch(:),rule(i1,i2,i3,i4)%ell(:)))/eps
  tab(2,:) = U1(rule(i1,i2,i3,i4)%cm(:),rule(i1,i2,i3,i4)%ch(:),rule(i1,i2,i3,i4)%ell(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'U1', tab(:,iage)
  end do

  tab(1,:) = (U(rule(i1,i2,i3,i4)%cm(:),rule(i1,i2,i3,i4)%ch(:)+eps,rule(i1,i2,i3,i4)%ell(:))&
       & -U(rule(i1,i2,i3,i4)%cm(:),rule(i1,i2,i3,i4)%ch(:),rule(i1,i2,i3,i4)%ell(:)))/eps
  tab(2,:) = U2(rule(i1,i2,i3,i4)%cm(:),rule(i1,i2,i3,i4)%ch(:),rule(i1,i2,i3,i4)%ell(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'U2', tab(:,iage)
  end do

  tab(1,:) = (U(rule(i1,i2,i3,i4)%cm(:),rule(i1,i2,i3,i4)%ch(:),rule(i1,i2,i3,i4)%ell(:)+eps)&
       & -U(rule(i1,i2,i3,i4)%cm(:),rule(i1,i2,i3,i4)%ch(:),rule(i1,i2,i3,i4)%ell(:)))/eps
  tab(2,:) = U3(rule(i1,i2,i3,i4)%cm(:),rule(i1,i2,i3,i4)%ch(:),rule(i1,i2,i3,i4)%ell(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'U3', tab(:,iage)
  end do

  tab(1,:) = (H(rule(i1,i2,i3,i4)%d(2:T+1)+eps,rule(i1,i2,i3,i4)%nh(:))-H(rule(i1,i2,i3,i4)%d(2:t+1),rule(i1,i2,i3,i4)%nh(:)))/eps
  tab(2,:) =  H1(rule(i1,i2,i3,i4)%d(2:T+1),rule(i1,i2,i3,i4)%nh(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'H1', tab(:,iage)
  end do

  tab(1,:) = (H(rule(i1,i2,i3,i4)%d(2:T+1),rule(i1,i2,i3,i4)%nh(:)+eps)-H(rule(i1,i2,i3,i4)%d(2:T+1),rule(i1,i2,i3,i4)%nh(:)))/eps
  tab(2,:) =  H2(rule(i1,i2,i3,i4)%d(2:T+1),rule(i1,i2,i3,i4)%nh(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'H2', tab(:,iage)
  end do

  tab(1,:) = (G(rule(i1,i2,i3,i4)%np(:)+eps,rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:)) &
       & -G(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:)))/eps
  tab(2,:) =  G1(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'G1', tab(:,iage)
  end do

  tab(1,:) = (G(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:)+eps,rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:)) &
       & -G(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:)))/eps
  tab(2,:) =  G2(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'G2', tab(:,iage)
  end do

  tab(1,:) = (G(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:)+eps,rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:)) &
       & -G(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:)))/eps
  tab(2,:) =  G3(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'G3', tab(:,iage)
  end do

  tab(1,:) = (G(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:)+eps,rule(i1,i2,i3,i4)%cc(:)) &
       & -G(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:)))/eps
  tab(2,:) =  G4(rule(i1,i2,i3,i4)%np(:),rule(i1,i2,i3,i4)%nh(:),rule(i1,i2,i3,i4)%ell(:),rule(i1,i2,i3,i4)%s(:),rule(i1,i2,i3,i4)%cc(:))

  write(6,*)
  do iage = 1, T
     write(6,10) 'G4', tab(:,iage)
  end do

  write(6,*)
  write(6,10) 'U1', (U(rule(i1,i2,i3,i4)%cm(1)+eps,rule(i1,i2,i3,i4)%ch(1),rule(i1,i2,i3,i4)%ell(1))&
       & -U(rule(i1,i2,i3,i4)%cm(1),rule(i1,i2,i3,i4)%ch(1),rule(i1,i2,i3,i4)%ell(1)))/eps, &
       & U1(rule(i1,i2,i3,i4)%cm(1),rule(i1,i2,i3,i4)%ch(1),rule(i1,i2,i3,i4)%ell(1))
  write(6,10) 'U2', (U(rule(i1,i2,i3,i4)%cm(1),rule(i1,i2,i3,i4)%ch(1)+eps,rule(i1,i2,i3,i4)%ell(1))&
       & -U(rule(i1,i2,i3,i4)%cm(1),rule(i1,i2,i3,i4)%ch(1),rule(i1,i2,i3,i4)%ell(1)))/eps, &
       & U2(rule(i1,i2,i3,i4)%cm(1),rule(i1,i2,i3,i4)%ch(1),rule(i1,i2,i3,i4)%ell(1))
  write(6,10) 'U3', (U(rule(i1,i2,i3,i4)%cm(1),rule(i1,i2,i3,i4)%ch(1),rule(i1,i2,i3,i4)%ell(1)+eps)&
       & -U(rule(i1,i2,i3,i4)%cm(1),rule(i1,i2,i3,i4)%ch(1),rule(i1,i2,i3,i4)%ell(1)))/eps, &
       & U3(rule(i1,i2,i3,i4)%cm(1),rule(i1,i2,i3,i4)%ch(1),rule(i1,i2,i3,i4)%ell(1))
  write(6,10) 'F1', (F(rule(i1,i2,i3,i4)%a(1)+eps,rule(i1,i2,i3,i4)%nm(1))&
       & -F(rule(i1,i2,i3,i4)%a(1),rule(i1,i2,i3,i4)%nm(1)))/eps, F1(rule(i1,i2,i3,i4)%a(1),rule(i1,i2,i3,i4)%nm(1))
  write(6,10) 'F2', (F(rule(i1,i2,i3,i4)%a(1),rule(i1,i2,i3,i4)%nm(1)+eps)&
       & -F(rule(i1,i2,i3,i4)%a(1),rule(i1,i2,i3,i4)%nm(1)))/eps, F2(rule(i1,i2,i3,i4)%a(1),rule(i1,i2,i3,i4)%nm(1))
  write(6,10) 'H1', (H(rule(i1,i2,i3,i4)%d(2)+eps,rule(i1,i2,i3,i4)%nh(1))&
       & -H(rule(i1,i2,i3,i4)%d(2),rule(i1,i2,i3,i4)%nh(1)))/eps, H1(rule(i1,i2,i3,i4)%d(2),rule(i1,i2,i3,i4)%nh(1))
  write(6,10) 'H2', (H(rule(i1,i2,i3,i4)%d(2),rule(i1,i2,i3,i4)%nh(1)+eps)&
       & -H(rule(i1,i2,i3,i4)%d(2),rule(i1,i2,i3,i4)%nh(1)))/eps, H2(rule(i1,i2,i3,i4)%d(2),rule(i1,i2,i3,i4)%nh(1))
  write(6,10) 'G1', (G(rule(i1,i2,i3,i4)%np(1)+eps,rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1))&
       & -G(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1)))/eps, &
       & G1(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1))
  write(6,10) 'G2', (G(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1)+eps,rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1))&
       & -G(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1)))/eps, &
       & G2(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1))
  write(6,10) 'G3', (G(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1)+eps,rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1))&
       & -G(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1)))/eps, &
       & G3(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1))
  write(6,10) 'G4', (G(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1)+eps,rule(i1,i2,i3,i4)%cc(1))&
       & -G(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1)))/eps, &
       & G4(rule(i1,i2,i3,i4)%np(1),rule(i1,i2,i3,i4)%nh(1),rule(i1,i2,i3,i4)%ell(1),rule(i1,i2,i3,i4)%s(1),rule(i1,i2,i3,i4)%cc(1))

10 format(1x,a2,3(1x,e20.10))
end subroutine CHECK_PARTIALS
!=============================================================================
function CAGG_S(cm_in, ch_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG
  implicit none
  real(long), intent(in) :: cm_in, ch_in
  real(long) :: CAGG_S

  CAGG_S = AGG((/cm_in,ch_in/), (/var%psi,1d0-var%psi/), xi)
end function CAGG_S
!=============================================================================
function C1_S(cm_in, ch_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in) :: cm_in, ch_in
  real(long) :: C1_S

  C1_S = DAGG((/cm_in,ch_in/), (/var%psi,1d0-var%psi/), xi, 1)
end function C1_S
!=============================================================================
function C2_S(cm_in, ch_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in) :: cm_in, ch_in
  real(long) :: C2_S

  C2_S = DAGG((/cm_in,ch_in/), (/var%psi,1d0-var%psi/), xi, 2)
end function C2_S
!=============================================================================
function F(k_in, nm_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG
  implicit none
  real(long), intent(in) :: k_in, nm_in
  real(long) :: F

  F = AGG((/k_in,nm_in/), (/alpha,1d0-alpha/), 0d0)
end function F
!=============================================================================
function F1(k_in, nm_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in) :: k_in, nm_in
  real(long) :: F1

  F1 = DAGG((/k_in,nm_in/), (/alpha,1d0-alpha/), 0d0, 1)
end function F1
!=============================================================================
function F2(k_in, nm_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in) :: k_in, nm_in
  real(long) :: F2

  F2 = DAGG((/k_in,nm_in/), (/alpha,1d0-alpha/), 0d0, 2)
end function F2
!=============================================================================
function H_S(d_in, nh_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG
  implicit none
  real(long), intent(in) :: d_in, nh_in
  real(long) :: H_S

  H_S = AGG((/d_in,nh_in/), (/var%eta,1d0-var%eta/), zeta)
end function H_S
!=============================================================================
function H1_S(d_in, nh_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in) :: d_in, nh_in
  real(long) :: H1_S

  H1_S = DAGG((/d_in,nh_in/), (/var%eta,1d0-var%eta/), zeta, 1)
end function H1_S
!=============================================================================
function H2_S(d_in, nh_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in) :: d_in, nh_in
  real(long) :: H2_S

  H2_S = DAGG((/d_in,nh_in/), (/var%eta,1d0-var%eta/), zeta, 2)
end function H2_S
!=============================================================================
function U_S(cm_in, ch_in, ell_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : CAGG
  implicit none
  real(long), intent(in) :: cm_in, ch_in, ell_in
  real(long) :: U_S
  real(long) :: cons

  cons = CAGG(cm_in,ch_in)

  if ((cons <= 0d0) .or. (ell_in <= 0d0)) then
     U_S = -1d10 + 1d20*(min(0d0, cm_in) + min(0d0,ch_in) + min(0d0, ell_in))
  elseif (gamma == 1d0) then
     U_S = log(cons) + var%omega*log(ell_in)
  else
     U_S = (cons * ell_in**var%omega)**(1d0-gamma)/(1d0-gamma)
  endif
end function U_S
!=============================================================================
function U1_S(cm_in, ch_in, ell_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : CAGG, C1
  implicit none
  real(long), intent(in) :: cm_in, ch_in, ell_in
  real(long) :: cons, U1_S

  if (cm_in <= 0d0) then
     U1_S = 1d10 - 1d10*cm_in
  else if ((ch_in <= 0d0) .or. (ell_in <= 0)) then
     U1_S = 0d0
  else if (gamma == 1d0) then
     U1_S = C1(cm_in,ch_in)/CAGG(cm_in,ch_in)
  else
     cons = CAGG(cm_in,ch_in)
     U1_S = (cons * ell_in**var%omega)**(1d0-gamma) * C1(cm_in,ch_in)/cons
  endif
end function U1_S
!=============================================================================
function U2_S(cm_in, ch_in, ell_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : CAGG, C2
  implicit none
  real(long), intent(in) :: cm_in, ch_in, ell_in
  real(long) :: cons, U2_S

  if (ch_in <= 0d0) then
     U2_S = 1d10 - 1d10*ch_in
  else if ((cm_in <= 0d0) .or. (ell_in <= 0)) then
     U2_S = 0d0
  else if (gamma == 1d0) then
     U2_S = C2(cm_in,ch_in)/CAGG(cm_in,ch_in)
  else
     cons = CAGG(cm_in,ch_in)
     U2_S = (cons * ell_in**var%omega)**(1d0-gamma) * C2(cm_in,ch_in)/cons
  endif
end function U2_S
!=============================================================================
function U3_S(cm_in, ch_in, ell_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : CAGG
  implicit none
  real(long), intent(in) :: cm_in, ch_in, ell_in
  real(long) :: U3_S

  if (ell_in <= 0d0) then
     U3_S = 1d10 - 1d10*ell_in
  else if ((cm_in <= 0d0) .or. (ch_in <= 0)) then
     U3_S = 0d0
  else if (gamma == 1d0) then
     U3_S = var%omega/ell_in
  else
     U3_S = (CAGG(cm_in,ch_in) * ell_in**var%omega)**(1d0-gamma) * var%omega/ell_in
  endif
end function U3_S
!=============================================================================
function G_S(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG
  implicit none
  real(long), intent(in):: np_in, nh_in, ell_in, s_in, cc_in
  real(long)     :: G_S
  real(long) :: ccs, temp

  ccs = theta_h*nh_in + theta_ell*ell_in + s_in
  G_S = AGG((/np_in,ccs/), (/nu,1d0-nu/), varphi)
  if (cc_in == 0d0) G_S = 0d0
end function G_S
!=============================================================================
function G1_S(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG, DAGG
  implicit none
  real(long), intent(in) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long)      :: G1_S
  real(long) :: ccs, temp

  ccs = theta_h*nh_in + theta_ell*ell_in + s_in
  G1_S = DAGG((/np_in,ccs/), (/nu,1d0-nu/), varphi, 1)

  if (cc_in == 0d0) G1_S = 0d0
end function G1_S
!=============================================================================
function G2_S(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG, DAGG
  implicit none
  real(long), intent(in) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long)      :: G2_S
  real(long) :: ccs, temp

  ccs = theta_h*nh_in + theta_ell*ell_in + s_in
  G2_S = theta_h*DAGG((/np_in,ccs/), (/nu,1d0-nu/), varphi, 2)
  if (cc_in == 0d0) G2_S = 0d0
end function G2_S
!=============================================================================
function G3_S(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG, DAGG
  implicit none
  real(long), intent(in) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long)      :: G3_S
  real(long) :: ccs, temp

  ccs = theta_h*nh_in + theta_ell*ell_in + s_in
  G3_S = theta_ell*DAGG((/np_in,ccs/), (/nu,1d0-nu/), varphi, 2)
  if (cc_in == 0d0) G3_S = 0d0
end function G3_S
!=============================================================================
function G4_S(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG, DAGG
  implicit none
  real(long), intent(in) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long)      :: G4_S
  real(long) :: ccs, temp

  ccs = theta_h*nh_in + theta_ell*ell_in + s_in
  G4_S = DAGG((/np_in,ccs/), (/nu,1d0-nu/), varphi, 2)
  if (cc_in == 0d0) G4_S = 0d0
end function G4_S
!=============================================================================
function G_V(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG
  implicit none
  real(long), intent(in), dimension(:) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long), dimension(size(np_in))      :: G_V
  real(long), dimension(:,:), allocatable :: x_mat
  real(long), dimension(:), allocatable :: temp

  allocate(x_mat(size(np_in),2))
  x_mat(:,1) = np_in
  x_mat(:,2) = theta_h*nh_in + theta_ell*ell_in + s_in
  G_V = AGG(x_mat, (/nu,1d0-nu/), varphi)
  deallocate(x_mat)
  where (cc_in == 0d0) G_V = 0d0
end function G_V
!=============================================================================
function G1_V(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG, DAGG
  implicit none
  real(long), intent(in), dimension(:) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long), dimension(size(np_in))      :: G1_V
  real(long), dimension(:,:), allocatable :: x_mat, z_mat

  allocate(x_mat(size(np_in),2))
  x_mat(:,1) = np_in
  x_mat(:,2) = theta_h*nh_in + theta_ell*ell_in + s_in
  G1_V = DAGG(x_mat, (/nu,1d0-nu/), varphi, 1)
  deallocate(x_mat)
  where (cc_in == 0d0) G1_V = 0d0
end function G1_V
!=============================================================================
function G2_V(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG, DAGG
  implicit none
  real(long), intent(in), dimension(:) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long), dimension(size(np_in))      :: G2_V
  real(long), dimension(:,:), allocatable :: x_mat, z_mat

  allocate(x_mat(size(np_in),2))
  x_mat(:,1) = np_in
  x_mat(:,2) = theta_h*nh_in + theta_ell*ell_in + s_in
  G2_V = theta_h*DAGG(x_mat, (/nu,1d0-nu/), varphi, 2)
  deallocate(x_mat)
  where (cc_in == 0d0) G2_V = 0d0
end function G2_V
!=============================================================================
function G3_V(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG, DAGG
  implicit none
  real(long), intent(in), dimension(:) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long), dimension(size(np_in))      :: G3_V
  real(long), dimension(:,:), allocatable :: x_mat, z_mat

  allocate(x_mat(size(np_in),2))
  x_mat(:,1) = np_in
  x_mat(:,2) = theta_h*nh_in + theta_ell*ell_in + s_in
  G3_V = theta_ell*DAGG(x_mat, (/nu,1d0-nu/), varphi, 2)
  deallocate(x_mat)
  where (cc_in == 0d0) G3_V = 0d0
end function G3_V
!=============================================================================
function G4_V(np_in, nh_in, ell_in, s_in, cc_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG, DAGG
  implicit none
  real(long), intent(in), dimension(:) :: np_in, nh_in, ell_in, s_in, cc_in
  real(long), dimension(size(np_in))      :: G4_V
  real(long), dimension(:,:), allocatable :: x_mat, z_mat

  allocate(x_mat(size(np_in),2))
  x_mat(:,1) = np_in
  x_mat(:,2) = theta_h*nh_in + theta_ell*ell_in + s_in
  G4_V = DAGG(x_mat, (/nu,1d0-nu/), varphi, 2)
  deallocate(x_mat)
  where (cc_in == 0d0) G4_V = 0d0
end function G4_V
!=============================================================================
function CAGG_V(cm_in, ch_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG
  implicit none
  real(long), intent(in), dimension(:) :: cm_in, ch_in
  real(long), dimension(size(cm_in))      :: CAGG_V
  real(long), dimension(:,:), allocatable :: x_mat

  allocate(x_mat(size(cm_in),2))

  x_mat(:,1) = cm_in
  x_mat(:,2) = ch_in

  CAGG_V = AGG(x_mat, (/var%psi,1d0-var%psi/), xi)
end function CAGG_V
!=============================================================================
function C1_V(cm_in, ch_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in), dimension(:) :: cm_in, ch_in
  real(long), dimension(size(cm_in))      :: C1_V
  real(long), dimension(:,:), allocatable :: x_mat

  allocate(x_mat(size(cm_in),2))

  x_mat(:,1) = cm_in
  x_mat(:,2) = ch_in

  C1_V = DAGG(x_mat, (/var%psi,1d0-var%psi/), xi, 1)
end function C1_V
!=============================================================================
function C2_V(cm_in, ch_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in), dimension(:) :: cm_in, ch_in
  real(long), dimension(size(cm_in))      :: C2_V
  real(long), dimension(:,:), allocatable :: x_mat

  allocate(x_mat(size(cm_in),2))

  x_mat(:,1) = cm_in
  x_mat(:,2) = ch_in

  C2_V = DAGG(x_mat, (/var%psi,1d0-var%psi/), xi, 2)
end function C2_V
!=============================================================================
function H_V(d_in, nh_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : AGG
  implicit none
  real(long), intent(in), dimension(:) :: d_in, nh_in
  real(long), dimension(size(d_in))       :: H_V
  real(long), dimension(:,:), allocatable :: x_mat

  allocate(x_mat(size(d_in),2))

  x_mat(:,1) = d_in
  x_mat(:,2) = nh_in

  H_V = AGG(x_mat, (/var%eta,1d0-var%eta/), zeta)
end function H_V
!=============================================================================
function H1_V(d_in, nh_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in), dimension(:) :: d_in, nh_in
  real(long), dimension(size(d_in))       :: H1_V
  real(long), dimension(:,:), allocatable :: x_mat

  allocate(x_mat(size(d_in),2))

  x_mat(:,1) = d_in
  x_mat(:,2) = nh_in

  H1_V = DAGG(x_mat, (/var%eta,1d0-var%eta/), zeta, 1)
end function H1_V
!=============================================================================
function H2_V(d_in, nh_in)
  use PRECISION
  use CG_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(long), intent(in), dimension(:) :: d_in, nh_in
  real(long), dimension(size(d_in))       :: H2_V
  real(long), dimension(:,:), allocatable :: x_mat

  allocate(x_mat(size(d_in),2))

  x_mat(:,1) = d_in
  x_mat(:,2) = nh_in

  H2_V = DAGG(x_mat, (/var%eta,1d0-var%eta/), zeta, 2)
end function H2_V
!=============================================================================
function U_V(cm_in, ch_in, ell_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : CAGG
  implicit none
  real(long), intent(in), dimension(:) :: cm_in, ch_in, ell_in
  real(long), dimension(size(cm_in))      :: U_V
  real(long), dimension(:), allocatable :: cons

  allocate(cons(size(cm_in)))

  cons = CAGG(cm_in,ch_in)

  if (gamma == 1d0) then
     where ((cons <= 0d0) .or. (ell_in <= 0d0))
        U_V = -1d10 + 1d10*(min(0d0,cm_in) + min(0d0,ch_in) + min(0d0,ell_in))
     elsewhere
        U_V = log(cons) + var%omega*log(ell_in)
     end where
  else
     where ((cons <= 0d0) .or. (ell_in <= 0d0))
        U_V = -10d30
     elsewhere
        U_V = (cons * ell_in**var%omega)**(1d0-gamma)/(1d0-gamma)
     end where
  end if
end function U_V
!=============================================================================
function U1_V(cm_in, ch_in, ell_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : CAGG, C1
  implicit none
  real(long), intent(in), dimension(:) :: cm_in, ch_in, ell_in
  real(long), dimension(size(cm_in))      :: U1_V
  real(long), dimension(:), allocatable :: cons

  allocate(cons(size(cm_in)))

  if (gamma == 1d0) then
     where (cm_in <= 0d0)
        U1_V = 1d10 - 1d10*cm_in
     elsewhere ((ch_in <= 0d0) .or. (ell_in <= 0))
        U1_V = 0d0
     elsewhere
        U1_V = C1(cm_in,ch_in)/CAGG(cm_in,ch_in)
     end where
  else
     where (cm_in <= 0d0)
        U1_V = 1d10 - 1d10*cm_in
     elsewhere ((ch_in <= 0d0) .or. (ell_in <= 0))
        U1_V = 0d0
     elsewhere
        cons = CAGG(cm_in,ch_in)
        U1_V = (cons * ell_in**var%omega)**(1d0-gamma) * C1(cm_in,ch_in)/cons
     end where
  endif
end function U1_V
!=============================================================================
function U2_V(cm_in, ch_in, ell_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : CAGG, C2
  implicit none
  real(long), intent(in), dimension(:) :: cm_in, ch_in, ell_in
  real(long), dimension(size(cm_in))      :: U2_V
  real(long), dimension(:), allocatable :: cons

  allocate(cons(size(cm_in)))

  if (gamma == 1d0) then
     where (ch_in <= 0d0)
        U2_V = 1d10 - 1d10*ch_in
     elsewhere ((cm_in <= 0d0) .or. (ell_in <= 0))
        U2_V = 0d0
     elsewhere
        U2_V = C2(cm_in,ch_in)/CAGG(cm_in,ch_in)
     end where
  else
     where (ch_in <= 0d0)
        U2_V = 1d10 - 1d10*ch_in
     elsewhere ((cm_in <= 0d0) .or. (ell_in <= 0))
        U2_V = 0d0
     elsewhere
        cons = CAGG(cm_in,ch_in)
        U2_V = (cons * ell_in**var%omega)**(1d0-gamma) * C2(cm_in,ch_in)/cons
     end where
  endif
end function U2_V
!=============================================================================
function U3_V(cm_in, ch_in, ell_in)
  use PRECISION
  use CG_COMMON
  use CG_ROUTINES, only : CAGG
  implicit none
  real(long), intent(in), dimension(:) :: cm_in, ch_in, ell_in
  real(long), dimension(size(cm_in)) :: U3_V

  if (gamma == 1d0) then
     where (ell_in <= 0d0)
        U3_V = 1d10 - 1d10*ell_in
     elsewhere ((cm_in <= 0d0) .or. (ch_in <= 0))
        U3_V = 0d0
     elsewhere
        U3_V = var%omega/ell_in
     end where
  else
     where (ell_in <= 0d0)
        U3_V = 1d10 - 1d10*ell_in
     elsewhere ((cm_in <= 0d0) .or. (ch_in <= 0))
        U3_V = 0d0
     elsewhere
        U3_V = (CAGG(cm_in,ch_in) * ell_in**var%omega)**(1d0-gamma) * var%omega/ell_in
     end where
  endif
end function U3_V
!=============================================================================
pure function AGG_V(x, alpha, sigma)
  use PRECISION
  implicit none
  real(long), dimension (:), intent(in) :: x
  real(long) :: AGG_V
  real(long), dimension(:), intent(in) :: alpha
  real(long), intent(in) :: sigma
  integer :: i, N
  
  N = size(x,1)
  
  if (sigma == 0d0) then
     AGG_V = 1d0
  else
     AGG_V = 0d0
  endif

  do i = 1, N
     if (sigma == 0d0) then
        if (x(i) > 0d0) then
           AGG_V = AGG_V * x(i)**alpha(i)
        else
           AGG_V = 0d0
        end if
     else
        if (x(i) > 0d0) then
           AGG_V = AGG_V + alpha(i)*x(i)**sigma
        endif
     endif
  end do

  if ((sigma /= 0d0) .and. (AGG_V > 0d0)) AGG_V = AGG_V**(1d0/sigma)
  if ((sigma <= 0d0) .and. (minval(x) <= 0d0)) AGG_V = 0d0
end function AGG_V
!=============================================================================
pure function DAGG_V(x, alpha, sigma, ix)
  use PRECISION
  use TOOLS, only: AGG_V
  implicit none
  real(long), dimension (:), intent(in) :: x, alpha
  real(long), intent(in) :: sigma
  integer, intent(in) :: ix
  real(long) :: DAGG_V
  
  if (x(ix) > 0d0) then
     DAGG_V = AGG_V(x, alpha, sigma)
     if (sigma == 0d0) then
        DAGG_V = alpha(ix)/x(ix) * DAGG_V
     else
        DAGG_V = alpha(ix)*x(ix)**(sigma-1d0) * DAGG_V**(1d0-sigma)
     endif
  else
     if (sigma >= 0d0) then
        DAGG_V = 1d50
     else
        DAGG_V = alpha(ix)**(1d0/sigma)
     endif
  endif
     
end function DAGG_V
!=============================================================================
recursive subroutine SECANT(FUNC, xx, ff, tolf, tol, maxit, pctptb, ptb)
!
!  Subroutine to solve non-linear equations.  Uses an n-dimensional
!  secant method.
!
!  Coded by:
!                 Paul Gomme
!                 Department of Economics
!                 Concordia University
!                 Montreal, Quebec, Canada
!
!  Algorithm from:
!      Ortega, J.M. and W.C. Rheinboldt [1970]. "Iterative Solution of
!           Nonlinear Equations in Several Variables", New York:
!           Academic Press.
!
!  ARGUMENTS
!    FUNC ..... INPUT: returns function evaluations
!    xx ....... INPUT: initial guess (vector of length N)
!               OUTPUT: solution to nonlinear equations
!    ff ....... OUTPUT: best function evaluations
!    tolf ..... INPUT: maximum allowable deviation of functions from zero
!    tol ...... INPUT, OPTIONAL: convergence criterion for inputs
!    maxit .... INPUT, OPTIONAL: maximum number of iterations
!
!    N .......... number of equations/unknowns
!    X .......... Nx(N+1) matrix
!    F .......... Nx(N+1) matrix
!    DIST ....... (N+1)x1 vector
!    A .......... (N+1)x(N+1) matrix
!    B .......... (N+1)x1 vector
!    XWORST ..... points to worst guess
!    XBEST ...... points to best guess
!    XLAST ...... points to last guess
!
  use PRECISION
  use lapack95, only : GESV
  implicit none
  real(long), dimension(:), intent(inout) :: xx
  real(long), dimension(:), intent(out)   :: ff
  real(long), intent(in)                  :: tolf
  real(long), intent(in), optional        :: tol
  integer, intent(in), optional           :: maxit
  real(long), intent(in), optional        :: pctptb
  real(long), intent(in), optional        :: ptb
  integer                                 :: N
  real(long)                              :: tolx
  real(long), allocatable, dimension(:,:) :: x, f, a
  real(long), allocatable, dimension(:)   :: b, dist, xwork
  integer                                 :: i, icount, badsteps, badbadsteps, itmax, info
  integer                                 :: xworst, xbest
  integer, allocatable, dimension(:)      :: indx

  interface
     function FUNC(x)
       use precision
       real(long), dimension(:), intent(in) :: x
       real(long), dimension(size(x)) :: FUNC
     end function FUNC
  end interface

  if (present(tol)) then ! set x tolerance
     tolx = tol
  else
     tolx = tolf
  endif

  if (present(maxit)) then ! set maximum number of iterations
     itmax = maxit
  else
     itmax = 500
  endif

  N = size(xx)
  allocate(x(N,N+1), f(N,N+1), a(N+1,N+1), b(N+1), dist(N+1), xwork(N), &
       & indx(N))

  call init_random_seed

  badbadsteps = -1
  badsteps = 1000
  xbest = 1
  x(:,1) = xx

  COUNT: do icount = 1,itmax
     if (badbadsteps > 10) exit		! too many really bad steps
     if (badsteps > 10) then		! if too many bad steps, reinitialize
        badbadsteps = badbadsteps + 1	! increment number of really bad steps

        xwork = x(:,xbest)
        call RANDOM_NUMBER(x)
        if (present(pctptb)) then
           x = 1d0 + (x-0.5d0)*pctptb
        else
           x = 1d0 + (x-0.5d0)*0.01d0
        end if
        x(:,xbest) = 1d0

        call RANDOM_NUMBER(f)
        if (present(ptb)) then
           f = (f-0.5d0)*ptb
        else
           f = (f-0.5d0)*1d-3
        end if
        f(:,xbest) = 0d0
	forall (i=1:N+1) x(:,i) = x(:,i)*xwork + f(:,i)

        do i = 1, N+1
           xwork = x(:,i)
           ff = FUNC(xwork)		! evaluate each guess
           f(:,i) = ff
           dist(i) = sqrt(dot_product(ff,ff))
           if (dist(i) < tolf) then	! found a solution which is good enough
              xbest = i
              exit COUNT
           endif
        end do
     endif

     xbest = MINLOC(dist,1)		! index of best evaluation
     xworst = MAXLOC(dist,1)		! index of worst evaluation

     if (dist(xbest) < tolf) exit COUNT	! best solution is good enough
     !  best and worst solution are close enough that we quit
     if (maxval(abs(x(:,xbest) - x(:,xworst))) < tolx) exit COUNT

     b = 0d0
     b(1) = 1d0
     a(1,:) = 1d0
     a(2:N+1,:) = f
     call GESV(a, b, info=info)		! solve for weights on guesses
     xwork = MATMUL(x,b)	! generate a new guess
     ff = FUNC(xwork)

!  make sure that the new guess is better than the worst of the previous guesses
     BADSTEP: do badsteps = 0,15
        if (sqrt(dot_product(ff,ff)) < dist(xworst)) exit BADSTEP
!  got a bad guess; move back towards the *best* previous guess
        xwork = (xwork + x(:,xbest)) * 0.5d0
        ff = FUNC(xwork)
     end do BADSTEP

     x(:,xworst) = xwork		! replace worst previous guess with new one
     f(:,xworst) = ff
     dist(xworst) = sqrt(dot_product(ff,ff))	! same with distance metric
  end do COUNT
  xx = x(:,xbest)
  ff = f(:,xbest)

  deallocate(x,f,a,b,dist,xwork,indx)
end subroutine SECANT
