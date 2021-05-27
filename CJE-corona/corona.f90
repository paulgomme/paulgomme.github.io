! Coding by:
!    Paul Gomme
!    Department of Economics
!    Concordia University
!    Montreal, QC, Canada
!

! i_experiment:
! 1. tau_h, 2025
! 2. tau_k, 2025
! 3. g, 2025
! 4. tau_h, 2029
! 5. tau_k, 2029
! 6. g, 2029
! 7. g -> tau_h
! 8. g -> tau_k
! 9. tau_h -> g
! 10. tau_k -> g
! 11. tau_h, higher d/y
! 12. tau_k, higher d/y
! 13. g, higher d/y
! 14. tau_h, Rd=2%
! 15. tau_k, Rd=2%
! 16. g, Rd=2%
! 17. tau_h, 2*omega
! 18. tau_k, 2*omega
! 19. g, 2*omega
! 20. tau_h, zeta=2
! 21. tau_k, zeta=2
! 22. g, zeta=2

module CORONA_COMMON
  use NRTYPE
  integer                        :: i_experiment = 1
  integer                        :: i_fp
  integer                        :: T0 = 1
  integer, parameter             :: TMAX = 200*12
  integer                        :: T2 = 19
  integer                        :: NPER = 4
  real(dp)                       :: lifetime

  type model_var
     real(dp), dimension(TMAX) :: c        ! Cons. of private good
     real(dp), dimension(TMAX+1) :: d      ! Government debt
     real(dp), dimension(TMAX) :: def      ! Government primary deficit
     real(dp), dimension(TMAX) :: g        ! Government good
     real(dp), dimension(TMAX) :: h        ! hours worked
     real(dp), dimension(TMAX+1) :: k      ! Capital stock
     real(dp), dimension(TMAX) :: Rd       ! Return to bond
     real(dp), dimension(TMAX) :: r        ! capital rental rate
     real(dp), dimension(TMAX) :: tau_lump ! Lump-sum tax
     real(dp), dimension(TMAX) :: tau_k    ! Capital income tax rate
     real(dp), dimension(TMAX) :: tau_h    ! Labor income tax rate
     real(dp), dimension(TMAX) :: w        ! Real wage
     real(dp), dimension(TMAX) :: x        ! Investment
     real(dp), dimension(TMAX) :: xi       ! bonds-in-the-utility function
     real(dp), dimension(TMAX) :: y        ! Output
     real(dp), dimension(TMAX) :: z        ! Total factor productivity
  end type model_var

  type(model_var) :: v

  type param
     real(dp) :: alpha = 0.3d0 ! capital share
     real(dp) :: beta ! Discount factor
     real(dp) :: delta = 0.068d0 ! Depr. rate of capital
     real(dp) :: gamma = 1d0 ! 2d0 ! Coef. or rel. risk aversion
     real(dp) :: omega = 0.054d0 ! 0.015d0 ! Feed back rule parameter
     real(dp) :: phi = 1d0 ! Utility weight on labor
     real(dp) :: psi = 0d0 ! Adjustment cost parameter
     real(dp) :: Rd = 1.0085d0! Return on government debt
     real(dp) :: Rk = 1.0755d0 ! Return on government debt
     real(dp) :: rho_z = 0.8d0 ! AR parameter, TFP
     real(dp) :: theta = 0.5d0 ! Frisch labor supply elasticity
     real(dp) :: vartheta = 1d0 ! utility weight on public goods
     real(dp) :: xi = 0d0 ! utility weight on government bonds
     real(dp) :: zeta = 2d0/3d0 ! elasticity of substitution: private vs public goods
  end type param

  type(param) :: p

  type vars
     real(dp) :: c ! Consumpion of private good (st. st.)
     real(dp) :: d ! Government debt (st. st.)
     real(dp) :: def ! Primary deficit (st. st.)
     real(dp) :: def_y ! Target for primary deficit-to-output ratio
     real(dp) :: d_y = 1.05d0 ! Target for gov. debt-to-output ratio
     real(dp) :: g ! Consumption of public good (st. st.)
     real(dp) :: g_y = 0.17486d0 ! Gov. expenditure share
     real(dp) :: h ! hours worked
     real(dp) :: k ! Capital stock (st. st.)
     real(dp) :: Rd ! Return on government debt
     real(dp) :: Rk ! Return on government debt
     real(dp) :: r ! Rental rate for capital (st. st.)
     real(dp) :: tau_c = 0d0 ! was 0.0359d0 ! Consumption tax
     real(dp) :: tau_k = 0.2385d0 ! Capital income tax (steady state)
     real(dp) :: tau_h = 0.2930d0 ! Labor income tax (steady state)
     real(dp) :: tau_lump ! Lump sum tax/transfer
     real(dp) :: w ! wage rate
     real(dp) :: x ! Investment (st. st.)
     real(dp) :: y ! Output (st. st.)
     real(dp) :: z = 1d0 ! TFP (steady state)
  end type vars

  type(vars) :: s, n

  type observations
     real(dp), dimension(120) :: y
     real(dp), dimension(140) :: g, tau_lump, labor_taxes, capital_taxes
  end type observations

  type(observations) :: obs

  type fiscal_policy
     real(dp), dimension(140) :: g, b, tau, tau_h, tau_k
  end type fiscal_policy

  type(fiscal_policy) :: fp

  real     :: Time0, Time1
end module CORONA_COMMON
!=============================================================================
module CORONA_FUNCTIONS
  interface
     function WELFARE(xi_in)
       use NRTYPE
       real(dp), intent(in) :: xi_in
       real(dp) :: WELFARE
     end function WELFARE
  end interface

  interface
     subroutine LOAD_GUESS(guess)
       use NRTYPE
       real(dp), dimension(:,:), intent(out) :: guess
     end subroutine LOAD_GUESS
  end interface

  interface
     function SS(x_in)
       use NRTYPE
       real(dp), dimension(:), intent(in) :: x_in
       real(dp), dimension(size(x_in))    :: SS
     end function SS
  end interface

  interface
     function NEWSS(x_in)
       use NRTYPE
       real(dp), dimension(:), intent(in) :: x_in
       real(dp), dimension(size(x_in))    :: NEWSS
     end function NEWSS
  end interface

  interface FCN
     function FCN(guess, t_in)
       use NRTYPE
       real(dp), dimension(:,:), intent(in) :: guess
       integer, intent(in)                    :: t_in
       real(dp), dimension(size(guess,1))   :: FCN
     end function FCN
  end interface

  interface F
     function F(capital, labor, z_shock)
       use NRTYPE
       real(dp), intent(in) :: capital, labor, z_shock
       real(dp) :: F
     end function F
  end interface

  interface F1
     function F1(capital, labor, z_shock)
       use NRTYPE
       real(dp), intent(in) :: capital, labor, z_shock
       real(dp) :: F1
     end function F1
  end interface

  interface F2
     function F2(capital, labor, z_shock)
       use NRTYPE
       real(dp), intent(in) :: capital, labor, z_shock
       real(dp) :: F2
     end function F2
  end interface

  interface U
     function U(c_in,g_in,d_in,h_in,xi_in)
       use NRTYPE
       real(dp), intent(in) :: c_in,g_in,d_in,h_in, xi_in
       real(dp) :: U
     end function U
  end interface
  
  interface U1
     function U1(c_in,g_in,d_in,h_in,xi_in)
       use NRTYPE
       real(dp), intent(in) :: c_in,g_in,d_in,h_in,xi_in
       real(dp) :: U1
     end function U1
  end interface
  
  interface U2
     function U2(c_in,g_in,d_in,h_in,xi_in)
       use NRTYPE
       real(dp), intent(in) :: c_in,g_in,d_in,h_in,xi_in
       real(dp) :: U2
     end function U2
  end interface
  
  interface U3
     function U3(c_in,g_in,d_in,h_in,xi_in)
       use NRTYPE
       real(dp), intent(in) :: c_in,g_in,d_in,h_in,xi_in
       real(dp) :: U3
     end function U3
  end interface

  interface U4
     function U4(c_in,g_in,d_in,h_in,xi_in)
       use NRTYPE
       real(dp), intent(in) :: c_in,g_in,d_in,h_in,xi_in
       real(dp) :: U4
     end function U4
  end interface
  
  interface
     subroutine EXTPATH(FUNC, xbest, fbest, ftol, f_term)
       use PRECISION
       real(long), dimension(:,:), intent(inout) :: xbest
       real(long), dimension(:,:), intent(out) :: fbest
       real(long), intent(in) :: ftol
       logical, intent(in) :: f_term
       interface
          function FUNC(x, tt)
            use precision
            real(long), dimension(:,:), intent(in) :: x
            integer, intent(in) :: tt
            real(long), dimension(size(x,1)) :: FUNC
          end function FUNC
       end interface
     end subroutine EXTPATH
  end interface
end module CORONA_FUNCTIONS
!=============================================================================
program CORONA
  use NRTYPE
  use CORONA_COMMON
  use TOOLS, only : LINSPACE
  implicit none
  integer :: t

  call init_random_seed

  print *, 'Enter experiment number:'
  read(5,*) i_experiment

  if (i_experiment == 1) then
     i_fp = 1
     call STEADY_STATE
  elseif (i_experiment == 2) then
     i_fp = 2
     call STEADY_STATE
  elseif (i_experiment == 3) then
     i_fp = 3
     call STEADY_STATE
  elseif (i_experiment == 4) then
     i_fp = 1
     T2 = T2 + 4*dble(NPER)
     call STEADY_STATE
  elseif (i_experiment == 5) then
     i_fp = 2
     T2 = T2 + 4*dble(NPER)
     call STEADY_STATE
  elseif (i_experiment == 6) then
     i_fp = 3
     T2 = T2 + 4*dble(NPER)
     call STEADY_STATE
  elseif (i_experiment == 7) then
     i_fp = 3
     call STEADY_STATE
  elseif (i_experiment == 8) then
     i_fp = 3
     call STEADY_STATE
  elseif (i_experiment == 9) then
     i_fp = 1
     call STEADY_STATE
  elseif (i_experiment == 10) then
     i_fp = 2
     call STEADY_STATE
  elseif (i_experiment == 11) then
     i_fp = 1
     call STEADY_STATE
     call NEW_STEADY_STATE
  elseif (i_experiment == 12) then
     i_fp = 2
     call STEADY_STATE
     call NEW_STEADY_STATE
  elseif (i_experiment == 13) then
     i_fp = 3
     call STEADY_STATE
     call NEW_STEADY_STATE
  elseif (i_experiment == 14) then
     i_fp = 1
     call STEADY_STATE
     call NEW_STEADY_STATE
  elseif (i_experiment == 15) then
     i_fp = 2
     call STEADY_STATE
     call NEW_STEADY_STATE
  elseif (i_experiment == 16) then
     i_fp = 3
     call STEADY_STATE
     call NEW_STEADY_STATE
  elseif (i_experiment == 17) then
     i_fp = 1
     p%omega = 2d0 * p%omega
     call STEADY_STATE
  elseif (i_experiment == 18) then
     i_fp = 2
     p%omega = 2d0 * p%omega
     call STEADY_STATE
  elseif (i_experiment == 19) then
     i_fp = 3
     p%omega = 2d0 * p%omega
     call STEADY_STATE
  elseif (i_experiment == 20) then
     i_fp = 1
     p%zeta = 2d0
     call STEADY_STATE
  elseif (i_experiment == 21) then
     i_fp = 2
     p%zeta = 2d0
     call STEADY_STATE
  elseif (i_experiment == 22) then
     i_fp = 3
     p%zeta = 2d0
     call STEADY_STATE
  end if

  call READ_OBS
  call SOLVE

  if (i_experiment == 7) then
     T0 = T2
     v%g(:) = s%g
     v%g(1:140) = obs%g(:)
     i_fp = 1
     call SOLVE
  elseif (i_experiment == 8) then
     T0 = T2
     v%g(:) = s%g
     v%g(1:140) = obs%g(:)
     i_fp = 2
     call SOLVE
  elseif (i_experiment == 9) then
     T0 = T2
     v%tau_h = s%tau_h
     i_fp = 3
     call SOLVE
  elseif (i_experiment == 10) then
     T0 = T2
     v%tau_k = s%tau_k
     i_fp = 3
     call SOLVE
  end if

  call DO_WELFARE

  print *, i_experiment, 'Done.'
end program
!=============================================================================
subroutine READ_OBS
  use CORONA_COMMON
  use TOOLS, only : LINSPACE
  implicit none
  integer :: i
  real(dp) :: junk

  obs%y(:) = 0d0

  open(unit=55, file='../R/corona.dat', status='unknown')
  do i = 2, NPER+1
     read(55,*) obs%y(i)
  end do
  close(55)

  obs%y = (1d0 + obs%y) * s%y

  fp%g(:) = 0d0
  fp%b(:) = 0d0
  fp%tau(:) = 0d0
  fp%tau_h(:) = 0d0
  fp%tau_k(:) = 0d0

  ! Read all of the coronavirus-related fiscal policy variables.
  open(unit=756, file='../US-Fiscal-Policy/fp.dat', status='unknown')
  do i = 1, 47
     read(756,*) fp%g(i+1), fp%b(i+1), fp%tau(i+1), fp%tau_h(i+1), fp%tau_k(i+1)
  end do
  close(55)
  
  ! Treat the unemployment compensation as lump-sum (windfalls)
  fp%tau_h = fp%b + fp%tau_h

  obs%g(:)             = s%g + fp%g(:) * s%y
  obs%labor_taxes(:)   = s%tau_h * s%w * s%h - fp%tau_h(:) * s%y
  obs%capital_taxes(:) = s%tau_k * (s%r - p%delta) * s%k - fp%tau_k(:) * s%y
  obs%tau_lump(:)      = s%tau_lump - fp%tau(:) * s%y

  v%g(1:140)          = obs%g(:)
  v%tau_lump(1:140)   = obs%tau_lump(:)

end subroutine READ_OBS
!=============================================================================
subroutine SOLVE
  use NRTYPE
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : LOAD_GUESS, FCN
  use TOOLS, only : EXTPATH
  implicit none
  real(dp), dimension(:,:), allocatable :: guess, feval
  real(dp) :: maxd, maxd_init
  integer :: NG
  integer :: j, t

  NG = 13

  Time0 = SECNDS(0.0)
  Time1 = SECNDS(0.0)

  allocate(guess(NG,TMAX), feval(NG,TMAX))

  guess = 0d0
  feval = 0d0

  call LOAD_GUESS(guess)

  call EXTPATH(FCN, guess(:,T0:TMAX), feval(:,T0:TMAX), 1d-12, .false.)

  maxd = maxval(abs(feval))
  if (maxd > 1d-5) then
     print *, '*** FAIL ***'
  else
     call DATA_DUMP
  end if
  
10 format(1x,i8,99(1x,e22.12e3))

  deallocate(guess, feval)
end subroutine SOLVE
!=============================================================================
function FCN(guess, t_in)
  use NRTYPE
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : F, F1, F2, U1, U2, U3, U4

  implicit none

  real(dp), dimension(:,:), intent(in) :: guess
  integer, intent(in)                    :: t_in
  real(dp), dimension(size(guess,1))   :: FCN
  integer                                :: j, tt, tx, t

  t = T0-1 + t_in

  j = 0
  j = j+1; v%k(t:t+2)     = guess(j,:)
  j = j+1; v%d(t:t+2)     = guess(j,:)
  j = j+1; v%c(t-1:t+1)   = guess(j,:)
  j = j+1; v%h(t-1:t+1)   = guess(j,:)
  j = j+1; v%w(t-1:t+1)   = guess(j,:)
  j = j+1; v%Rd(t-1:t+1)  = guess(j,:)
  j = j+1; v%r(t-1:t+1)   = guess(j,:)
  j = j+1; v%y(t-1:t+1)   = guess(j,:)
  j = j+1; v%def(t-1:t+1) = guess(j,:)
  j = j+1; v%z(t-1:t+1)   = guess(j,:)
  j = j+1; v%tau_h(t-1:t+1) = guess(j,:)
  j = j+1; v%tau_k(t-1:t+1) = guess(j,:)
  j = j+1; v%g(t-1:t+1)     = guess(j,:)

  j = 0
  ! The Euler equations and constraints.
  FCN = 1d44
  ! Accumulation of physical capital
  j=j+1; FCN(j) = p%beta * U1(v%c(t+1),v%g(t+1),v%d(t+1),v%h(t+1),v%xi(t+1)) / (1d0+s%tau_c) * ((1d0-p%psi) * ((1d0-v%tau_k(t+1))*v%r(t+1) + v%tau_k(t+1)*p%delta) + 1d0-p%delta) - U1(v%c(t),v%g(t),v%d(t),v%h(t),v%xi(t)) / (1d0+s%tau_c)
  ! Accumulation of government bonds
  j=j+1; FCN(j) = p%beta * (U1(v%c(t+1),v%g(t+1),v%d(t+1),v%h(t+1),v%xi(t+1)) / (1d0+s%tau_c) * v%Rd(t) + U3(v%c(t+1),v%g(t+1),v%d(t+1),v%h(t+1),v%xi(t+1))) - U1(v%c(t),v%g(t),v%d(t),v%h(t),v%xi(t)) / (1d0+s%tau_c)
  j=j+1; FCN(j) = (1d0-v%tau_h(t)) * v%w(t) * U1(v%c(t),v%g(t),v%d(t),v%h(t),v%xi(t)) / (1d0+s%tau_c) + U4(v%c(t),v%g(t),v%d(t),v%h(t),v%xi(t))
  ! Goods market clearing
  j=j+1; FCN(j) = v%c(t) + v%g(t) + (v%k(t+1) - (1d0-p%delta)*v%k(t)) / (1d0-p%psi) - v%y(t)
  j=j+1; FCN(j) = F(v%k(t),v%h(t),v%z(t)) - v%y(t)
  ! Evoluation of government debt
  j=j+1; FCN(j) = v%d(t) * v%Rd(t-1) + v%def(t) - v%d(t+1)
  j=j+1; FCN(j) = F1(v%k(t),v%h(t),v%z(t)) - v%r(t)
  j=j+1; FCN(j) = F2(v%k(t),v%h(t),v%z(t)) - v%w(t)
  j=j+1; FCN(j) = v%g(t) &
       & - s%tau_c*v%c(t) &
       & - v%tau_h(t)*v%w(t)*v%h(t) &
       & - v%tau_k(t)*(v%r(t)-p%delta)*v%k(t) &
       & - v%tau_lump(t) &
       & - v%def(t)

  if (t > 5) then
     j=j+1; FCN(j) = p%rho_z * log(v%z(t-1)) - log(v%z(t))
  else
     j=j+1; FCN(j) = v%y(t) - obs%y(t)
  end if

  if (t > T2) then
     j=j+1; FCN(j) = v%def(t)/v%y(t) - s%def_y + p%omega*(v%d(t)/v%y(t-1) - s%d_y)
     if (i_fp /= 1) then
        j = j+1; FCN(j) = v%tau_h(t) - s%tau_h
     end if
     if (i_fp /= 2) then
        j = j+1; FCN(j) = v%tau_k(t) - s%tau_k
     end if
     if (i_fp /= 3) then
        j = j+1; FCN(j) = v%g(t) - s%g
     end if
  else
     j=j+1; FCN(j) = v%tau_h(t) * v%w(t) * v%h(t) - obs%labor_taxes(t)
     j=j+1; FCN(j) = v%tau_k(t) * (v%r(t) - p%delta) * v%k(t) - obs%capital_taxes(t)
     j=j+1; FCN(j) = v%g(t) - obs%g(t)
  end if
end function FCN
!=============================================================================
subroutine LOAD_GUESS(guess)
  use NRTYPE
  use CORONA_COMMON
  implicit none
  real(dp), dimension(:,:), intent(out) :: guess
  integer :: j, t

  j = 0

  j = j+1; guess(j,:) = v%k(2:TMAX+1)
  j = j+1; guess(j,:) = v%d(2:TMAX+1)
  j = j+1; guess(j,:) = v%c(:)
  j = j+1; guess(j,:) = v%h(:)
  j = j+1; guess(j,:) = v%w(:)
  j = j+1; guess(j,:) = v%Rd(1:TMAX)
  j = j+1; guess(j,:) = v%r(:)
  j = j+1; guess(j,:) = v%y(:)
  j = j+1; guess(j,:) = v%def(:)
  j = j+1; guess(j,:) = v%z(:)
  j = j+1; guess(j,:) = s%tau_h
  j = j+1; guess(j,:) = s%tau_k
  j = j+1; guess(j,:) = s%g
end subroutine LOAD_GUESS
!=============================================================================
subroutine DATA_DUMP
  use NRTYPE
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : U, U1, U2, U3, U4
  implicit none
  integer                        :: t, tx, is, tt
  real(dp)                     :: t_orig, t_now, t_gr_start, t_gr_end
  real(dp), dimension(TMAX)      :: rd_annual
  character(len=10)    :: sexp
  character(len=200)  :: file_name, level_name

  write(sexp,'(I5.5)') i_experiment

  level_name = 'data/corona-' // TRIM(sexp) // '.txt'
  file_name = 'data/corona-' // TRIM(sexp) // '.pct'

  open(unit=96, file=TRIM(level_name), status='unknown')
  open(unit=42, file=TRIM(file_name), status='unknown')

  rd_annual(:) = s%Rd

  tt = 5*NPER

  v%x(:) = s%x
  do t = 2, TMAX
     v%x(t) = (v%k(t+1) - (1d0-p%delta)*v%k(t)) / (1d0 - p%psi)
     rd_annual(t) = (dble(tt-1)*rd_annual(t-1) + v%Rd(t)) / dble(tt)
  end do

  write(42,100)
  write(96,100)

  do tx = 1, TMAX
     t = max(1, tx)
     write(96,10) 2019.5d0 + dble(tx)/dble(NPER), &
          & v%y(t), &
          & v%c(t), &
          & v%h(t), &
          & v%k(t), &
          & dble(NPER)*100d0*(rd_annual(t)-1d0), &
          & 100d0*v%r(t), &
          & v%w(t), &
          & 100d0*v%tau_h(t), &
          & 100d0*v%tau_k(t), &
          & v%g(t), &
          & v%d(t), &
          & v%d(t)/(dble(NPER)*v%y(t)), &
          & v%def(t), &
          & v%def(t)/v%y(t), &
          & v%x(t), &
          & v%z(t), &
          & dble(NPER)*100d0*(v%Rd(t)-1d0), &
          & dble(NPER)*100d0*(1d0-v%tau_k(t))*(v%r(t)-p%delta), &
          & U(v%c(t),v%g(t),v%d(t),v%h(t),v%xi(t)), &
          & v%tau_lump(t), &
          & (1d0-v%tau_h(t))*v%w(t)
     write(42,10) 2020 + dble(tx)/dble(NPER), &
          & 100d0*(v%y(t)/s%y-1d0), & !2
          & 100d0*(v%c(t)/s%c-1d0), & !3
          & 100d0*(v%h(t)/s%h-1d0), & !4
          & 100d0*(v%k(t)/s%k-1d0), & !5
          & dble(NPER)*100d0*(rd_annual(t)-1d0), & !6
          & 100d0*(v%r(t)/s%r-1d0), & !7
          & 100d0*(v%w(t)/s%w-1d0), & !8
          & 100d0*(v%tau_h(t)-s%tau_h), & !9
          & 100d0*(v%tau_k(t)-s%tau_k), & !10
          & 100d0*(v%g(t)/s%g-1d0), & !11
          & 100d0*(v%d(t)/(s%d)-1d0), & !12
          & v%d(t)/(dble(NPER)*v%y(t)), & !13
          & 100d0*(v%def(t)/s%def-1d0), & !14
          & 100d0*v%def(t)/v%y(t), & !15
          & 100d0*(v%x(t)/s%x-1d0), & !16
          & 100d0*(v%z(t)-s%z), & !17
          & dble(NPER)*100d0*(v%Rd(t)-1d0), & !18
          & dble(NPER)*100d0*(1d0-v%tau_k(t))*(v%r(t)-p%delta), & !19
          & U(v%c(t),v%g(t),v%d(t),v%h(t),v%xi(t)), & !20
          & 100d0*(v%tau_lump(t) - s%tau_lump), & ! 21
          & 100d0*((1d0-v%tau_h(t))*v%w(t) / ((1d0-s%tau_h)*s%w) - 1d0)
  end do

  close(96)
  close(42)

10 format(f12.6, 99(',',1x,e20.12e3,:))
100 format("date,y,c,h,k,rd,r,w,tauh,tauk,g,d,dy,def,defy,x,z,Rd,Rk,Util,lump,wtau")
end subroutine DATA_DUMP
!============================================================================
subroutine DO_WELFARE
  use NRTYPE
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : WELFARE, U
  use TOOLS, only : ROOT_SEEK
  implicit none
  real(dp) :: wc, dtau
  integer :: t
  character(len=200) :: file_name, base_case
  type(model_var) :: baseline, alt

  file_name = 'welfare.txt'
  open(unit=777, file=file_name, access='append')
  file_name = 'lifetime.txt'
  open(unit=670, file=file_name, form='unformatted', access='stream')

  if (i_experiment == 3 .or. i_experiment == 22) then
     lifetime = U(v%c(TMAX),v%g(TMAX),v%d(TMAX),v%h(TMAX),v%xi(TMAX)) / (1d0 - p%beta)
     do t = TMAX-1, 1, -1
        lifetime = U(v%c(t),v%g(t),v%d(t),v%h(t),v%xi(t)) + p%beta * lifetime
     end do
     write(670) lifetime, v%c, v%g, v%d, v%h, v%xi
     wc = 0d0
  else
     read(670) lifetime,  baseline%c, baseline%g, baseline%d, baseline%h, baseline%xi
     wc = ROOT_SEEK(WELFARE, -0.01d0, .2d0, 1d-8)
  end if

  close(670)

  if (i_fp == 2) then
     dtau = 100d0*(v%tau_k(T2+1)-v%tau_k(T2))
  elseif (i_fp == 1) then
     dtau = 100d0*(v%tau_h(T2+1)-v%tau_h(T2))
  else
     dtau = 100d0*(v%g(T2+1)/v%g(1)-1d0)
  end if
  
  if (i_experiment == 1) then
     base_case = 'Labor'
  elseif (i_experiment == 2) then
     base_case = 'Capital'
  elseif (i_experiment == 3) then
     base_case = 'Public goods'
     write(777,*) 'Start 2025'
  elseif (i_experiment == 4) then
     base_case = 'Labor'
  elseif (i_experiment == 5) then
     base_case = 'Capital'
  elseif (i_experiment == 6) then
     base_case = 'Public goods'
     write(777,*) 'Start 2029'
  elseif (i_experiment == 7) then
     base_case = 'Labor'
     write(777,*) ' Switch from public goods to'
  elseif (i_experiment == 8) then
     base_case = 'Capital'
  elseif (i_experiment == 9) then
     base_case = 'Labor'
     write(777,*) 'Switch to public goods from'
  elseif (i_experiment == 10) then
     base_case = 'Capital'
  elseif (i_experiment == 11) then
     base_case = 'Labor'
  elseif (i_experiment == 12) then
     base_case = 'Capital'
  elseif (i_experiment == 13) then
     base_case = 'Public goods'
     write(777,*) 'Higher debt-output target'
  elseif (i_experiment == 14) then
     base_case = 'Labor'
  elseif (i_experiment == 15) then
     base_case = 'Capital'
  elseif (i_experiment == 16) then
     base_case = 'Public goods'
     write(777,*) 'Increase bond rate'
  elseif (i_experiment == 17) then
     base_case = 'Labor'
  elseif (i_experiment == 18) then
     base_case = 'Capital'
  elseif (i_experiment == 19) then
     base_case = 'Public goods'
     write(777,*) 'Stronger feedback'
  elseif (i_experiment == 20) then
     base_case = 'Labor'
  elseif (i_experiment == 21) then
     base_case = 'Capital'
  elseif (i_experiment == 22) then
     base_case = 'Public goods'
     write(777,*) 'Higher private-public good elasticity'
  end if

  write(777,10) ADJUSTL(base_case), &
       & v%d(T2)/v%y(T2)/dble(NPER), &
       & dtau, &
       & 100d0*(v%y(T2+1)/s%y-1d0), &
       & 100d0*(v%c(T2+1)/s%c-1d0), &
       & 100d0*(v%h(T2+1)/s%h-1d0), &
       & -100d0*wc

  close(777)

10 format((1x,a50), 6(1x,f25.16))
end subroutine DO_WELFARE
!============================================================================
function WELFARE(xi_in)
  use NRTYPE
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : U

  implicit none

  real(dp), intent(in) :: xi_in
  real(dp) :: WELFARE
  integer :: t

  WELFARE = U((1d0+xi_in)*v%c(TMAX),v%g(TMAX),v%d(TMAX),v%h(TMAX),p%xi) / (1d0 - p%beta)

  do t = TMAX-1, 1, -1
     WELFARE = U((1d0+xi_in)*v%c(t),v%g(t),v%d(t),v%h(t),p%xi) + p%beta * WELFARE
  end do

  WELFARE = WELFARE - lifetime
end function WELFARE
!============================================================================
subroutine STEADY_STATE
  use NRTYPE
  use TOOLS, only : NEWTON
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : SS, F, F1, F2, U, U1, U2, U3, U4

  implicit none

  real(dp), dimension(:), allocatable  :: x_guess, f_eval
  integer :: j, t

  ! Initial guesses

  p%beta   = 0.96d0**(1d0/dble(NPER))
  p%xi     = .1d0
  p%vartheta = .3d0

  p%delta = 1d0 - (1d0 - p%delta)**(1d0/dble(NPER))
  p%Rd = p%Rd**(1d0/dble(NPER))
  p%Rk = p%Rk**(1d0/dble(NPER))
  s%d_y = s%d_y * dble(NPER)
  p%omega = p%omega / dble(NPER)

  s%c  = 0.865d0
  s%h  = 0.65d0
  s%k  = 26.15d0
  s%y  = 1.23d0
  s%r  = 0.0049d0
  s%w  = 2.59d0
  s%Rd = p%Rd
  s%Rk = p%Rk
  s%g  = 0.216d0
  s%d = 15.54d0

  allocate(x_guess(13), f_eval(13))

  j = 0
  j = j+1; x_guess(1) = log(s%c)
  j = j+1; x_guess(j) = log(s%h)
  j = j+1; x_guess(j) = log(s%k)
  j = j+1; x_guess(j) = log(s%y)
  j = j+1; x_guess(j) = log(s%r)
  j = j+1; x_guess(j) = log(s%w)
  j = j+1; x_guess(j) = log(s%Rd)
  j = j+1; x_guess(j) = log(s%Rk)
  j = j+1; x_guess(j) = log(s%g)
  j = j+1; x_guess(j) = log(s%d)
  j = j+1; x_guess(j) = log(p%vartheta)
  p%psi = .25d0
  p%xi = .05d0
  j = j+1; x_guess(j) = log(p%xi)
  j = j+1; x_guess(j) = log(p%psi)

LOOP: do j = 1, 10
     call NEWTON(SS, x_guess, f_eval, 1d-12, maxstep=50)
     if (maxval(abs(f_eval)) < 1d-12) exit LOOP
  end do LOOP
  f_eval = SS(x_guess)

  if (maxval(abs(f_eval))>1d-8) then
     print *, 'Steady state: *** FAIL ***'
     stop
  end if

  s%def = (1d0-s%Rd) * s%d
  s%def_y = s%def / s%y
  s%tau_lump = s%g - s%tau_c*s%c - s%tau_h*s%w*s%h - s%tau_k*(s%r-p%delta)*s%k - s%def

  v%c(:)        = s%c
  v%d(:)        = s%d
  v%def(:)      = s%def
  v%g(:)        = s%g
  v%h(:)        = s%h
  v%k(:)        = s%k
  v%Rd(:)       = s%Rd
  v%r(:)        = s%r
  v%tau_lump(:) = s%tau_lump
  v%tau_k(:)    = s%tau_k
  v%tau_h(:)    = s%tau_h
  v%w(:)        = s%w
  v%x(:)        = s%x
  v%y(:)        = s%y
  v%z(:)        = s%z
  v%xi(:)       = p%xi

  write(6,10) s%y, s%c, s%g, s%h, s%k, 100d0*s%r, s%w, &
        U(s%c,s%g,s%d,s%h,p%xi), &
        & U1(s%c,s%g,s%d,s%h,p%xi), &
        & U2(s%c,s%g,s%d,s%h,p%xi), &
        & U3(s%c,s%g,s%d,s%h,p%xi), &
        & U4(s%c,s%g,s%d,s%h,p%xi), &
        & F(s%k,s%h,s%z), &
        & F1(s%k,s%h,s%z), &
        & F2(s%k,s%h,s%z), &
        & s%tau_h * s%w * s%h, s%tau_k * (s%r - p%delta) * s%k, &
        & s%z, dble(NPER)*100d0*(s%Rd-1d0), dble(NPER)*100d0*(1d0-s%tau_k)*(s%r-p%delta), &
        & s%d, s%d

  write(6,20) p%alpha, p%beta, p%delta, p%gamma, p%omega, p%phi, p%psi, p%theta, p%vartheta, p%xi, p%zeta, &
       & s%d_y, s%def_y, s%tau_h, s%tau_c, s%tau_k, s%tau_lump, s%g_y

10 format(/ 'Output', t30, f20.12 &
        & / 'Private Consumption', t30, f20.12 &
        & / 'Government Consumption', t30, f20.12 &
        & / 'Labor', t30, f20.12 &
        & / 'Capital stock', t30, f20.12 &
        & / '100 * Capital rental', t30, f20.12 &
        & / 'Real wage', t30, f20.12 &
        & / 'U', t30, f20.12 &
        & / 'U1', t30, f20.12 &
        & / 'U2', t30, f20.12 &
        & / 'U3', t30, f20.12 &
        & / 'U4', t30, f20.12 &
        & / 'F', t30, f20.12 &
        & / 'F1', t30, f20.12 &
        & / 'F2', t30, f20.12 &
        & / 'Labor income tax revenue', t30, f20.12 &
        & / 'Capital income tax revenue', t30, f20.12 &
        & / 'TFP', t30, f20.12 &
        & / 'Price of debt', t30, f20.12 &
        & / 'Interest rate', t30, f20.12 &
        & / 'Return to capital', t30, f20.12 &
        & / 'Value of debt', t30, f20.12 &
        & / 'Government debt', t30, f20.12 )
20 format(/ 'alpha', t20, f20.12 &
        & / 'beta', t20, f20.12 &
        & / 'delta', t20, f20.12 &
        & / 'gamma', t20, f20.12 &
        & / 'omega', t20, f20.12 &
        & / 'phi', t20, f20.12 &
        & / 'psi', t20, f20.12 &
        & / 'theta', t20, f20.12 &
        & / 'vartheta', t20, f20.12 &
        & / 'xi', t20, f20.12 &
        & / 'zeta', t20, f20.12 &
        & / 's%d_y', t20, f20.12 &
        & / 's%def_y', t20, f20.12 &
        & / 'tau_h', t20, f20.12 &
        & / 'tau_c', t20, f20.12 &
        & / 'tau_k', t20, f20.12 &
        & / 'tau_lump', t20, f20.12 &
        & / 's%g_y', t20, f20.12 /)
30 format('s%def_y   = ', f20.12, ';' &
     & /'s%d_y     = ', f20.12, ';' &
     & /'tau_lump  = ', f20.12, ';' &
     & /'tau_c     = ', f20.12, ';' &
     & /'tau_h     = ', f20.12, ';' &
     & /'tau_k     = ', f20.12, ';' &
     & /'p%beta    = ', f20.12, ';' &
     & /'p%omega   = ', f20.12, ';' &
     & /'p%delta   = ', f20.12, ';' &
     & /'p%alpha   = ', f20.12, ';' &
     & /'p%psi      = ', f20.12, ';' &
     & /'p%xi      = ', f20.12, ';' &
     & /'s%def     = ', f20.12, ';' &
     & /'s%d       = ', f20.12, ';')
end subroutine STEADY_STATE
!=============================================================================
function SS(x_in)
  use NRTYPE
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : F, F1, F2, U1, U2, U3, U4

  implicit none

  real(dp), dimension(:), intent(in) :: x_in
  real(dp), dimension(size(x_in))       :: SS
  real(dp), dimension(:), allocatable :: x_restr
  integer :: i

  allocate(x_restr(size(x_in)))

  x_restr = max(-80000d0, min(300d0, x_in))

  i = 0
  i = i+1; s%c     = exp(x_restr(i))
  i = i+1; s%h    = exp(x_restr(i))
  i = i+1; s%k     = exp(x_restr(i))
  i = i+1; s%y     = exp(x_restr(i))
  i = i+1; s%r     = exp(x_restr(i))
  i = i+1; s%w     = exp(x_restr(i))
  i = i+1; s%Rd    = exp(x_restr(i))
  i = i+1; s%Rk    = exp(x_restr(i))
  i = i+1; s%g     = exp(x_restr(i))
  i = i+1; s%d     = exp(x_restr(i))
  i = i+1; p%vartheta = exp(x_restr(i))
  i = i+1; p%xi    = exp(x_restr(i))
  i = i+1; p%psi    = exp(x_restr(i))

  s%x = p%delta * s%k

  SS(:) = 4d66
  i = 0
  i = i+1; SS(i) = 1d0 - p%beta * ((1d0-p%psi) * ((1d0-s%tau_k)*s%r + s%tau_k*p%delta) + 1d0 - p%delta)
  i = i+1; SS(i) = U2(s%c,s%g,s%d,s%h,p%xi) - U1(s%c,s%g,s%d,s%h,p%xi)
  i = i+1; SS(i) = p%beta*(s%Rd*U1(s%c,s%g,s%d,s%h,p%xi)/(1d0+s%tau_c) + U3(s%c,s%g,s%d,s%h,p%xi)) - U1(s%c,s%g,s%d,s%h,p%xi)/(1d0+s%tau_c)
  i = i+1; SS(i) = s%y - F(s%k,s%h,s%z)
  i = i+1; SS(i) = s%c + s%g + s%x/(1d0-p%psi) - s%y
  i = i+1; SS(i) = F1(s%k,s%h,s%z) - s%r
  i = i+1; SS(i) = F2(s%k,s%h,s%z) - s%w
  i = i+1; SS(i) = (1d0-s%tau_h) * s%w * U1(s%c,s%g,s%d,s%h,p%xi) + (1d0+s%tau_c)*U4(s%c,s%g,s%d,s%h,p%xi)
  i = i+1; SS(i) = s%g - s%g_y * s%y
  i = i+1; SS(i) = (1d0 + (1d0-s%tau_k)*(s%r-p%delta)) - s%Rk
  i = i+1; SS(i) = s%d_y * s%y - s%d
  i = i+1; SS(i) = s%Rd - p%Rd
  i = i+1; SS(i) = s%Rk - p%Rk
end function SS     
!============================================================================
subroutine NEW_STEADY_STATE
  use NRTYPE
  use TOOLS, only : NEWTON
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : NEWSS, F, F1, F2, U, U1, U2, U3

  implicit none

  real(dp), dimension(:), allocatable  :: x_guess, f_eval
  integer :: j, t

  if (i_experiment >= 14) then
     allocate(x_guess(11), f_eval(11))
  else
     allocate(x_guess(10), f_eval(10))
  end if

  n = s

  j = 0
  j = j+1; x_guess(1) = log(s%c)
  j = j+1; x_guess(j) = log(s%h)
  j = j+1; x_guess(j) = log(s%k)
  j = j+1; x_guess(j) = log(s%y)
  j = j+1; x_guess(j) = log(s%r)
  j = j+1; x_guess(j) = log(s%w)
  j = j+1; x_guess(j) = log(1.02d0**(1d0/dble(NPER)))
  j = j+1; x_guess(j) = log(s%Rk)
  j = j+1; x_guess(j) = log(s%d)
  j = j+1; x_guess(j) = 0d0
  if (i_experiment >= 14) j = j+1; x_guess(j) = p%xi

LOOP: do j = 1, 10
     call NEWTON(NEWSS, x_guess, f_eval, 1d-12, maxstep=50)
     if (maxval(abs(f_eval)) < 1d-12) exit LOOP
  end do LOOP
  f_eval = NEWSS(x_guess)

  if (maxval(abs(f_eval))>1d-8) then
     print *, 'Problem solving for steady state. (calibrate)'
     stop
  end if

  n%def = (1d0 - n%Rd) * n%d

  n%def_y = n%def / n%y
  s%d_y = n%d_y
  s%def_y = n%def_y

  if (i_experiment >= 14) then
     do t = 2*dble(NPER)+1,TMAX
        v%xi(t) = 0.3d0**(1d0/dble(NPER)) * v%xi(t-1) + (1d0 - 0.3d0**(1d0/dble(NPER))) * x_guess(11)
     end do
  end if

end subroutine NEW_STEADY_STATE
!=============================================================================
function NEWSS(x_in)
  use NRTYPE
  use CORONA_COMMON
  use CORONA_FUNCTIONS, only : F, F1, F2, U1, U2, U3, U4

  implicit none

  real(dp), dimension(:), intent(in)  :: x_in
  real(dp), dimension(size(x_in))     :: NEWSS
  real(dp), dimension(:), allocatable :: x_restr
  real(dp) :: newfp, newxi
  integer :: i

  allocate(x_restr(size(x_in)))

  x_restr = max(-80000d0, min(300d0, x_in))

  i = 0
  i = i+1; n%c  = exp(x_restr(i))
  i = i+1; n%h  = exp(x_restr(i))
  i = i+1; n%k  = exp(x_restr(i))
  i = i+1; n%y  = exp(x_restr(i))
  i = i+1; n%r  = exp(x_restr(i))
  i = i+1; n%w  = exp(x_restr(i))
  i = i+1; n%Rd = exp(x_restr(i))
  i = i+1; n%Rk = exp(x_restr(i))
  i = i+1; n%d  = exp(x_restr(i))
  if (i_fp == 1) then
     i=i+1; n%tau_h = s%tau_h + x_restr(i)
  elseif (i_fp == 2) then
     i=i+1; n%tau_k = s%tau_k + x_restr(i)
  elseif (i_fp == 3) then
     i=i+1; n%g = s%g + x_restr(i)
  end if
  if (i_experiment < 14) then
     newxi = p%xi
  else
     i = i+1; newxi = x_restr(i)
  end if

  if (i_experiment == 11) then
     n%d_y = 1.3753300957321057d0 * dble(NPER)
  elseif (i_experiment == 12) then
     n%d_y = 1.4133329082376145d0 * dble(NPER)
  elseif (i_experiment == 13) then
     n%d_y = 1.4032945945434823d0 * dble(NPER)
  end if

  n%def = (1d0 - n%Rd) * n%d

  n%x = p%delta * n%k

  NEWSS(:) = 4d66
  i = 0
  i = i+1; NEWSS(i) = 1d0 - p%beta * ((1d0-p%psi) * ((1d0-n%tau_k)*n%r + n%tau_k*p%delta) + 1d0 - p%delta)
  i = i+1; NEWSS(i) = p%beta*(n%Rd*U1(n%c,n%g,n%d,n%h,newxi)/(1d0+n%tau_c) + U3(n%c,n%g,n%d,n%h,newxi)) - U1(n%c,n%g,n%d,n%h,newxi)/(1d0+n%tau_c)
  i = i+1; NEWSS(i) = n%y - F(n%k,n%h,n%z)
  i = i+1; NEWSS(i) = n%c + n%g + n%x/(1d0-p%psi)  - n%y
  i = i+1; NEWSS(i) = F1(n%k,n%h,n%z) - n%r
  i = i+1; NEWSS(i) = F2(n%k,n%h,n%z) - n%w
  i = i+1; NEWSS(i) = (1d0-n%tau_h) * n%w * U1(n%c,n%g,n%d,n%h,newxi) + (1d0+n%tau_c)*U4(n%c,n%g,n%d,n%h,newxi)
  i = i+1; NEWSS(i) = (1d0 + (1d0-n%tau_k)*(n%r-p%delta)) - n%Rk
  i = i+1; NEWSS(i) = n%d_y * n%y - n%d
  i = i+1; NEWSS(i) = n%g - n%tau_c*n%c - n%tau_h*n%w*n%h - n%tau_k*(n%r-p%delta)*n%k - n%tau_lump - n%def
  if (i_experiment >= 14) then
     i = i+1; NEWSS(i) = n%Rd - 1.02d0**(1d0/dble(NPER))
  end if
end function NEWSS
!=============================================================================
function F(capital, labor, z_shock)
  use NRTYPE
  use CORONA_COMMON
  use TOOLS, only : AGG
  implicit none
  real(dp), intent(in) :: capital, labor, z_shock
  real(dp) :: F
  
  F = z_shock*AGG([capital,labor], [p%alpha,1d0-p%alpha], 0d0)
end function F
!=============================================================================
function F1(capital, labor, z_shock)
  use NRTYPE
  use CORONA_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(dp), intent(in) :: capital, labor, z_shock
  real(dp) :: F1
  
  F1 = z_shock*DAGG([capital,labor], [p%alpha,1d0-p%alpha], 0d0, 1)
end function F1
!=============================================================================
function F2(capital, labor, z_shock)
  use NRTYPE
  use CORONA_COMMON
  use TOOLS, only : DAGG
  implicit none
  real(dp), intent(in) :: capital, labor, z_shock
  real(dp) :: F2
  
  F2 = z_shock*DAGG([capital,labor], [p%alpha,1d0-p%alpha], 0d0, 2)
end function F2
!=============================================================================
function U(c_in, g_in, d_in,  h_in, xi_in)
  use NRTYPE
  use CORONA_COMMON
  use TOOLS, only : AGG

  implicit none

  real(dp), intent(in) :: c_in, g_in, d_in, h_in, xi_in
  real(dp) :: U

  if (c_in <= 0d0 .or. d_in <= 0d0) then
     U = -1d30
  elseif (p%gamma == 1d0) then
     U = log(AGG([c_in,g_in], [p%vartheta,1d0-p%vartheta], (p%zeta-1d0)/p%zeta)) &
          & + xi_in * log(d_in) &
          & - p%phi * h_in**(1d0+1d0/p%theta)
  else
     U = (c_in * g_in**(p%vartheta*(1d0-p%gamma)) &
          & * d_in**xi_in)**(1d0-p%gamma) &
          & * (1d0 - p%phi*(1d0-p%gamma)*h_in**(1d0+1d0/p%theta))**p%gamma &
          & / (1d0-p%gamma)
  end if
end function U
!=============================================================================
function U1(c_in, g_in, d_in, h_in, xi_in)
  use NRTYPE
  use CORONA_COMMON
  use TOOLS, only : AGG, DAGG

  implicit none

  real(dp), intent(in) :: c_in, g_in, d_in, h_in, xi_in
  real(dp)             :: U1

  if (c_in <= 0d0) then
     U1 = 1d20
  elseif (d_in <= 0d0) then
     U1 = 0d0
  elseif (p%gamma == 1d0) then
     U1 = DAGG([c_in,g_in], [p%vartheta,1d0-p%vartheta], (p%zeta-1d0)/p%zeta, 1) &
          & / AGG([c_in,g_in], [p%vartheta,1d0-p%vartheta], (p%zeta-1d0)/p%zeta)
  else
     U1 = c_in**(-p%gamma) * g_in**(p%vartheta*(1d0-p%gamma)) &
          & * d_in**(xi_in*(1d0-p%gamma)) &
          & * (1d0 - p%phi*(1d0-p%gamma)*h_in**(1d0+1d0/p%theta))**p%gamma
  endif
end function U1
!=============================================================================
function U2(c_in, g_in, d_in, h_in, xi_in)
  use NRTYPE
  use CORONA_COMMON
  use TOOLS, only : AGG, DAGG

  implicit none

  real(dp), intent(in) :: c_in, g_in, d_in, h_in, xi_in
  real(dp)             :: U2

  if (g_in <= 0d0) then
     U2 = 1d20
  elseif (c_in <= 0d0) then
     U2 = 0d0
  elseif (p%gamma == 1d0) then
     U2 = DAGG([c_in,g_in], [p%vartheta,1d0-p%vartheta], (p%zeta-1d0)/p%zeta, 2) &
          & / AGG([c_in,g_in], [p%vartheta,1d0-p%vartheta], (p%zeta-1d0)/p%zeta)
  else
     U2 = p%vartheta * c_in**(1d0-p%gamma) * g_in**(p%vartheta*(1d0-p%gamma)-1d0) &
          & * d_in**(xi_in*(1d0-p%gamma)) &
          & * (1d0 - p%phi*(1d0-p%gamma)*h_in**(1d0+1d0/p%theta))**p%gamma
  endif
end function U2
!=============================================================================
function U3(c_in, g_in, d_in, h_in, xi_in)
  use NRTYPE
  use CORONA_COMMON

  implicit none

  real(dp), intent(in) :: c_in, g_in, d_in, h_in, xi_in
  real(dp)             :: U3

  if (d_in <= 0d0) then
     U3 = 1d20
  elseif (c_in <= 0d0) then
     U3 = 0d0
  elseif (p%gamma == 1d0) then
     U3 = xi_in / d_in
  else
     U3 = xi_in * c_in**(1d0-p%gamma) * g_in**(p%vartheta*(1d0-p%gamma)) &
          & * d_in**(xi_in*(1d0-p%gamma)-1d0) &
          & * (1d0 - p%phi*(1d0-p%gamma)*h_in**(1d0+1d0/p%theta))**p%gamma
  endif
end function U3
!=============================================================================
function U4(c_in, g_in, d_in, h_in, xi_in)
  use NRTYPE
  use CORONA_COMMON

  implicit none

  real(dp), intent(in) :: c_in, g_in, d_in, h_in, xi_in
  real(dp)             :: U4

  if (h_in <= 0d0) then
     U4 = 1d20
  elseif (c_in <= 0d0 .or. d_in <= 0d0) then
     U4 = 0d0
  elseif (p%gamma == 1d0) then
     U4 = -p%phi * (1d0 + 1d0/p%theta) * h_in**(1d0/p%theta)
  else
     U4 = - c_in**(1d0-p%gamma) * g_in**(p%vartheta*(1d0-p%gamma)) &
          & * d_in**(xi_in*(1d0-p%gamma)) &
          & * (1d0 - p%phi*(1d0-p%gamma)*h_in**(1d0+1d0/p%theta))**(p%gamma-1d0) &
          & * p%gamma * p%phi * (1d0+1d0/p%theta)*h_in**(1d0/p%theta)
  endif
end function U4
!=============================================================================
subroutine EXTPATH(FUNC, xbest, fbest, ftol, f_term)
  use PRECISION
  use lapack95, only : GESV
  use TOOLS, only : EYE
  implicit none
  real(long), dimension(:,:), intent(inout) :: xbest
  real(long), dimension(:,:), intent(out) :: fbest
  real(long), intent(in) :: ftol
  logical, intent(in) :: f_term
  real(long) :: dbest, dnew
  real(long), dimension(:), allocatable :: ff, fff
  real(long), dimension(:,:), allocatable :: xx, xnew, fnew, d, step, Aye, xxx
  real(long), dimension(:,:,:), allocatable :: Jac, C
  real(long) :: ptb = 1d-7
  integer :: i, j, it, N, T, icount, ITMAX=100, info

  interface
     function FUNC(x, tt)
       use precision
       real(long), dimension(:,:), intent(in) :: x
       integer, intent(in) :: tt
       real(long), dimension(size(x,1)) :: FUNC
     end function FUNC
  end interface

  T = size(xbest,2)
  N = size(xbest,1)

  allocate(xx(N,3), ff(N), xnew(N,T), fnew(N,T), step(N,T))
  allocate(Jac(3,N,N), C(T,N,N), d(N,T), Aye(N,N))
  allocate(xxx(N,3), fff(N))

  fbest = 0d0
  fnew = 0d0

  do it = 2, T-1
     xx = xbest(:,it-1:it+1)
     fbest(:,it) = FUNC(xx, it)
  end do

  dbest = DIST(fbest)

  COUNT: do icount = 1, ITMAX
     if (dbest < ftol) exit COUNT

     do it = 2, T-1
        do i = 1, 3
           do j = 1, N
              xx = xbest(:,it-1:it+1)
              xx(j,i) = xx(j,i)*(1d0+ptb) + ptb
              ff = FUNC(xx, it)
              Jac(i,:,j) = (ff - fbest(:,it)) / (xx(j,i) - xbest(j,it+i-2))
           end do
        end do

        if (it == 2) then
           C(it,:,:) = eye(N)
           call GESV(Jac(2,:,:), C(it,:,:), info=info)
           if (info /= 0) print *, 'EXTPATH info=', info
           d(:,it) = -matmul(C(it,:,:), fbest(:,it))
           C(it,:,:) = matmul(C(it,:,:), Jac(3,:,:))
        else
           Jac(2,:,:) = Jac(2,:,:) - matmul(Jac(1,:,:), C(it-1,:,:))
           C(it,:,:) = eye(N)
           call GESV(Jac(2,:,:), C(it,:,:), info=info)
           if (info /= 0) print *, 'EXTPATH info=', info
           d(:,it) = -matmul(C(it,:,:), &
                & fbest(:,it) + matmul(Jac(1,:,:), d(:,it-1)))
           C(it,:,:) = matmul(C(it,:,:), Jac(3,:,:))
        end if
     end do

     step(:,T) = 0d0
     if (f_term) then
        step(:,T-1) = d(:,T-1)
     else
        Aye = eye(N) + C(T-1,:,:)
        step(:,T-1) = d(:,T-1)
        call GESV(Aye, step(:,T-1), info=info)
        step(:,T) = step(:,T-1)
     end if

     do it = T-2, 2, -1
        step(:,it) = d(:,it) - matmul(C(it,:,:), step(:,it+1)) 
     end do
     step(:,1) = 0d0

     STEPS: do i = 1, 50
        xnew = xbest + step
        do it = 2, T-1
           fnew(:,it) = FUNC(xnew(:,it-1:it+1), it)
        end do
        dnew = DIST(fnew)
        if (dnew < dbest) exit STEPS
        step = 0.5d0 * step
     end do STEPS

     if (dnew > dbest) exit COUNT
     if (maxval(abs(step)) < ftol) exit COUNT

     xbest = xnew
     fbest = fnew
     dbest = dnew
  end do COUNT

  deallocate(xx, ff, xnew, fnew, Jac, C, d)

contains
  function DIST(ff)
    real(long), dimension(:,:), intent(in) :: ff
    real(long) :: DIST
    DIST = maxval(abs(ff))
  end function DIST
end subroutine EXTPATH
