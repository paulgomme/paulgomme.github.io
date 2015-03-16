// Code to solve Gomme, Rogerson, Rupert and Wright, "Home Production in
// a Life-cycle Model", NBER Macroeconomics Annual 2004, pp. 415-461
//
// Coding by Paul Gomme, Concordia University
//
// Code written in Scilab: http://www.scilab.org
//
clear;
clearglobal;
//cd ("~/Work/scilab");

rand('seed');
rand('normal');

function [retval] = AGGREGATOR(x, alpha, sigma)
   if (sigma == 0) then
      [x_nr,x_nc] = size(x);
      retval = prod((x.*(x>0)).^(ones(x_nr,1).*.alpha),2);
   elseif (sigma < 0) then // check
      retval = prod((x>0)')'.*real(((x.*(x>0)).^sigma*alpha').^(1/sigma));
   else // check
      retval = ((x.*(x>0)).^sigma*alpha').^(1/sigma);
   end
endfunction

function [retval] = DAGGREGATOR(x, alpha, sigma, i)
   if (sigma == 0) then
      [x_nr,x_nc] = size(x);
      retval = prod(real((x.*(x>0)).^(ones(x_nr,1).*.alpha)),2) ...
         * alpha(i).*x(:,i).^(-1) + (x(:,i)<0)*10^20;
   else // check
      _y = real(((x.*(x>0)).^sigma*alpha').^((1-sigma)/sigma) ...
         *alpha(i).*x(:,i).^(sigma-1))
      if (sigma < 0) then
         retval = _y.*prod((x>0)')' + (x(:,i)<0)*alpha(i)^(1/sigma);
      else
         retval = _y + (x(:,i)<=0)*10^20;
      end
    end
endfunction

function [f] = z_choose(s,t)
   f = abs(s) > abs(t)
endfunction

function [Mcs,Mss,grad,info] = SCHUR_FIRST_ORDER(x_bar, LOGLIN, NS)
// Solve for log-linear decision rules (approximated around the
// model's steady state) using a generalized Schur method.
//
// Input:
//    x_bar: log of steady state, ordered as: future state,
//           future controls, current state, current controls
//    LOGLIN: function describing the first-order conditions,
//            equilibrium conditions, and other constraints
//            - input: x_bar
//            - output: a vector of length N=size(x_bar,1)/2
//    NS: number of state variables
// Output:
//    Mcs: matrix relating current state to current controls
//    Mss: matrix relating current state to future controls
//    grad: matrix of first-order derivatives of LOGLIN

//   "Check of log linearization:"
//   LOGLIN(x_bar)

N = size(x_bar,1)/2;

grad = zeros(N,2*N);
eps=1e-5;
for i = 1:530
  x1 = x_bar;
  x2 = x_bar;
  x1(i) = x1(i)*(1+eps)+eps;
  x2(i) = x2(i)*(1-eps)-eps;
  dx = x1(i)-x2(i);
  grad(:,i) = (LOGLIN(x1)-LOGLIN(x2))*x_bar(i)/dx;
end

B = -grad(:,N+1:2*N);
A = grad(:,1:N);

[As,Bs,Z,dim]=schur(A,B,z_choose);

//   print(%io(2), dim);

if (dim == NS)
  info = 0;
else
  info = 1;
  print(%io(2), "Economic problem in Schur decomposition");
  print(%io(2), "Number of states:                   ", NS);
  print(%io(2), "Number of correctly ordered elements", dim);
end

Z11 = Z(1:NS, 1:NS);
S11 = As(1:NS, 1:NS);
T11 = Bs(1:NS, 1:NS);
Z21 = Z(NS+1:N, 1:NS);

Mcs = real(Z21*inv(Z11));
Mss = real(Z11*inv(S11)*T11*inv(Z11));
endfunction   

function [u] = U(cm, ch, nm, nh)
u = log(C(cm,ch)) - g_omega .*(nm+nh).^g_gamma
endfunction

function [u1] = U1(cm, ch, nm, nh)
u1 = real(C1(cm,ch)./C(cm,ch) + 10^10*(cm<=0));
endfunction

function [u2] = U2(cm, ch, nm, nh)
u2 = real(C2(cm,ch)./C(cm,ch) + 10^10*(ch<=0));
endfunction

function [u3] = U3(cm, ch, nm, nh)
u3 = real(-g_omega.*g_gamma.*(nm+nh).^(g_gamma-1) - 10^10*(nm+nh<=0));
endfunction

function [u4] = U4(cm, ch, nm, nh)
u4 = real(-g_omega.*g_gamma.*(nm+nh).^(g_gamma-1) - 10^10*(nm+nh<=0));
endfunction

function [f] = F(capital, labor, z)
f = z*AGGREGATOR([capital,labor],[g_alpha,1-g_alpha],0);
endfunction

function [f1] = F1(capital, labor, z)
f1 = z*DAGGREGATOR([capital,labor],[g_alpha,1-g_alpha],0,1);
endfunction

function [f2] = F2(capital, labor, z)
f2 = z*DAGGREGATOR([capital,labor],[g_alpha,1-g_alpha],0,2);
endfunction

function [h] = H(capital, labor, z)
h = z*AGGREGATOR([capital,labor],[g_eta,1-g_eta],g_zeta);
endfunction

function [h1] = H1(capital, labor, z)
h1 = z*DAGGREGATOR([capital,labor],[g_eta,1-g_eta],g_zeta,1);
endfunction

function [h2] = H2(capital, labor, z)
h2 = z*DAGGREGATOR([capital,labor],[g_eta,1-g_eta],g_zeta,2);
endfunction

function [c] = C(cm, ch)
[r,c] = size(cm);
for i = 1:r
  c(i) = AGGREGATOR([cm(i),ch(i)],[g_psi(i),1-g_psi(i)],g_xi);
end
endfunction

function [c1] = C1(cm, ch)
[r,c] = size(cm);
for i = 1:r
c1(i) = DAGGREGATOR([cm(i),ch(i)],[g_psi(i),1-g_psi(i)],g_xi,1);
end
endfunction

function [c2] = C2(cm, ch)
[r,c] = size(cm);
for i = 1:r
c2(i) = DAGGREGATOR([cm(i),ch(i)],[g_psi(i),1-g_psi(i)],g_xi,2);
end
endfunction

function [funk] = CALIBRATE_PARAMETERS(x_in)
global g_eta g_beta g_beta_star g_omega g_psi g_ratio_star g_r_star g_w_star;
global g_cm_star g_hm_star g_hh_star g_km_star g_kh_star;
global g_cm_save g_hm_save g_hh_save g_km_save g_kh_save;
global g_AKM_star g_AXM_star g_AHM_star g_AHME_star g_AKH_star g_AXH_star g_AY_star;


g_eta = exp(x_in(1))/(1+exp(x_in(1)));
g_beta = x_in(2);
if (g_i_calibrate == 2) then
  g_delta_m = x_in(3);
  g_delta_h = x_in(4);
end
if (g_i_calibrate == 4) then
  g_omega = x_in(3)*ones(g_T,1);
  g_psi = x_in(4)*ones(g_T,1);
end
g_beta_star = g_beta / g_growth;

if (g_i_calibrate < 4) then
  guess_x = [log(g_cm_save); log(g_omega); log(g_psi); g_km_save(2:g_T+1); g_kh_save(2:g_T+1); log(g_ratio_star)];
else
  guess_x = [log(g_cm_save); g_hm_target; log(g_hh_target); g_km_save(2:g_T+1); g_kh_save(2:g_T+1); log(g_ratio_star)];
end

guess_x = fsolve(guess_x, STEADY_STATE);

g_ratio_star = exp(guess_x(g_NN*g_T+1));
g_r_star = F1(g_ratio_star, 1, g_zm_bar);
g_w_star = F2(g_ratio_star, 1, g_zm_bar);

f_eval = STEADY_STATE(guess_x);
if (max(abs(f_eval)) > 10^(-8)) then
  print(%io(2), 'Calibrate Parameters: problem in fsolve.');
  print(%io(2), 'Capital-labor ratio:', g_ratio_star);
end

g_cm_star = exp(guess_x(1:g_T));
if (g_i_calibrate < 4) then
  g_omega(1:g_T)    = exp(guess_x(g_T+1:2*g_T));
  g_psi(1:g_T)      = exp(guess_x(2*g_T+1:3*g_T));
  g_hm_star(1:g_T)   = g_hm_target(1:g_T);
  g_hh_star(1:g_T)   = g_hh_target(1:g_T);
else
  g_hm_star(1:g_T) = guess_x(g_T+1:2*g_T);
  g_hh_star(1:g_T) = guess_x(2*g_T+1:3*g_T);
end
g_km_star(1:g_T+1) = [ 0; guess_x(3*g_T+1:4*g_T) ];
g_kh_star(1:g_T+1) = [ g_kh0;, guess_x(4*g_T+1:5*g_T) ];

g_cm_save(1:g_T) = g_cm_star(1:g_T);
g_km_save(1:g_T) = g_km_star(2:g_T+1);
g_kh_save(1:g_T) = g_kh_star(2:g_T+1);

g_AKM_star  = sum(g_km_star);
g_AXM_star  = (g_growth - 1 + g_delta_m)*g_AKM_star;
g_AHM_star  = sum(max(0,g_hm_star));
g_AHME_star = max(0,g_hm_star)'*g_em_star;
g_AKH_star  = sum(g_kh_star);
g_AXH_star  = (g_growth - 1 + g_delta_h)*g_AKH_star;
g_AY_star   = F(g_AKM_star, g_AHME_star, g_zm_bar);

if (g_i_calibrate == 1) then
  funk(1) = g_AKM_star - g_km_y_target * g_AY_star;
  funk(2) = g_AKH_star - g_kh_y_target * g_AY_star;
elseif (g_i_calibrate == 2) then
  funk(1) = F1(g_AKM_star,g_AHME_star,g_zm_bar) - g_delta_m - g_rr_target;
  funk(2) = g_km_y_target/g_kh_y_target - g_AKM_star/g_AKH_star;
  funk(3) = g_AXM_star - g_xm_y_ratio * g_AY_star;
  funk(4) = g_AXH_star - g_xh_y_ratio * g_AY_star;
else
  funk(1) = g_AXM_star - g_xm_y_ratio * g_AY_star;
  funk(2) = g_AXH_star - g_xh_y_ratio * g_AY_star;
  if (g_i_calibrate == 4) then
    funk(3) = sum(g_hm_star) - sum(g_hm_target);
    funk(4) = sum(g_hh_star) - sum(g_hh_target);
  end
end
funk'
endfunction

function [f] = STEADY_STATE(x)
global g_omega g_psi;

cm = exp(x(1:g_T));
if (g_i_calibrate < 4) then
  hm    = g_hm_target;
  hh    = g_hh_target;
  g_omega = exp(x(g_T+1:2*g_T));
  g_psi   = exp(x(2*g_T+1:3*g_T));
else
  hm = x(g_T+1:2*g_T);
  hh = exp(x(2*g_T+1:3*g_T));
end
km  = [ 0; x(3*g_T+1:4*g_T) ];
kh  = [ g_kh0; x(4*g_T+1:5*g_T) ];

ratio = exp(x(5*g_T+1));

rstar = F1(ratio,1,g_zm_bar);
wstar = F2(ratio,1,g_zm_bar);

AKMstar = sum(km);
AHMEstar = hm'*g_em_star;
tau = (g_tau_h*wstar*AHMEstar + g_tau_k*AKMstar*(rstar-g_delta_m) ...
    - g_G_Y_ratio*F(AKMstar, AHMEstar, g_zm_bar)) / g_T;

hsave = H(kh(1:g_T),hh,g_zh_bar);
h1save = H1(kh(1:g_T),hh,g_zh_bar);
h2save = H2(kh(1:g_T),hh,g_zh_bar);

u1save = U1(cm,hsave,hm,hh)
u2save = U2(cm,hsave,hm,hh)
u3save = U3(cm,hsave,hm,hh)
u4save = U4(cm,hsave,hm,hh)

f(1:g_T) = tau + (1-g_tau_h)*wstar*g_em_star.*hm + (1-g_delta_h)*kh(1:g_T) + (1+(1-g_tau_k)*(rstar-g_delta_m))*km(1:g_T) - (cm + g_growth*(km(2:g_T+1) + kh(2:g_T+1)));
f(g_T+1:2*g_T) = (1-g_tau_h)*wstar*g_em_star.*u1save + u3save;
if (g_i_calibrate == 4) then
  f(g_T+g_NW+1:2*g_T) = hm(g_NW+1:g_T)
end
f(2*g_T+1:3*g_T) = h2save.*u2save + u4save;
f(3*g_T+1:4*g_T-1) = g_beta_star*((1-g_tau_k)*(rstar-g_delta_m)+1)*u1save(2:g_T) - u1save(1:g_T-1);
f(4*g_T) = km(g_T+1);
f(4*g_T+1:5*g_T-1) = g_beta_star*((1-g_delta_h)*u1save(2:g_T) + u2save(2:g_T).*h1save(2:g_T)) - u1save(1:g_T-1);
f(5*g_T) = kh(g_T+1) - g_kh0;

calc_ratio = sum(km) / (max(0,hm)'*g_em_star);

f(5*g_T+1) = ratio - calc_ratio;
endfunction

function [f] = LOG_LIN(xhat)
zm          = xhat(g_N+1);
km(1:g_T+1) = [ 0; xhat(g_N+2:g_N+g_T); 0 ];
zh          = xhat(g_N+g_T+1);
kh(1:g_T+1) = [ g_kh0; xhat(g_N+g_T+2:g_N+2*g_T); g_kh0 ];
cm(1:g_T)   = xhat(g_N+g_NS+1:g_N+g_NS+g_T);
hm(1:g_T)   = 0;
hm(1:g_NW)  = xhat(g_N+g_NS+g_T+1:g_N+g_NS+g_T+g_NW);
hh(1:g_T)   = xhat(g_N+g_NS+g_NW+g_T+1:g_N+g_NS+g_NW+2*g_T);

zmprime          = xhat(1);
kmprime(1:g_T+1) = [ 0; xhat(2:g_T); 0 ];
zhprime          = xhat(g_T+1);
khprime(1:g_T+1) = [ g_kh0; xhat(g_T+2:2*g_T); g_kh0 ];
cmprime(1:g_T)   = xhat(g_NS+1:g_NS+g_T);
hmprime(1:g_T)   = 0;
hmprime(1:g_NW)  = xhat(g_NS+g_T+1:g_NS+g_T+g_NW);
hhprime(1:g_T)   = xhat(g_NS+g_NW+g_T+1:g_NS+g_NW+2*g_T);

wage = F2(sum(km(:)), hm'*g_em_star, zm);
rental = F1(sum(km(:)), hm'*g_em_star, zm);
wageprime = F2(sum(kmprime(:)), hmprime'*g_em_star, zmprime);
rentalprime = F1(sum(kmprime(:)), hmprime'*g_em_star, zmprime);
tau = (g_tau_h*wage*(hm'*g_em_star) + g_tau_k*sum(km(:))*(rental-g_delta_m) - g_G_Y_ratio*F(sum(km(:)), hm'*g_em_star, zm)) / g_T;

ch(1:g_T) = H(kh(1:g_T),hh(1:g_T),zh);
u1save(1:g_T) = U1(cm(1:g_T),ch(1:g_T),hm(1:g_T),hh(1:g_T));
u2save(1:g_T) = U2(cm(1:g_T),ch(1:g_T),hm(1:g_T),hh(1:g_T));
u3save(1:g_T) = U3(cm(1:g_T),ch(1:g_T),hm(1:g_T),hh(1:g_T));
u4save(1:g_T) = U4(cm(1:g_T),ch(1:g_T),hm(1:g_T),hh(1:g_T));
h1save(1:g_T) = H1(kh(1:g_T),hh(1:g_T),zh);
h2save(1:g_T) = H2(kh(1:g_T),hh(1:g_T),zh);

chprime(1:g_T) = H(khprime(1:g_T),hhprime(1:g_T),zhprime);
u1prime(1:g_T) = U1(cmprime(1:g_T),chprime(1:g_T),hmprime(1:g_T),hhprime(1:g_T));
u2prime(1:g_T) = U2(cmprime(1:g_T),chprime(1:g_T),hmprime(1:g_T),hhprime(1:g_T));
u3prime(1:g_T) = U3(cmprime(1:g_T),chprime(1:g_T),hmprime(1:g_T),hhprime(1:g_T));
u4prime(1:g_T) = U4(cmprime(1:g_T),chprime(1:g_T),hmprime(1:g_T),hhprime(1:g_T));
h1prime(1:g_T) = H1(khprime(1:g_T),hhprime(1:g_T),zhprime);
h2prime(1:g_T) = H2(khprime(1:g_T),hhprime(1:g_T),zhprime);

f = 10^20*ones(g_N,1);

f1 = (1-g_tau_h)*wage*g_em_star(1:g_NW).*u1save(1:g_NW) + u3save(1:g_NW);
f2 = tau + (1-g_tau_h)*wage*g_em_star(1:g_T).*hm(1:g_T) + (1+(1-g_tau_k)*(rental-g_delta_m))*km(1:g_T) + (1-g_delta_h)*kh(1:g_T) - (cm(1:g_T) + g_growth*(kmprime(2:g_T+1) + khprime(2:g_T+1)));
f3 = u2save(1:g_T).*h2save(1:g_T) + u4save(1:g_T);
f4 = g_beta_star*u1prime(2:g_T) * (1 + (1-g_tau_k)*(rentalprime-g_delta_m)) - u1save(1:g_T-1);
f5 = g_beta_star*(u1prime(2:g_T)*(1-g_delta_h) + u2prime(2:g_T).*h1prime(2:g_T)) - u1save(1:g_T-1);
f6 = log(zmprime) - g_rho_m*log(zm);
f7 = log(zhprime) - g_rho_h*log(zh);
f = [f1;f2;f3;f4;f5;f6;f7];
endfunction

function [ctrl_out, cur_st_out, fut_st_out] = SIMULATE(state_in, f_stochastic)
cur_st_out = state_in;
ctrl_out = g_Mcs*cur_st_out;
fut_st_out = g_Mss*cur_st_out;
if (f_stochastic) then
  shocks = g_ae*rand(2,1);
  fut_st_out(1) = fut_st_out(1) + g_sd_shock(1).*shocks(1);
  !fut_st_out(g_T+1) = fut_st_out(2) + g_sd_shock(2).*shocks(2);
end
endfunction

function [dat_out] = SIM_AGG(control, current_state, future_state)
cm(1:g_T) = (control(1:g_T)+1).*g_cm_star;
hm(1:g_T) = 0;
hm(1:g_NW) = control(g_T+1:g_T+g_NW);
hm(1:g_T) = (hm(1:g_T)+1).*g_hm_star(1:g_T);
hh(1:g_T) = (control(g_T+g_NW+1:2*g_T+g_NW)+1).*g_hh_star(1:g_T);

zm = (current_state(1)+1)*g_zm_bar;
km(1:g_T+1) = ([0; current_state(2:g_T); 0]+1).*g_km_star(1:g_T+1);
zh = (current_state(g_T+1)+1)*g_zh_bar;
kh(1:g_T+1) = ([0; current_state(2+g_T:2*g_T); 0]+1).*g_kh_star(1:g_T+1);

kmprime(1:g_T+1) = ([0; future_state(2:g_T); 0]+1).*g_km_star(1:g_T+1);
khprime(1:g_T+1) = ([0; future_state(2+g_T:2*g_T); 0]+1).*g_kh_star(1:g_T+1);

A_KM = sum(km(:));
A_KH = sum(kh(:));
A_K = A_KM + A_KH;
A_HM = sum(hm(:));
A_HH = sum(hh(:));
A_H = A_HM + A_HH;
A_HME = hm'*g_em_star;
A_HHE = sum(hh(:));
A_CM = sum(cm(:));
ch(1:g_T) = H(kh(1:g_T),hh(1:g_T),zh);
A_CH = sum(ch(:));
A_Y = F(A_KM, A_HME, zm);
A_PROD = A_Y / A_HM;
A_C = sum(C(cm(1:g_T),ch(1:g_T)));
A_XM = g_growth*sum(kmprime(:)) - (1-g_delta_m)*A_KM;
A_XH = g_growth*sum(khprime(:)) - (1-g_delta_h)*A_KH;
A_X = A_XM + A_XH;

HM_1_5 = sum(hm(1:5));
HM_6_15 = sum(hm(6:15));
HM_16_25 = sum(hm(16:25));
HM_26_35 = sum(hm(26:35));
HM_36_45 = sum(hm(36:45));
HM_46_55 = sum(hm(g_NW-5:g_NW));

wage = F2(A_KM, A_HME, zm);
rental = F1(A_KM, A_HME, zm);
dat_out = [ A_Y, A_C, A_CM, A_CH, A_X, A_XM, A_XH, A_H, A_HM, HM_1_5, HM_6_15, HM_16_25, HM_26_35, HM_36_45, HM_46_55, A_HME, A_HH, A_HHE, A_PROD, A_K, A_KM, A_KH, wage, rental ];
endfunction

function [x_mean,x_var,ac,x_corr] = CALCULATE_MOMENTS(series)
[NS,NOBS] = size(series);

x_mean = mean(series,'r')';

vcv = series'*series / g_NOBS
x_var = diag(vcv);
x_corr = (vcv - x_mean.*.x_mean') ./ sqrt(x_var.*.x_var');

mean1 = mean(series(1:g_NOBS-1,:),'r')';
mean2 = mean(series(2:g_NOBS,:),'r')';
temp = series(1:g_NOBS-1,:)'*series(1:g_NOBS-1,:) / (g_NOBS-1);
var1 = diag(temp) - mean1^2;
temp = series(2:g_NOBS,:)'*series(2:g_NOBS,:) / (g_NOBS-1);
var2 = diag(temp) - mean2^2;
ac = (sum(series(1:g_NOBS-1,:).*series(2:g_NOBS,:),'r')'/(g_NOBS-1) - mean1.*mean2) ./ sqrt(var1.*var2);
endfunction

function [xcorr] = LEAD_LAG(series, ix, NLAG)
[NOBS,NS] = size(series);
xcorr = zeros(NS,2*NLAG+1);

for ilag = -NLAG:NLAG
  index = ilag + NLAG + 1;
  istart = max(1,1-ilag);
iend = min(NOBS, NOBS-ilag);
NOBS2 = NOBS - abs(ilag);
avg1 = mean(series(istart+ilag:iend+ilag,:),'r')';
avg2 = mean(series(istart:iend,ix),'r')';
temp = series(istart+ilag:iend+ilag,:)'*series(istart+ilag:iend+ilag,:);
var1 = diag(temp) / NOBS2 - avg1^2;
temp = series(istart:iend,ix)'*series(istart:iend,ix);
var2 = diag(temp) / NOBS2 - avg2^2;

xcorr(:,index) = (series(istart+ilag:iend+ilag,:)'*series(istart:iend,ix)/NOBS2 - avg1*avg2) ./ sqrt(var1*var2);
end
endfunction

function [hpmat] = HPMATRIX(T, lambda)
d = (1+6*lambda)*ones(T,1);
x = diag(d);
d = -4*lambda*ones(T-1,1);
x = x + diag(d,1) + diag(d,-1);
d = lambda*ones(T-2,1);
x = x + diag(d,2) + diag(d,-2);
x(1,1) = 1+lambda;
x(1,2) = -2*lambda;
x(2,1) = -2*lambda;
x(2,2) = 1+5*lambda;
x(T,T) = 1+lambda;
x(T-1,T) = -2*lambda;
x(T,T-1) = -2*lambda;
x(T-1,T-1) = 1+5*lambda;
hpmat = inv(x);
endfunction

// INITIALIZATION

g_beta  = 0.966938;
g_gamma = 2.5;
g_xi    = 0.45;
g_alpha = 0.3;
g_delta_m = 0.065362022;
g_rho_m = 0.895264;
g_eta = 0.21283;
g_zeta = 0.0;
g_delta_h = 0.056798433;
g_rho_h = 0.8059652;
g_tau_h = 0.25;
g_tau_k = 0.5;
g_kh0 = 0.2;
g_sd_shock = [0.0152772; 0.0];
g_ecorr = [1, 0; 0, 1];
g_km_y_ratio = 1.374857667;
g_kh_y_ratio = 1.693561388;
g_xm_y_ratio = 0.124841013;
g_xh_y_ratio = 0.12031495;
g_rr_target = 0.08;
g_growth = 1.01836657453;
g_NN = 5;
g_T = 55;
g_NRET = 10;
g_NOBS = 39;
g_NREP = 1000;
g_zm_bar = 1.0;
g_zh_bar = 1.0;
g_G_Y_ratio = 0.2;
g_beta_star = g_beta / g_growth;

g_i_calibrate = 3;

g_NW = g_T - g_NRET;
g_NS = 2*g_T;
g_NC = 2*g_T+g_NW;
g_N = g_NS+g_NC;

if (g_i_calibrate < 3) then
  g_delta_m = g_xm_y_ratio / g_km_y_ratio - (g_growth - 1);
  g_delta_h = g_xh_y_ratio / g_kh_y_ratio - (g_growth - 1);
end
if (g_i_calibrate == 1) then
  g_alpha = (g_rr_target + g_delta_m)*g_km_y_target;
end

hours = [  62.86857,  50.38002;
  63.90805,  51.13020;
  64.82909,  51.84599;
  65.63413,  52.52827;
  66.32558,  53.17791;
  66.90588,  53.79576;
  67.37744,  54.38272;
  67.74270,  54.93963;
  68.00408,  55.46737;
  68.16402,  55.96681;
  68.22494,  56.43882;
  68.18925,  56.88426;
  68.05940,  57.30401;
  67.83781,  57.69893;
  67.52691,  58.06988;
  67.12911,  58.41775;
  66.64686,  58.74339;
  66.08257,  59.04768;
  65.43867,  59.33149;
  64.71759,  59.59568;
  63.92175,  59.84111;
  63.05359,  60.06867;
  62.11553,  60.27922;
  61.11000,  60.47363;
  60.03942,  60.65276;
  58.90622,  60.81749;
  57.71282,  60.96867;
  56.46166,  61.10719;
  55.15516,  61.23391;
  53.79575,  61.3497, 
  52.38585,  61.45542;
  50.92789,  61.55195;
  49.42430,  61.64016;
  47.87751,  61.72090;
  46.28993,  61.79506;
  44.66401,  61.86350;
  43.00216,  61.92708;
  41.30681,  61.98668;
  39.58039,  62.04317;
  37.82533,  62.09741;
  36.04404,  62.15027;
  34.23897,  62.20263;
  32.41254,  62.25534;
  30.56716,  62.30928;
  28.70528,  62.36531;
  26.82931,  62.42431;
  24.94169,  62.48714;
  23.04483,  62.55468;
  21.14118,  62.62778;
  19.23314,  62.70733;
  17.32316,  62.79418;
  15.41366,  62.88921;
  13.50706,  62.99328;
  11.60579,  63.10727;
  9.712275,  63.23203];

g_hm_target = hours(:,1)/224;
g_hm_target(g_NW+1:g_T) = 0;
g_hh_target = hours(:,2)/224;

g_em_star = [ 1.958646;
 2.006947;
 2.053957;
 2.099677;
 2.144107;
 2.187246;
 2.229094;
 2.269652;
  2.30892;
 2.346897;
 2.383583;
 2.418979;
 2.453085;
   2.4859;
 2.517425;
 2.547659;
 2.576603;
 2.604256;
 2.630619;
 2.655691;
 2.679473;
 2.701964;
 2.723165;
 2.743076;
 2.761696;
 2.779025;
 2.795064;
 2.809813;
  2.82327;
 2.835438;
 2.846315;
 2.855901;
 2.864198;
 2.871203;
 2.876919;
 2.881343;
 2.884477;
 2.886321;
 2.886874;
 2.886137;
 2.884109;
 2.880791;
 2.876182;
 2.870283;
 2.863094;
 2.854614;
 2.844843;
 2.833782;
  2.82143;
 2.807788;
 2.792856;
 2.776633;
 2.759119;
 2.740315;
 2.720221];

g_em_star = exp(g_em_star);
g_em_star = g_em_star / g_em_star(1);

if (g_i_calibrate == 1) then
  g_em_star = 1;
end

steady_state=[    0.2907846166,         0.2806632589,         0.2249108036,         0.0000000000,         0.2000000000,         1.0000000000,         0.5899871206,         0.2193618227,         1.4788038651,         0.2601621431;
    0.2857520433,         0.2853037946,         0.2282598214,        -0.2170152805,         0.3235196742,         1.0494865032,         0.5848030252,         0.2458467700,         1.4949949046,         0.2687827111;
    0.2804941156,         0.2894155804,         0.2314553125,        -0.3340882487,         0.3438385450,         1.1000009022,         0.5700740545,         0.2517949494,         1.5129292063,         0.2679468099;
    0.2750345295,         0.2930095089,         0.2345012054,        -0.4421132917,         0.3646602539,         1.1514603427,         0.5554732475,         0.2576027779,         1.5324602860,         0.2672081419;
    0.2693995358,         0.2960963393,         0.2374013839,        -0.5403876040,         0.3859422290,         1.2037732517,         0.5410298067,         0.2632661874,         1.5534578586,         0.2665748659;
    0.2636179903,         0.2986869643,         0.2401596429,        -0.6282437432,         0.4076374847,         1.2568392028,         0.5267733699,         0.2687812214,         1.5758036594,         0.2660545034;
    0.2577205253,         0.3007921429,         0.2427800000,        -0.7050574845,         0.4296960354,         1.3105514463,         0.5127330514,         0.2741445010,         1.5993918311,         0.2656536799;
    0.2517398653,         0.3024227679,         0.2452662054,        -0.7702528456,         0.4520643577,         1.3647974099,         0.4989379770,         0.2793526884,         1.6241279975,         0.2653780637;
    0.2457107493,         0.3035896429,         0.2476221875,        -0.8233076862,         0.4746855230,         1.4194564264,         0.4854175774,         0.2844028762,         1.6499234746,         0.2652325042;
    0.2396700768,         0.3043036607,         0.2498518304,        -0.8637593951,         0.4974989492,         1.4743998122,         0.4722019576,         0.2892923371,         1.6766939943,         0.2652209692;
    0.2336561605,         0.3045756250,         0.2519590179,        -0.8912120032,         0.5204416758,         1.5294940585,         0.4593211713,         0.2940186974,         1.7043624790,         0.2653462954;
    0.2277085390,         0.3044162946,         0.2539475893,        -0.9053412521,         0.5434486844,         1.5846015695,         0.4468052929,         0.2985798932,         1.7328582962,         0.2656101385;
    0.2218682166,         0.3038366071,         0.2558214732,        -0.9058984845,         0.5664524904,         1.6395781776,         0.4346850958,         0.3029742050,         1.7621114820,         0.2660131309;
    0.2161777431,         0.3028473661,         0.2575845089,        -0.8927150764,         0.5893830060,         1.6942734401,         0.4229923614,         0.3072000461,         1.7920542474,         0.2665548287;
    0.2106798810,         0.3014594196,         0.2592405357,        -0.8657094311,         0.6121698159,         1.7485362336,         0.4117584793,         0.3112562607,         1.8226245579,         0.2672334340;
    0.2054185383,         0.2996835268,         0.2607935268,        -0.8248877259,         0.6347405864,         1.8022087572,         0.4010159206,         0.3151420272,         1.8537585878,         0.2680461860;
    0.2004375501,         0.2975306250,         0.2622472768,        -0.7703500502,         0.6570231436,         1.8551341287,         0.3907967943,         0.3188567456,         1.8853978086,         0.2689890109;
    0.1957814591,         0.2950114732,         0.2636057143,        -0.7022903461,         0.6789441434,         1.9071500360,         0.3811340298,         0.3224001453,         1.9174815742,         0.2700569937;
    0.1914939988,         0.2921369196,         0.2648727232,        -0.6210021473,         0.7004316597,         1.9580968370,         0.3720595120,         0.3257723384,         1.9499539140,         0.2712441513;
    0.1876194131,         0.2889178125,         0.2660521429,        -0.5268763702,         0.7214129332,         2.0078108516,         0.3636056179,         0.3289735423,         1.9827561825,         0.2725439421;
    0.1842005207,         0.2853649554,         0.2671478125,        -0.4204066957,         0.7418176743,         2.0561329296,         0.3558027762,         0.3320044063,         2.0158346536,         0.2739492069;
    0.1812799048,         0.2814892411,         0.2681637054,        -0.3021864765,         0.7615760346,         2.1029013785,         0.3486806778,         0.3348658959,         2.0491310454,         0.2754528230;
    0.1788983800,         0.2773014732,         0.2691036607,        -0.1729120091,         0.7806212227,         2.1479609569,         0.3422660834,         0.3375592377,         2.0825928171,         0.2770477116;
    0.1770957572,         0.2728125000,         0.2699715625,        -0.0333791734,         0.7988881994,         2.1911576249,         0.3365833873,         0.3400859230,         2.1161653634,         0.2787275202;
    0.1759108160,         0.2680331250,         0.2707712500,         0.1155174893,         0.8163137261,         2.2323391904,         0.3316541134,         0.3424475872,         2.1497927684,         0.2804870397;
    0.1753799196,         0.2629741964,         0.2715066518,         0.2727836001,         0.8328387284,         2.2713605201,         0.3274949775,         0.3446463166,         2.1834206180,         0.2823225644;
    0.1755371479,         0.2576465179,         0.2721815625,         0.4373285600,         0.8484080692,         2.3080845929,         0.3241176379,         0.3466843604,         2.2169979555,         0.2842322671;
    0.1764145109,         0.2520609821,         0.2727999554,         0.6079694254,         0.8629701857,         2.3423788136,         0.3215287603,         0.3485643735,         2.2504701343,         0.2862168111;
    0.1780428922,         0.2462283929,         0.2733656696,         0.7834356102,         0.8764754800,         2.3741132516,         0.3197307811,         0.3502889336,         2.2837793146,         0.2882796271;
    0.1804479928,         0.2401595982,         0.2738825893,         0.9623639472,         0.8888832408,         2.4031779325,         0.3187176800,         0.3518612797,         2.3168783281,         0.2904266878;
    0.1836554531,         0.2338654018,         0.2743545536,         1.1433153619,         0.9001529037,         2.4294599746,         0.3184806250,         0.3532844448,         2.3497090759,         0.2926672384;
    0.1876869117,         0.2273566518,         0.2747854911,         1.3247687398,         0.9102507779,         2.4528607587,         0.3190042384,         0.3545619379,         2.3822167642,         0.2950133115;
    0.1925600946,         0.2206441964,         0.2751792857,         1.5051273002,         0.9191498936,         2.4732968062,         0.3202673217,         0.3556975642,         2.4143512908,         0.2974794887;
    0.1982925889,         0.2137388839,         0.2755397321,         1.6827312147,         0.9268235640,         2.4906830746,         0.3222472958,         0.3566948266,         2.4460506698,         0.3000831702;
    0.2048953558,         0.2066514732,         0.2758708036,         1.8558475284,         0.9332564546,         2.5049605853,         0.3249146424,         0.3575580920,         2.4772660390,         0.3028434293;
    0.2123798001,         0.1993929018,         0.2761763393,         2.0226915675,         0.9384325215,         2.5160670804,         0.3282405632,         0.3582912658,         2.5079325729,         0.3057815305;
    0.2207515173,         0.1919739286,         0.2764601786,         2.1814167489,         0.9423456806,         2.5239648039,         0.3321917920,         0.3588987882,         2.5379980253,         0.3089196170;
    0.2300139457,         0.1844054018,         0.2767262500,         2.3301287930,         0.9449935776,         2.5286232888,         0.3367348990,         0.3593852052,         2.5674024716,         0.3122809476;
    0.2401681390,         0.1766981696,         0.2769784375,         2.4668886348,         0.9463779760,         2.5300220042,         0.3418366396,         0.3597550573,         2.5960820341,         0.3158894518;
    0.2512111625,         0.1688630804,         0.2772205804,         2.5897131226,         0.9465074949,         2.5281580649,         0.3474630662,         0.3600130960,         2.6239763367,         0.3197691020;
    0.2631382091,         0.1609108929,         0.2774565625,         2.6965838397,         0.9453939986,         2.5230361557,         0.3535818648,         0.3601640799,         2.6510178657,         0.3239441211;
    0.2759404815,         0.1528525446,         0.2776903125,         2.7854455599,         0.9430562113,         2.5146785946,         0.3601608103,         0.3602130678,         2.6771395578,         0.3284383205;
    0.2896078810,         0.1446988393,         0.2779256250,         2.8542155978,         0.9395151298,         2.5031151095,         0.3671701942,         0.3601648667,         2.7022682153,         0.3332754331;
    0.3041263947,         0.1364605357,         0.2781664286,         2.9007802178,         0.9347984826,         2.4883926999,         0.3745807667,         0.3600246721,         2.7263314836,         0.3384785407;
    0.3194803482,         0.1281485714,         0.2784165625,         2.9230019567,         0.9289368864,         2.4705677932,         0.3823655340,         0.3597975350,         2.7492491730,         0.3440703914;
    0.3356522006,         0.0000000000,         0.2786799554,         2.9187202390,         0.9219641969,         2.4497059575,         0.3904995030,         0.3594885794,         4.7373378399,         0.3500734312;
    0.3526219727,         0.0000000000,         0.2789604464,         2.7062438293,         0.9139184859,         2.4258864403,         0.3989589995,         0.3591029105,         4.6195746797,         0.3565094982;
    0.3703671015,         0.0000000000,         0.2792619643,         2.4683887336,         0.9048422891,         2.3992015632,         0.4077214166,         0.3586458320,         4.4984302863,         0.3633998151;
    0.3888646606,         0.0000000000,         0.2795883036,         2.2031914753,         0.8947788186,         2.3697488996,         0.4167666163,         0.3581223155,         4.3742363503,         0.3707655196;
    0.4080888304,         0.0000000000,         0.2799434375,         1.9086073624,         0.8837762809,         2.3376402959,         0.4260747876,         0.3575376941,         4.2473410378,         0.3786270085;
    0.4280133674,         0.0000000000,         0.2803311607,         1.5825132206,         0.8718836639,         2.3029939642,         0.4356279477,         0.3568969624,         4.1181019543,         0.3870044825;
    0.4486111871,         0.0000000000,         0.2807554018,         1.2227032210,         0.8591514489,         2.2659339185,         0.4454094046,         0.3562051640,         3.9868774707,         0.3959179829;
    0.4698539461,         0.0000000000,         0.2812200000,         0.8268848585,         0.8456323258,         2.2265938583,         0.4554030792,         0.3554672459,         3.8540380911,         0.4053870296;
    0.4917119543,         0.0000000000,         0.2817288839,         0.3926756580,         0.8313813420,         2.1851161827,         0.4655932155,         0.3546882726,         3.7199597850,         0.4154305678;
    0.5141557640,         0.0000000000,         0.2822858482,        -0.0823973457,         0.8164531932,         2.1416466591,         0.4759652095,         0.3538729800,         3.5850185439,         0.4260673787];

g_cm_save = steady_state(:,1);
g_hm_save = steady_state(:,2);
g_hh_save = steady_state(:,3);
g_km_save = [steady_state(:,4); 0];
g_kh_save = [steady_state(:,5); g_kh0];
g_psi     = steady_state(:,7);
g_omega   = steady_state(:,9);
g_ratio_star = 1.76943;

guess(1:2) = [log(g_eta)-log(1-g_eta); g_beta];

if (g_i_calibrate == 2) then
  guess(3:4) = [g_delta_m; g_delta_h];
end

if (g_i_calibrate == 4) then
  guess(3:4) = [g_omega(1); g_psi(1)];
end

g_cm_star = zeros(g_T,1);
g_hm_star = zeros(g_T,1);
g_hh_star = zeros(g_T,1);
g_km_star = zeros(g_T+1,1);
g_kh_star = zeros(g_T+1,1);

// Calibrate key model parameters (and solve for steady state)

guess = fsolve(guess, CALIBRATE_PARAMETERS);
print(%io(2), 'Calibration finished.');

g_eta = exp(guess(1))/(1+exp(guess(1)));
g_beta = guess(2);
if (g_i_calibrate == 2) then
  g_delta_m = guess(3);
  g_delta_h = guess(4);
end
if (g_i_calibrate == 4) then
  g_omega = guess(3)*ones(g_T,1);
  g_psi = guess(4)*ones(g_T,1);
end
g_beta_star = g_beta / g_growth;

g_cm_star = g_cm_save;
g_hm_star = g_hm_target;
g_hh_star = g_hh_target;
g_km_star = g_km_save;
g_kh_star = g_kh_save;

// Solve for first-order linear approximations of decision rules

x_bar=[g_zm_bar; g_km_save(2:g_T); g_zh_bar; g_kh_save(2:g_T); g_cm_save;g_hm_target(1:g_NW); g_hh_target];
x_bar=[x_bar;x_bar];
[g_Mcs,g_Mss,grad,info] = SCHUR_FIRST_ORDER(x_bar, LOG_LIN, g_NS);
print(%io(2), 'Solved for decision rules.');

g_NLAG = 4;
g_NAG = 24;

sstrg = ['Output'; 'Consumption'; '- Market'; '- Home';
    'Investment'; '- Market'; '- Home'; 
    'Hours'; '- Market'; '- < 25'; '- 25-34'; '- 35-44'; '- 45-54'; '- 55-64'; '- last 5';
    '- Home'; '- < 25'; '- 25-34'; '- 35-44'; '- 45-54'; '- 55-64'; '- last 5';
    'Productivity'; 'Capital'; '- Market'; '- Home'; 'Real Wage'; 'Rental Rate'];

hpmat = HPMATRIX(g_NOBS, 100);

current_state = zeros(g_NS,1);
future_state = zeros(g_NS,1);
control = zeros(g_NC,1);
raw_data = zeros(g_NOBS,g_NAG);

x_mean = zeros(g_NAG,1);
x_sd = zeros(g_NAG,1);
x_corr = zeros(g_NAG,g_NAG);
x_autocorr = zeros(g_NAG,1);
x_corrlag = zeros(g_NAG,2*g_NLAG+1);

g_ae = chol(g_ecorr)';

for i_count = 1:g_NREP
   mprintf('%i\r', i_count);
   for i = 1:50
      [control,current_state,future_state] = SIMULATE(future_state, 1);
      raw_data(1,:) = SIM_AGG(control, current_state, future_state);
   end
   for i = 1:g_NOBS
      [control,current_state,future_state] = SIMULATE(future_state, 1);
      raw_data(i,:)= SIM_AGG(control, current_state, future_state);
   end
   raw_data = log(raw_data);
   hp_data = raw_data - hpmat*raw_data;
   [tmean,tvar,tac,tcorr] = CALCULATE_MOMENTS(hp_data);
   [tcorrlag] = LEAD_LAG(hp_data, 1, g_NLAG);
   x_mean = x_mean+tmean;
   x_sd = x_sd+sqrt(tvar);
   x_corr = x_corr + tcorr;
   x_autocorr = x_autocorr + tac;
   x_corrlag = x_corrlag + tcorrlag;
end
x_mean = x_mean / g_NREP;
x_sd = x_sd / g_NREP;
x_corr = x_corr / g_NREP;
x_autocorr = x_autocorr / g_NREP;
x_corrlag = x_corrlag / g_NREP;

printf('\n\nStandard deviation and cross-correlation with output\n');
printf('%-20s  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f\n', sstrg, 100*x_sd, x_corrlag);
