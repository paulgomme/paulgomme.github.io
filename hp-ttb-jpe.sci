// Code to solve "Home Production Meets Time-to-Build" by Gomme, Kydland
// and Rupert. Coding by Paul Gomme. All rights reserved.
//
// Coding by Paul Gomme, Concordia University
//
// Code written in Scilab: http://www.scilab.org
//

clear;
clearglobal;

function [retval] = AGGREGATOR(x, alpha, sigma)
   if (sigma == 0) then
      [x_nr,x_nc] = size(x);
      retval = prod((x.*(x>0)).^(ones(x_nr,1).*.alpha),2);
   elseif (sigma < 0) then
      retval = prod((x>0)')'.*real(((x.*(x>0)).^sigma*alpha').^(1/sigma));
   else
      retval = ((x.*(x>0)).^sigma*alpha').^(1/sigma);
   end
endfunction

function [retval] = DAGGREGATOR(x, alpha, sigma, i)
   if (sigma == 0) then
      [x_nr,x_nc] = size(x);
      retval = prod(real((x.*(x>0)).^(ones(x_nr,1).*.alpha)),2) ...
         * alpha(i).*x(:,i).^(-1) + (x(:,i)<0)*10^20;
   else
      _y = real(((x.*(x>0)).^sigma*alpha').^((1-sigma)/sigma) ...
         *alpha(i).*x(:,i).^(sigma-1))
      if (sigma < 0) then
         retval = _y.*prod((x>0)')' + (x(:,i)<0)*alpha(i)^(1/sigma);
      else
         retval = _y + (x(:,i)<=0)*10^20;
      end
    end
endfunction

function [u] = U(cm, ch, nm, nh)
cons = C(cm,ch);
leisure = 1.0 - nm - nh;
if (g_gamma == 1) then
  _y = g_omega*log(cons)+(1.0-g_omega)*log(leisure);
  u = real(_y) -10^10*(imag(_y)>0);
else
  _y = (cons.^g_omega .* leisure.^(1.0-g_omega)).^(1-g_gamma)/(1-g_gamma);
  u = real(_y) - 10^10*(imag(_y)>0);
end
endfunction

function [u1] = U1(cm, ch, nm, nh)
cons = C(cm,ch);
leisure = 1.0 - nm - nh;
if (g_gamma == 1) then
  u1 = real(g_omega.*C1(cm,ch)./cons + 10^10*(cm<=0));
else
  u1 = real((cons.^g_omega .* leisure.^(1.0-g_omega)).^(1-g_gamma) ...
      .* g_omega.*C1(cm,ch)./cons + 10^10*(cm<=0));
end
endfunction

function [u2] = U2(cm, ch, nm, nh)
cons = C(cm,ch);
leisure = 1.0 - nm - nh;
if (g_gamma == 1) then
  u2 = real(g_omega.*C2(cm,ch)./cons + 10^10*(ch<=0));
else
  u2 = real((cons.^g_omega .* leisure.^(1.0-g_omega)).^(1-g_gamma) ...
      .* g_omega.*C2(cm,ch)./cons + 10^10*(ch<=0));
end
endfunction

function [u3] = U3(cm, ch, nm, nh)
cons = C(cm,ch);
leisure = 1.0 - nm - nh;
if (g_gamma == 1) then
  u3 = real(-(1.0-g_omega)./leisure + 10^10*(leisure<=0));
else
  u3 = real((cons.^g_omega .* leisure.^(1.0-g_omega)).^(1-g_gamma) ...
      .* (1.0-g_omega)./leisure + 10^10*(leisure<=0));
end
endfunction

function [u4] = U4(cm, ch, nm, nh)
cons = C(cm,ch);
leisure = 1.0 - nm - nh;
if (g_gamma == 1) then
  u4 = real(-(1.0-g_omega)./leisure + 10^10*(leisure<=0));
else
  u4 = real((cons.^g_omega .* leisure.^(1.0-g_omega)).^(1-g_gamma) ...
      .* (1.0-g_omega)./leisure + 10^10*(leisure<=0));
end
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
    c = AGGREGATOR([cm,ch],[g_psi,1-g_psi],g_xi);
endfunction

function [c1] = C1(cm, ch)
    c1 = DAGGREGATOR([cm,ch],[g_psi,1-g_psi],g_xi,1);
endfunction

function [c2] = C2(cm, ch)
    c2 = DAGGREGATOR([cm,ch],[g_psi,1-g_psi],g_xi,2);
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

  grad = numdiff(LOGLIN,x_bar);

   N = size(x_bar,1)/2;

   B = -grad(:,N+1:2*N);
   A = grad(:,1:N);

   [As,Bs,Z,dim]=schur(A,B,z_choose);

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

rand('seed')
rand('normal');

function [ctrl_out, cur_st_out, fut_st_out] = SIMULATE(state_in, f_stochastic)
   cur_st_out = state_in;
   ctrl_out = g_Mcs*cur_st_out;
   fut_st_out = g_Mss*cur_st_out;
   if (f_stochastic) then
      fut_st_out(1:2) = fut_st_out(1:2) + g_sd_shock.*(g_ae*rand(2,1));
   end
endfunction

function [dat_out] = SIM_AGG(control, current_state, future_state)
zm = (1.0+current_state(1))*g_zm_bar;
zh = (1.0+current_state(2))*g_zh_bar;
km = (1.0+current_state(3))*g_km_bar;
kh = (1.0+current_state(4))*g_kh_bar;
s(1:g_TTB-1) = (1.0+current_state(5:3+g_TTB)).*g_s_bar(1:g_TTB-1);
cm = (1.0+control(1))*g_cm_bar;
ch = (1.0+control(2))*g_ch_bar;
hm = (1.0+control(3))*g_hm_bar;
hh = (1.0+control(4))*g_hh_bar;
s(g_TTB) = (1.0+control(5))*g_s_bar(g_TTB);

km_prime = (1.0+future_state(3))*g_km_bar;
kh_prime = (1.0+future_state(4))*g_kh_bar;

k_prime = km_prime+kh_prime;
c = C(cm,ch);
xm = g_phi'*s;
xh = kh_prime - (1.0-g_delta_h)*kh;
x = xm+xh;
hrs = hm+hh;
y = F(km,hm,zm);
prdy = y/hm;

dat_out = [ y, c, cm, ch, x, xm, xh, hrs, hm, hh, prdy, k_prime, km_prime, kh_prime ]
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
   ac = (sum(series(1:g_NOBS-1,:).*series(2:g_NOBS,:),'r')'/(g_NOBS-1) ...
        - mean1.*mean2) ./ sqrt(var1.*var2);
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

function [f] = BGFCN(x)
   global g_omega g_psi g_eta g_alpha g_delta_m g_delta_h;

   km = x(1);
   kh = x(2);
   hm = x(3);
   hh = x(4);
   cm = x(5);
   ch = x(6);
   lm = x(7:6+g_TTB);
   g_omega  = x(7+g_TTB);
   g_psi    = x(8+g_TTB);
   g_eta    = x(9+g_TTB);
   g_alpha  = x(10+g_TTB);
   g_delta_m = x(11+g_TTB);
   g_delta_h = x(12+g_TTB);
 
   f(1) = cm + g_delta_m*km + g_delta_h*kh - F(km,hm,g_zm_bar);
   f(2) = H(kh,hh,g_zh_bar) - ch;
   f(3) = U1(cm,ch,hm,hh)*F2(km,hm,g_zm_bar)*(1.0-g_tau_h) + U3(cm,ch,hm,hh)
   f(4) = U2(cm,ch,hm,hh)*H2(kh,hh,g_zh_bar) + U4(cm,ch,hm,hh);
   f(5) = g_beta*(U1(cm,ch,hm,hh)*(F1(km,hm,g_zm_bar)*(1.0-g_tau_k) ...
       + g_delta_m*g_tau_k) + lm(1)*(1.0-g_delta_m)) - lm(1);
   f(6) = g_beta*U2(cm,ch,hm,hh)*H1(kh,hh,g_zh_bar) ...
       - (1.0-(1.0-g_delta_h)*g_beta)*U1(cm,ch,hm,hh)
   for i=1:g_TTB-1
     f(6+i) = g_beta*(-U1(cm,ch,hm,hh)*g_phi(i) + lm(i)) - lm(i+1);
   end
   f(6+g_TTB) = g_phi(g_TTB)*U1(cm,ch,hm,hh) - lm(g_TTB);
   f(7+g_TTB) = hm - 0.33d0
   f(9+g_TTB) = g_km_y_ratio*F(km,hm,g_zm_bar) - km
   f(10+g_TTB) = g_kh_y_ratio*F(km,hm,g_zm_bar) - kh
   f(11+g_TTB) = g_xm_y_ratio*F(km,hm,g_zm_bar) - g_delta_m*km
   f(12+g_TTB) = g_xh_y_ratio*F(km,hm,g_zm_bar) - g_delta_h*kh
endfunction

function [f] = LOG_LIN(x)
   zm = exp(x(g_N+1));
   zh = exp(x(g_N+2));
   km = exp(x(g_N+3));
   kh = exp(x(g_N+4));
   s(1:g_TTB-1) = exp(x(g_N+5:g_N+g_TTB+3));
   cm = exp(x(g_N+g_NS+1));
   ch = exp(x(g_N+g_NS+2));
   hm = exp(x(g_N+g_NS+3));
   hh = exp(x(g_N+g_NS+4));
   s(g_TTB) = exp(x(g_N+g_NS+5));
   lm = exp(x(g_N+g_NS+6:2*g_N));

   zm_prime = exp(x(1));
   zh_prime = exp(x(2));
   km_prime = exp(x(3));
   kh_prime = exp(x(4));
   s_prime(1:g_TTB-1) = exp(x(5:g_TTB+3));
   cm_prime = exp(x(g_NS+1));
   ch_prime = exp(x(g_NS+2));
   hm_prime = exp(x(g_NS+3));
   hh_prime = exp(x(g_NS+4));
   s_prime(g_TTB) = exp(x(g_NS+5));
   lm_prime = exp(x(g_NS+6:g_N));


   f(1) = (1.0-g_tau_h)*F2(km,hm,zm)*U1(cm,ch,hm,hh) + U3(cm,ch,hm,hh);
   f(2) = H2(kh,hh,zh)*U2(cm,ch,hm,hh) + U4(cm,ch,hm,hh);
   f(3) = g_phi(g_TTB)*U1(cm,ch,hm,hh) - lm(g_TTB);
   f(4) = g_beta*(U1(cm_prime,ch_prime,hm_prime,hh_prime) ...
       *((1.0-g_tau_k)*F1(km_prime,hm_prime,zm_prime) + g_delta_m*g_tau_k) ...
       + lm_prime(1)*(1.0-g_delta_m)) - lm(1);
   f(5) = g_beta*(U1(cm_prime,ch_prime,hm_prime,hh_prime)*(1.0-g_delta_h) ...
       + U2(cm_prime,ch_prime,hm_prime,hh_prime)*H1(kh_prime,hh_prime,zh_prime)) ...
       - U1(cm,ch,hm,hh);
   f(6) = F(km,hm,zm) + (1.0-g_delta_h)*kh - cm - kh_prime - g_phi'*s;
   f(7) = (1.0-g_delta_m)*km + s(1) - km_prime;
   f(8) = H(kh,hh,zh) - ch;
   f(9:7+g_TTB) = s(2:g_TTB) - s_prime(1:g_TTB-1);
   f(8+g_TTB:6+2*g_TTB) = g_beta*(-U1(cm_prime,ch_prime,hm_prime,hh_prime)*g_phi(1:g_TTB-1) ...
       + lm_prime(1:g_TTB-1)) - lm(2:g_TTB);
   f(7+2*g_TTB) = log(zm_prime) - g_rho_m*log(zm);
   f(8+2*g_TTB) = log(zh_prime) - g_rho_h*log(zh);
endfunction

global g_alpha g_omega g_gamma g_rho g_hbar g_beta g_delta g_sd_shock g_xi;

g_alpha = 0.33;
g_omega = 0.33;
g_gamma = 1.0;
g_rho   = 0.95;
g_hbar  = 0.2869;
g_beta  = 0.985538362;
g_delta_m = 0.03;
g_delta_h = 0.025;
g_xi    = 0.0;
g_psi   = 0.5;
g_rho_m = 0.95;
g_rho_h = 0.95;
g_eta = 0.3;
g_zeta = 0.0;
g_tau_k = 0.7;
g_tau_h = 0.25;
g_km_y_ratio = 4;
g_kh_y_ratio = 5;
g_xm_y_ratio = 0.118;
g_xh_y_ratio = 0.135;
g_TTB = 4;
g_phi = ones(g_TTB,1)/g_TTB;
g_sd_shock = [0.00763; 0.00763];
g_ecorr = [1, 2/3; 2/3, 1];
g_zm_bar = 1.0;
g_zh_bar = 1.0;

g_NS = 3+g_TTB;
g_NC = 5+g_TTB;
g_N = g_NS+g_NC;

x = [.99; 1.385; .125; .128; .183; .262; ones(g_TTB,1); g_omega; g_psi; g_eta; g_alpha; g_delta_m; g_delta_h];
x = fsolve(x, BGFCN);

print(%io(2), x);

g_km_bar = x(1);
g_kh_bar = x(2);
g_hm_bar = x(3);
g_hh_bar = x(4);
g_cm_bar = x(5);
g_ch_bar = x(6);
g_lm_bar = x(7:6+g_TTB);
g_omega  = x(7+g_TTB);
g_psi    = x(8+g_TTB);
g_eta    = x(9+g_TTB);
g_alpha  = x(10+g_TTB);
g_delta_m = x(11+g_TTB);
g_delta_h = x(12+g_TTB);

g_s_bar = ones(g_TTB,1)*g_delta_m*g_km_bar;

print(%io(2), g_km_bar, g_kh_bar, g_hm_bar, g_hh_bar, g_cm_bar, g_ch_bar);
print(%io(2), g_omega, g_psi, g_eta, g_alpha, g_delta_m, g_delta_h);

x_bar = [ g_zm_bar; g_zh_bar; g_km_bar; g_kh_bar; g_s_bar(1:g_TTB-1); ...
	g_cm_bar; g_ch_bar; g_hm_bar; g_hh_bar; g_s_bar(g_TTB); g_lm_bar ];
x_bar = log(x_bar);
x_bar = [x_bar; x_bar];


[g_Mcs,g_Mss,g_grad,info] = SCHUR_FIRST_ORDER(x_bar, LOG_LIN, g_NS);

g_NREP = 1000;
g_NOBS = 200;
g_NLAG = 4;
g_NAG = 14;

global g_Mcs g_Mss g_NREP g_NOBS g_NLAG g_NAG;

sstrg = ['Output'; 'Consumption'; '- Market'; 
        '- Home'; 'Investment'; '- Market'; 
        '- Home'; 'Hours'; '- Market'; '- Home'; 
        'Productivity'; 'Capital'; '- Market'; '- Home'];

hpmat = HPMATRIX(g_NOBS, 1600);

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
x_mean = x_mean/g_NREP;
x_sd = x_sd/g_NREP;
x_corr = x_corr/g_NREP;
x_autocorr = x_autocorr/g_NREP;
x_corrlag = x_corrlag / g_NREP;

printf('\n\nStandard deviation and cross-correlation with output\n');
printf('%-20s  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f\n', sstrg, 100*x_sd, x_corrlag);


printf('\nCorrelation: market and home investment: %8.2f\n\n',x_corr(6,7));
