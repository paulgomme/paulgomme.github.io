% First, run R on : hangover.r

raw = csvread('us-quarterly.csv');

yr = raw(:,1);

pl = 8;
pu = 100;

y = raw(:,2);
y_bp = bpass(y, pl, pu, 1);
y_dt = bpass(y, 2, pu, 1);

c = raw(:,3);
c_bp = bpass(c, pl, pu, 1);
c_dt = bpass(c, 2, pu, 1);

h = raw(:,4);
h_bp = bpass(h, pl, pu, 1);
h_dt = bpass(h, 2, pu, 1);

x = raw(:,5);
x_bp = bpass(x, pl, pu, 1);
x_dt = bpass(x, 2, pu, 1);

g = raw(:,6);
g_bp = bpass(g, pl, pu, 1);
g_dt = bpass(g, 2, pu, 1);

debt = raw(:,7);
debt_bp = debt;
debt_dt = debt;

validIndices = ~isnan(debt);
xxx = debt(validIndices);

debt_bp(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = bpass(xxx, pl, pu, 1);
debt_dt(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = bpass(xxx, 2, pu, 1);

surplus = raw(:,8);
surplus_bp = surplus;
surplus_dt = surplus;

validIndices = ~isnan(surplus);
xxx = surplus(validIndices);

surplus_bp(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = bpass(xxx, pl, pu, 1);
surplus_dt(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = bpass(xxx, 2, pu, 1);

real_wage = raw(:,9);
real_wage_bp = real_wage;
real_wage_dt = real_wage;

validIndices = ~isnan(real_wage);
xxx = log(real_wage(validIndices));

real_wage_bp(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = bpass(xxx, pl, pu, 1);
real_wage_dt(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = bpass(xxx, 2, pu, 1);

quarter_tau_n = raw(:,10);
quarter_tau_c = raw(:,11);
quarter_tau_k = raw(:,12);

quarter_solow_residual = raw(:,13);
quarter_solow_residual_bp = quarter_solow_residual;
quarter_solow_residual_dt = quarter_solow_residual;
validIndices = ~isnan(quarter_solow_residual);
xxx = log(quarter_solow_residual(validIndices));

quarter_solow_residual_bp(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = bpass(xxx, pl, pu, 1);
quarter_solow_residual_dt(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = bpass(xxx, 2, pu, 1);



dlmwrite('us-quarterly.txt', [yr y 100*y_bp 100*y_dt ...
                    c 100*c_bp 100*c_dt ...
                    h 100*h_bp 100*h_dt ...
                    x 100*x_bp 100*x_dt ...
                    g 100*g_bp 100*g_dt ...
                    debt debt_bp debt_dt ...
                    100*surplus surplus_bp surplus_dt ...
                    real_wage real_wage_bp real_wage_dt ...
                    quarter_tau_n quarter_tau_c quarter_tau_k ...
                    quarter_solow_residual quarter_solow_residual_bp quarter_solow_residual_dt], ...
         'delimiter', '\t','precision','%.6f')

dlmwrite('us-quarterly.dat', [yr y_bp g_bp], 'delimiter', '\t','precision','%.10f')

mraw = csvread('us-monthly.csv');

tau_lump = mraw(:,23);
validIndices = ~isnan(tau_lump);
tau_lump = log(tau_lump(validIndices));

xxx = bpass(tau_lump, pl*3, pu*3, 1);
tau_lump_bp = mraw(:,7);
tau_lump_bp(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = xxx;

xxx = bpass(tau_lump, 2, pu*3, 1);
tau_lump_dt = mraw(:,7);
tau_lump_dt(find(validIndices, 1, 'first'):find(validIndices, 1, 'last')) = xxx;

dlmwrite('us-monthly.txt', [mraw tau_lump_bp tau_lump_dt], 'delimiter', '\t','precision','%.10f');
dlmwrite('us-monthly.dat', [mraw(:,1) mraw(:,4:7) mraw(:,9) mraw(:,11) ...
                    1000000*mraw(:,13) tau_lump_bp], 'delimiter', '\t','precision','%.10f');
