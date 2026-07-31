function results = fuel_pin_1D_TH(L, Nz, q_prime, params)
% ===============================================================
% 1D Steady-State Thermal-Hydraulics Solver for a Fuel Pin
%
% INPUTS:
%   z        : axial position vector (m)
%   q_prime  : linear heat rate profile (W/m)
%   params   : structure containing geometry & properties
%
% OUTPUT:
%   results  : structure containing temperature and density fields
%
% ===============================================================

%% ------------------------
% Extract Parameters
% ------------------------

R_f      = params.R_f;
R_ci     = params.R_ci;
R_co     = params.R_co;
m_dot    = params.m_dot;
cp       = params.cp;
rho_ref  = params.rho_ref;
T_ref    = params.T_ref;
beta     = params.beta;
mu       = params.mu;
k_cool   = params.k_cool;
T_inlet  = params.T_inlet;
pitch    = params.pitch;
resistivity = params.resistivity;

%% ------------------------
% Preliminaries
% ------------------------

dz = L / Nz;

Pr = cp * mu / k_cool;

T_bulk         = zeros(1,Nz);
T_clad_outer   = zeros(1,Nz);
T_fuel_surface = zeros(1,Nz);
T_center       = zeros(1,Nz);
rho            = zeros(1,Nz);
% Cell-centered quantities
T_bulk_avg = zeros(1, Nz);

T_bulk(1) = T_inlet;
rho(1)    = rho_ref;

%% ------------------------
% Hydraulics
% ------------------------


A_flow = pitch^2 - pi * R_co^2;
D_h = 4 * A_flow / (pi * 2 * R_co);

velocity = m_dot/(rho_ref*A_flow);
Re = rho_ref*velocity*D_h/mu;

h = 0.023*(k_cool/D_h)*Re^0.8*Pr^0.4;

% ---------------------------------
% Axial Finite-Volume March
% ---------------------------------

% Face temperatures (Nz+1 faces)
T_bulk_face = zeros(1, Nz+1);
T_bulk_face(1) = T_inlet;


for i = 1:Nz

    % --- Energy balance (conservative) ---
    T_bulk_face(i+1) = T_bulk_face(i) + ...
        q_prime(i)*dz/(m_dot*cp);

    % --- Cell-average bulk temperature ---
    T_bulk_avg(i) = 0.5*(T_bulk_face(i) + T_bulk_face(i+1));

    T_bulk(i) = T_bulk_avg(i);
    % --- Cell-average density ---
    rho(i) = rho_ref*(1 - beta*(T_bulk_avg(i) - T_ref));

    % --- Hydraulics at cell-average state ---
    velocity = m_dot/(rho(i)*A_flow);
    Re = rho(i)*velocity*D_h/mu;
    h = 0.023*(k_cool/D_h)*Re^0.8*Pr^0.4;

    % --- Convection (cell centered) ---
    T_clad_outer(i) = T_bulk_avg(i) + ...
        q_prime(i)/(2*pi*R_co*h);

    % --- Cladding conduction ---
    %deltaT_clad = q_prime(i)/(2*pi*k_clad) * ...
    %    log(R_co/R_ci);
    deltaT_clad = q_prime(i) * resistivity / (2 * pi * R_co);

    T_fuel_surface(i) = T_clad_outer(i) + deltaT_clad;

    % --- Fuel conduction ---
    %q_vol = q_prime(i)/(pi*R_f^2);
    %deltaT_fuel = q_vol*R_f^2/(4*k_f);

    %T_center(i) = T_fuel_surface(i) + deltaT_fuel;

    % Define integral of k(T)
    Ik = @(T) conductivity_integral(T);   % <-- YOU provide this

    % RHS of equation
    rhs = q_prime(i)/(4*pi);

    % Solve Ik(T_center) - Ik(T_surface) = rhs
    f = @(Tc) Ik(Tc) - Ik(T_fuel_surface(i)) - rhs;

    % Initial guess (important!)
    Tc_guess = T_fuel_surface(i) + 500;  % crude but usually safe

    T_center(i) = fzero(f, Tc_guess);
	
end

rho_rel = rho / rho_ref;

% ------------------------
% Package Outputs
% ------------------------

results.T_center       = T_center;
results.T_fuel_surface = T_fuel_surface;
results.T_fuel         = 4/9 * T_center + 5/9 * T_fuel_surface;
results.T_clad_outer   = T_clad_outer;
results.T_bulk         = T_bulk;
results.rho            = rho;
results.rho_rel        = rho_rel;

end

%
% From Todreas and Kazimi
%
function kdt = conductivity_integral(T)


	% First convert kelvin to farenheit
	Tf = T * 9/5 - 459.67;

	% Then evalute kdt
	kdt = -170.9124 + 5.597256 * Tf - 3.368695 * 1E-3 * Tf^2 ...
		+ 1.962784 * 1E-6 * Tf^3 - 8.391225 * 1E-10 * Tf^4 ...
		+ 2.404192 * 1E-13 * Tf^5 - 4.275284 * 1E-17 * Tf^6 ...
		+4.249043 * 1E-21 * Tf^7 - 1.797017 * 1E-25 * Tf^8;


	% Then convert from Btu/hr ft to W/m
	kdt = kdt * 0.961519259;

end
