function results = single_channel_TH_TM(L, Nz, q_prime, params)
    % ===============================================================
    % 1D Steady-State Thermal-Hydraulics Thermal-Mechanics Solver for a Fuel Pin
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
    
    %% -----------------------------
    % Geometry
    % -----------------------------
    
    Ro = params.R_co;
    
    [Nx, Ny] = size(q_prime);

    P_D = params.pitch ./ (2*Ro);
    A_flow = params.pitch^2 - pi*Ro.^2;
    D_h = 4*A_flow ./ (2*pi*Ro);
    dz  = L / Nz;

    pitch = params.pitch;   % m

    [xg,yg] = meshgrid((0.5:Nx-0.5)*pitch,(0.5:Ny-0.5)*pitch);

    %% ============================================================
    % Material properties
    % ============================================================

    alpha = 0.8e-5;     % 1/K
    Tref  = 298;
 
    %% =========================================================
    % 1. TH: Coolant temperature
    % =========================================================

    cp   = params.cool_cp(params.T_inlet);
    dTdz = q_prime / (params.m_dot * cp);
    Tc   = params.T_inlet + dTdz * dz / 2;  % channel coolant temperature is avg inlet-outlet

    %% =========================================================
    % 2. Radial heat transfer (local)
    % =========================================================

    % Flow properties
    rho = params.cool_rho(Tc);
    mu  = params.cool_mu(Tc);
    cp  = params.cool_cp(Tc);
    k   = params.cool_k(Tc);

    v  = params.m_dot ./ (rho .* A_flow);
    Re = rho .* v .* D_h ./ mu;
    Pr = mu .* cp ./ k;
    Pe = Re .* Pr;
    Nu = Nu_hex(Pe, P_D);

    % Heat transfer coefficient
    h = Nu .* k ./ D_h;

    % Current geometry
    rf  = params.R_f;
    rci = params.R_ci;
    rco = params.R_co;

    % Outer cladding
    Tco = Tc + q_prime ./ (2*pi*rco*h);

    % Inner cladding
    Tci = Tco + q_prime * log(rco/rci) ./ (2*pi*params.clad_k(Tco));

    % Fuel surface
    hg  = params.gap_h(Tci, rf, rci);
    Tfs = Tci + q_prime ./ (2*pi*rf*hg);

    % Fuel centerline
    Tfc = Tfs + q_prime ./ (4*pi*params.fuel_k(Tfs));

    % Average fuel temperature
    Tf = 4/9 * Tfc + 5/9 * Tfs;

    %% =========================================================
    % 3. Thermo-mechanics (radial displacement)
    % =========================================================

    Xnew(:,Nx) = xg(:,Nx);

    for i = Nx-1:-1:1

        Tedge = 0.5*(Tc(:,i)+Tc(:,i+1));

        Pedge = pitch .* (1 + alpha*(Tedge-Tref));

        Xnew(:,i) = Xnew(:,i+1) - Pedge;

    end

    Ynew(1,:) = yg(1,:);

    for j = 2:Ny

        Tedge = 0.5*(Tc(j,:)+Tc(j-1,:));

        Pedge = pitch .* (1 + alpha*(Tedge-Tref));

        Ynew(j,:) = Ynew(j-1,:) + Pedge;

    end

    %% Output results
    results.T_fuel   = Tf;
    results.T_bulk   = Tc;
    results.rho      = params.cool_rho(Tc);
    results.rho_rel  = results.rho / params.rho_ref;
    results.Xnew     = Xnew;
    results.Ynew     = Ynew;

    %% -----------------------------
    % Correlations
    % -----------------------------
    function Nu = Nu_hex(Pe, PD)

        if PD >= 1.2
            Nu = 4 + 0.33 * (PD^3.8) .* (Pe/100).^0.86 + 0.16 * PD * 5;
        elseif Pe > 300
            Nu = (-16.15 + 24.96*PD - 8.55*PD^2) .* Pe.^0.3;
        else
            Nu = 4.496 * (-16.15 + 24.96*PD - 8.55*PD^2);
        end

    end

end
