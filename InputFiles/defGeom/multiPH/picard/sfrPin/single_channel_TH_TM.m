function results = single_channel_TH_TM(L, Nz, q, axialExp, params)
    % ===============================================================
    % 1D Steady-State Thermal-Hydraulics Thermal-Mechanics Solver for a Fuel Pin
    %
    % INPUTS:
    %   z        : axial position vector (m)
    %   q        : heat rate profile (W/m)
    %   params   : structure containing geometry & properties
    %
    % OUTPUT:
    %   results  : structure containing temperature and density fields
    %
    % ===============================================================

    %% -----------------------------
    % Initial guesses
    % -----------------------------
    Tc  = zeros(Nz,1);
    Tco = zeros(Nz,1);
    Tci = zeros(Nz,1);
    Tf  = zeros(Nz,1);
    Tfs = zeros(Nz,1);
    Tfc = zeros(Nz,1);

    Rf = params.R_f * ones(Nz+1,1);
    Ri = params.R_ci * ones(Nz+1,1);
    Ro = params.R_co * ones(Nz+1,1);
    
    bondArea = params.R_ci^2 - params.R_f^2;
    cladArea = params.R_co^2 - params.R_ci^2;

    dz = zeros(Nz,1);
    for i = 1:Nz
       dz(i) = L / Nz;
       dz(i) = dz(i) + axialExp * 2 / Nz * (i>Nz/2);
    end

    q_prime = q ./ dz';

    %% -----------------------------
    % Iteration controls
    % -----------------------------
    maxIter = 100;

    for iter = 1:maxIter
        
        %% -----------------------------
        % Geometry
        % -----------------------------
        P_D = params.pitch ./ (2*Ro);
        A_flow = (sqrt(3)/2)*params.pitch^2 - pi*Ro.^2;
        D_h = 4*A_flow ./ (2*pi*Ro);

        Tc_old = Tc;
        Tf_old = Tf;
        Rf_old = Rf;

        %% =========================================================
        % 1. TH: Coolant temperature
        % =========================================================

        Tc(1) = params.T_inlet;

        for i = 1:Nz-1
        
            cp   = params.cool_cp(Tc(i));
            dTdz = q_prime(i) / (params.m_dot * cp);
            Tc(i+1) = Tc(i) + dTdz * dz(i);
       
        end

        %% =========================================================
        % 2. Radial heat transfer (local)
        % =========================================================

        for i = 1:Nz

            T_i = Tc(i);
            
            % Flow properties
            rho = params.cool_rho(T_i);
            mu  = params.cool_mu(T_i);
            cp  = params.cool_cp(T_i);
            k   = params.cool_k(T_i);
            
            v  = params.m_dot / (rho * A_flow(i));
            Re = rho * v * D_h(i) / mu;
            Pr = mu * cp / k;
            Pe = Re * Pr;
            Nu = Nu_hex(Pe, P_D(i));
            
            % Heat transfer coefficient
            h = Nu * k / D_h(i);
            
            % Current geometry
            rf  = Rf(i);
            rci = Ri(i);
            rco = Ro(i);
            
            % Outer cladding
            Tco(i) = T_i + q_prime(i) / (2*pi*rco*h);

            % Inner cladding
            Tci(i) = Tco(i) + q_prime(i) * log(rco/rci) / (2*pi*params.clad_k(Tco(i)));
            
            % Fuel surface
            hg    = params.gap_h(Tci(i), rf, rci);
            Tfs(i) = Tci(i) + q_prime(i) / (2*pi*rf*hg);
            
            % Fuel centerline
            Tfc(i) = Tfs(i) + q_prime(i) / (4*pi*params.fuel_k(Tfs(i)));

        end
        
        % Average fuel temperature
        Tf = 4/9 * Tfc + 5/9 * Tfs;

        %% =========================================================
        % 3. Thermo-mechanics (radial + axial)
        % =========================================================

        beta = params.fuel_beta(Tf);

        % Radial expansion (fuel)
        Rf(2:end) = params.R_f .* (1 + beta .* (Tf - 298));
        Ri = sqrt(Rf.^2 + bondArea);
        Ro = sqrt(Ri.^2 + cladArea);

        % Axial expansion
        epsz = beta .* (Tf - 298);
        length = sum((1 + epsz) .* (L/Nz));

        %% =========================================================
        % 4. Convergence check
        % =========================================================
        errT  = max(abs(Tf - Tf_old));
        errTc = max(abs(Tc - Tc_old));
        errR  = max(abs(Rf - Rf_old));

        if errT < 0.1 && errTc < 0.1 && errR < 1e-8
            results.T_fuel   = Tf;
            results.T_bulk   = Tc;
            results.rho      = params.cool_rho(Tc);
            results.rho_rel  = results.rho / params.rho_ref;
            results.axialExp = length - L;
            results.radii    = [Rf - params.R_f, Ri - params.R_ci, Ro - params.R_co];
            break;
        end

    end

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
