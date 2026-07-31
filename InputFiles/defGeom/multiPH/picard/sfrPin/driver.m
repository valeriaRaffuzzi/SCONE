
function finished = driver(path)

L = 0.34;  % [m]
Nz = 34;
axialExp = 0;

fuelName = 'fuel';
coolName = 'cool';
temperatureFile = [path,'/tempFile'];
densityFile  = [path,'/densFile'];
geometryFile = [path,'/geomFile'];
tallyFile = [path,'/sconeOut.m'];
plotFile  = [path,'/imgY.bmp'];
input = [path,'/sfrPin'];

% Parameter structure
params.R_f   = 0.00216;  % [m]
params.R_ci  = 0.002539; % [m]
params.R_co  = 0.00292;  % [m]
params.pitch = 0.00759; % [m]
params.z_bottom = -0.17; % [m]
params.z_top    = 0.51; % [m]

params.fueldLL = @(T) ...
    (T < 868).*(1.76e-5.*(T-298)) + ...
    (T >= 868 & T < 938).*(1.003e-2 + 7.43e-5.*(T-868)) + ...
    (T >= 938).*(1.52e-2 + 2.01e-5.*(T-938));

params.fuel_rho  = @(T) 14100./(1 + 5.28e-5*(T-298));   % kg/m^3
params.fuel_k    = @(T) 16;      % W/m/K
params.fuel_cp   = @(T) 280 + 0.08*(T-300); % J/kg/K
params.fuel_beta = @(T) 5.28e-5; % 1/K

params.clad_k   = @(T) 4.397e-3*T + 22.47; % W/m/K
params.clad_rho = @(T) 7750;   % kg/m^3
params.clad_cp  = @(T) 420 + 0.20*(T-300); % J/kg/K

params.gap_k = @(T) 93 - 5.81e-2*T + 1.173e-5*T.^2; % W/m/K
params.gap_h = @(T,rf,rc) params.gap_k(T)./(rc.*log(rc./rf));  % W/m2/K

params.cool_rho = @(T) 1014 - 0.235*T;  % [kg/m3]
params.cool_cp  = @(T) 1658 - 0.8479*T + 4.454e-4*T.^2;  % J/kg/K
params.cool_mu  = @(T) exp(-6.4406 - 0.3958*log(T) + 556.835./T);  % Pa s
params.cool_k   = @(T) params.gap_k(T);

params.T_inlet = 643; % [K]
params.m_dot   = 0.0565; %[kg/s]

params.T_ref   = 643; % [K]
params.rho_ref = params.cool_rho(params.T_ref);

dz = L/Nz;
shape = [1 1 Nz*2];
pitch = [params.pitch*200 params.pitch*200 dz*100];

maxIter = 10;
iter    = 0;
idx     = 0;

runScone = @(idx) ['/home/vr339/SCONE/build/scone.out ',input, num2str(idx),' --omp 40']; % > code.log 2>&1 &'];

% Stuff to save
length = zeros(1, maxIter);
rad_f  = zeros(Nz+1, maxIter);
powRes = zeros(Nz, maxIter);
rhoFuelRes = zeros(Nz*2, maxIter);
TRes       = zeros(Nz*2, maxIter);
rhoRes     = zeros(Nz*2, maxIter);
TcoolRes   = zeros(Nz*2, maxIter);

while iter < maxIter

    % Run SCONE
    unix(runScone(idx));
    idx = 1;
    
    % Check for SCONE output
    if exist(tallyFile, 'file')

        % Read SCONE output and scale to power
        q = getAxialPower(tallyFile);
        powRes(:,iter+1) = q;

        % Rename and save tally and plot file
        movefile(tallyFile,['result',num2str(iter),'.m']);
        if exist(plotFile, 'file')
            movefile(plotFile,['plot',num2str(iter),'.bmp']);
        end
    
	    % Do TH
        disp('TH solve');
        results = single_channel_TH_TM(L, Nz, q, axialExp, params);
        disp('TH finished');

        axialExp = results.axialExp;
        length(1, iter+1)       = results.axialExp + L;
        rad_f(1:Nz+1, iter+1)   = results.radii(:,1);
        TRes(1:Nz,iter+1)       = results.T_fuel;
        TRes(Nz+1:2*Nz,iter+1)  = results.T_fuel(end);
        rhoFuelRes(1:Nz,iter+1) = L*params.R_f^2 ./ ( (results.axialExp + L).*...
                                  (params.R_f+(results.radii(1:end-1,1) + results.radii(2:end,1))/2).^2 );
        rhoFuelRes(Nz+1:2*Nz,iter+1) = 1.0;
        rhoRes(1:Nz,iter+1)         = results.rho_rel;
        rhoRes(Nz+1:2*Nz,iter+1)    = rhoRes(Nz, iter+1);
        TcoolRes(1:Nz, iter+1)      = results.T_bulk;
        TcoolRes(Nz+1:2*Nz, iter+1) = TcoolRes(Nz, iter+1);

        writeCartesianField(temperatureFile, 'temperature', {fuelName, coolName}, ...
                            {TRes(:,iter+1),TcoolRes(:,iter+1)}, shape, pitch)
        writeCartesianField(densityFile, 'density', {fuelName, coolName}, ...
                            {rhoFuelRes(:,iter+1), rhoRes(:,iter+1)}, shape, pitch)
        writeDeformationField(geometryFile, 'geometry', params.R_f, params.R_ci, ...
                              params.R_co, params.pitch, L, Nz, params.z_bottom, ...
                              params.z_top, results.axialExp, results.radii)

    end
    
    iter = iter + 1;

end

save 'couplingResults' 'powRes' 'rhoFuelRes' 'TRes' 'rhoRes' 'TcoolRes' 'length' 'rad_f';

finished = 1;

end
