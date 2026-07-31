
function finished = driver(path)

L = 0.343;  % [m]
Nz = 1;

fuelName1 = 'fuel1';
fuelName2 = 'fuel2';
coolName  = 'cool';
temperatureFile = [path,'/tempFile'];
densityFile  = [path,'/densFile'];
geometryFile = [path,'/geomFile'];
fileIn  = [path,'/toDriver.dat'];
fileOut = [path,'/toSCONE.dat'];
tallyFile = [path,'/sconeOut.m'];
plotFile  = [path,'/imgZ.bmp'];
input = [path,'/sfrPin'];

% Parameter structure
params.R_f   = 0.00216;  % [m]
params.R_ci  = 0.002539; % [m]
params.R_co  = 0.00292;  % [m]
params.pitch = 0.00906; % [m]

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
params.m_dot   = 0.0865; %[kg/s]

params.T_ref   = 643; % [K]
params.rho_ref = params.cool_rho(params.T_ref);

dz = L/Nz;
shape  = [38 38 0];
pitch  = [params.pitch params.pitch 0]*100;
origin = [-params.pitch*9.5 params.pitch*9.5 0]*100;

maxIter = 20;
iter = 0;
running = true;

runScone = ['/home/vr339/SCONE/build/scone.out ',input,' --omp 40 > code.log 2>&1 &'];

% Stuff to save
xRes = zeros(38, 38, maxIter);
yRes = zeros(38, 38, maxIter);
powRes = zeros(38, 38, maxIter);
TRes   = zeros(38, 38, maxIter);
rhoRes = zeros(38, 38, maxIter);
TcoolRes = zeros(38, 38, maxIter);

% Start SCONE
unix(runScone);

while running && iter < maxIter

    % Check for SCONE output
    exists = false;

    while ~exists
        pause(0.1);
		
        if exist(fileIn, 'file')

            disp('Data exchange');
            % ---- Read signal ----
            fid = fopen(fileIn, 'r');
            sig = strtrim(fgetl(fid));
            fclose(fid);

            % Remove file after reading
            delete(fileIn);

            switch sig
                case 'SIGUSR1'

                    running = true;
	    
                    % Read SCONE output and scale to power
                    q = getRadialPower(tallyFile);
                    powRes(:,:,iter+1) = q;
                    q_prime = q / dz;

                    % Rename and save tally and plot file
                    movefile(tallyFile,['result',num2str(iter),'.m']);
                    if exist(plotFile, 'file')
                        movefile(plotFile,['plot',num2str(iter),'.bmp']);
                    end

                % if SCONE is done, end running
                case 'SIGTERM'

                    running = false;

                otherwise

                    error('Wrong signal');

            end

            exists = true;

        end

    end

    if running
    
	    % Do TH
        disp('TH solve');
        results = single_channel_TH_TM(L, Nz, q_prime, params);
        disp('TH finished');
        
        xRes(:,:,iter+1)     = results.Xnew;
        yRes(:,:,iter+1)     = results.Ynew;
        TRes(:,:,iter+1)     = results.T_fuel;
        rhoRes(:,:,iter+1)   = results.rho_rel * params.rho_ref;
        TcoolRes(:,:,iter+1) = results.T_bulk;

        writeCartesianField(temperatureFile, 'temperature', {fuelName1, fuelName2, coolName}, ...
                            {results.T_fuel,results.T_fuel,results.T_bulk}, origin, shape, pitch)
        
        writeCartesianField(densityFile, 'density', coolName, results.rho_rel, origin, shape, pitch)
        
        writeDeformationField(geometryFile, 'geometry', origin, shape, pitch, results.Xnew, results.Ynew)

        % Signal to SCONE
        fid = fopen(fileOut, 'w');
        fprintf(fid,'SIGUSR1\n');
        fclose(fid);

    end

    iter = iter + 1;

end

% Signal to SCONE that coupling is over
if iter >= maxIter && running
    fid = fopen(fileOut, 'w');
    fprintf(fid,'SIGTERM\n');
    fclose(fid);
end

save 'couplingResults' 'powRes' 'TRes' 'rhoRes' 'TcoolRes' 'xRes' 'yRes';

finished = 1;

end
