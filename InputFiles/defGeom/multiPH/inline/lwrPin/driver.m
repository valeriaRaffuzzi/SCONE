
function finished = driver(path)

L = 3.6576;
Nz = 36;

fuelName = 'fuel';
coolName = 'cool';
temperatureFile = [path,'/tempFile'];
densityFile = [path,'/densFile'];
fileIn = [path,'/toDriver.dat'];
fileOut = [path,'/toSCONE.dat'];
tallyFile = [path,'/sconeOut.m'];
plotFile = [path,'imgZ.bmp'];
input = [path,'/gillPin'];

% Parameter structure
params.R_f = 0.0049275; % [m]
params.R_ci = 0.0049275; % [m]
params.R_co = 0.005588; % [m]
params.m_dot = 0.4; %[kg/s]
params.cp = 5230; % [J/kg/K]
params.rho_ref = 810.6; % [kg/m3]
params.T_ref = 515; % [K]
params.beta = 22.1e-4; % [1/K]
params.mu = 9.2e-5; % [Pa s]
params.k_cool = 0.5331; % [W/m/K]
params.T_inlet = 515; % [K]
params.pitch = 1.4723/100; % [m]
params.resistivity = 0.81431 / (100^2); % [m2 K/W]

dz = L/Nz;
shape = [1 1 Nz];
pitch = [1.4723 1.4723 dz*100];

tallySize = [2, 1, Nz];

maxIter = 200;
iter = 0;
running = true;

runScone = ['/home/vr339/SCONE/build/scone.out ',input,' --omp 40 > code.log 2>&1 &'];

% Stuff to save
powRes = zeros(Nz, maxIter);
TRes = powRes;
rhoRes = powRes;
TcoolRes = powRes;

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
                    q = getAxialPower(tallyFile);
                    powRes(:,iter+1) = q;
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
        results = fuel_pin_1D_TH(L, Nz, q_prime, params);
        disp('TH finished');

        TRes(:,iter+1) = results.T_fuel;
        rhoRes(:,iter+1) = results.rho_rel * params.rho_ref;
        TcoolRes(:, iter+1) = results.T_bulk;

        writeCartesianField(temperatureFile, 'temperature', {fuelName, coolName}, {results.T_fuel, results.T_bulk}, shape, pitch)
        writeCartesianField(densityFile, 'density', coolName, results.rho_rel, shape, pitch)

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

save 'couplingResults' 'powRes' 'TRes' 'rhoRes' 'TcoolRes';

finished = 1;

end
