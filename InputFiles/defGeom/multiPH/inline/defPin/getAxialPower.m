function axialPow = getAxialPower(filename)

    % Clear any old variables
    clear axialPow_Res

    % 2. Force MATLAB to forget any cached versions of the script
    [~, name, ~] = fileparts(filename);
    clear(name);

    % Run the tally file (must define tally_axialPow)
    run(filename);
    
    % Check variable exists
    if ~exist('axialPow_Res','var')
        error('axialPow_Res not found after running tallyFile.m');
    end
    
    % Check dimensions
    sz = size(axialPow_Res);
    if length(sz) ~= 3 || sz(1) < 1 || sz(2) < 1
        error('axialPow_Res does not have expected dimensions [2,1,X]');
    end
    
    % Extract [1,1,:] and reshape to 1D row vector
    axialPow = squeeze(axialPow_Res(1,1,:)).';
    
    axialPow = axialPow * 1.6E-13;

end
