function writeDeformationField(filename, fieldName, origin, shape, pitch, x_new, y_new)

    %% Process data
    x_origin = origin(1);
    y_origin = origin(2);
    
    r_outer = 0.906;
    r_flat  = 0.161;
    r_shift = 0.745;
    
    NzLength = 200;
    z0 = -100.0;
    
    [Ny,Nx] = size(x_new);
    
    % Undeformed lattice
    [X0,Y0] = meshgrid((0.5:Nx-0.5)*pitch(1),(0.5:Ny-0.5)*pitch(2));
    
    %%

    fid = fopen(filename, 'w');
    if fid == -1
        error('Could not open file for writing.');
    end

    %% Header
    fprintf(fid,'type latDisplacementField; origin (%.6g %.6g %.6g); pitch (%d %d 0); shape (%d %d 0); fields ( ',...
            origin, pitch(1:2),Nx,Ny);

    for i = 1:Ny
        for j = 1:Nx
            fprintf(fid,'pos%d_%d ',i,j);
        end
    end

    fprintf(fid,');\n\n');

    %% Individual fields

    for i = 1:Ny

        for j = 1:Nx

            x0 = X0(i,j);
            y0 = Y0(i,j);

            dx = x_new(i,j)*100 - x0;
            dy = y_new(i,j)*100 - y0;

            coeff = hypot(dx,dy);

            if coeff > 1e-12

                dirx = dx/coeff;
                diry = dy/coeff;

            else

                dirx  = 0.0;
                diry  = 0.0;
                coeff = 0.0;

            end

            % origin opposite displacement

            xfield = x0 - r_outer/2*dirx + 3*x_origin;
            yfield = y0 - r_outer/2*diry - y_origin;

             fprintf(fid,...
            ['pos%d_%d {' ...
             'type funcDisplacementField; ' ...
             'origin (%.6f %.6f %.6f); ' ...
             'length %.6f; ' ...
             'axis z; ' ...
             'function flat; ' ...
             'coefficients (%.8e); ' ...
             'direction (%.8f %.8f); ' ...
             'r_outer %.6f; ' ...
             'r_flat %.6f; ' ...
             'r_shift %.6f; ' ...
             '}\n'],...
                    i,j,...
                    xfield,yfield,z0,...
                    NzLength,...
                    coeff,...
                    dirx,diry,...
                    r_outer,...
                    r_flat,...
                    r_shift);

        end

    end

    fclose(fid);

end
