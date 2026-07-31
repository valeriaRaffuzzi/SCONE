function writeDeformationField(filename, fieldName, r_f, r_ci, r_co, pitch, L, Nz, z_bottom, z_top, axialExp, radii)

    fid = fopen(filename, 'w');
    if fid == -1
        error('Could not open file for writing.');
    end

    %% Header
    fprintf(fid, 'type pinDisplacementField;\n');

    fprintf(fid, 'origin (%.6g %.6g %.6g);\n', [0.0 0.0 z_bottom*100]);
    fprintf(fid, 'r_fuel %.6g;\n', r_f*100);
    fprintf(fid, 'r_gap %.6g;\n', r_ci*100);
    fprintf(fid, 'r_clad %.6g;\n', r_co*100);
    fprintf(fid, 'r_outer %.6g;\n', pitch*100/2);
    fprintf(fid, 'z_top %.6g;\n', z_top*100);
    fprintf(fid, 'z_bottom %.6g;\n', z_bottom*100);
    fprintf(fid, 'halflength %.6g;\n', L*100/2);
    fprintf(fid, 'delta_z %.6g;\n', axialExp*100);
    fprintf(fid, 'nodes %d;\n', Nz);

    %% Data blocks

    fprintf(fid, 'delta_f (');
    for i = 1:Nz+1
        fprintf(fid, '%.8g ', radii(i,1)*100);
    end
    fprintf(fid, ');\n');

    fprintf(fid, 'delta_g (');
    for i = 1:Nz+1
        fprintf(fid, '%.8g ', radii(i,2)*100);
    end
    fprintf(fid, ');\n');

    fprintf(fid, 'delta_c (');
    for i = 1:Nz+1
        fprintf(fid, '%.8g ', radii(i,3)*100);
    end
    fprintf(fid, ');\n');

    fclose(fid);

end
