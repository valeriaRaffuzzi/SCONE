function writeCartesianField(filename, fieldName, materialName, data, origin, shape, pitch)

% Allow single material input (backward compatible)
if ischar(materialName)
    materialName = {materialName};
    data = {data};
end

nMat = numel(materialName);
[Ny,Nx] = size(data{1});

% Check consistency
for m = 1:nMat
    if length(data{m}) ~= [Ny,Nx]
        error('All data vectors must have same length');
    end
end

fid = fopen(filename, 'w');
if fid == -1
    error('Could not open file for writing.');
end

%% Header
fprintf(fid, 'type cartesianField;\n');

% --- materials list ---
fprintf(fid, 'materials (');
for m = 1:nMat
    fprintf(fid, '%s', materialName{m});
    if m < nMat
        fprintf(fid, ' ');
    end
end
fprintf(fid, ');\n');

fprintf(fid, 'origin (%.6g %.6g %.6g);\n', origin);
fprintf(fid, 'shape (%d %d %d);\n', shape);
fprintf(fid, 'pitch (%.6g %.6g %.6g);\n', pitch);
fprintf(fid, 'default -1;\n');

%% Data blocks

for m = 1:nMat

    fprintf(fid, '%s (\n', materialName{m});

    for i = 1:Ny
        for j = 1:Nx
            fprintf(fid, '%.8g ', data{m}(i,j));
        end
        fprintf(fid, '\n');
    end

    fprintf(fid, '\n);\n');

end

fclose(fid);

end
