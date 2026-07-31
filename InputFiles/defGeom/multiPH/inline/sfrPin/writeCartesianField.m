function writeCartesianField(filename, fieldName, materialName, data, shape, pitch)

% Allow single material input (backward compatible)
if ischar(materialName)
    materialName = {materialName};
    data = {data};
end

nMat = numel(materialName);
Nz = length(data{1});

% Check consistency
for m = 1:nMat
    if length(data{m}) ~= Nz
        error('All data vectors must have same length');
    end
end

if shape(3) ~= Nz
    error('Shape Nz must match length of data vector');
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

fprintf(fid, 'origin (0 0 0);\n');
fprintf(fid, 'shape (%d %d %d);\n', shape);
fprintf(fid, 'pitch (%.6g %.6g %.6g);\n', pitch);
fprintf(fid, 'default -1;\n');

%% Data blocks

values_per_line = 10;

for m = 1:nMat

    fprintf(fid, '%s (\n', materialName{m});

    for i = Nz:-1:1
        fprintf(fid, '%.8g ', data{m}(i));

        if mod(Nz - i + 1, values_per_line) == 0
            fprintf(fid, '\n');
        end
    end

    fprintf(fid, '\n);\n');

end

fclose(fid);

end
