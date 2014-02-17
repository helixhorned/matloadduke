% [PAL, SHTAB]=READPAL(FILENAME [, CLAMPNORM])  Reads a BUILD palette file (which is a linear dump
%  of RBG values as bytes) and return a palette usable for colormap() etc.
% If optional argument CLAMPNORM is true, normalize the maximal value to 1.0.
%
% SHTAB: base shade table, 256-by-32 uint8. Only if FILENAME is file name.
%
% PAL=READPAL(DATA, ...) where DATA is of type uint8...
function [pal, shtab] = readpal(filename, clampnorm)

    pal = zeros(0,3);

    if (ischar(filename))
        fid = fopen(filename);

        if (fid<0)
            error(sprintf('Couldn''t open `%s''.', filename));
            return
        end

        [data,cnt] = fread(fid, 768, 'uint8');
        if (cnt ~= 768)
            fclose(fid);
            error('Couln''t read 768 bytes from file.');
        end
    elseif (strcmp(class(filename), 'uint8'))
        data = double(filename(1:768));
        data = data(:);
    else
        error('First argument must be either file name (char) of data (uint8).');
    end

    pal = reshape(data, 3,256).'/255;

    shtab = [];
    if (nargout >= 2 && ischar(filename))
        [numshades, cnt] = fread(fid, 1, 'int16');
        if (cnt ~= 1)
            fclose(fid);
            error('Couldn''t read 2 bytes from file.');
        end
        if (~(numshades >= 1 && numshades <= 256))
            fclose(fid);
            error(['Invalid number of shades: ' num2str(numshades)]);
        end

        [shtab, cnt] = fread(fid, numshades*256, 'uint8=>double');
        if (cnt ~= numshades*256)
            fclose(fid);
            error('Couldn''t read shade table from file.');
        end

        shtab = reshape(shtab, 256, numshades);

        fclose(fid);
    end

    if (nargin > 1 && clampnorm)
        pal = pal./max(max(pal));
    end
end
