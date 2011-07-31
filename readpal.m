% PAL=READPAL(FILENAME [, CLAMPNORM])  Reads a BUILD palette file (which is a linear dump
%  of RBG values as bytes) and return a palette usable for colormap() etc.
% If optional argument CLAMPNORM is true, normalize the maximal value to 1.0.
% PAL=READPAL(DATA, ...) where DATA is of type uint8...
function pal=readpal(filename, clampnorm)

    pal = zeros(0,3);

    if (ischar(filename))
        fid = fopen(filename);

        if (fid<0)
            error(sprintf('Couldn''t read `%s''.', ferror(fid)));
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

    if (ischar(filename))
        fclose(fid);
    end

    if (nargin > 1 && clampnorm)
        pal = pal./max(max(pal));
    end
end
