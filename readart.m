% Usage:
%  Init: [s,f]=readart(), this will do
%    s = cell(1,16384); flags=zeros(1,16384,'int32');
%
%  Then, for every art file:
%    [s,flags] = readart(artfilename, s, flags);
%
%  The result is a cell array s with s{i} being tile i-1 (correction for MATLAB 1-based indexing)
%  as a matrix directly passable to image() etc.  Use readpal() to read the corresponding palette
%  and set it with colormap() then.
function [s,gpicanm]=readart(filename, s, gpicanm)

    if (nargin==0)
        s = cell(1,16384);
        gpicanm = zeros(1,16384, 'int32');
        return
    end
    
    read_file = ischar(filename);

    if (read_file)
        [fid,msg]=fopen(filename);

        if (fid < 0)
            warning(msg);
            return
        end

        [data,cnt] = fread(fid, 4, 'int32');
        if (cnt ~= 4)
            warning('couldn''t read four 32-bit ints');
            fclose(fid);
            return
        end

        artversion = data(1);
    elseif (strcmp(class(filename), 'uint8'))
        data = filename(:).';  % row vector
        artversion = double(getint4(data(1:4)));
    else
        error('First argument must be either file name (char) of data (uint8).');
    end

    if (artversion ~= 1)
        if (read_file)
            fclose(fid);
            warning(sprintf('`%s'' is not an ART file or has wrong art file version.', filename));
        else
            warning('Wrong art file version.');
        end

        return
    end

    if (read_file)
        numtiles = data(2);
        localtilestart = data(3);
        localtileend = data(4);

%        disp(sprintf('%s %d-%d (%d)', filename, localtilestart, localtileend, localtileend-localtilestart+1));
    else
        numtiles = double(getint4(data(5:8)));
        localtilestart = double(getint4(data(9:12)));
        localtileend = double(getint4(data(13:16)));
    end

    localnumtiles = localtileend-localtilestart+1;

    % Tile sizes
    if (read_file)
        xsiz = fread(fid, localnumtiles, 'int16');
        ysiz = fread(fid, localnumtiles, 'int16');
        picanm = fread(fid, localnumtiles, '*int32');
    else
        ofs = 16;
        len = 2*localnumtiles;
        xsiz = double(getint2(reshape(data(ofs+1:ofs+len), [2 localnumtiles]).'));
        ofs = ofs+len;
        ysiz = double(getint2(reshape(data(ofs+1:ofs+len), [2 localnumtiles]).'));
        ofs = ofs+len;
        len = len*2;
        picanm = getint4(reshape(data(ofs+1:ofs+len), [4 localnumtiles]).');
        ofs = ofs+len;
    end

    if (nargin>=3 && nargout>=2)
        gpicanm(localtilestart+1 : localtilestart+localnumtiles) = picanm;
    end

    sumsize = max(xsiz, 0).' * max(ysiz, 0);

    if (read_file)
        alltiles = fread(fid, sumsize, '*uint8');
    else
        len = sumsize;
        alltiles = data(ofs+1:ofs+len).';  % column vector
    end

    b = 1;
    for i=1:localnumtiles
        s{localtilestart+i} = reshape(alltiles(b : b + xsiz(i)*ysiz(i)-1), ysiz(i), xsiz(i));
        b = b + xsiz(i)*ysiz(i);
    end

    clear alltiles;

    if (read_file)
        fclose(fid);
    end
end

function i=getint4(bytes)
    i = sum(bitshift(uint32(bytes), repmat([0 8 16 24], size(bytes,1), 1)), 2, 'native');
    i = typecast(i, 'int32');
end
function i=getint2(bytes)
    i = sum(bitshift(uint16(bytes), repmat([0 8], size(bytes,1), 1)), 2, 'native');
    i = typecast(i, 'int16');
end
