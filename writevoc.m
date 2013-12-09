% [OK, ERRMSG] = WRITEVOC(FILENAME, X, FS [, DOERR])
function [ok, errmsg] = writevoc(filename, x, fs, doerr)
    ok = false;
    errmsg = '';

    if (~ischar(filename) || ~isvector(filename))
        error('FILENAME must be a file name')
    end

    if (~isnumeric(x) || ~isvector(x))
        error('X must be a numeric vector')
    end
    numsamples = numel(x);

    if (~isnumeric(fs) || numel(fs)~=1)
        error('FS must be numeric scalar')
    end

    if (nargin < 3)
        doerr = false;
    end

    [fd, msg] = fopen(filename, 'w+b');
    if (fd < 0)
        errmsg = sprintf('Couldn''t open "%s" for writing: %s', filename, msg);
        if (doerr)
            error(errmsg)
        end
        return
    end

    %% Write main header
    fwrite(fd, uint8(['Creative Voice File' 26]), 'uint8');
    fwrite(fd, uint16(26), 'uint16');  % header size
    ver = 256*1 + 20;  % version
    fwrite(fd, uint16(ver), 'uint16');
    fwrite(fd, int16(-1-ver + 0x1234), 'int16');  % check

    %% Write block-prefixing header
    fwrite(fd, uint8(9), 'uint8');
    blksz = 12 + 2*numsamples;
    by = @(sh)bitand(bitshift(blksz,-sh), 255);
    fwrite(fd, uint8([by(0) by(8) by(16)]), 'uint8');

    %% Write header of blocktype 9
    fwrite(fd, int32(fs), 'int32');  % sample rate
    fwrite(fd, uint8([16, 1, 4 0, 0 0 0 0]), 'uint8');  % bits per sample, numchannels, coded_id, reserved

    %% Write the actual data
    fwrite(fd, int16(32768 * x), 'int16');

    ok = true;
    fclose(fd);
end
