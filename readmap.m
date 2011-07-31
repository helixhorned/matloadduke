% MAP=READMAP(FILENAME [, CONVERTINDICES])  Read BUILD map file.
%
% CONVERTINDICES specifies whether some fields which represent indices
% should be corrected for MATLAB's one-based indexing (that is, have a 1 added)
% If omitted, it defaults to true.
function [map,err]=readmap(filename, convertindices)

argout_havemap = (nargout>0);
argout_haveerr = (nargout>1);

if (nargin<2)
    convertindices=true;
end

SIZEOF_SECT = 40;
SIZEOF_WALL = 32;
SIZEOF_SPRI = 44;

err = 0;
map = struct();

if (ischar(filename))
    readfmt = '*uint8';

    fid = fopen(filename);

    if (fid<0)
        err = 1;
        if (~argout_haveerr)
            error(sprintf('Couldn''t open file `%s''.', filename));
        end
        return
    end

    data = fread(fid, Inf, '*uint8').';  % data: row vector
    fclose(fid);

elseif(strcmp(class(filename), 'uint8'))
    data = filename(:).';

else
    error('First argument must be either file name (char) of data (uint8).');
end

if (length(data) < 22)
    err = 2;
    if (~argout_haveerr)
        error('File shorter than header length.');
    end
    return
end

map.mapversion = getint4(data(1:4));
map.posx = getint4(data(5:8));
map.posy = getint4(data(9:12));
map.posz = getint4(data(13:16));
map.ang = getint2(data(17:18));
map.cursectnum = getint2(data(19:20));

convfunc = struct('int8',@getint1, 'uint8',@(x)(x.'), ...
                  'int16',@getint2, 'uint16',@getuint2, ...
                  'int32',@getint4, 'uint32',@getuint4);
bytecnt = struct('int8',1, 'uint8',1, 'int16',2, 'uint16',2, 'int32',4, 'uint32',4);

% SECTORS
map.numsectors = getint2(data(21:22));
b = 22;
B = b;

numsectors = double(map.numsectors);
sec = struct( ...
    'wallptr', zeros(1, 1, 'int16'), ...
    'wallnum', zeros(1, 1, 'int16'), ...
    'ceilingz', zeros(1, 1, 'int32'), ...
    'floorz', zeros(1, 1, 'int32'), ...
    'ceilingstat', zeros(1, 1, 'uint16'), ...  % instead of int16
    'floorstat', zeros(1, 1, 'uint16'), ...  % instead of int16
    'ceilingpicnum', zeros(1, 1, 'int16'), ...
    'ceilingheinum', zeros(1, 1, 'int16'), ...
    'ceilingshade', zeros(1, 1, 'int8' ), ...
    'ceilingpal', zeros(1, 1, 'uint8'), ...
    'ceilingxpanning', zeros(1, 1, 'uint8'), ...
    'ceilingypanning', zeros(1, 1, 'uint8'), ...
    'floorpicnum', zeros(1, 1, 'int16'), ...
    'floorheinum', zeros(1, 1, 'int16'), ...
    'floorshade', zeros(1, 1, 'int8'), ...
    'floorpal', zeros(1, 1, 'uint8'), ...
    'floorxpanning', zeros(1, 1, 'uint8'), ...
    'floorypanning', zeros(1, 1, 'uint8'), ...
    'visibility', zeros(1, 1, 'uint8'), ...
    'filler', zeros(1, 1, 'uint8'), ...
    'lotag', zeros(1, 1, 'int16'), ...
    'hitag', zeros(1, 1, 'int16'), ...
    'extra', zeros(1, 1, 'int16'));

fn = fieldnames(sec);
numfn = length(fn);

for j=1:numfn
    daclass = class(sec(1).(fn{j}));
    funct = convfunc.(daclass);
    numbytes = bytecnt.(daclass);
    % idx matrix: (numsectors, numbytes)
    sec.(fn{j}) = funct(data(...
        b + repmat(1:numbytes, numsectors, 1) + repmat(((0:numsectors-1).').*SIZEOF_SECT, 1, numbytes) ));
    b = b+numbytes;
end

b = B + SIZEOF_SECT*numsectors;
B = b;

if convertindices
    sec.wallptr = sec.wallptr+1;
end

map.sector = sec;


% WALLS
map.numwalls = getint2(data(b+1:b+2));
b = b+2;
numwalls = double(map.numwalls);

wal = struct( ...
    'x', zeros(1, 1, 'int32'), ...
    'y', zeros(1, 1, 'int32'), ...
    'point2', zeros(1, 1, 'int16'), ...
    'nextwall', zeros(1, 1, 'int16'), ...
    'nextsector', zeros(1, 1, 'int16'), ...
    'cstat', zeros(1, 1, 'uint16'), ...  % instead of int16
    'picnum', zeros(1, 1, 'int16'), ...
    'overpicnum', zeros(1, 1, 'int16'), ...
    'shade', zeros(1, 1, 'int8' ), ...
    'pal', zeros(1, 1, 'uint8'), ...
    'xrepeat', zeros(1, 1, 'uint8'), ...
    'yrepeat', zeros(1, 1, 'uint8'), ...
    'xpanning', zeros(1, 1, 'uint8'), ...
    'ypanning', zeros(1, 1, 'uint8'), ...
    'lotag', zeros(1, 1, 'int16'), ...
    'hitag', zeros(1, 1, 'int16'), ...
    'extra', zeros(1, 1, 'int16'));

fn = fieldnames(wal);
numfn = length(fn);

for j=1:numfn
    daclass = class(wal(1).(fn{j}));
    funct = convfunc.(daclass);
    numbytes = bytecnt.(daclass);
    % idx matrix: (numwalls, numbytes)
    wal.(fn{j}) = funct(data(...
        b + repmat(1:numbytes, numwalls, 1) + repmat(((0:numwalls-1).').*SIZEOF_WALL, 1, numbytes) ));
    b = b+numbytes;
end

b = B + 2 + SIZEOF_WALL*numwalls;
B = b;

if convertindices
    wal.point2 = wal.point2 + 1;
    wal.nextwall = wal.nextwall + 1;
    wal.nextsector = wal.nextsector + 1;
end

map.wall = wal;


% SPRITES
map.numsprites = getint2(data(b+1:b+2));
b = b+2;
numsprites = double(map.numsprites);

spr = struct( ...
    'x', zeros(1, 1, 'int32'), ...
    'y', zeros(1, 1, 'int32'), ...
    'z', zeros(1, 1, 'int32'), ...
    'cstat', zeros(1, 1, 'uint16'), ...  % instead of int16
    'picnum', zeros(1, 1, 'int16'), ...
    'shade', zeros(1, 1, 'int8'), ...
    'pal', zeros(1, 1, 'uint8'), ...
    'clipdist', zeros(1, 1, 'uint8'), ...
    'filler', zeros(1, 1, 'uint8'), ...
    'xrepeat', zeros(1, 1, 'uint8'), ...
    'yrepeat', zeros(1, 1, 'uint8'), ...
    'xoffset', zeros(1, 1, 'int8' ), ...
    'yoffset', zeros(1, 1, 'int8' ), ...
    'sectnum', zeros(1, 1, 'int16'), ...
    'statnum', zeros(1, 1, 'int16'), ...
    'ang', zeros(1, 1, 'int16'), ...
    'owner', zeros(1, 1, 'int16'), ...
    'xvel', zeros(1, 1, 'int16'), ...
    'yvel', zeros(1, 1, 'int16'), ...
    'zvel', zeros(1, 1, 'int16'), ...
    'lotag', zeros(1, 1, 'int16'), ...
    'hitag', zeros(1, 1, 'int16'), ...
    'extra', zeros(1, 1, 'int16'));

fn = fieldnames(spr);
numfn = length(fn);

for j=1:numfn
    daclass = class(spr(1).(fn{j}));
    funct = convfunc.(daclass);
    numbytes = bytecnt.(daclass);
    % idx matrix: (numsprites, numbytes)
    spr.(fn{j}) = funct(data(...
        b + repmat(1:numbytes, numsprites, 1) + repmat(((0:numsprites-1).').*SIZEOF_SPRI, 1, numbytes) ));
    b = b+numbytes;
end


if convertindices
    spr.sectnum = spr.sectnum + 1;
end

map.sprite = spr;

end  % function readmap


% Byte array -> integer conversion
% The first byte is the least significant one (little endian).
function i=getuint4(bytes)
    i = sum(bitshift(uint32(bytes), repmat([0 8 16 24], size(bytes,1), 1)), 2, 'native');
end
function i=getint4(bytes)
    i = sum(bitshift(uint32(bytes), repmat([0 8 16 24], size(bytes,1), 1)), 2, 'native');
    i = typecast(i, 'int32');
end
function i=getuint2(bytes)
    i = sum(bitshift(uint16(bytes), repmat([0 8], size(bytes,1), 1)), 2, 'native');
end
function i=getint2(bytes)
    i = sum(bitshift(uint16(bytes), repmat([0 8], size(bytes,1), 1)), 2, 'native');
    i = typecast(i, 'int16');
end
function i=getint1(byte)
    i = typecast(byte, 'int8').';  % col vec
end
