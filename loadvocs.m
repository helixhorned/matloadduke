% [METAS,SOUNDS] = LOADVOCS(FILES)  loads all files in FILES as Creative Voice files.
%  FILES can be
%    - a character matrix with a file name in each row (such as returned by `ls').
%    - a cell array of file name strings
%  METAS is a struct array of METAs (see documentation of READVOC) of the same length
%    as FILES row count.
%  SOUNDS is a cell array of the same length.
function [metas,sounds]=loadvocs(files, noconvert)

if (nargin < 2)
    noconvert = false;
end

if (iscell(files))
    numfiles = length(files);
else
    numfiles = size(files,1);
end

metas = readvoc();
metas(2:numfiles) = metas;

sounds = cell(1,numfiles);

loadsounds = (nargout > 1);

for i=1:numfiles
    if (iscell(files))
        fn = files{i};
    else
        fn = deblank(files(i,:));
    end

    if (isempty(fn))
        continue;
    end

    if (loadsounds)
        [metas(i), sounds{i}] = readvoc(fn, noconvert);
    else
        metas(i) = readvoc(fn);
    end
end
