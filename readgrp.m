% [FILENAMES,FILESIZES]=READGRP(GROUPFILENAME) retrieves informations about a BUILD group file.
% [DATA,FILENAMES]=READGRP(GROUPFILENAME, PATTERN), where PATTERN is a string starting with '/'...
function [data,files]=readgrp(groupfilename, filelist)

only_return_infos = (nargin < 2);

if (~only_return_infos)
    filelist_is_filename = ischar(filelist) && length(filelist)>0 && filelist(1)~='/';
    filelist_is_regexp = ischar(filelist) && length(filelist)>0 && filelist(1)=='/';
    filelist_is_list = iscellstr(filelist);
    
    if (~filelist_is_filename && ~filelist_is_regexp && ~filelist_is_list)
        error('FILELIST must be a string or string cell');
    end
    
    if (filelist_is_list)
        error('Cell string FILELIST not implemented.');
    end
end

readfmt = '*uint8';

fid = fopen(groupfilename);

if (fid<0)
    error(sprintf('Couldn''t open file `%s''.', groupfilename));
end

[header, cnt] = fread(fid, 16, readfmt);

if (cnt ~= 16)
    fclose(fid);
    error('Couldn''t read header.');
end

header = header(:).';
if (~strcmp(char(header(1:12)), 'KenSilverman'))
    fclose(fid);
    error(sprintf('File %s is not a BUILD group file.', groupfilename));
end

numfiles = double(getuint4(header(13:16)));

[filetab, cnt] = fread(fid, [16 numfiles], readfmt);
filetab = filetab.';
% filetab: (numfiles, 20) uint8 matrix, where first 12 bytes in each row are
% the file name and the rest 4 is the file size

if (cnt ~= 16*numfiles)
    fclose(fid);
    error('Couldn''t read file table.');
end

filenames = char(filetab(:, 1:12));  % (numfiles, 12) char matrix
filenames = deblank(mat2cell(filenames, ones(numfiles, 1)));  % convert to string cell
filesizes = double(getuint4(filetab(:, 13:16)));   % (numfiles, 1) matrix

if (only_return_infos)
    data = filenames;
    files = filesizes;
    return
end

curofs = 16 + 16*numfiles;
fileoffs = curofs + [0; cumsum(filesizes)];
fileoffs = fileoffs(1:end-1);   % (numfiles, 1) matrix, absolute offsets


if (filelist_is_filename)
    match = find(strcmpi(filenames, filelist));
elseif (filelist_is_regexp)
    re = filelist(2:end);
    match = ~cellfun(@isempty, regexpi(filenames, re));  % logical array
    match = find(match);
end

data = cell(size(match));
if (nargout>1)
    files = filenames(match);
end

for k=1:length(match)
    i = match(k);
    if (fileoffs(i)~=curofs)
        if (fseek(fid, fileoffs(i), 'bof')==-1)
            fclose(fid);
            error(sprintf('fseek failed: %s', ferror(fid)));
        end
    end

    [data{k}, cnt] = fread(fid, filesizes(i), readfmt);
    if (cnt ~= filesizes(i))
        fclose(fid);
        error(sprintf('Failed reading grouped file `%s''.', filenames{i}));
    end
    data{k} = data{k}.';

    curofs = curofs+filesizes(i);
end

%{
if (filelist_is_filename)
    if (length(match) > 0)
        match = match{1};
    end
end
%}

end  % readgrp


function i=getuint4(bytes)
    i = sum(bitshift(uint32(bytes), repmat([0 8 16 24], size(bytes,1), 1)), 2, 'native');
end
