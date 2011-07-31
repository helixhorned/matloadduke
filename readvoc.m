
% [META,SND] = READVOC(FILENAME [, NOCONVERT])  Read Creative Voice VOC file.
%
% By default, samples are converted to double precision and normalized such
% that a value of 1.0 corresponds to the maximum possible amplitude in the
% original sample format.
% If optional second argument NOCONVERT is true, do not carry out this conversion.
% SND is a matrix with #samples rows and #channels columns.
%
% META = READVOC(FILENAME)  returns only the metadata of the VOC file.
%
% META = READVOC()  returns an empty META struct.
%
% The META struct contains the fields
%   .fs:  sample rate in samples/second)
%   .format:  sample format, currently supported for conversion are
%             0 (8-bit unsigned PCM) and 4 (16-bit signed PCM)
%   .numchan:  number of channels
%   .version:  VOC file version, numeric pair [major minor]
%   .error:  a numeric value greater 0 if an error occurred
%   (.errorstr:  a string describing it in that case)
function [meta,snd]=readvoc(filename, noconvert)

meta = struct();
meta.fs = 0;  % sampling rate
meta.format = 0;  % sample format ("codec id")
meta.numchan = 0;  % number of channels
meta.version = uint8([0 0]);  % [major minor]
meta.error = 0;  % whether an error occurred
meta.errorstr = '';  % a string desribing it

meta.blockcnt_ = zeros(1,9);

blockcnt = zeros(1,9);

snd = uint8([]);

if (nargin==0)
    return
elseif (nargin==1)
    noconvert = false;
end

fid = fopen(filename);

if (fid<0)
    meta.error = 1;
    meta.errorstr = 'Couldn''t open file.';
    return
end

% from http://wiki.multimedia.cx/index.php?title=Creative_Voice:
% [Adjusted to 1-based indices and edited. All multi-byte ints are little endian.]
%
%  Main header
% 
% bytes 1-19   Identifier string containing: Creative Voice File
% byte  20     0x1A (EOF). This is belived to be used to abort printing of file
% bytes 21-22  Total size of this main header (usually 0x001A)
% bytes 23-24  Version number, calculated as (major<<8)|minor, i.e.
%  byte 23       minor is usually 0x0A or 0x14
%  byte 24       major is usually 0x01
% bytes 25-26  Validity check. This must be equal to ~version + 0x1234
% 
%  Data blocks
%  All the different data blocks begin with a common header:
% 
% byte  1      block type
% bytes 2-4    block size (NOT including this common header)
% 
% The data following this common block header depends on the block type. 

if (nargout > 1)
    % Read the whole file at once. The less fread calls, the better.
    readlen = Inf;
else
    % main header length + max(block specific header length)
    readlen = 26+12;
end
data = fread(fid, readlen, '*uint8');  % data: column vector
fclose(fid);

if (length(data) < 26)
    meta.error = 2;
    meta.errorstr = 'File shorter than header length.';
    return
end

if (~strcmp(char(data(1:19)'), 'Creative Voice File'))
    meta.error = 3;
    meta.errorstr = 'Not a Creative Voice VOC file.';
    return
end

if (btoi(data(21:22))~=26)
    meta.error = 4;
    meta.errorstr = 'Non-standard header length.';
    return
end

meta.version = [data(24) data(23)];  % [major minor]

%%

if (length(data)==26)
    meta.error = 5;
    meta.errorstr = 'File doesn''t contain data beyond the header.';
    return    
end

b = uint32(26);  % base offset, now: base of first block generic header
e = uint32(0);  % current block end

have_sound_meta = false;
codecid = uint8(0);  % codec id
freqdiv = uint8(0);  % frequency divisor

% loop over blocks
while (1)
    if (b+4 > length(data))  % this would skip a trailing terminator but so what...
        break
    end

    blocktype = data(b+1);
    blocksize = btoi(data(b+2:b+4));  % block size (NOT including this common header)
    b = b+4;  % now base of block data (i.e. block specific header)

    switch blocktype
      case 0,  % terminator (not mandatory)
        break  % the while loop

      case 1,
        % Block type 0x01: Sound data [mono]
        %
        % byte  1      frequency divisor
        % byte  2      codec id
        % bytes 3..n   the audio data
        %
        % The sample rate is defined as
        % 1000000/(256 - frequency_divisor)

        fdiv = data(b+1);
        ccid = data(b+2);
        if (have_sound_meta)
            if (~strcmp(class(ccid),class(codecid)) || ccid~=codecid || fdiv~=freqdiv || 1~=meta.numchan)
                meta.error = 6;
                meta.errstr = 'Multiple formats in one file not supported.';
                return
            end
        else
            codecid = ccid;
            freqdiv = fdiv;
            meta.fs = floor(1000000/(256-double(freqdiv)));
            meta.format = codecid;
            meta.numchan = 1;
            have_sound_meta = true;

            if (nargout < 2)  % if no sound data requested, out
                break
            end
        end

        snd = [snd; data(b+3:b+blocksize)];
        
      case 2,
         % Block type 0x02: Sound data continuation
         % bytes 3..n    the audio data
         % This block uses the same codec parameters as the previous "Sound data" block.

         snd = [snd; data(b+3:b+blocksize)];

      case {3,4,5,6,7},
        % silence, marker, text, repeat start & end
        % not implemented
        
      case 8,
        % Block type 0x08: Extra info
        % 
        % bytes 1-2    frequency divisor
        % byte  3      codec id
        % byte  4      channels number - 1
        % 
        % The sample rate is defined as
        % 256000000/(nb_channels * (65536 - frequency_divisor)).
        % This block must be followed by a "Sound data" block, and it supercedes its codec parameters.

        fdiv = btoi(data(b+1:b+2));
        ccid = data(b+3);
        nc = double(data(b+4))+1;
        if (have_sound_meta)
            if (~strcmp(class(ccid),class(codecid)) || ccid~=codecid || fdiv~=freqdiv || nc~=meta.numchan)
                meta.error = 6;
                meta.errstr = 'Multiple formats in one file not supported.';
                return
            end
        else
            codecid = ccid;
            freqdiv = fdiv;
            meta.numchan = nc;
            meta.fs = floor(256000000/(65536-double(freqdiv)));
            meta.format = codecid;
            have_sound_meta = true;
        
            if (nargout < 2)
                break
            end
        end

      case 9,
        % Block type 0x09: Sound data (New format)
        % 
        % This block type is probably only valid in version 1.20 (0x0114) or greater files.
        % 
        % bytes 1-4    sample rate
        % byte  5      bits per sample
        % byte  6      channels number
        % bytes 7-8    codec_id
        % bytes 9-12   reserved
        % bytes 13..n  the audio data

        fs = double(btoi(data(b+1:b+4)));
%        bps = data(b+5);
        nc = double(data(b+6));
        ccid = btoi(data(b+7:b+8));
        if (have_sound_meta)
            if (~strcmp(class(ccid),class(codecid)) || ccid~=codecid || fs~=meta.fs || nc~=meta.numchan)
                meta.error = 6;
                meta.errstr = 'Multiple formats in one file not supported.';
                return
            end
        else
            codecid = ccid;
            meta.numchan = nc;
            meta.fs = fs;
            meta.format = codecid;
            have_sound_meta = true;

            if (nargout < 2)
                break
            end
        end

%        disp(filename);
%        [size(data,1), b,blocksize, b+blocksize]
        try
            snd = [snd; data(b+13:b+blocksize)];
        catch
            snd = [snd; data(b+13 : min(b+blocksize, end-1))];
            meta.error = 9;
            meta.errorstr = 'Block length exceeds file size in block type 9.';
        end

      otherwise,
        meta.error = 7;
        meta.errorstr = 'Unknown block type encountered.';
        return
    end

    blockcnt(blocktype) = blockcnt(blocktype)+1;

    b = b+blocksize;  % now base of block generic header
end

meta.blockcnt_ = blockcnt;

if (nargout > 1)
    % Possibly convert samples
    if (codecid == 4)  % 16 bit signed PCM
        snd16 = zeros(numel(snd)/2, 1, 'int32');
        for i=1:numel(snd)/2
            snd16(i) = btoi(snd(2*i-1 : 2*i));
        end
        snd = snd16;
    elseif (codecid == 0)  % 8 bit unsigned PCM
        snd = int32(snd)-int32(128);
    else
        if (~noconvert)
            meta.error = 8;
            meta.errstr = 'Sample format not supported.';
        end
    end

    % Possibly normalize
    if (~noconvert && meta.error~=8)
        snd = double(snd);
        if (codecid==0)
            snd = snd/128;
        else
            snd = snd/32768;
        end
    end

    % Possibly reshape if we have more than one channel
    if (meta.numchan>1)
        % Unpack samples. Result is a (#samples, #channels) matrix
        snd = reshape(snd, meta.numchan, []).';
    end
end

end



% Convert byte array to 32-bit unsigned integer.
% The least significant byte is the first (little endian).
function i=btoi(bytes)
    i = uint32(0);
    sh = 0;
    for n=1:length(bytes)
        i = i + bitshift(uint32(bytes(n)),sh);
        sh = sh+8;
    end
end
