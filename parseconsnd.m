% SOUNDS = PARSECONSND(USER_CON_FILENAME)  Parse CON file and retrieve
%  its sound definitions (`definesound').
%
% SOUNDS = PARSECONSND(USER_CON_FILENAME, DEFS_FILE_NAME)  Instead of returning
%  the sounds in occurence order, make them indexed by the CON define number.
%  This means that there may be holes in the returned STRUCT array; string keys
%  will be empty an numeric keys will be 0.  Aliased sounds (those falling on
%  the same define num) will be handled such that one of them is returned,
%  but without guarantees as to which one.
%
% SOUNDS is a struct array with the following fields:
%  .def:  The definition name, e.g. 'PRED_ROAM'
%  .fn:  The file name of the sound file, e.g. 'roam06.voc'
%  .pitch:  pitch variation, a pair [low high]?
%  .prio:  priority
%  .volume:  volume adjustment
%  .flags:  a bitmask of the following values
%   BIT 0 (1) Repeat
%   BIT 1 (2) MUSICANDSFX (used for)
%   BIT 2 (4) A Duke Voice
%   BIT 3 (8) Par. Lockout
%   BIT 4 (16) Glob. Heard (sndist = 0)
%
% BUGS/LIMITATIONS:
%  - comments aren't parsed in the CON files
%  - if loading in define number order, sound #0 (KICK_HIT in Duke3D) will not
%    be loaded since we keep the original numbers and MATLAB/Octave indexes
%    starting with 1
function sounds=parseconsnd(user_filename, defs_filename)

sounds = struct();

sounds.def = '';
sounds.fn = '';
sounds.pitch = [0 0];
sounds.prio = 0;
sounds.flags = 0;
sounds.volume = 0;

dummysound = sounds;

WS = '\s+';
NUM = '(-?[0-9]+)';

user_toks = open_and_read_re(...
    user_filename, ['definesound' WS '([A-Za-z0-9_]+)' WS '([^\s]+)' ...
                    WS NUM WS NUM WS NUM WS NUM WS NUM]);
numsounds = numel(user_toks);

sounds(2:numsounds) = sounds;

for i=1:numsounds
    sounds(i).def = user_toks{i}{1};
    sounds(i).fn = user_toks{i}{2};
    sounds(i).pitch(1) = str2double(user_toks{i}{3});
    sounds(i).pitch(2) = str2double(user_toks{i}{4});
    sounds(i).prio = str2double(user_toks{i}{5});
    sounds(i).flags = str2double(user_toks{i}{6});
    sounds(i).volume = str2double(user_toks{i}{7});
end

if (nargin > 1)
    defs_toks = open_and_read_re(...
        defs_filename, ['define' WS '([A-Za-z0-9_]+)' WS NUM]);

    numdefs = numel(defs_toks);

    defs = struct();

    for i=1:numdefs
        if (isvarname(defs_toks{i}{1}))
            defs.(defs_toks{i}{1}) = str2double(defs_toks{i}{2});
        end
    end

    maxsounddef = 0;

    snddefnum = zeros(1,numsounds);

    for i=1:numsounds
        try
            defnum = defs.(sounds(i).def);
            snddefnum(i) = defnum;
        catch
            warning(sprintf('%d''th parsed sound (%s, %s) failed define look-up', i, ...
                            sounds(i).fn, sounds(i).def));
            continue;
        end

        if (defnum > maxsounddef)
            maxsounddef = defnum;
        end
    end

    haveholes = false;
    if (find(snddefnum==0))
        warning('we have holes...');
        haveholes = true;
    end
    tmp = snddefnum;
    tmp(tmp==0) = [];
    if (~isequal(sort(tmp), unique(tmp)))
        warning('we have aliases...');  % more than one candidate for one sound
    end

    sounds_in_deforder = dummysound;
    sounds_in_deforder(1:maxsounddef) = dummysound;

    sounds(numsounds+1) = dummysound;
    snddefnum(snddefnum==0) = numsounds+1;

    % reorder!
    sounds_in_deforder(snddefnum) = sounds(1:numsounds);

    % pass the reordered struct array back
    sounds = sounds_in_deforder;
end

end  % primary function


function toks = open_and_read_re(filenam, the_regex)
	toks = [];

    fid = fopen(filenam);

    if (fid < 0)
        error(sprintf('Couldn''t open "%s"\n', filenam));
    end

    txt = fread(fid, Inf, '*char')';
    fclose(fid);

    toks = regexp(txt, the_regex, 'tokens');  % 'tokens' is more unified than 'names' between MATLAB and Octave
end
