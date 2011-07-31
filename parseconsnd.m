% SOUNDS = PARSECONSND(FILENAME)  Parse CON file and retrieve its sound definitions (`definesound').
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
function sounds=parseconsnd(filename)

sounds = struct();

sounds.def = '';
sounds.fn = '';
sounds.pitch = [0 0];
sounds.prio = 0;
sounds.flags = 0;
sounds.volume = 0;

fid = fopen(filename);

if (fid < 0)
    return
end

txt = fread(fid, Inf, '*char')';

WS = '\s+';
NUM = '(-?[0-9]+)';
re = ['definesound' WS '([A-Za-z0-9_]+)' WS '([^\s]+)' WS NUM WS NUM WS NUM WS NUM WS NUM];

toks = regexp(txt, re, 'tokens');  % 'tokens' is more unified than 'names' between MATLAB and Octave
numsounds = numel(toks);

sounds(2:numsounds) = sounds;

for i=1:numsounds
    sounds(i).def = toks{i}{1};
    sounds(i).fn = toks{i}{2};
    sounds(i).pitch(1) = str2double(toks{i}{3});
    sounds(i).pitch(2) = str2double(toks{i}{4});
    sounds(i).prio = str2double(toks{i}{5});
    sounds(i).flags = str2double(toks{i}{6});
    sounds(i).volume = str2double(toks{i}{7});
end
