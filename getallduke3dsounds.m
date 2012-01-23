function [d,m,s]=getallduke3dsounds(noconvert)

if (nargin < 1)
    noconvert = false;
end

d = parseconsnd('../grp/USER.CON', '../grp/DEFS.CON');
[m,s] = loadvocs({d.fn}, noconvert);
