% UPSAMPLEALL(D,M,S, FSNEW)
function upsampleall(d,m,s, fsnew)
    if (numel(dir()) > 2)
        fprintf('upsampleall: Refusing to run in a non-empty directory.\n');
        return
    end

    numsounds = numel(d);

    for i=1:numsounds
        if (isempty(s{i}))
            continue
        end

        fn = d(i).fn;
        xx = upsample(s{i}, m(i).fs, fsnew);
        [ok, err] = writevoc(fn, xx, fsnew);
        if (~ok)
            fprintf('%s\n', err);
        end
    end
end
