% PLOTBUILDMAP(MAP [, DRAWZ, DRAWCEIL, AX])  Plot a BUILD map obtained with READMAP() with default parameters.
% By default, produce an overhead plot of the map. If DRAWZ is true, draw the floor with z coordinates.
%
% Example:
%  [d,n]=readgrp('DUKE3D.GRP', '/.*\.MAP');
%  m = cellfun(@readmap, d);
%  plotbuildmap(m(1), true)
function plotbuildmap(map, drawz, drawceil, ax)

whitewallopts = {'color',[.1 .1 .1]};
redwallopts = {'color',[.7 .7 .7], 'linewidth',0.7};

have_axes = (nargin>=3);

if (nargin<2)
    drawz = false;
end

if (nargin<3)
    drawceil = false;
end

if (~have_axes)
    try OCTAVE_VERSION
        plot3([0 0], [0 0], [0 0]);
        ax = gca;
    catch
        ax = axes();
    end
end

numsectors = double(map.numsectors);
numwalls = double(map.numwalls);
numsprites = double(map.numsprites);

xx = double(map.wall.x);
yy = -double(map.wall.y);

pfz = zeros(size(xx));
pcz = zeros(size(xx));

fzz = -double(map.sector.floorz)/16;
czz = -double(map.sector.ceilingz)/16;

% precalculate point z's
if (drawz)
    for sec=1:numsectors
        w1 = double(map.sector.wallptr(sec));
        numw = double(map.sector.wallnum(sec));

        b = w1;
        e = w1+numw-1;

        x = xx(b:e);
        y = yy(b:e);

        if (drawz)
            fz = [ones(size(x)).*fzz(sec)];
            cz = [ones(size(x)).*czz(sec)];

            fsloped = bitand(map.sector.floorstat(sec), 2);
            csloped = bitand(map.sector.ceilingstat(sec), 2);

            if (fsloped || csloped)
                v = [-(y(2)-y(1)); x(2)-x(1)];
                dists = ([x(3:end)-x(1), y(3:end)-y(1)])*v./hypot(v(1),v(2));
            end
            
            if (fsloped)
                tmp = dists.*double(map.sector.floorheinum(sec))/4096;
                fz(3:end) = fz(3:end) + tmp;
            end
            pfz(b:e) = fz;

            if (csloped && drawceil)
                tmp = dists.*double(map.sector.ceilingheinum(sec))/4096;
                cz(3:end) = cz(3:end) + tmp;
            end
            pcz(b:e) = cz;
        end
    end
end

for sec=1:numsectors
    w1 = double(map.sector.wallptr(sec));
    numw = double(map.sector.wallnum(sec));

    b = w1;
    e = w1+numw-1;

    if (all(map.wall.picnum(b:e)==0) && ~any(map.sprite.sectnum==sec) && ...
        map.sector.floorpicnum(sec)==0 && map.sector.ceilingpicnum(sec)==0)
        continue
    end

    x = [xx(b:e); nan];
    y = [yy(b:e); nan];

    if (drawz)
        fz = [pfz(b:e); nan];
        cz = [pcz(b:e); nan];
    end

    % ones where point is the last one in a loop
    lastinloop = (map.wall.point2(b:e) < (b:e).');
    loopends = find(lastinloop);

    twosided = (map.wall.nextsector(b:e) > 0);

    lastb = 1;
    for i=1:length(loopends)
        laste = loopends(i);

        % e.g. lastb=1, laste=4
        idx = [lastb:laste;
               lastb+1:laste, lastb];
        % idx = [1 2 3 4;  % walls instead of points
        %        2 3 4 1]

        %% draw red walls
        which = find(~twosided(lastb:laste));  % mask out *white* walls
        idx2 = idx;
        idx2(:, which) = numw+1;  % indexes nan
        zidx = idx2(1, (idx2(1, :)~=numw+1));  % those wall-points are two-sided
        idx2 = idx2(:).';
        % now idx2 contains only indices to points
        % which participate in at least one red wall (or numw+1 -> nan)

        if (~all(idx2==numw+1))
            opts = redwallopts;
            if (map.sector.lotag(sec)==32767)
                opts{2} = [.6 .6 1.0];
                opts{end+1} = 'linewidth'; opts{end+1}=2;
            end

            if (drawz)
                line(x(idx2), y(idx2), fz(idx2), opts{:});
                if (drawceil)
                    opts = redwallopts;
                    opts{2} = opts{2}*8/7;
                    line(x(idx2), y(idx2), cz(idx2), opts{:});
                end

                tmpx = [x(zidx), x(zidx), nan(length(zidx), 1)].';
                tmpy = [y(zidx), y(zidx), nan(length(zidx), 1)].';
                tmpz = [fz(zidx), pfz(map.wall.point2(map.wall.nextwall(zidx+w1-1))), nan(length(zidx),1)].';
                % dim: (3, num...)

                nodiff = (tmpz(1,:)==tmpz(2,:));
                tmpx(:, nodiff) = [];
                tmpy(:, nodiff) = [];
                tmpz(:, nodiff) = [];

                line(tmpx(:), tmpy(:), tmpz(:), redwallopts{:});
            else
                line(x(idx2), y(idx2), redwallopts{:});
            end
        end

        %% draw solid walls
        which = find(twosided(lastb:laste));
        idx2 = idx;
        idx2(:, which) = numw+1;
        idx2 = idx2(:).';

        if (~all(idx2==numw+1))
            opts = whitewallopts;
            if (map.sector.lotag(sec)==32767)
                opts{2}(3) = 0.7;
                opts{end+1} = 'linewidth'; opts{end+1}=2;
            end

            if (drawz)
                line(x(idx2), y(idx2), fz(idx2), opts{:});
                if (drawceil)
                    opts = whitewallopts;
                    opts{2} = opts{2}*2;
                    line(x(idx2), y(idx2), cz(idx2), opts{:});
                end
            else
                line(x(idx2), y(idx2), whitewallopts{:});
            end
        end

        lastb = laste+1;
    end
end

if (~have_axes)
    axis('equal', 'tight');
    set(ax, 'xtick',[], 'ytick',[], 'ztick',[], 'box','off');
    set(gcf, 'renderer', 'zbuffer');
end
