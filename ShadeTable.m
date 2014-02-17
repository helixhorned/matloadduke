classdef ShadeTable < handle
    properties
        numshades

        % The shade table, (256 x numshades)
        sht
        % The same thing, arranged in a square (16 x 16 x numshades)
        shtsq

        % The base palette (256 x 3), range [0 .. 1]
        pal
        % The same thing, arranged in a square (16 x 16 x 3)
        palsq

        % A permutation on the base palette's color indices
        perm
    end

    methods
        % perm16: for Duke3D, [0 1, 2 3, 5 4, 6 7, 8 13, 10 11, 12 9, 14 15]
        function self = ShadeTable(filename, perm16)
            [self.pal, self.sht] = readpal(filename, true);

            self.numshades = size(self.sht, 2);

            if (nargin >= 2)
                assert(isvector(perm16) && numel(perm16)==16 && isequal(sort(perm16), 0:15), ...
                       'PERM16 must be a permutation of 0:15');
                idxs = reshape(1:256, 16, 16);
                idxs = idxs(:, perm16+1);
                idxs = reshape(idxs, 1, 256);

                self.pal = self.pal(idxs, :);

                self.sht = self.sht(idxs, :);
                self.sht = idxs(self.sht+1)-1;

                self.perm = idxs;
            end

            self.shtsq = reshape(self.sht, 16, 16, self.numshades);
            self.palsq = reshape(self.pal, 16, 16, 3);
        end

        % figure('position', [100 100, 1200 150])
        % print('basepal0_rgb.png', '-dpng', '-S1200,150')
        function plotPalRGB(self, gaps, dots)
            gaps = (nargin >= 2 && gaps);
            dots = (nargin >= 3 && dots);

            pal = self.pal;
            pp = self.pal;
            ii = (0:255);
            oii = ii;

            if (gaps)
                ii = [ii(1:32) nan ii(33:64) nan, ...
                      ii(65:80) nan ii(81:96) nan ii(97:128) nan, ...
                      ii(129:144) nan ii(145:160) nan ii(161:192) nan, ...
                      ii(193:208) nan ii(209:224) nan ii(225:240) nan ii(241:256)];

                nn = [nan nan nan];
                pp = [pp(1:32,:); nn; pp(33:64,:); nn; ...
                      pp(65:80,:); nn; pp(81:96,:); nn; pp(97:128,:); nn; ...
                      pp(129:144,:); nn; pp(145:160,:); nn; pp(161:192,:); nn; ...
                      pp(193:208,:); nn; pp(209:224,:); nn; pp(225:240,:); nn; pp(241:256,:)];
            end
            ii = ii';

            hold off;

            plot(ii, pp(:,1), 'r');
            hold on;
            plot(ii, pp(:,2), 'color', [0, .6, 0]);
            plot(ii, pp(:,3), 'b');

            if (dots)
                plot(oii([96 65]), pal([96 65],1), ':');
                plot(oii([96 65]), pal([96 65],2), ':');
                plot(oii([96 65]), pal([96 65],3), ':');

                plot(oii([144 209]), pal([144 209],1), ':');
                plot(oii([144 209]), pal([144 209],2), ':');
                plot(oii([144 209]), pal([144 209],3), ':');
            end

            set(gca, 'xtick',0:16:256, 'xlim',[1 256]);

            xlabel('color index');
            ylabel('R/G/B intensity');

            title('R/G/B of base palette 0 against color index');
        end

        function plotShadeRamp(self, cidx)
            pal = self.pal;
            pr = pal(:, 1);
            pg = pal(:, 2);
            pb = pal(:, 3);

            cidxs = self.sht(cidx+1, :);
            r = pr(cidxs+1);
            g = pg(cidxs+1);
            b = pb(cidxs+1);

            nsh = self.numshades;
            ii = 0:nsh;

            hold off;
            plot(ii, r, 'r.-');
            hold on;
            plot([0 nsh], [r(1) 0], 'r:');

            plot(ii, g, 'g.-');
            plot(ii, b, 'b.-');

            plot([0 nsh], [g(1) 0], 'color',[0 .6 0], 'linestyle',':');
            plot([0 nsh], [b(1) 0], 'b:');

            set(gca, 'xlim', [0 nsh]);

            legend('actual', 'expected (linear ramp)');

            xlabel('shade index');
            ylabel('R/G/B intensity');

            title(sprintf('R/G/B of basepal(palookup[s][%d]) against shade s', cidx));
        end
    end
end
