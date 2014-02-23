classdef LookupSet < handle
    properties
        % Cell array of 256-vectors
        lookups
    end

    methods
        function self = LookupSet(filename)
            fid = fopen(filename);

            if (fid<0)
                error(sprintf('Couldn''t open `%s''.', filename));
                return
            end

            [numlookups, cnt] = fread(fid, 1, 'uint8');
            if (cnt ~= 1)
                fclose(fid);
                error('Couln''t read one byte from file.');
            end

            self.lookups = cell(1, numlookups);

            for i=1:numlookups
                [palnum, cnt] = fread(fid, 1, 'uint8');
                if (cnt ~= 1)
                    fclose(fid);
                    error('Couln''t read one byte from file.');
                end

                [self.lookups{palnum}, cnt] = fread(fid, 256, 'uint8');
                if (cnt ~= 256)
                    fclose(fid);
                    error('Couln''t read lookup #d from file.');
                end
            end

            fclose(fid);
        end

        function compare(self, other)
            assert(isa(other, 'LookupSet'), 'OTHER must be a LookupSet object handle');

            numour = numel(self.lookups);
            numtheir = numel(other.lookups);

            for i=1:min(numour, numtheir)
                lookup1 = self.lookups{i};
                lookup2 = other.lookups{i};

                if (isempty(lookup1) ~= isempty(lookup2))
                    fprintf('%d: one has, other doesn''t\n', i);
                    continue
                end

                if (~isequal(lookup1, lookup2))
                    fprintf('%d: %d color indices differ\n', i, sum(lookup1(:) ~= lookup2(:)));
                end
            end
        end

        function printRemap16(self)
            for i=1:numel(self.lookups)
                remaptab = self.lookups{i};

                if (~isempty(remaptab))
                    remaptab = reshape(remaptab, 16, 16);
                    cmptab = repmat(remaptab(1, :), 16, 1);

                    hexs = floor(cmptab(1, :) / 16);

                    for j=1:16
                        cmptab(:, j) = cmptab(1, j):cmptab(1, j)+15;
                    end

                    % NOTE: I'm calling 16-tuples (sexdecatuples?) 'hexs' for brevity.

                    if (isequal(cmptab, remaptab))
                        fprintf('%2d:  { ', i);
                    else
                        fprintf('%2d*: { ', i);
                    end

                    for j=1:16
                        realhex = remaptab(:, j);
                        cmphex = cmptab(:, j);

                        if (isequal(cmphex, realhex))
                            if (hexs(j) ~= j-1)
                                fprintf('[%d]=%d, ', j-1, hexs(j));
                            end
                        else
                            str = num2str(unique(floor(realhex.'/16)));
                            fprintf('[%d]={%s}, ', j-1, regexprep(str, ' +', ','));
                       end
                    end

                    fprintf('}\n');
                end
            end
        end
    end
end
