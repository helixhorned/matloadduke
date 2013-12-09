% XX = UPSAMPLE(X, FS, FSNEW)
function [xx, xi] = upsample(x, fs, fsnew)
    fac = fsnew/fs;
    assert(fac > 1, 'FSNEW must be greater than FS');

    n = numel(x);
    npow2 = n;
    if (mod(npow2,2)==1)
        npow2 = npow2+1;
    end

    nn = ceil(fac*n);
    nnpow2 = nn;
    if (mod(nnpow2,2)==1)
        nnpow2 = nnpow2+1;
    end

    fac2 = nnpow2/npow2;

%    [n npow2 nn nnpow2]

    fx = fft(x, npow2);
    assert(numel(fx) == npow2);

    % 1: DC
    % 2:n+1: freqs...

    fxpad = fx(1:npow2/2+1);
    fxpad(npow2/2+2:nnpow2) = 0;
    fxpad(nnpow2+(0:-1:(-npow2/2+1))) = conj(fxpad(2:npow2/2+1));

    xinv = ifft(fxpad);
    xx = fac2*real(xinv(1:nn));
    if (nargout >= 2)
        xi = fac2*imag(xinv(1:nn));
    end
end
