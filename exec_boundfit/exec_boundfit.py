import numpy as np
import os
from pathlib import Path
import subprocess


def exec_boundfit(infile, filemode='ascii',
                  xcol=1, ycol=2,
                  fittype=None,
                  xmin=None, xmax=None,
                  medfiltwidth=1,
                  rescaling=None, xfactor=None, yfactor=None,
                  poldeg=2, knots=None, xi=1000, alfa=2.0, beta=0.0, tau=0.0,
                  side=1, yrmstol=1E-5, nmaxiter=1000,
                  crefine=None, nrefine=None,
                  sampling=1000,
                  outbasefilename=None):
    """Execute boundfit

    Parameters
    ----------
    infile : str
        Input file name containing the spectra to be fitted.
    filemode : str
        Label indicating the format of the input file. 
        It must be either 'ascii' or 'fits'.
    xcol : integer
        Column number corresponding to X data.
    ycol : integer
        Column number corresponding to Y data.
    fittype : int
        It must be one of the following:
        1: Simple polynomial (generic version)
        2: Simple polynomial (simplified version)
        3: Adaptive splines
    xmin : float
        Mininum X value to be employed. If None, use minimum value
        in data set.
    xmax : float
        Maximum X to be employed. If None, use maximum value
        in data set.
    medfiltwidth : int
        Window size for median filtering (1=no filtering).
    rescaling : string
        If not None, it must be either 'normalise' or 'factors'.
        The option 'normalise' indicates that the data ranges
        are normalised to [-1,1]. The option 'factors' allows the
        user to apply a different multiplicative factor to each
        range.
    xfactor : float
        Multiplicative factor for X data when rescaling='factors'.
    yfactor : float
        Multiplicative factor for Y data when rescaling='factors'.
    poldeg : integer
        Polynomial degree for fittype=1 or 2.
    knots : integer or array-like object
        Total number of knots (single number) or array with
        intermediate knot location. This parameter is needed when
        using fittype=3.
    xi : float
        Asymmetry coefficient.
    alfa : float
        Power for distances.
    beta : float
        Power for errors.
    tau : float
        Cut-off parameter for errors.
    side : int
        Boundary side: 1 for upper- and 2 for lower-boundary fits.
    yrmstol : float
        Numerical precision YRMSTOL.
    nmaxiter : int
        Maximum number of iterations.
    crefine : str
        Type of refinement: 'XY' refine both X and Y location of
        each knot, whereas 'Y' refines only in the Y direction.
    nrefine : int
        Number of refinements of the X and Y knot location.
    sampling : int
        Sampling between xmin and xmax when computing the output
        fit.
    outbasefilename : str
        Output file name to store fit results. If None, the base name
        from infile is used, appending the following suffixes:
        - _linfit.bfg: fit computed from 'xmin' to 'xmax' using
          'sampling' points.
        - _predf.bfg: predictions for each data point within the
          fitted range
        - _predo.bfg: predictions for each data point in the
          original data set (including points outside fitted range)
        - _coeff.bfg: fit coefficients
        - .log: execution log containing the terminal output
        
    """
    
    # protections
    if filemode not in ['ascii', 'fits']:
        raise ValueError('Invalid mode in exec_boundfit: ' + str(filemode))
    if fittype is None:
        raise ValueError('You must specify a value for fittype')
    if filemode == 'fits':
        raise ValueError("mode='fits' not implemented yet")
    if rescaling == 'rescale':
        if xfactor is None:
            raise ValueError('You must specify a value for xfactor')
        if yfactor is None:
            raise ValueError('You must specify a value for yfactor')
    if fittype == 3:
        if knots is None:
            raise ValueError('You must specify a value for knots')
        if type(knots) is not int:
            knots = np.asarray(knots)
        if crefine is not None:
            if nrefine is None:
                raise ValueError('You must specify a value for nrefine')
        
    # determine output file names
    if outbasefilename is None:
        outbasefilename = Path(infile).stem
    outfile0 = outbasefilename + '_data.bft'
    outfile1 = outbasefilename + '_linfit.bft'
    outfile2 = outbasefilename + '_predf.bft'
    outfile3 = outbasefilename + '_predo.bft'
    outfile4 = outbasefilename + '_coeff.bft'
    logfile = outbasefilename + '.log'
    
    # remove output files if they already exists
    for dumfile in (outfile0, outfile1, outfile2, outfile3, outfile4, logfile):
        if os.path.exists(dumfile):
            print('WARNING> Deleting existing file: ' + dumfile)
            p = subprocess.Popen('rm ' + dumfile, shell=True)
            p.wait()
    p = subprocess.Popen('touch .running_BoundFit', shell=True)
    p.wait()
    
    # execute boundfit
    p = subprocess.Popen(
        'boundfit', shell=True,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        close_fds=True,
        universal_newlines=True
    )
    
    def pwrite(value):
        p.stdin.write(str(value) + '\n')
        
    pwrite(infile)    # Input data file name
    pwrite(0)         # No. of initial rows to be skipped
    pwrite(0)         # No. of rows to be read (0=ALL)
    pwrite(xcol)      # Column No. for X data    
    pwrite(ycol)      # Column No. for Y data    
    pwrite(0)         # Column No. for err(Y) data (0=NONE)
    pwrite('n')       # Using the whole x-range
    
    # Xmin?
    if xmin is None:
        pwrite('')
    else:
        pwrite(xmin)
    
    # Xmax?
    if xmax is None:
        pwrite('')
    else:
        pwrite(xmax)

    # Median filtering
        pwrite(medfiltwidth)

    # Normalise data ranges to [-1,+1] (y/n) or (r)escale
    if rescaling is None:
        pwrite('n')
    elif rescaling == 'normalise':
        pwrite('y')
    elif rescaling == 'factors':
        pwrite('r')
        pwrite(xfactor)   # Multiplicative factor for X data
        pwrite(yfactor)   # Multiplicative factor for Y data

    # Select type of fit:
    # (1) Simple polynomial (generic version)
    # (2) Simple polynomial (simplified version)
    # (3) Adaptive splines
    pwrite(fittype)
    
    if fittype in [1, 2]:
        if fittype == 1:
            pwrite('n')   # Using fit constraints
        pwrite(poldeg)    # Polynomial degree
        pwrite(xi)        # Asymmetry coefficient (xi)
        if fittype == 1:
            pwrite(alfa)  # Power for distances
            pwrite(beta)  # Power for errors
        if fittype == 2:
            pwrite('n')   # Weighting with errors
        pwrite(tau)       # Cut-off parameter for errors (tau)
        pwrite(side)      # Side: 1=upper, 2=lower
        pwrite(yrmstol)   # YRMSTOL for coefficients
        pwrite(nmaxiter)  # Nmaxiter
        if fittype == 1:
            pwrite('n')   # Incremental fit of coefficients
    elif fittype == 3:
        pwrite('n')       # Using fit constraints
        if type(knots) is np.ndarray:
            # note that in this case the array of knots do not
            # include the two knots at the borders
            nknots = len(knots) + 2
        elif type(knots) is int:
            nknots = knots
        else:
            raise ValueError('Invalid knots value: ', str(knots))
        pwrite(nknots)    # Number of knots
        if type(knots) is np.ndarray:
            pwrite('n')   # Equidistant knot arrangement
            for iknot in range(nknots - 2):
                pwrite(knots[iknot])  # X-coordinate of intermediate knot
        elif type(knots) is int:
            pwrite('y')   # Equidistant knot arrangement
        pwrite(xi)        # Asymmetry coefficient (xi)
        pwrite(alfa)      # Power for distances
        pwrite(beta)      # Power for errors
        pwrite(tau)       # Cut-off parameter for errors (tau)
        pwrite(side)      # Side: 1=upper, 2=lower
        pwrite(yrmstol)   # YRMSTOL for coefficients
        pwrite(nmaxiter)  # Nmaxiter
        pwrite(1111)      # NSEED, negative to call srand(time())
        pwrite('n')       # Enhanced verbosity
        if crefine is not None:
            if crefine == 'XY':
                pwrite('r')  # Refine X and Y position-> all knots (one at a time)
            elif crefine == 'Y':
                pwrite('y')  # Refine Y position -> all knots (one at a time)
            else:
                raise ValueError('Unexpectec crefine value: ' + str(crefine))
            pwrite(nrefine)   # Nrefine
        pwrite(0)         # Exit refinement process
    else:
        raise ValueError('Invalid fittype: ' + str(fittype))
    
    for option, outfile in zip(
        ['D', '1', '2', '3', 'C', '0'],
        [outfile0, outfile1, outfile2, outfile3, outfile4, '']
    ):
        # Option?
        # (D) Save fitted data
        # (1) Save last fit
        # (2) Save fit predictions (fitted range)
        # (3) Save fit predictions (original range)
        # (C) Save fit coefficients
        # (N) New fit
        # (0) EXIT
        pwrite(option)
        if option == '1':
            pwrite('')
            pwrite('') 
            pwrite(sampling)
        if option != '0':
            # Output ASCII file name?
            pwrite(outfile)
            print('INFO> Generating file: ' + outfile)

    p.stdin.close()

    with open(logfile, 'w') as f:
        logtext = p.stdout.read()
        f.write(logtext)
        print('INFO> Generating file: ' + logfile)

    # check that boundfit has finished properly
    if logtext.splitlines()[-1] != "End of BoundFit execution!":
        logerr = True
    else:
        logerr = False

    p.stdout.close()
    p.wait()
    
    p = subprocess.Popen('rm .running_BoundFit', shell=True)
    p.wait()

    if logerr:
        raise ValueError("Error while executing boundfit. Check log file.")
