import numpy as np
import os
from pathlib import Path
from scipy.interpolate import interp1d
import subprocess


def exec_boundfit(infile, filemode='ascii',
                  xcol=1, ycol=2, eycol=None,
                  fittype=None,
                  xmin=None, xmax=None,
                  medfiltwidth=1,
                  rescaling=None, xfactor=None, yfactor=None,
                  poldeg=2, knots=None, xi=1000, alpha=2.0, beta=0.0, tau=0.0,
                  imode=1, rigidity=0, nrigidity=1000,
                  side=1, yrmstol=1E-5, nmaxiter=1000,
                  crefine=None, nrefine=None,
                  sampling=1000,
                  outbasefilename=None, verbosity=0):
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
    eycol : integer or None
        Column number corresponding to EY data (errors).
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
    alpha : float
        Power for distances.
    beta : float
        Power for errors.
    tau : float
        Cut-off parameter for errors.
    imode : int
        End conditions mode for spline fit (see cubspl.f for details).
        A value equal to 1 indicates the use of natural splines.
    rigidity : float
        Rigidity factor: forces the fit to minimize also the arc
        length (only for fittype=3).
    nrigidity : int
        Number of points to sample the X range in order to estimate
        the arc length (only for fittype=3 and rigidity>0.0).
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
        - _data.bft: fitted data
        - _linfit.bfg: fit computed from 'xmin' to 'xmax' using
          'sampling' points.
        - _predf.bfg: predictions for each data point within the
          fitted range
        - _predo.bfg: predictions for each data point in the
          original data set (including points outside fitted range)
        - _coeff.bfg: fitted coefficients
        - .log: execution log containing the terminal output
    verbosity : int
        Verbosity level:
        - 0: none
        - 1: only the input file name
        - 2: input file name and additional details
        
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
            # check knots are sorted
            if np.any(knots[:-1] > knots[1:]):
                raise ValueError('Knot locations must be sorted!')
        if crefine is not None:
            if nrefine is None:
                raise ValueError('You must specify a value for nrefine')
        
    if verbosity > 0:
        print('[exec_boundfit] Working with file: ' + str(infile))

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
            if verbosity > 1:
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
        p.stdin.write('{}\n'.format(str(value)))
        
    pwrite(infile)     # Input data file name
    pwrite(0)          # No. of initial rows to be skipped
    pwrite(0)          # No. of rows to be read (0=ALL)
    pwrite(xcol)       # Column No. for X data
    pwrite(ycol)       # Column No. for Y data
    if eycol is None:  # Column No. for err(Y) data (0=None)
        pwrite(0)
    else:
        pwrite(eycol)
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
            pwrite(alpha)  # Power for distances
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
        pwrite(alpha)      # Power for distances
        pwrite(beta)      # Power for errors
        pwrite(tau)       # Cut-off parameter for errors (tau)
        pwrite(rigidity)  # Rigidity factor (0=none)
        if rigidity > 0:  # Number of points to sample arc length
            pwrite(nrigidity)
        pwrite(imode)     # IMODE (end conditions mode)
        pwrite(side)      # Side: 1=upper, 2=lower
        pwrite(yrmstol)   # YRMSTOL for coefficients
        pwrite(nmaxiter)  # Nmaxiter
        pwrite(1111)      # NSEED, negative to call srand(time())
        pwrite('n')       # Enhanced verbosity
        if nknots > 2:
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
            if verbosity > 1:
                print('INFO> Generating file: ' + outfile)

    p.stdin.close()

    with open(logfile, 'w') as f:
        logtext = p.stdout.read()
        f.write(logtext)
        if verbosity > 1:
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


class BoundaryDef:
    """
    Auxiliary class to store parameters to fit a single boundary.

    Parameters
    ----------
    xminfit : float
        Minimum X value to be employed in the fit. None indicates
        that no restriction is applied.
    xmaxfit : float
        Maximum X value to be employed in the fit. None indicates
        that no restriction is applied.
    xminuseful : float
        Minimum useful X value to be employed when computing the
        fitted boundary. None indicates that no restriction is applied.
    xmaxuseful : float
        Maximum useful X value to be employed when computing the
        fitted boundary. None indicates that no restriction is applied.
    knots : integer or array-like object
        Total number of knots (single number) or array with
        intermediate knot location. If zero, no spline fit is performed
        and the original data is returned.
    xi : float
        Asymmetry coefficient.
    alpha : float
        Power for distances.
    beta : float
        Power for errors.
    tau : float
        Cut-off parameter for errors.
    rigidity : float
        Rigidity factor: forces the fit to minimize also the arc
        length (only for fittype=3).
    nrigidity : int
        Number of points to sample the X range in order to estimate
        the arc length (only for fittype=3 and rigidity>0.0).
    crefine : str
        Type of refinement: 'XY' refine both X and Y location of
        each knot, whereas 'Y' refines only in the Y direction.
    nrefine : int
        Number of refinements of the X and Y knot location.
    side : int
        Boundary side: 1 for upper- and 2 for lower-boundary fits.
    outbasefilename : str
        Prefix for output file names. See documentation of exec_boundfit()
        for details.

    Attributes
    ----------
    Identical to parameters.

    """
    def __init__(self,
                 xminfit=None, xmaxfit=None,
                 xminuseful=None, xmaxuseful=None,
                 knots=None,
                 xi=1000.0, alpha=2.0, beta=0.0, tau=0.0,
                 rigidity=0.0, nrigidity=1000,
                 crefine=None,
                 nrefine=100, side=1, outbasefilename='test'):

        self.xminfit = xminfit
        self.xmaxfit = xmaxfit
        self.xminuseful = xminuseful
        self.xmaxuseful = xmaxuseful
        self.knots = knots
        self.xi = xi
        self.alpha = alpha
        self.beta = beta
        self.tau = tau
        self.rigidity = rigidity
        self.nrigidity = nrigidity
        self.crefine = crefine
        self.nrefine = nrefine
        self.side = side
        self.outbasefilename = outbasefilename


class SuperBoundary:
    """
    Merge boundary regions previously defined with BoundDef.

    Note: fittype=3 (adaptive splines) is used when calling exec_boundfit()

    Parameters
    ----------
    xfit : 1D array-like object
        X values to be fitted.
    yfit : 1D array-like object
        Y values to be fitted.
    eyfit : 1D array-like object or None
        EY values to be used in the fit.
    listboundregions : list of BoundaryDef objects or None
        List with BoundaryDef instances providing the required
        parameters to be employed to fit each individual boundary.
    xfactor : float or None
        Multiplicative factor for X data.
    yfactor : float or None
        Multiplicative factor for Y data.
    medfiltwidth : int
        Window size for median filtering (1=no filtering).

    Attributes
    ----------
    Identical to parameters.

    """

    def __init__(self, xfit, yfit, eyfit=None, listboundregions=None,
                 xfactor=None, yfactor=None, medfiltwidth=None, imode=1):
        if listboundregions is None:
            raise SystemError('listboundregions=None')
        self.xfit = np.asarray(xfit)
        self.yfit = np.asarray(yfit)
        if eyfit is None:
            self.eyfit = None
        else:
            self.eyfit = np.asarray(eyfit)
        self.xfactor = xfactor
        self.yfactor = yfactor
        self.medfiltwidth = medfiltwidth
        self.imode = imode
        xmin = min(self.xfit)
        xmax = max(self.xfit)
        self.nbr = len(listboundregions)
        for ibr in range(self.nbr):
            br = listboundregions[ibr]
            if not isinstance(br, BoundaryDef):
                raise ValueError('Expected BoundaryDef instance not found')
            if br.xminfit is None:
                br.xminfit = xmin
            if br.xmaxfit is None:
                br.xmaxfit = xmax
            if br.xminuseful is None:
                br.xminuseful = br.xminfit
            if br.xmaxuseful is None:
                br.xmaxuseful = br.xmaxfit
            # check boundary regions are given in the right order
            if ibr > 0:
                br_prev = listboundregions[ibr-1]
                if br.xminuseful < br_prev.xminuseful:
                    raise SystemError('Wrong xminuseful={} for region #{}'.format(br.xminuseful, ibr))
                if br.xmaxuseful < br_prev.xmaxuseful:
                    raise SystemError('Wrong xmaxuseful={} for region #{}'.format(br.xmaxuseful, ibr))

        self.listboundregions = listboundregions

        # generate temporary output file to store the data to be fitted
        dumfile = 'test_tmp.dat'
        if os.path.exists(dumfile):
            p = subprocess.Popen('rm ' + dumfile, shell=True)
            p.wait()
        xcol = 1
        ycol = 2
        if eyfit is None:
            np.savetxt(dumfile, np.column_stack([xfit, yfit]))
            eycol = None
        else:
            np.savetxt(dumfile, np.column_stack([xfit, yfit, eyfit]))
            eycol = 3
        # perform the individual fits
        for br in listboundregions:
            if br.knots == 0:
                # do not fit: return original data
                xknot = []
                yknot = []
                xlast = None
                ylast = None
                with open('test_data.bft', 'wt') as f:
                    for xdum, ydum in zip(xfit, yfit):
                        if br.xminfit <= xdum <= br.xmaxfit:
                            f.write('{:e}  {:e}\n'.format(xdum, ydum))
                            if len(xknot) == 0:
                                xknot = [xdum]
                                yknot = [ydum]
                            xlast = xdum
                            ylast = ydum
                xknot.append(xlast)
                yknot.append(ylast)
                br.xknot = np.array(xknot)
                br.yknot = np.array(yknot)
                np.savetxt('test_predo.bft', np.column_stack([xfit, yfit]))
            elif br.knots >= 2:
                # spline fit
                exec_boundfit(
                    infile=dumfile,
                    xcol=xcol, ycol=ycol, eycol=eycol,
                    medfiltwidth=self.medfiltwidth,
                    xmin=br.xminfit, xmax=br.xmaxfit,
                    fittype=3, rescaling='factors',
                    xfactor=self.xfactor, yfactor=self.yfactor,
                    knots=br.knots,
                    xi=br.xi, alpha=br.alpha, beta=br.beta, tau=br.tau,
                    imode=self.imode,
                    rigidity=br.rigidity, nrigidity=br.nrigidity,
                    crefine=br.crefine, nrefine=br.nrefine,
                    side=br.side,
                    outbasefilename=br.outbasefilename
                )
                filec = 'test_coeff.bft'
                with open(filec) as f:
                    coeffdata = f.readlines()
                nknots = int(coeffdata[0].split()[0])
                xknot = []
                yknot = []
                for i in range(nknots):
                    xknot.append(float(coeffdata[i + 1].split()[1]))
                    yknot.append(float(coeffdata[i + 1].split()[2]))
                br.xknot = np.array(xknot)
                br.yknot = np.array(yknot)
            else:
                raise SystemError('Invalid knots: {}'.format(br.knots))
            filed = 'test_data.bft'
            tablad = np.genfromtxt(filed)
            br.xfitd = tablad[:, 0]
            br.yfitd = tablad[:, 1]
            filef = 'test_predo.bft'
            tablaf = np.genfromtxt(filef)
            br.predo = tablaf[:, 1]
            br.funinterp = interp1d(xfit, br.predo, kind='linear')

        nxvalues = len(self.xfit)
        yboundary = np.zeros(nxvalues, dtype=float)
        nfit = np.zeros(nxvalues, dtype=int)

        # merge different regions into a single continuum (point by point)
        for i in range(nxvalues):
            xdum = self.xfit[i]
            if xdum < listboundregions[0].xminuseful:
                # left extrapolation
                yboundary[i] = listboundregions[0].predo[i]
            elif xdum > listboundregions[self.nbr-1].xmaxuseful:
                # right extrapolation
                yboundary[i] = listboundregions[self.nbr-1].predo[i]
            else:
                # interpolation
                list_br = []
                ibr_left = -1
                ibr_right = -1
                for ibr in range(self.nbr):
                    br = listboundregions[ibr]
                    if br.xminuseful <= xdum <= br.xmaxuseful:
                        list_br.append(ibr)
                    if br.xmaxuseful < xdum:
                        ibr_left = ibr
                    if ibr_right < 0:
                        if br.xminuseful > xdum:
                            ibr_right = ibr
                if len(list_br) == 0:
                    # no region available for this point (interpolate from left and right regions)
                    if ibr_left < 0 or ibr_right < 0:
                        raise SystemError('Unexpected ibr_left={}, ibr_right={} for x={}'.format(
                            ibr_left, ibr_right, xdum
                        ))
                    br_left = listboundregions[ibr_left]
                    x1 = br_left.xmaxuseful
                    y1 = br_left.funinterp(x1)
                    br_right = listboundregions[ibr_right]
                    x2 = br_right.xminuseful
                    y2 = br_right.funinterp(x2)
                    yboundary[i] = y1 + (xdum - x1) * (y2 - y1)/(x2 - x1)
                elif len(list_br) == 1:
                    # single region: the easiest case
                    yboundary[i] = listboundregions[list_br[0]].predo[i]
                elif len(list_br) == 2:
                    # two overlapping regions: perform merge
                    x1 = listboundregions[list_br[1]].xminuseful
                    x2 = listboundregions[list_br[0]].xmaxuseful
                    if x1 == x2:
                        y1 = listboundregions[list_br[0]].funinterp(x1)
                        y2 = listboundregions[list_br[1]].funinterp(x2)
                        yboundary[i] = (y1 + y2) / 2
                    elif x1 > x2:
                        raise SystemError('Unexpected x1={} > x2={}'.format(x1, x2))
                    else:
                        f = (xdum - x1) / (x2 - x1)
                        y1 = listboundregions[list_br[0]].funinterp(xdum)
                        y2 = listboundregions[list_br[1]].funinterp(xdum)
                        yboundary[i] = (1 - f) * y1 + f * y2
                else:
                    raise SystemError('Triple overlap for x={}'.format(xdum))

        self.yboundary = np.array(yboundary)
        self.nfit = nfit

        # merge knots
        self.xknot = listboundregions[0].xknot
        self.yknot = listboundregions[0].yknot
        self.knotregion = np.ones_like(self.xknot, dtype=int)
        if self.nbr > 1:
            for ireg in range(1, self.nbr):
                self.xknot = np.concatenate((self.xknot, listboundregions[ireg].xknot))
                self.yknot = np.concatenate((self.yknot, listboundregions[ireg].yknot))
                self.knotregion = np.concatenate(
                    (self.knotregion,
                     np.ones_like(listboundregions[ireg].xknot, dtype=int) * (ireg + 1))
                )

    def plot(self, ax=None,
             xmin=None, xmax=None, ymin=None, ymax=None,
             xlabel=None, ylabel=None, title=None):

        if ax is None:
            raise ValueError('ax=None is not valid in this function')

        ax.plot(self.xfit, self.yfit, color='C0', linestyle='-', alpha=0.3, label='Original data')

        for ibr, br in enumerate(self.listboundregions):
            if self.medfiltwidth is not None:
                label = None
                if ibr == 0:
                    label = 'Fitted data'
                ax.plot(br.xfitd, br.yfitd, color='black', alpha=0.5, label=label)
            xdum = self.xfit
            ydum = br.predo
            lok = np.logical_and(br.xminfit <= xdum, xdum <= br.xmaxfit)
            color = 'C{:d}'.format(ibr + 1)
            ax.plot(xdum[lok], ydum[lok], color=color, linestyle=':')
            lok = np.logical_and(br.xminuseful <= xdum, xdum <= br.xmaxuseful)
            ax.plot(xdum[lok], ydum[lok], color=color,
                    label='Continuum region#{}'.format(ibr + 1))
            ax.plot(br.xknot, br.yknot, 'o', color=color, alpha=0.5)
        ax.plot(self.xfit, self.yboundary, color='k', linestyle=':', label='fitted continuum')

        if xmin is not None:
            ax.set_xlim(left=xmin)
        if xmax is not None:
            ax.set_xlim(right=xmax)
        if ymin is not None:
            ax.set_ylim(bottom=ymin)
        if ymax is not None:
            ax.set_ylim(top=ymax)
        if xlabel is not None:
            ax.set_xlabel(xlabel)
        if ylabel is not None:
            ax.set_ylabel(ylabel)
        if title is not None:
            ax.set_title(title)
        ax.legend()

    def save_ascii(self, fname, normalize=False):
        if normalize:
            meanvalue = np.mean(self.yboundary)
        else:
            meanvalue = 1.0
        np.savetxt(fname, np.column_stack([self.xfit, self.yboundary/meanvalue]))
