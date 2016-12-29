Using the program
=================

Setting the input data
------------------------

The execution of the program only requires to type the name of the program at the prompt line, assuming that the installation of **BoundFit** has placed the executable file at a suitable directory in the searching path of the user:

::

    % boundfit

::

  ***********************************************
         Welcome to boundfit (version 4.0)
  -----------------------------------------------
  > For more details see:
      Cardiel, N., 2009, MNRAS, 396, 680-695
  > and visit:
    https://github.com/nicocardiel/boundfit
  ***********************************************

  Input data file name (wildcars allowed) [*] ? example.dat

The first step is to indicate the name of the ASCII file containing the data for which one wants to determine the boundary fits. In this file the data must be arranged in columns. Lines starting with a "#" symbol are considered as comments and ignored.

Since the program accepts wildcars, hitting ``RETURN`` without given a file name commands the program to return a list with the existing files at the current directory. In the above example, the file :download:`example.dat <data/example.dat>` contains 100 points randomly drawn from the function :math:`y(x)={1 \over x}`, assuming an error bar :math:`\sigma_{y}=10` for all the points.

::

    No. of initial rows to be skipped.......[0] ? [RETURN]
    No. of rows to be read (0=ALL)..........[0] ? [RETURN]
    Column No. for X data.......................? 1
    Column No. for Y data.......................? 2
    Column No. for err(Y) data (0=NONE).....[0] ? [RETURN]
    
    Reading file...
    File read and closed!

    >>> No. of rows with comments (unread)......:            0
    >>> No. of rows read........................:          100
    >>> Xmin, Xmax..............................:   9.40159522E-03  0.30284780    
    >>> Ymin, Ymax..............................:   -16.781338       110.25208    
    >>> EYmin, EYmax............................:   1.00000000      1.00000000    

    Are you using the whole x-range...(y/n) [y] ? y

    Normalise data ranges to [-1,+1]  (y/n) [y] ? y

The program asks the user to indicate the initial number of rows to be skipped (remember that lines starting by "#" are ignored in any case) and the number of lines (with data) to be read. Hitting ``RETURN`` the program accepts the default values, as given within the square brackets. Next the user have to indicate the column numbers where :math:`x`, :math:`y` and :math:`\sigma_{y}` are going to be found in the ASCII file. Then the program reads the file, showing some basic information about the data. The program then allows to define the effective x-range in which to perform the fit (by default the whole x-range spanned by the data is used). After that, the program asks the user to indicate whether the data ranges have to be normalised before the fitting procedure. As explained in Appendix B of Cardiel2009_, this normalisation typically helps to reduce numerical errors and the number of iterations required during the numerical minimisation.

::

    (1) Simple polynomial (generic version)
    (2) Simple polynomial (simplified version)
    (3) Adaptive splines
    (0) EXIT
    Select type of fit........... (0,...,2) [0] ? 

At this point the user can choose between the two functional forms available for the boundary determination: simple polynomials or adaptive splines. In the case of simple polynomials, two different methods (*generic* and *simplified*) are available:

    1. **Simple polynomial (generic version)**: This option allows the user to define additional constraints (e.g. fixed points or/and derivatives) during the fit (see Appendix A in Cardiel2009_). The :math:`\alpha` and :math:`\beta` coefficients are free parameters. The minimisation is performed numerically using DOWNHILL.
    2. **Simple polynomial (simplified version)**: No additional constraints can be introduced in the fit. The coefficient :math:\alpha` is fixed to 2. The coefficient :math:`\beta` can only be either 0 (no error weighting) or 2 (error weighting). In this case the function to be minimised is the same as in ordinary least squares, and **BoundFit** performs this minimisation simply by following an iterative procedure, without the need of using the numerical method DOWNHILL.
    3. **Adaptive splines**: This option also allows the user to define additional constraints during the fit. The minimisation procedure is performed iteratively and, within each iteration, numerically using DOWNHILL.

.. note:: It is important to highlight that the polynomial fits obtained with option (1) and (2) are different since the minimisation procedures in both cases are also different. The user is advised to try both fits in order to determine which is the one that best suits her/his needs. Obviously, the fit to splines will also differ from any of the two polynomial fits.

Let's examine the three options separately.

Fitting simple polynomials (generic version)
---------------------------------------------

After selecting this type of fit, the users must specify all the relevant parameters.

::

    (1) Simple polynomial (generic version)
    (2) Simple polynomial (simplified version)
    (3) Adaptive splines
    (0) EXIT
    Select type of fit........... (0,...,2) [0] ? 1

    Are you using fit constraints.....(y/n) [n] ? [RETURN]
    Polynomial degree........... (0,...,16) [0] ? 5
    Asymmetry coefficient.........(xi) [1000.0] ? [RETURN]
    Power for distances...........(alpha) [2.0] ? [RETURN]
    Power for errors...............(beta) [0.0] ? [RETURN]
    Cut-off parameter for errors....(tau) [0.0] ? [RETURN]
    Side: 1=upper, 2=lower....... (1,...,2) [1] ? [RETURN]
    YRMSTOL for DOWNHILL.................[1E-5] ? [RETURN]
    Nmaxiter in DOWNHILL (1,...,1000000) [1000] ? [RETURN]
    Incremental fit of coefficients...(y/n) [n] ? [RETURN]

The following parameters have to be specified:

    * Use of fit constraints: *BoundFit* can perform constrained minimisation forcing the fits to pass through some fixed points. This is a refinement discussed in Appendix A of Cardiel2009_ and can be useful in some special situations. However if one just want to carry out a normal boundary fitting, this possibility can be skipped.
    * Polynomial degree for the fitted boundary.
    * Asymmetry coefficient ξ: this is one of the most important tunable parameters for the boundary fitting. Normally one needs to play around with this parameter using values ξ>>1 (typical values range from 100 to 10000).
    * Power for distances α: power employed to determine how distances are incorporated into the cost function. See Eq. (2) in Cardiel2009_.
    * Power for errors β: power employed to determine how error weighting is used. See Eq. (3) and (7) in Cardiel2009_.
    * Cut-off parameter for errors τ: this parameter allows some points to fall outside from the boundary. See Eq. (7) in Cardiel2009_.
    * Boundary side: 1 for upper boundary and 2 for lower boundary.
    * YRMSTOL for DOWNHILL: stopping criterium for the DOWNHILL simplex method. The minimisation procedure is halted when the r.m.s. of the values of the function to be minimised evaluated at all the vertices of the simplex is less than YRMSTOL.
    * Nmaxiter: maximum number of iterations allowed in DOWNHILL. Note that this is just an upper limit. However, if DOWNHILL finds a solution for the sought coefficients that satisfy the previous YRMSTOL criterum, the minimisation procedure is halted and the effective number of iterations can be much less than Nmaxiter. This parameter is used to avoid DOWNHILL for entering into an infinity loop.
    * Finally, **BoundFit** can proceed with the minimisation in an incremental way. This means that, for example, if the user is interested in fitting a polynomial of nth degree, the program first determines the polynomial of degree 0, then the polynomial of degree 1,..., and finally the polynomial of degree n. In each of these steps, the derived coefficients are used as an initial guess for the numerical minimisation of the following step. 
    
.. note:: Note that the use of this option leads to different polynomial fits. The user must check both options in order to identify which one fits her/his needs.
    
Once all the relevant parameters have been set, **BoundFit** peforms the computation of the requested fit (upper boundary in the previous example) and outputs the fitted coefficients:

::

    ***********************************************
    * Fit results:
    NEVAL:          141
    >>> A(00)= -0.29468319      4.85002558E-04
    >>> A(01)= -0.43597585      6.43450709E-04
    >>> A(02)= -0.14044669      1.89683036E-04
    >>> A(03)=   1.3940394      1.69469032E-03
    >>> A(04)=  0.62599963      1.63294651E-04
    >>> A(05)=  -1.7625306      2.29510572E-03
    -----------------------------------------------
    >>> bx,cx:    6.8155589       1.0640771    
    >>> by,cy:   1.57438889E-02  0.73579657    
    ***********************************************
    * Final coefficients:
    >>> a(00)=   144.39651    
    >>> a(01)=  -4206.3442    
    >>> a(02)=   61657.133    
    >>> a(03)=  -426852.41    
    >>> a(04)=   1371004.8    
    >>> a(05)=  -1646387.4    
    -----------------------------------------------

First the program shows the effective number of iterations NEVAL employed by DOWNHILL during the minimisation procedure. The resulting polynomial coefficients, corresponding to the normalized data ranges, are displayed as *A(00)*, *A(01)*,..., *A(05)*.

The values of *bx*, *cx*, *by* and *cy* correspond to the coefficients used for the normalization of the data ranges; see Appendix B of Cardiel2009_.

The final polynomial coefficients corresponding to the original data ranges are given as *a(00)*, *a(01)*,..., *a(05)*.

Once the fit has been computed, **BoundFit** offers the possibility of saving the results in different ways.

::

      (1) Save last fit
      (2) Save fit predictions
      (C) Save fit coefficients
      (N) New fit
      (0) EXIT
      Option..................................[0] ? 


Several options are available:

::

    (1) Save last fit

::

          Option..................................[0] ? 1
          Xmin.......................[9.40159708E-03] ? 
          Xmax...........................[0.30284780] ? 
          Number of points..... (2,...,100000) [1000] ? 
          Output ASCII file name......................? lastfit.dat

This option evaluates the fitted polynomial between *Xmin* and *Xmax* using a given number of points. The result is saved in the selected ASCII file.

::

    (2) Save fit predictions

::

    Option..................................[0] ? 2
    Output ASCII file name......................? predictions.dat

This option evaluates the fitted polynomial at the same x-coordinates of the input data, saving the result in the selected ASCII file.

:: 

    (C) Save fit coefficients

::

    Option..................................[0] ? 3
    Output ASCII file name......................? coefficients.dat

In this case the output file will contain a list with the fitted coefficients (one coefficient per line). The list is preceded by an integer number indicating the polynomial degree employed during the fit. In this particular example the contents of the file coefficients.dat is the following:

::

               5
               1   144.39651    
               2  -4206.3442    
               3   61657.133    
               4  -426852.41    
               5   1371004.8    
               6  -1646387.4    

::

    (N) New fit

This option returns the flow of the program to the menu offering the possibility to choose between a fit to a simple polynomial or to adaptive splines.

Fitting simple polynomials (simplified version)
-----------------------------------------------

After selecting this type of fit, the users must specify all the relevant parameters.

::

      (1) Simple polynomial (generic version)
      (2) Simple polynomial (simplified version)
      (3) Adaptive splines
      (0) EXIT
      Select type of fit........... (0,...,2) [0] ? 2

      Polynomial degree........... (0,...,16) [0] ? 5
      Asymmetry coefficient.........(xi) [1000.0] ? [RETURN]
      Are you weighting with errors.....(y/n) [n] ? [RETURN]
      Cut-off parameter for errors....(tau) [0.0] ? [RETURN]
      Side: 1=upper, 2=lower....... (1,...,2) [1] ? [RETURN]
      YRMSTOL for coefficients.............[1E-5] ? [RETURN]
      Nmaxiter.............(1,...,1000000) [1000] ? [RETURN]

The following parameters have to be specified:

    * Polynomial degree for the fitted boundary.
    * Asymmetry coefficient ξ: this is one of the most important tunable parameters for the boundary fitting. Normally one needs to play around with this parameter using values ξ>>1 (typical values range from 100 to 10000).
    * Error weighting: in this simplified version of the polynomial fit, the user can only choose between weighting with errors (β=2) or not (β=0). See Eq. (3) and (7) in Cardiel2009_.
    * Cut-off parameter for errors τ: this parameter allows some points to fall outside from the boundary. See Eq. (7) in Cardiel2009_.
    * Boundary side: 1 for upper boundary and 2 for lower boundary.
    * YRMSTOL for DOWNHILL: stopping criterium for the iterative procedure, which is halted when the values of the polynomial coefficients in a given iteration are the same as in the previous iteration within an error defined by YRMSTOL.
    * :math:`N_{maxiter}`: maximum number of iterations. Note that this is just an upper limit. However, if **BoundFit** finds a solution for the sought coefficients that satisfies the previous YRMSTOL criterum, the minimisation procedure is halted and the effective number of iterations can be much less than :math:`N_{maxiter}`. This parameter is used to avoid the iterative procedure for entering into an infinity loop.

Once all the relevant parameters have been set, **BoundFit** peforms the computation of the requested fit (upper boundary in the previous example) and outputs the fitted coefficients:

::

      ***********************************************
      * Initial fit results:
      >>> A(00)= -0.64562106    
      >>> A(01)= -0.45685810    
      >>> A(02)= -0.12850766    
      >>> A(03)=   1.4371268    
      >>> A(04)=  0.49451888    
      >>> A(05)=  -1.5838362    
      -----------------------------------------------
      >>> NEVAL, NFIT, NIN, NOUT:            0         100          50          50
      >>> NEVAL, NFIT, NIN, NOUT:            1         100          78          22
      >>> NEVAL, NFIT, NIN, NOUT:            2         100          91           9
      >>> NEVAL, NFIT, NIN, NOUT:            3         100          95           5
      >>> NEVAL, NFIT, NIN, NOUT:            4         100          94           6
      >>> NEVAL, NFIT, NIN, NOUT:            5         100          94           6

      ***********************************************
      * Final fit results:

      NEVAL:            5
      >>> A(00)= -0.35771856       0.0000000    
      >>> A(01)= -0.47641918       0.0000000    
      >>> A(02)=  0.11369579       0.0000000    
      >>> A(03)=   1.4995470       0.0000000    
      >>> A(04)=  0.43531218       0.0000000    
      >>> A(05)=  -1.8273156       0.0000000    
      -----------------------------------------------
      >>> bx,cx:    6.8155589       1.0640771    
      >>> by,cy:   1.57438889E-02  0.73579657    
      ***********************************************
      * Final coefficients:
      >>> a(00)=   143.41527    
      >>> a(01)=  -4084.7927    
      >>> a(02)=   59894.012    
      >>> a(03)=  -423160.47    
      >>> a(04)=   1392110.4    
      >>> a(05)=  -1706903.3    
      -----------------------------------------------

First the program shows an initial ordinary least-squares fit (with the coefficients corresponding to the normalised data ranges). Then the iterative procedure starts and for each step, the iteration number (NEVAL), number of points in the fit (NFIT) and number of points inside (NIN) and outside (NOUT) of the temporary boundary are displayed.

Next, the section "Final fit results:" displays the final number of iterations and the polynomial coefficients (still corresponding to the normalised data ranges). Immediately follows the transformation coefficients *bx*, *cx*, *by*, *cy* that are needed to recover the final polynomial coefficients in the original data ranges (see Appendix B of Cardiel2009_).

The final polynomial coefficients corresponding to the original data ranges are given as *a(00)*, *a(01)*,..., *a(05)*.

Once the fit has been computed, **BoundFit** offers the possibility of saving the results in different ways.

::

      (1) Save last fit
      (2) Save fit predictions
      (C) Save fit coefficients
      (N) New fit
      (0) EXIT
      Option..................................[0] ? 

Several options are available:

::

    (1) Save last fit

::

    Option..................................[0] ? 1
    Xmin.......................[9.40159708E-03] ? 
    Xmax...........................[0.30284780] ? 
    Number of points..... (2,...,100000) [1000] ? 
    Output ASCII file name......................? lastfit.dat

This option evaluates the fitted polynomial between Xmin and Xmax using a given number of points. The result is saved in the selected ASCII file.

::

    (2) Save fit predictions

::

    Option..................................[0] ? 2
    Output ASCII file name......................? predictions.dat

This option evaluates the fitted polynomial at the same x-coordinates of the input data, saving the result in the selected ASCII file.

::

    (C) Save fit coefficients

::

    Option..................................[0] ? c
    Output ASCII file name......................? coefficients.dat

In this case the output file will contain a list with the fitted coefficients (one coefficient per line). The list is preceded by an integer number indicating the polynomial degree employed during the fit. In this particular example the contents of the file coefficients.dat is the following:

::

               5
               1   143.41527    
               2  -4084.7927    
               3   59894.012    
               4  -423160.47    
               5   1392110.4    
               6  -1706903.3    

::

    (N) New fit

This option returns the flow of the program to the menu offering the possibility to choose between a fit to a simple polynomial or to adaptive splines.

::
    
    (0) EXIT

Stop the execution of the program.

Fitting adaptive splines
-------------------------

Similarly to the cases previously explained for simple polynomials, after selecting the type of fit, the users must specify all the relevant parameters.

::

      (1) Simple polynomial (generic version)
      (2) Simple polynomial (simplified version)
      (3) Adaptive splines
      (0) EXIT
      Select type of fit........... (0,...,2) [0] ? 3

      Are you using fit constraints.....(y/n) [n] ? [RETURN]
      Number of knots.................. (2,...,20)? 6
      Equidistant knot arrangement (y/n/r)....[y] ? n
      X-coordinate of knot # 1....................:   9.40159708E-03
      X-coordinate of knot # 6....................:   0.30284780    
      X-coordinate of knot # 2....................? 0.10
      X-coordinate of knot # 3....................? 0.12
      X-coordinate of knot # 4....................? 0.15
      X-coordinate of knot # 5....................? 0.25
      Asymmetry coefficient.........(xi) [1000.0] ? [RETURN]
      Power for distances...........(alpha) [2.0] ? [RETURN]
      Power for errors...............(beta) [0.0] ? [RETURN]
      Cut-off parameter for errors....(tau) [0.0] ? [RETURN]
      Side: 1=upper, 2=lower....... (1,...,2) [1] ? [RETURN]
      YRMSTOL for DOWNHILL.................[1E-5] ? [RETURN]
      Nmaxiter in DOWNHILL (1,...,1000000) [1000] ? [RETURN]
      NSEED, negative to call srand(time())..[-1] ? 1234
      Enhanced verbosity (y/n)................[n] ? [RETURN]

Most of the parameters are identical to the ones previously described for the case of boundary fitting to simple polynomials and they are not going to be explained again here. There are, however, a few important differences:

* Instead of a polynomial degree the user must indicate the total number of knots :math:`N_{knots}`.
* The initial knot arrangement must be set. The default option is to use an equidistant knot pattern, although the program allows the user to specify particular values for the initial X-coordinates of the inner knots (as shown in the above example) or to use an automatic arrangement in order to leave a similar number of points in each interval between consecutive knots. The initial arrangement can be refined, and this task is performed by improving the coordinates of each knot individually, one at a time chosen randomly. In order to be able to reproduce the random selection of knots when repeating the fit several times with the same input parameters, the user can specify the seed for the random number generator. Using a negative value indicates that the user wants the program to make a previous call to the system function ``srand(time())`` in order to get a random seed from the system's clock. Thus, using a positive value for NSEED allows the user to reproduce always the same results. 
* The boundary fit using adaptive splines performs a more complex minimisation process than in the case of simple polynomials. During the development of the code the program was written to output in the screen intermediate calculations. Since this information can be overwhelming for most users, by default the program assumes that the expected verbosity must be kept to a minimum.

After setting all the above parameters, **BoundFit** peforms the initial computation of a guess fit by using an equidistant pattern of knots. In this computation the y-coordinates of all the knots are refined at once using DOWNHILL.

Running DOWNHILL (minimising all the Y-coordinates)...

::

      >>> NEVAL:    188

      (1) Refine X and Y position-> 1 knot
      (2) Refine X position ------> 1 knot
      (3) Refine Y position ------> 1 knot
      (A) Add a single new knot
      (D) Delete single knot
      (M) Merge "touching" knots
      (R) Refine X and Y position-> all knots (one at a time)
      (0) EXIT
      Option..................................[0] ?

After the computation of the initial fit, **BoundFit** offers the user several possibilities to improve that fit, as shown in the previous menu. One can refine either the X or Y coordinate (or both) of a single knot, add or delete a knot, merge "touching" knots (knots that have collided after refining their location), or refine the position of all the knots (one at a time).

After the initial guess fit, the most suitable option is to refine all the knots. To do that one has to indicate the number of refinement processes :math:`N_{refine}`. Note that a refinement process is defined as the action of improving the location of all the knots, by choosing randomly a single knot, refining its coordinates, and repeating the process until finishing with all the knots.

::

      Option..................................[0] ? r
      Nrefine................... (0,...,1000) [1] ? 10
      >>> REFINEMENT #     1 --> 1,3,4,6,2,5
      >>> REFINEMENT #     2 --> 1,5,3,4,2,6
      >>> REFINEMENT #     3 --> 3,5,6,2,1,4
      >>> REFINEMENT #     4 --> 1,4,2,5,6,3
      >>> REFINEMENT #     5 --> 2,3,4,6,1,5
      >>> REFINEMENT #     6 --> 3,4,6,2,1,5
      >>> REFINEMENT #     7 --> 5,3,6,4,2,1
      >>> REFINEMENT #     8 --> 5,4,6,3,1,2
      >>> REFINEMENT #     9 --> 4,3,6,2,1,5
      >>> REFINEMENT #    10 --> 2,4,3,6,5,1

The output shows how the different knots (6 in this example) are refined. Once the refinement processes finish, the program shows again the previous menu. If one does not need to continue with the refinements, it is possible to exit from this program block and obtain the numerical results.

::

      (1) Refine X and Y position-> 1 knot
      (2) Refine X position ------> 1 knot
      (3) Refine Y position ------> 1 knot
      (A) Add a single new knot
      (D) Delete single knot
      (M) Merge "touching" knots
      (R) Refine X and Y position-> all knots (one at a time)
      (0) EXIT
      Option..................................[0] ? 0
      ***********************************************
      >>> bx,cx:    6.8155589       1.0640771    
      >>> by,cy:   1.57438889E-02  0.73579657    
      ***********************************************
      * Final knots:
      >>> Knot #01  X_knot,Y_knot:   9.40159708E-03   110.06384    
      >>> Knot #02  X_knot,Y_knot:   1.05896741E-02   106.63924    
      >>> Knot #03  X_knot,Y_knot:   9.98696908E-02   33.250072    
      >>> Knot #04  X_knot,Y_knot:   0.13728642       20.976780    
      >>> Knot #05  X_knot,Y_knot:   0.27777833       20.291601    
      >>> Knot #06  X_knot,Y_knot:   0.30284780       8.9463587    
      -----------------------------------------------
      * Final coefficients:
      >>> s_3,s_2,s_1 [01-02]:   10777584.       0.0000000      -2897.6851    
      >>> s_3,s_2,s_1 [02-03]:  -175581.95       38413.805      -2852.0464    
      >>> s_3,s_2,s_1 [03-04]:   132727.66      -8614.0732      -191.52579    
      >>> s_3,s_2,s_1 [04-05]:  -30860.875       6284.6338      -278.68597    
      >>> s_3,s_2,s_1 [05-06]:   89384.563      -6722.4756      -340.19922    
      -----------------------------------------------

      (1) Save last fit
      (2) Save fit predictions
      (C) Save fit coefficients
      (N) New fit
      (0) EXIT
      Option..................................[0] ? 

The program output contains:

    * The values of *bx*, *cx*, *by* and *cy* correspond to the coefficients used for the normalization of the data ranges; see Appendix B of Cardiel2009_.
    * The final *(x,y)* knot coordinates (from 1 to :math:`N_{knots}`).
    * The coefficients of the splines (from 1 to :math:`N_{knots}-1`), which follow the notation used in Eq. (5) of Cardiel2009_. Note that the coefficients *s_0* are not displayed since :math:`s_{0}(k)= y_{knot}(k)`.

Finally the user can save the results. The available options are the same previously explained for the case of simple polynomials. The only difference here is that in one selects option (C), the save data include both the knot locations and the spline coefficients.

::

      (1) Save last fit
      (2) Save fit predictions
      (C) Save fit coefficients
      (N) New fit
      (0) EXIT
      Option..................................[0] ? c
      Output ASCII file name......................? splinecoeff.dat

The content of the file *splinecoeff.dat* is the following:

::

           6
           1  9.40159708E-03   110.06384    
           2  1.05896741E-02   106.63924    
           3  9.98696908E-02   33.250072    
           4  0.13728642       20.976780    
           5  0.27777833       20.291601    
           6  0.30284780       8.9463587    
           1   10777584.       0.0000000      -2897.6851    
           2  -175581.95       38413.805      -2852.0464    
           3   132727.66      -8614.0732      -191.52579    
           4  -30860.875       6284.6338      -278.68597    
           5   89384.563      -6722.4756      -340.19922    

First, an integer number indicates the number of knots employed during the boundary fit. After that number the file contains the *(x,y)* coordinates of all the knots, from 1 to :math:`N_{knots}`. And finally the *s_3(k)*, *s_2(k)*, *s_1(k)* coefficients from :math:`k=1,...,N_{knots}-1` (note the order!).

Running the program within shell scripts
-----------------------------------------

A way to run the **BoundFit** with more flexibility is to execute the program with the help of a shell script. For example, the script :download:`boundfit_pol.tcsh<scripts/boundfit_pol.tcsh>` allows the users to fit a simple polynomial to a given data file with a single command line like

::

  % ./boundfit_pol.tcsh example.dat 5 1000 2 0 0 1 1000 lastfit.dat
  
The comment lines in this script explain which values are expected in the command line and in which order.

For the above script to work properly, the script file must have execute permission for the user. This can be set just by typing

::

  % chmod u+x boundfit_pol.tcsh
  
Note that the script takes the different parameters from the command line and passes them to the program at execution time. The order in which the parameters are written cannot be changed (unless the script file is modified).

The above is just a sample script. Obviously the user can employ any scripting language to wrap **BoundFit** in order to satisfy her/his own needs.

When a program is run from a script, the expected program's input does not appear on the screen while the script is being executed. For that reason an option has been introduced into **BoundFit** to avoid this problem. The program checks wether a hidden file called *.running_BoundFit* exists in the current directory. If this is the case, all the input information is sent again back to the screen. Note that this hidden file is created (and removed) at the beginning (end) of the previous sample script. 

.. _Cardiel2009: http://cdsads.u-strasbg.fr/abs/2009MNRAS.396..680C
