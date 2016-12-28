Installation
============

Typical Installation procedure
-------------------------------

**BoundFit** is written in Fortran 77 and it has been developed in a linux machine. The installation procedure should be trivial for most users. The only requirement is to have a relatively recent linux distribution, including a fortran compiler (e.g. g77 or gfortran) and the GNU autotools.

To install **BoundFit** you need to perform the following steps:

1. Download the compressed distribution tar file: :dowload:`boundfit-4.0.tar.gz<data/boundfit-4.0.tar.gz>`. Decompress and extract the contents of this file.
2. The previous actions must have created a new subdirectory containing the distribution source tree. Move into that subdirectory, e.g.

::

    % cd boundfit-4.0
    
3. Install the program by executing:

::

    % ./configure
    % make

4. At this point a compiled version of the program must be available in the subdirectory `src`. It is usual to finish the installation by placing this executable file in a common place, like `/usr/local/bin`, which traditionally is included in the searching path for executable files in any user account. This last step is accomplished by executing

::

    % sudo make install

The sudo command indicates that the user must have root privileges to write into the `/usr/local/bin` subdirectory. An alternative installation directory can be intentionally specified using the corresponding option of the configure command. For example, if a user wants to install the software in a subdirectory called `/home/user/software/bin/`, the appropriate command sequence is

::

    % ./configure --bindir=/home/user/software/bin
    % make
    % make install

Another less elegant possibility consists in defining the appropriate alias indicating that the program will instead reside in the original source directory. For example, if you are using the UNIX C-shell:

::

    % alias boundfit /home/user/software/boundfit-03.01/src/boundfit
    
or under the UNIX bash shell:

::

    % alias boundift=/home/user/software/boundfit-03.01/src/boundfit
    
(replacing `/home/user/software/` by the appropriate path)

Refined installation procedure
-------------------------------

By default, some "magic" numbers (Fortran PARAMETERs) are pre-defined in the code, but can be adequately modified at installation time. Note that **BoundFit** reads the data to be fitted from an external ASCII file.

    **NDATAMAX**: Maximum number of points to be fitted (default 100000 points).
    
    **LENLINEA**: Maximum length (in characters) of any single line of the ASCII file containing the data to be fitted (default: 1000 characters).

Both parameters can be modified to suit the user needs by indicating the desired values when executing configure. For example

::

    % ./configure NDATAMAX=3000 LENLINEA=7500
    % make
    % ...



