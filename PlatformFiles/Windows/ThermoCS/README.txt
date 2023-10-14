************************************************************************
******************************  ThermoC  *******************************
************************************************************************

CONTENTS:

- CONCEPT
- INSTALLATION
- USAGE
- SUBSTANCE-SPECIFIC DATA AND PARAMETERS
- MODEL-DEPENDENT SUBROUTINES FOR PURE FLUIDS
- MODEL-DEPENDENT SUBROUTINES FOR MIXTURES
- HISTORY

									
************************************************************************

CONCEPT

ThermoC is a program package for the calculation of thermodynamic
properties of pure fluid and binary fluid mixtures with arbitrary
equations of state (EOS) and mixing theories. The package consists
of main programs and some general thermodynamics subroutines which
are independent of the models used, and a (relatively small)
amount of model-dependent code, which can easily be extended by
the user. By linking the model-independent programs with the
model-dependent code, executable programs for a thermodynamic model
(an equation of state or a mixing theory) are obtained.

ThermoC uses dynamic loading ("shared libraries") to access model-
dependent code. 

The program package contains the following directories:
bin:		executables
html:		the ThermoC web interface
include:	header files
lib:		object file archives
main:		main programs
models:		model-specific subroutines
thsub:		model-independent subroutines
tmp:		temporary input files of the ThermoC web interface

These directories are linked to the thermoc directory:
data:		some sample input files and other useful data
ideal:		caloric properties of the perfect gas
parameters:	EOS parameters for the models
reaction:	thermochemical data for dehydration and analogous reactions
solid:		sublimation or melting curve data
viscosity:	parameters for the friction theory of viscosity


At present the following main programs are available:

charact1:	Brown's characteristic curves (Joule inversion, Boyle,
		Joule-Thomson inversion curves)
check1:		consistency test for user-supplied EOS modules
checkN:		consistency test for mixture modules
crit2:		critical curves of binary mixtures
difflimit1:	binary diffusion coefficient at zero concentration
expandN:	adiabatic expansion curves
ffe1:		vapour pressure curve of a pure fluid
ffe2:		VLE, LLE for binary mixtures (obsolete)
ffeN:		VLE, LLE for multi-component mixtures
mixN:		temperature and volume change upon isenthalpic-isobaric
		or isenthalpic-isentropic mixing of pure fluids
phase2:		VLE, LLE, SLE, SGE of binary mixtures (combines ffe2 and sfe2,
		but uses another search algorithm)
reduc1:		calculation of pure-fluid EOS parameters from exp. data
reduc2:		EOS cross parameter estimation for binary mixtures
reduc21:	EOS pure-fluid parameter estimation from mixture data
rsfe1:		dehydration of a solid compound (and analogous reactions)
sfe1:		sublimation pressure curve of a pure fluid
sfe2:		SLE, SGE for binary mixtures
sle1:		melting pressure curve of a pure compound
spinodal1:	spinodal curve of a pure compound
spinodal2:	spinodal curves of binary mixtures
surf1:		surface tension (from viscosity)
transit2:	solid/fluid flash of binary mixture; transitiometer simulation
virN:		virial coefficients of pure fluids or mixtures
visco1:		viscosity of pure fluids (friction theory)
viscofit1:	fitting of friction theory parameters
xthN:		single phase properties of fluids (including excess properties)
xth1s:		thermodynamic properties of a single solid phase
xth2s:		single phase properties of impure solids


************************************************************************

INSTALLATION
 
 0. Requirements:

    The compilation requires an ANSI C/C++ compiler. The Hewlett-Packard
    HP-UX cc and the GNU gcc are known to be sufficient.

    The (optional) user interface bin/thermoc is a UNIX shell script.
    The POSIX shell of HP-UX supports it; bash does not. Linux users
    are advised to install the Korn shell (pdksh or ksh).

    The (optional) web interface requires a local webserver that
    supports PHP4 or PHP5; Apache2 is known to work well.

    The (optional) graphical user interface (GUI) is based on the Qt4
    environment; it should be installed if this feature is required.
    Furthermore, "qmake" is required.


 1. Mathematical subroutines

 1.1 Unpacking:

     "cd" to a directory under which the mathC library is to reside
     (usually your home directory) and enter
	gzip -cd mathc.tar.gz | tar -xvf -
     This will create the directory math.

 1.2 Compiling:

     Go to math/C and enter
       ./configure
       make
     This will create a makefile for your computer and run the
     compilations. If the configure script or the make task fails,
     you need to edit the makefile manually. Refer to the README
     file in math/C for this.

     Repeat this step for the math/C++ directory.


 2. ThermoC parameters and data

 2.1 Unpacking

     "cd" to a directory under which the ThermoC parameters are to reside
     (usually your home directory) and enter
	gzip -cd thermoc_data.tar.gz | tar -xvf -
     This will create the directory thermoc_data.


 3. ThermoC

 3.1 Unpacking:

     "cd" to a directory under which the ThermoC program package is to reside
     (usually your home directory) and enter
	gzip -cd thermoc.tar.gz | tar -xvf -
     This will create the directory thermoc.

 3.2 Customizing:

     Go to thermoc and enter
	./configure
     If this succeeds, proceed with step 3.3; otherwise do the following
     depending on the error message you see:

 3.2.1 "cannot find math directory"
 
     Compile the math package (mathc.tar.gz) you received with the ThermoC
     software, then run configure again.

     If you installed the math package not under $HOME/math, invoke
     configure this way:
       MATHHOME=whereever_I_put_it ./configure

 3.2.1 "cannot find thermoc_data directory"
 
     Unpack the thermoc_data package (thermoc_data.tar.gz) you received with
     the ThermoC software, then run configure again.

     If you installed the thermoc_data package not under $HOME/thermoc_data,
     invoke configure this way:
       THPARAMS=whereever_I_put_it ./configure

 3.2.3 "operating system unknown"

     Edit the configure script (around line 90) and enter the necessary
     compiler and loader options (use one of the existing sections as
     a template!). Specifically, you need to give
     - the command that invokes the C++ compiler
     - the C++ compiler options for subroutines, using "position independent
       code" (PIC, needed for shared libraries)
     - the command that invokes the ANSI-C compiler
     - the C compiler options for subroutines, using "position independent
       code" (PIC, needed for shared libraries); furthermore, you must
       indicate which method is used for dynamic loading: The dlopen()
       method is activated with the option -DUSE_DLOPEN (This applies
       to some older C code only; the more recent C++ code always uses
       dlopen().)
     - the C compiler options for main programs which use shared libraries
     - the loader command that builds shared libraries
     - the loader command that builds an executable
     - the library options
     - the path to a POSIX compatible shell
     - the command to run a simple terminal
     - the command to write a string containing control characters to
       your terminal


 3.3 Compiling:

     Simply enter
	make

     If you want to use the GUI, also enter
       make gui


 3.4 Set permissions:

     Run the command
	set_permissions
     (See the next chapter for details!)



 4. Security/Access

    The set_permissions script lets you define who is allowed to use ThermoC
    and to write/modify parameter files. The default permissions are:
    - read and write access for the owner,
    - read and write access for users of the web interface (they must
      give a password first),
    - read access for members of the same group (they can run ThermoC
      programs except those that write parameter files),
    - no access for others.

    The web interface is optional. It requires that a web server process
    is running locally, e.g., apache2. Access to the ThermoC web pages
    is controlled by password. The password file, .htpasswd, is maintained
    with the command
      htpasswd [-c] $THHOME/.htpasswd USER
    USER is an arbitrary user name. It is possible, but not necessary,
    to use the names from /etc/passwd. Use the "-c" option to create
    a password file or to erase an existing one.

    In order to turn off password authentification, delete or rename
    html/.htaccess!

    The web server needs access to the ThermoC files. There are basically
    two ways to achieve this:
    1. You can link the ThermoC web pages to the HTTP daemon's document
       directory:
	  ln -s $THHOME/html /srv/www/htdocs/thermoc
       (assuming that this is the right place). Note that you need
       superuser privileges to do this!
    2. You can link the web pages to a HTML directory under your $HOME
       directory (which is usually named "public_html"):
          ln -s $THHOME/html $HOME/public_html
       For this method the HTTP daemon must run with the "user_dir"  and
       the "FollowSymLinks" options.
    Both methods require that the HTTP daemon can follow symbolic
    links (for Apache2 set the option "FollowSymLinks" in
    /etc/apache2/default-server.conf).

    The set_permissions script lets you customize the access permissions
    of the WWW pages, once these links have been set.

    The script may need the user name associated with the HTTP server
    daemon. It will make a guess, but you will be asked anyway.

    The error message "cannot set the access permissions" can occur
    if you want to block other users of the same computer from using
    ThermoC, but want to use the web interface. In this case the
    set_permissions script tries to generate ACLs (extended permissions).
    An error at this stage can have several reasons:
    - Your operating system does not support ACLs.
      [Install ACL support!]
    - You have ACL support installed, but your file system cannot handle
      ACL.
      [Linux users: ext2 and ext3 file systems must be mounted with the
      option "acl"; modify /etc/fstab accordingly, e.g.,
	...
	/dev/hda6 /home ext3 noatime,acl 1 2
	...
      You have to unmount and mount for the change to take effect.]
    - Your computer uses other ACL commands, e.g., "chacl" instead of
      "setfacl".
      [Look up the man pages of your ACL commands and edit the
      set_permissions script accordingly.]

    If you cannot use ACLs, you may set the file permissions of your
    ThermoC installation to give "others" access. This might compromise
    your security, however.

************************************************************************

USAGE

To perform thermodynamic calculations, make sure that your command path
contains thermoc/bin:
	THHOME=$HOME/thermoc; export THHOME
	PATH=$PATH:$THHOME/bin; export PATH
(assuming that you are using a UNIX platform and the sh/ksh/bash shell)

If you receive an error message about missing subroutines/libraries,
try setting the load path explicitly:
	LD_LIBRARY_PATH=$THHOME/lib:$MATHHOME/C/lib:$MATHHOME/C++/lib
	export LD_LIBRARY_PATH


with the web interface:

  Bring up a browser and point it to this URL:
    http://localhost/thermoc/index.html			(for Method 1)
  or
    http://localhost/~MY_NAME/thermoc/index.html	(for Method 2)

  If your computer is connected to the Internet and has its HTTP port
  open, you can access ThermoC also from other computers - world wide,
  if desired - by substituting your computer's IP or DNS name for
  "localhost".


with the GUI:

  Run
    gui/qt_thermoc/qt_thermoc

  Make selections on the GUI form (you may have to press "submit" or
  "select" buttons after some entries in order to make them take
  effect), then press "run". The results (if any) will appear in an
  edit window below. Modify the window contents, then press "save" to
  save the results to a file.


with thermoc:

  Enter 
	thermoc
  and follow the instructions. Results are shown in a display window
  *and* written to a log file.

  If you enter "?" when asked for the equation of state or mixing theory
  or select a nonexisting compound name/parameter set, the possible
  options are listed.

  The thermoc script maintains history files: If you run a program a
  second time, your previous entried will be displayed.
  - Hit RETURN to accept the displayed value.
  - Enter a new value to override the old one.
  - Enter a single dot to terminate a program.
    (If done at the wrong time, this may cause error messages.)


without thermoc:

  Enter the program name followed by the abbreviation of the equation of
  state (= directory names under thermoc/models) and eventually
  of the mixing theory, e.g.,
	ffe1 vdW
	ffeN vdW 1F
  to do vapour-liquid equilibria of pure fluids or binary mixtures using
  the van der Waals equation and 1-fluid theory. The available programs
  can be found in thermoc/main, the available equations of state in
  thermoc/models.

  Note that all error messages and prompts go to stderr, whereas regular
  output goes to stdout. Thus an output redirection like
	ffe1 vdW >results
  will save only the computed results to file "results", not the prompt
  messages. If you need to capture these, too, use
	ffe1 vdW 2>&1 >results

  Before you can compute properties, however, you have to store EOS
  parameters. Here is an example of a calculation:

  - Calculate parameters of nitrogen for the Redlich-Kwong equation (for
    a few parameters we have sample input files):
	reduc1 RK <thermoc/data/reduc1/N2.R

  - Now calculate the vapour pressure at 78 K, and then the vapour pressure
    curve from 60 K to 130 K with increment 5 K:
	ffe1 RK
	nitrogen
	1
	78
	(60 130 5)
	CTRL-d


Instructions on how to provide input can be found in the comment sections
at the top of each main program.


************************************************************************

SUBSTANCE-SPECIFIC DATA AND PARAMETERS

parameters:

thermoc/parameters contains the parameters for the various
equations of state. More specifically:
- thermoc/parameters/EOS contains the parameter files for the equation
  of state EOS
- thermoc/parameters/EOS/MIXRULE contains the parameter files for
  the mixing theory MIXRULE of the equation of state EOS
For details see parameters/README.

The parameter files are ASCII files; they can be viewed and modified
with any text editor (although the latter is not recommended).


ideal gas data:

thermoc/ideal stores tables of ideal gas heat capacities (as
(T, Cpm) pairs, ascending temperature order) in ASCII files.


solid state data:

thermoc/solid stores thermodynamic data of pure solid compounds
(mostly sublimation pressure curves and solid molar volumes). It is
possible to have more than 1 solid phase per substance. For details
see the solid/README file.

Note that these data must be entered in descending order.


************************************************************************

MODEL-SPECIFIC SUBROUTINES FOR PURE FLUIDS

For each equation of state there is a special directory under "models",
e.g. models/vdW. It contains at least a subroutine file mdl1.C
containing the model-dependent class definitions and subroutines:

ppar:		pointer to parameters in the class definition
par_in:		read parameters from a file
par_out:	print parameters
Ar:		residual Helmholtz energy
Z:		compression factor (Z = pV/nRT)
vcalc:		"recipe" for calculating the molar density for given
		pressure and temperature
xinit:		estimates for reduced critical properties
crit:		critical conditions

The first parameters of an EOS have a fixed meaning:
par[0]:		molar mass in g/mol
par[1]:		T*, characteristic temperature in K
par[2]:		v*, characteristic volume in cm^3/mol
Beyond that, the user is free to define parameters of his own.

All substance-specific data are communicated between subroutines
via the class "Model1". The auxiliary class Model1i is used for
dynamic linking of model functions. Proper reference to EOS parameters
is done this way:

- in class Model1i functions:

  double Model1i::Ar(double rho, double T) const {
    ...
    molar_mass = *_M;
    ...
  }

- in other functions:

  void f(const Model1 *mdl, ...) {
    ...
    b = mdl->param(2);
    ...
  }

It is recommended that the existing mdl1.C files are used as
templates when new files are constructed.


************************************************************************

MODEL-SPECIFIC SUBROUTINES FOR MIXTURES

These are contained in directories under the EOS directory, e.g.
for the 1-fluid mixing rules applied to the van der Waals EOS
see models/vdW/1F. The contents of the directories are similar
to the directories for pure fluids. The model-specific subroutines
are called mdlN.C and must contain the following:
ppar:		pointer to parameters in the class definition
par_alloc:	allocate memory for interaction parameters
par_in:		read parameters from a file
par_out:	print parameters
ident:		definition of the mixing theory name, number of
		parameters
Ar:		residual Helmholtz energy of the mixture
Z:		compression factor (Z = pV/nRT)
vcalc:		"recipe" for calculating the molar density for given
		pressure and temperature
mixpar:		combining rules

In analogy to Model1/Model1i the communication of parameters is done
via the class ModelN, and the auxiliary class ModelNi provides dynamic
linking.  Class ModelNi also contains the function mixtheory() for
1-fluid type mixing theories, but ModelN does not need it. The user
is free to define his own ModelNi when mixtheory() is not needed,
or needed with a different set of arguments.

It is of course possible to invoke the pure-fluid subroutines from
mdlN.C . But it should be noted that ThermoC is not restricted to
1-fluid theory.

Parameters of mixtures are accessed this way:

- in class ModelNi member functions:

  double ModelNi::Ar(double rho, double T, const Vector<double>& x) const {
    ...
    mass[2] = *_M[2];			// molar mass of component #2
    Ts00 = *_Ts[0][0];			// charact. temperature of #0
    Ts00 = mdl1[0]->param(1);		// alternatively
    Ts01 = *_Ts[0][1];			// Ts cross parameter
    ...
  }

- in other functions:

  void f(const ModelN *mdl, ...) {
    ...
    b[i] = mdl->param(i, 2, 0);		// covolume of component #i
    b[i] = mdl->mdl1[i]->param(2);	// alternatively
    b01 = mdl->param(0, 2, 1);		// covolume cross parameter
    ...
  }


************************************************************************

HISTORY

1		Fortran77 version

2		1st C version

3		2nd C version
3.4		with dynamic loading of model-specific libraries
3.5		limited solubility in the solid state
3.6		ASCII parameter files, separate parameter directory
3.7		new invocation style for pressure and Helmholtz energy
3.8		new definitions of species structure, partial support
		of multicomponent mixtures
3.9		"fence" contours for reference EOS; calculation of
		viscosity and limiting diffusion coefficients

4		C++ version
