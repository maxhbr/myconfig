" Vim syntax file
" Language:	Matlab
" Maintainer:	Fabrice Guy <fabrice.guy at gmail dot com>
"		Original authors: Mario Eusebio and Preben Guldberg
" Last Change:	2008 Oct 16 : added try/catch/rethrow and class statements
" 		2008 Oct 28 : added highlighting for most of Matlab functions
" 		2009 Nov 23 : added 'todo' keyword in the matlabTodo keywords 
" 		(for doxygen support)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword matlabStatement		return function
syn keyword matlabConditional		switch case else elseif end if otherwise break continue
syn keyword matlabRepeat		do for while
syn keyword matlabStorageClass		classdef methods properties events persistent global
syn keyword matlabExceptions		try catch rethrow throw

syn keyword matlabTodo			contained  TODO NOTE FIXME XXX
syn keyword matlabImport		import
" If you do not want these operators lit, uncommment them and the "hi link" below
syn match  matlabRelationalOperator	"\(==\|\~=\|>=\|<=\|=\~\|>\|<\|=\)"
syn match matlabArithmeticOperator	"[-+]"
syn match matlabArithmeticOperator	"\.\=[*/\\^]"
syn match matlabLogicalOperator		"[&|~]"
syn keyword matlabBoolean		true false

syn match matlabLineContinuation	"\.\{3}"

" String
syn region matlabString			start=+'+ end=+'+	oneline

" If you don't like tabs
syn match matlabTab			"\t"

" Standard numbers
syn match matlabNumber		"\<\d\+[ij]\=\>"
" floating point number, with dot, optional exponent
syn match matlabFloat		"\<\d\+\(\.\d*\)\=\([edED][-+]\=\d\+\)\=[ij]\=\>"
" floating point number, starting with a dot, optional exponent
syn match matlabFloat		"\.\d\+\([edED][-+]\=\d\+\)\=[ij]\=\>"
syn keyword matlabConstant	eps Inf NaN pi


" Transpose character and delimiters: Either use just [...] or (...) aswell
syn match matlabDelimiter		"[][]"
"syn match matlabDelimiter		"[][()]"
syn match matlabTransposeOperator	"[])a-zA-Z0-9.]'"lc=1

syn match matlabSemicolon		";"

syn match matlabComment			"%.*$"	contains=matlabTodo,matlabTab
syn region matlabBlockComment        start=+%{+    end=+%}+ contains=matlabBlockComment


" trigonometric
syn keyword matlabFunc 			acos acosd acosh acot acotd acoth acsc acscd acsch asec asecd asech asin asind asinh
syn keyword matlabFunc 			atan atan2 atand atanh cos cosd cosh cot cotd coth csc cscd csch hypot sec secd
syn keyword matlabFunc 			sech sin sind sinh tan tand tanh
" exponential
syn keyword matlabFunc 			exp expm1 log log10 log1p log2 nextpow2 nthroot pow2 reallog realpow realsqrt sqrt
" Complex
syn keyword matlabFunc 			abs angle complex conj cplxpair  imag real sign unwrap
" Rounding and Remainder
syn keyword matlabFunc 			ceil fix floor idivide mod rem round
"Discrete Math (e.g., Prime Factors)
syn keyword matlabFunc 			factor factorial gcd isprime lcm nchoosek perms primes rat rats
"Polynomials
syn keyword matlabFunc 			conv deconv poly polyder polyeig polyfit polyint polyval polyvalm residue roots
"Numeric Types
syn keyword matlabFunc 			arrayfun cast cat class find intmax intmin intwarning ipermute isa isequal isequalwithequalnans isfinite isinf isnan isnumeric isreal isscalar isvector permute realmax realmin reshape squeeze zeros
"Characters and Strings
syn keyword matlabFunc 			cellstr char eval findstr isstr regexp sprintf sscanf strcat strcmp strcmpi strings strjust strmatch strread strrep strtrim strvcat
"Structures
syn keyword matlabFunc 			cell2struct deal fieldnames getfield  isfield isstruct orderfields rmfield setfield struct struct2cell structfun
"Cell Arrays
syn keyword matlabFunc 			cell cell2mat celldisp cellfun cellplot iscell iscellstr mat2cell num2cell
"Function Handles
syn keyword matlabFunc 			feval func2str functions str2func
"Java Classes and Objects
syn keyword matlabFunc 			clear depfun exist im2java inmem javaaddpath javaArray javachk Generate javaclasspath javaMethod javaObject javarmpath methodsview usejava which
"Data Type Identification
syn keyword matlabFunc 			ischar isfloat isinteger isjava islogical isobject validateattributes who whos
"Data type conversion
"Numeric
syn keyword matlabFunc 			double int8 int16 int32 int64 single typecast uint8 uint16 uint32 uint64
"String to Numeric
syn keyword matlabFunc 			base2dec bin2dec hex2dec hex2num str2double str2num unicode2native
"Numeric to String
syn keyword matlabFunc 			dec2base dec2bin dec2hex int2str mat2str native2unicode num2str
"Other Conversions
syn keyword matlabFunc 			datestr logical num2hex str2mat
"String Creation
syn keyword matlabFunc 			blanks
"String Identification
syn keyword matlabFunc 			isletter isspace isstrprop validatestring 
"String Manipulation
syn keyword matlabFunc 			deblank lower upper
"String Parsing
syn keyword matlabFunc 			regexpi regexprep regexptranslate strfind strtok 
"String Evaluation
syn keyword matlabFunc 			evalc evalin
"String Comparison
syn keyword matlabFunc 			strncmp strncmpi
"Bit-wise Functions
syn keyword matlabFunc			bitand bitcmp bitget bitmax bitor bitset bitshift bitxor swapbytes
"Logical Functions
syn keyword matlabFunc			all and any iskeyword isvarname not or xor
"Predefined Dialog Boxes
syn keyword matlabFunc dialog errordlg export2wsdlg helpdlg inputdlg listdlg msgbox printdlg printpreview questdlg uigetdir uigetfile uigetpref uiopen uiputfile uisave uisetcolor uisetfont waitbar warndlg
"Deploying User Interfaces
syn keyword matlabFunc guidata guihandles movegui openfig
"Developing User Interfaces
syn keyword matlabFunc addpref getappdata getpref ginput guide inspect isappdata ispref rmappdata rmpref setappdata setpref uisetpref waitfor waitforbuttonpress
"User Interface Objects
syn keyword matlabFunc uibuttongroup uicontextmenu uicontrol uimenu uipanel uipushtool uitoggletool uitoolbar menu
"Finding Objects from Callbacks
syn keyword matlabFunc findall findfigs findobj gcbf gcbo 
"GUI Utility Functions
syn keyword matlabFunc align getpixelposition listfonts selectmoveresize setpixelposition textwrap uistack
"Controlling Program Execution
syn keyword matlabFunc uiresume uiwait	
"Basic Plots and Graphs
syn keyword matlabFunc box errorbar hold  loglog  plot plot3 plotyy polar semilogx semilogy subplot
"Plotting Tools
syn keyword matlabFunc figurepalette pan plotbrowser plotedit plottools propertyeditor rotate3d  showplottool zoom 

"Annotating Plots
syn keyword matlabFunc annotation clabel datacursormode datetick gtext legend  line rectangle texlabel title xlabel ylabel zlabel
"Area, Bar, and Pie Plots
syn keyword matlabFunc area bar barh bar3 bar3h pareto pie pie3
"Contour Plots
syn keyword matlabFunc contour contour3  contourc contourf ezcontour ezcontourf
"Direction and Velocity Plots
syn keyword matlabFunc comet comet3 compass feather quiver quiver3 
"Discrete Data Plots
syn keyword matlabFunc stairs stem stem3
"Function Plots
syn keyword matlabFunc ezmesh ezmeshc ezplot ezplot3 ezpolar ezsurf ezsurfc fplot 
"Histograms
syn keyword matlabFunc hist histc rose
"Polygons and Surfaces
syn keyword matlabFunc convhull cylinder delaunay delaunay3 delaunayn dsearch dsearchn ellipsoid fill fill3 inpolygon pcolor  polyarea rectint ribbon slice sphere tsearch tsearchn voronoi waterfall
"Scatter/Bubble Plots
syn keyword matlabFunc plotmatrix scatter scatter3
"Animation
syn keyword matlabFunc getframe im2frame movie  noanimate
"Bit-Mapped Images
syn keyword matlabFunc frame2im image imagesc imfinfo imformats imread imwrite ind2rgb
"Printing
syn keyword matlabFunc frameedit hgexport orient print printopt saveas 
"Handle Graphics
syn keyword matlabFunc allchild ancestor copyobj delete gca gco get ishandle propedit set
"Object 
syn keyword matlabFunc axes figure hggroup hgtransform light patch
"root object	
syn keyword matlabFunc surface text
"Plot Objects
syn keyword matlabFunc clf close closereq drawnow gcf hgload hgsave newplot opengl refresh
"Axes Operations
syn keyword matlabFunc axis cla grid ishold makehgtform
"Operating on Object Properties
syn keyword matlabFunc linkaxes linkprop refreshdata
"Data analysis
"Basic Operations
syn keyword matlabFunc brush cumprod cumsum linkdata prod sort sortrows sum 
"Descriptive Statistics
syn keyword matlabFunc corrcoef cov max mean median min mode std var
"Filtering and Convolution
syn keyword matlabFunc conv2 convn detrend filter filter2 
"Interpolation and Regression
syn keyword matlabFunc interp1 interp2 interp3 interpn mldivide mrdivide
"Fourier Transforms
syn keyword matlabFunc fft fft2 fftn fftshift fftw ifft ifft2 ifftn ifftshift
"Derivatives and Integrals
syn keyword matlabFunc cumtrapz del2 diff gradient trapz 
"File Operations
syn keyword matlabFunc cd copyfile dir fileattrib filebrowser isdir lookfor ls matlabroot mkdir movefile pwd recycle rehash rmdir toolboxdir type what
"Operating System Interface
syn keyword matlabFunc clipboard computer dos getenv hostid maxNumCompThreads perl setenv system unix winqueryreg
"MATLAB Version and License
syn keyword matlabFunc ismac ispc isstudent isunix javachk license prefdir usejava ver verLessThan version 
"Basic Information
syn keyword matlabFunc disp display isempty issparse length ndims numel size 
"Elementary Matrices and Arrays
syn keyword matlabFunc blkdiag diag eye freqspace ind2sub linspace logspace meshgrid ndgrid ones rand randn sub2ind 
"Array Operations
syn keyword matlabFunc accumarray bsxfun cross dot kron tril triu 
"Array Manipulation
syn keyword matlabFunc circshift flipdim fliplr flipud horzcat inline repmat rot90 shiftdim vectorize vertcat 
"Specialized Matrices
syn keyword matlabFunc compan gallery hadamard hankel hilb invhilb magic pascal rosser toeplitz vander wilkinson
"Matrix Analysis
syn keyword matlabFunc cond condeig det norm normest null orth rank rcond rref subspace trace
"Linear Equations
syn keyword matlabFunc chol cholinc condest funm ilu inv linsolve lscov lsqnonneg lu luinc pinv qr 
"Eigenvalues and Singular Values
syn keyword matlabFunc balance cdf2rdf eig eigs gsvd hess ordeig ordqz ordschur rsf2csf schur sqrtm ss2tf svd svds
"Matrix Logarithms and Exponentials
syn keyword matlabFunc expm logm 
"Factorization
syn keyword matlabFunc cholupdate planerot qrdelete qrinsert qrupdate qz 
"Interpolation
syn keyword matlabFunc griddata griddata3 griddatan interp1q interpft mkpp padecoef pchip ppval spline unmkpp 
"Delaunay Triangulation and Tessellation
syn keyword matlabFunc tetramesh trimesh triplot trisurf 
"Convex Hull
syn keyword matlabFunc convhulln	
"Voronoi Diagrams
syn keyword matlabFunc voronoin
"Cartesian Coordinate System Conversion
syn keyword matlabFunc cart2pol cart2sph pol2cart sph2cart 
"Ordinary Differential Equations (IVP)
syn keyword matlabFunc decic deval ode15i ode23 ode45 ode113 ode15s ode23s ode23t ode23tb odefile odeget odeset odextend
"Delay Differential Equations
syn keyword matlabFunc dde23 ddeget ddesd ddeset 
"Boundary Value Problems
syn keyword matlabFunc bvp4c bvp5c bvpget bvpinit bvpset bvpxtend
"Partial Differential Equations
syn keyword matlabFunc pdepe pdeval 
"Optimization
syn keyword matlabFunc fminbnd fminsearch fzero optimget optimset
"Numerical Integration (Quadrature)
syn keyword matlabFunc dblquad quad quadgk quadl quadv triplequad
"Specialized Math
syn keyword matlabFunc airy besselh besseli besselj besselk bessely beta betainc betaln ellipj ellipke erf erfc erfcx erfinv erfcinv expint gamma gammainc gammaln legendre psi
"Elementary Sparse Matrices
syn keyword matlabFunc spdiags speye sprand sprandn sprandsym
"Full to Sparse Conversion
syn keyword matlabFunc full sparse spconvert
"Working with Sparse Matrices
syn keyword matlabFunc nnz nonzeros nzmax spalloc spfun spones spparms spy 
"Reordering Algorithms
syn keyword matlabFunc amd colamd colperm dmperm ldl randperm symamd symrcm 
"Linear Algebra
syn keyword matlabFunc spaugment sprank 
"Linear Equations (Iterative Methods)
syn keyword matlabFunc bicg bicgstab cgs gmres lsqr minres pcg qmr symmlq 
"Tree Operations
syn keyword matlabFunc etree etreeplot gplot symbfact treelayout treeplot 
"Timeseries
"General Purpose
syn keyword matlabFunc getdatasamplesize getqualitydesc timeseries tsprops tstool
"Data Manipulation
syn keyword matlabFunc addsample ctranspose delsample getabstime getinterpmethod getsampleusingtime idealfilter resample setabstime setinterpmethod synchronize transpose
"Event Data
syn keyword matlabFunc addevent delevent gettsafteratevent gettsafterevent gettsatevent gettsbeforeatevent gettsbeforeevent  gettsbetweenevents
"Descriptive Statistics
syn keyword matlabFunc iqr

"Time Series Collections
"General Purpose
syn keyword matlabFunc tscollection
"Data Manipulation
syn keyword matlabFunc addsampletocollection addts delsamplefromcollection gettimeseriesnames removets settimeseriesnames
"Set Functions
syn keyword matlabFunc intersect ismember issorted setdiff setxor union unique 
"Date and Time Functions
syn keyword matlabFunc addtodate calendar clock cputime date datenum datevec eomday etime now weekday
"M-File Functions and Scripts
syn keyword matlabFunc addOptional addParamValue addRequired createCopy depdir echo input inputname inputParser mfilename namelengthmax nargchk nargin nargout nargoutchk parse pcode
"script	Script M-file description
syn keyword matlabFunc varargin varargout
"Evaluation of Expressions and Functions
syn keyword matlabFunc ans assert builtin pause run script symvar
"Timer Functions
syn keyword matlabFunc isvalid start startat stop timer timerfind timerfindall wait 
"Variables and Functions in Memory
syn keyword matlabFunc assignin datatipinfo genvarname isglobal memory mislocked mlock munlock pack
"Control Flow
syn keyword matlabFunc parfor
"Error Handling
syn keyword matlabFunc addCause error ferror getReport last lasterr lasterror lastwarn warning
"Classes and Objects
syn keyword matlabFunc addlistener addprop dynamicprops 
"events 	Display class event names
syn keyword matlabFunc findprop getdisp handle hgsetget inferiorto loadobj metaclass notify saveobj setdisp subsasgn subsindex subsref substruct superiorto 
"File Name Construction
syn keyword matlabFunc filemarker fileparts filesep fullfile tempdir tempname 
"Opening, Loading, Saving Files
syn keyword matlabFunc daqread filehandle importdata load open save uiimport winopen 
"Memory Mapping
syn keyword matlabFunc memmapfile
"Low-Level File I/O
syn keyword matlabFunc fclose feof fgetl fgets fopen fprintf fread frewind fscanf fseek ftell fwrite 

"Text Files
syn keyword matlabFunc csvread csvwrite dlmread dlmwrite textread textscan
"XML Documents
syn keyword matlabFunc xmlread xmlwrite xslt
"Microsoft Excel Functions
syn keyword matlabFunc xlsfinfo xlsread xlswrite
"Lotus 1-2-3 Functions
syn keyword matlabFunc wk1finfo wk1read wk1write
"Common Data Format (CDF)
syn keyword matlabFunc cdfepoch cdfinfo cdfread cdfwrite todatenum 
"Flexible Image Transport System
syn keyword matlabFunc fitsinfo fitsread 
"Hierarchical Data Format (HDF)
syn keyword matlabFunc hdf hdf5 hdf5info hdf5read hdf5write hdfinfo hdfread hdftool 
"Band-Interleaved Data
syn keyword matlabFunc multibandread multibandwrite 


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_matlab_syntax_inits")
  if version < 508
    let did_matlab_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink matlabTransposeOperator	matlabOperator
  HiLink matlabLineContinuation		Special
  HiLink matlabLabel			Label
  HiLink matlabConditional		Conditional
  HiLink matlabRepeat			Repeat
  HiLink matlabTodo			Todo
  HiLink matlabString			String
  HiLink matlabDelimiter		Identifier
  HiLink matlabTransposeOther		Identifier
  HiLink matlabNumber			Number
  HiLink matlabFloat			Float
  HiLink matlabConstant			Constant
  HiLink matlabImplicit			matlabStatement
  HiLink matlabStatement		Statement
  HiLink matlabSemicolon		SpecialChar
  HiLink matlabComment			Comment
  HiLink matlabBlockComment		Comment
  HiLink matlabImport			Include
  HiLink matlabBoolean			Boolean
  HiLink matlabStorageClass		StorageClass

  HiLink matlabArithmeticOperator	matlabOperator
  HiLink matlabRelationalOperator	matlabOperator
  HiLink matlabLogicalOperator		matlabOperator
  HiLink matlabOperator			Operator
  HiLink matlabExceptions		Exception
  HiLink matlabFunc			Function

"optional highlighting
  "HiLink matlabIdentifier		Identifier
  "HiLink matlabTab			Error
  delcommand HiLink
endif

let b:current_syntax = "matlab"

"EOF	vim: ts=8 noet tw=100 sw=8 sts=0
