## Copyright (C) 2011-2014 Carnë Draug
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{p} =} inputParser ()
## Create object @var{p} of the inputParser class.
##
## This class is designed to allow easy parsing of function arguments.  The
## class supports four types of arguments:
##
## @enumerate
## @item mandatory (see @command{addRequired});
##
## @item optional (see @command{addOptional});
##
## @item named (see @command{addParamValue} and @command{addParameter});
##
## @item switch (see @command{addSwitch}).
## @end enumerate
##
## After defining the function API with these methods, the supplied arguments
## can be parsed with the @command{parse} method and the parsing results
## accessed with the @command{Results} accessor.
##
## @end deftypefn
## @deftypefn {Accessor method} {} inputParser.Parameters
## Return list of parameter names already defined.
##
## @end deftypefn
## @deftypefn {Accessor method} {} inputParser.Results
## Return structure with argument names as fieldnames and corresponding values.
##
## @end deftypefn
## @deftypefn {Accessor method} {} inputParser.Unmatched
## Return structure similar to @command{Results}, but for unmatched parameters.
## See the @command{KeepUnmatched} property.
##
## @end deftypefn
## @deftypefn {Accessor method} {} inputParser.UsingDefaults
## Return cell array with the names of arguments that are using default values.
##
## @end deftypefn
## @deftypefn {Class property} {} inputParser.CaseSensitive = @var{boolean}
## Set whether matching of argument names should be case sensitive.  Defaults
## to false.
##
## @end deftypefn
## @deftypefn {Class property} {} inputParser.FunctionName = @var{name}
## Set function name to be used in error messages; Defaults to empty string.
##
## @end deftypefn
## @deftypefn {Class property} {} inputParser.KeepUnmatched = @var{boolean}
## Set whether an error should be given for non-defined arguments.  Defaults to
## false.  If set to true, the extra arguments can be accessed through
## @code{Unmatched} after the @code{parse} method.  Note that since
## @command{Switch} @command{Parameter} and @command{ParamValue} arguments can be mixed, it is
## not possible to know the unmatched type.  If argument is found unmatched
## it is assumed to be of the @command{ParamValue} (resp. @command{Parameter}) type and it is expected to
## be followed by a value.
##
## @end deftypefn
## @deftypefn {Class property} {} inputParser.StructExpand = @var{boolean}
## Set whether a structure can be passed to the function instead of
## parameter/value pairs.  Defaults to true.  Not implemented yet.
##
## The following example shows how to use this class:
##
## @example
## @group
## function check (varargin)
## @c The next two comments need to be indented by one for alignment
##   p = inputParser ();                      # create object
##   p.FunctionName = "check";                # set function name
##   p.addRequired ("pack", @@ischar);         # mandatory argument
##   p.addOptional ("path", pwd(), @@ischar);  # optional argument
##
##   ## create a function handle to anonymous functions for validators
##   val_mat = @@(x) isvector (x) && all (x <= 1) && all (x >= 0);
##   p.addOptional ("mat", [0 0], val_mat);
##
##   ## create two arguments of type "ParamValue"
##   val_type = @@(x) any (strcmp (x, @{"linear", "quadratic"@}));
##   p.addParamValue ("type", "linear", val_type);
##   val_verb = @@(x) any (strcmp (x, @{"low", "medium", "high"@}));
##   p.addParamValue ("tolerance", "low", val_verb);
##
##   ## create a switch type of argument
##   p.addSwitch ("verbose");
##
##   p.parse (varargin@{:@});  # Run created parser on inputs
##
##   ## the rest of the function can access inputs by using p.Results.
##   ## for example, get the tolerance input with p.Results.tolerance
## endfunction
## @end group
##
## check ("mech");           # valid, use defaults for other arguments
## check ();                 # error, one argument is mandatory
## check (1);                # error, since ! ischar
## check ("mech", "~/dev");  # valid, use defaults for other arguments
##
## check ("mech", "~/dev", [0 1 0 0], "type", "linear");  # valid
##
## ## following is also valid.  Note how the Switch argument type can
## ## be mixed into or before the ParamValue argument type (but it
## ## must still appear after any Optional argument).
## check ("mech", "~/dev", [0 1 0 0], "verbose", "tolerance", "high");
##
## ## following returns an error since not all optional arguments,
## ## `path' and `mat', were given before the named argument `type'.
## check ("mech", "~/dev", "type", "linear");
## @end example
##
## @emph{Note 1}: A function can have any mixture of the five API types but
## they must appear in a specific order.  @command{Required} arguments must be
## first and can be followed by any @command{Optional} arguments.  Only
## the @command{ParamValue}, @command{Parameter} and @command{Switch} arguments may be mixed
## together and they must appear at the end.
##
## @emph{Note 2}: If both @command{Optional} and @command{ParamValue} (resp. @command{Parameter}) arguments
## are mixed in a function API then once a string Optional argument fails to
## validate it will be considered the end of the @command{Optional}
## arguments.  The remaining arguments will be compared against any
## @command{ParamValue}, @command{Parameter} or @command{Switch} arguments.
##
## @seealso{nargin, validateattributes, validatestring, varargin}
## @end deftypefn

## -*- texinfo -*-
## @deftypefn  {Function File} {} addOptional (@var{argname}, @var{default})
## @deftypefnx {Function File} {} addOptional (@var{argname}, @var{default}, @var{validator})
## Add new optional argument to the object @var{parser} of the class inputParser
## to implement an ordered arguments type of API
##
## @var{argname} must be a string with the name of the new argument.  The order
## in which new arguments are added with @command{addOptional}, represents the
## expected order of arguments.
##
## @var{default} will be the value used when the argument is not specified.
##
## @var{validator} is an optional anonymous function to validate the given
## values for the argument with name @var{argname}.  Alternatively, a
## function name can be used.
##
## See @command{help inputParser} for examples.
##
## @emph{Note}: if a string argument does not validate, it will be considered a
## ParamValue key.  If an optional argument is not given a validator, anything
## will be valid, and so any string will be considered will be the value of the
## optional argument (in @sc{matlab}, if no validator is given and argument is
## a string it will also be considered a ParamValue key).
##
## @end deftypefn

## -*- texinfo -*-
## @deftypefn  {Function File} {} addParamValue (@var{argname}, @var{default})
## @deftypefnx {Function File} {} addParamValue (@var{argname}, @var{default}, @var{validator})
## Add new parameter to the object @var{parser} of the class inputParser to
## implement a name/value pair type of API.
##
## @var{argname} must be a string with the name of the new parameter.
##
## @var{default} will be the value used when the parameter is not specified.
##
## @var{validator} is an optional function handle to validate the given values
## for the parameter with name @var{argname}.  Alternatively, a function name
## can be used.
##
## See @command{help inputParser} for examples.
##
## @end deftypefn

## -*- texinfo -*-
## @deftypefn  {Function File} {} addParameter (@var{argname}, @var{default})
## @deftypefnx {Function File} {} addParameter (@var{argname}, @var{default}, @var{validator})
## @deftypefnx {Function File} {} addParameter (@var{argname}, @var{default}, @var{validator}, 'PartialMatchPriority', @var{priority})
## Add new parameter to the object @var{parser} of the class inputParser to
## implement a name/value pair type of API.
##
## @var{argname} must be a string with the name of the new parameter.
##
## @var{default} will be the value used when the parameter is not specified.
##
## @var{validator} is an optional function handle to validate the given values
## for the parameter with name @var{argname}.  Alternatively, a function name
## can be used.
##
## @var{priority} is a scalar value used to resolve ambiguous matchings when
## partial matching is enabled.
##
## See @command{help inputParser} for examples.
##
## @end deftypefn

## -*- texinfo -*-
## @deftypefn  {Function File} {} addRequired (@var{argname})
## @deftypefnx {Function File} {} addRequired (@var{argname}, @var{validator})
## Add new mandatory argument to the object @var{parser} of inputParser class.
##
## This method belongs to the inputParser class and implements an ordered
## arguments type of API.
##
## @var{argname} must be a string with the name of the new argument.  The order
## in which new arguments are added with @command{addrequired}, represents the
## expected order of arguments.
##
## @var{validator} is an optional function handle to validate the given values
## for the argument with name @var{argname}.  Alternatively, a function name
## can be used.
##
## See @command{help inputParser} for examples.
##
## @emph{Note}: this can be used together with the other type of arguments but
## it must be the first (see @command{@@inputParser}).
##
## @end deftypefn

## -*- texinfo -*-
## @deftypefn {Function File} {} addSwitch (@var{argname})
## Add new switch type of argument to the object @var{parser} of inputParser
## class.
##
## This method belongs to the inputParser class and implements a switch
## arguments type of API.
##
## @var{argname} must be a string with the name of the new argument.  Arguments
## of this type can be specified at the end, after @code{Required} and
## @code{Optional}, and mixed between the @code{ParamValue}.  They default to
## false.  If one of the arguments supplied is a string like @var{argname},
## then after parsing the value of @var{parse}.Results.@var{argname} will be
## true.
##
## See @command{help inputParser} for examples.
##
## @end deftypefn

## -*- texinfo -*-
## @deftypefn {Function File} {} parse (@var{varargin})
## Parses and validates list of arguments according to object @var{parser} of
## the class inputParser.
##
## After parsing, the results can be accessed with the @command{Results}
## accessor.  See @command{help inputParser} for a more complete description.
##
## @end deftypefn

## Author: Carnë Draug <carandraug@octave.org>

classdef inputParser < handle
  properties
    ## TODO set input checking for this properties
    CaseSensitive     = false;
    FunctionName      = "";
    KeepUnmatched     = false;
    PartialMatching = true;   # TODO unimplemented
    # StructExpand    = true;   # TODO unimplemented
  endproperties

  properties (SetAccess = protected)
    Parameters    = cell ();
    Results       = struct ();
    Unmatched     = struct ();
    UsingDefaults = cell ();
  endproperties

  properties (Access = protected)
    ## Since Required and Optional are ordered, they get a cell array of
    ## structs with the fields "name", "def" (default), and "val" (validator).
    Required   = cell ();
    Optional   = cell ();
    ## ParamValue and Swicth are unordered so we have a struct whose fieldnames
    ## are the argname, and values are a struct with fields "def" and "val"
    ParamValue = struct ();
    Switch     = struct ();

    ## List of ParamValues and Switch names to ease searches
    ParamValueNames = cell ();
    SwitchNames     = cell ();

    ## When checking for fieldnames in a Case Insensitive way, this variable
    ## holds the correct identifier for the last searched named using the
    ## is_argname method.
    last_name  = "";
  endproperties

  properties (Access = protected, Constant = true)
    ## Default validator, always returns scalar true.
    def_val = @() true;
    ## Default matching priority.
    def_match = 1;
  endproperties

  methods
    function addRequired (this, name, val = inputParser.def_val)
      if (nargin < 2 || nargin > 3)
        print_usage ();
      elseif (numel (this.Optional) || numel (fieldnames (this.ParamValue))
              || numel (fieldnames (this.Switch)))
        error (["inputParser.addRequired: can't have a Required argument " ...
                "after Optional, ParamValue, or Switch"]);
      endif
      this.validate_name ("Required", name);
      this.Required{end+1} = struct ("name", name, "val", val);
    endfunction

    function addOptional (this, name, def, val = inputParser.def_val)
      if (nargin < 3 || nargin > 4)
        print_usage ();
      elseif (numel (fieldnames (this.ParamValue))
              || numel (fieldnames (this.Switch)))
        error (["inputParser.Optional: can't have Optional arguments " ...
                "after ParamValue or Switch"]);
      endif
      this.validate_name ("Optional", name);
      this.validate_default ("Optional", name, def, val);
      this.Optional{end+1} = struct ("name", name, "def", def, "val", val);
    endfunction

    function addParamValue (this, name, def, val = inputParser.def_val)
      if (nargin < 3 || nargin > 4)
        print_usage ();
      endif
      this.validate_name ("ParamValue", name);
      this.validate_default ("ParamValue", name, def, val);
      this.ParamValue.(name).def = def;
      this.ParamValue.(name).val = val;
      this.ParamValue.(name).pri = inputParser.def_match;
    endfunction

    ## Add new addParameter method. 
    function addParameter (this, name, def, val = inputParser.def_val, ...
                           pmp_str, mPV)
      if (nargin < 3 || nargin == 5 || nargin > 6)
        print_usage ();
      endif

      addParamValue(this, name, def, val)
      if (nargin == 6 && strcmp (pmp_str, "PartialMatchPriority"))
        this.ParamValue.(name).pri = mPV;
      endif
    endfunction

    function addSwitch (this, name)
      if (nargin != 2)
        print_usage ();
      endif
      this.validate_name ("Switch", name);
      this.Switch.(name).def = false;
    endfunction

    function parse (this, varargin)
      if (numel (varargin) < numel (this.Required))
        if (this.FunctionName)
          print_usage (this.FunctionName);
        else
          this.error ("not enough input arguments");
        endif
      endif
      pnargin = numel (varargin);

      this.ParamValueNames = fieldnames (this.ParamValue);
      this.SwitchNames     = fieldnames (this.Switch);

      ## Evaluate the Required arguments first
      nReq = numel (this.Required);
      for idx = 1:nReq
        req = this.Required{idx};
        this.validate_arg (req.name, req.val, varargin{idx});
      endfor

      vidx = nReq;  # current index in varargin

      ## Search for a list of Optional arguments
      idx  = 0;     # current index on the array of Optional
      nOpt = numel (this.Optional);
      while (vidx < pnargin && idx < nOpt)
        opt = this.Optional{++idx};
        in  = varargin{++vidx};
        if (! opt.val (in))
          ## If it does not match there's two options:
          ##    1) input is actually wrong and we should error;
          ##    2) it's a ParamValue or Switch name and we should use the
          ##       the default for the rest.
          if (ischar (in))
            idx--;
            vidx--;
            break
          else
            this.error (sprintf ("failed validation of %s",
                                 toupper (opt.name)));
          endif
        endif
        this.Results.(opt.name) = in;
      endwhile

      ## Fill in with defaults of missing Optional
      while (idx++ < nOpt)
        opt = this.Optional{idx};
        this.UsingDefaults{end+1} = opt.name;
        this.Results.(opt.name) = opt.def;
      endwhile

      ## Search unordered Options (Switch and ParamValue)
      while (vidx++ < pnargin)
        name = varargin{vidx};
        if (this.is_argname ("ParamValue", find_parameter_name(this, name)))
          if (vidx++ > pnargin)
            this.error (sprintf ("no matching value for option '%s'",
                                 toupper (name)));
          endif
          this.validate_arg (this.last_name,
                             this.ParamValue.(this.last_name).val,
                             varargin{vidx});
        elseif (this.is_argname ("Switch", name))
          this.Results.(this.last_name) = true;
        else
          if (vidx++ < pnargin && this.KeepUnmatched)
            this.Unmatched.(name) = varargin{vidx};
          else
            this.error (sprintf ("argument '%s' is not a valid parameter",
                                  toupper (name)));
          endif
        endif
      endwhile
      ## Add them to the UsingDeafults list
      this.add_missing ("ParamValue");
      this.add_missing ("Switch");

    endfunction

    function display (this)
      if (nargin > 1)
        print_usage ();
      endif
      printf ("inputParser object with properties:\n\n");
      b2s = @(x) ifelse (any (x), "true", "false");
      printf (["   CaseSensitive   : %s\n   FunctionName    : %s\n" ...
               "   KeepUnmatched   : %s\n   PartialMatching : %s\n" ...
               "   StructExpand    : %s\n\n"],
               b2s (this.CaseSensitive), b2s (this.FunctionName),
               b2s (this.KeepUnmatched), b2s (this.PartialMatching),
               b2s (this.StructExpand));
      printf ("Defined parameters:\n\n   {%s}\n",
              strjoin (this.Parameters, ", "));
    endfunction
  endmethods

  methods (Access = private)
    function validate_name (this, type, name)
      if (! isvarname (name))
        error ("inputParser.add%s: NAME is an invalid identifier", method);
      elseif (any (strcmpi (this.Parameters, name)))
        ## Even if CaseSensitive is "on", we still shouldn't allow
        ## two args with the same name.
        error ("inputParser.add%s: argname '%s' has already been specified",
               type, name);
      endif
      this.Parameters{end+1} = name;
    endfunction

    function validate_default (this, type, name, def, val)
      if (! feval (val, def))
        error ("inputParser.add%s: failed validation for '%s' default value",
               type, name);
      endif
    endfunction

    function validate_arg (this, name, val, in)
        if (! val (in))
          this.error (sprintf ("failed validation of %s", toupper (name)));
        endif
        this.Results.(name) = in;
    endfunction

    function r = is_argname (this, type, name)
      if (this.CaseSensitive)
        r = isfield (this.(type), name);
        this.last_name = name;
      else
        fnames = this.([type "Names"]);
        l = strcmpi (name, fnames);
        r = any (l(:));
        if (r)
          this.last_name = fnames{l};
        endif
      endif
    endfunction

    function add_missing (this, type)
      unmatched = setdiff (fieldnames (this.(type)), fieldnames (this.Results));
      for namec = unmatched(:)'
        name = namec{1};
        this.UsingDefaults{end+1} = name;
        this.Results.(name) = this.(type).(name).def;
      endfor
    endfunction

    function error (this, msg)
      where = "";
      if (this.FunctionName)
        where = [this.FunctionName ": "];
      endif
      error ("%s%s", where, msg);
    endfunction

    ## Find parameter name for given substr. If no match is found,
    ## return empty string.
    function str = find_parameter_name (this, substr)
      skip = false;
      str = '';
      ## Check if substr matches any parameter name.
      ## If search was successful, skip rest of the method.
      if (this.is_argname ("ParamValue", substr))
        skip = true;
        str = substr;
      end
      ## Find best match. Throw error on ambiguous results.
      if ((this.PartialMatching) && (! skip))
        res = strncmpi (substr, this.ParamValueNames, numel(substr));
        if sum(res(:)) > 1
          ## We have found several matches.
	  ## Use matching priority to resolve conflicts by searching the match
	  ## with lowest priority.
          count = 0;
          pos = find(res);
          str = this.ParamValueNames{pos(1)};
          for idx = 2:sum(res(:))
            cand = this.ParamValueNames{pos(idx)};
            if (this.ParamValue.(cand).pri < this.ParamValue.(str).pri)
              str = this.ParamValueNames{pos(idx)};
              count = 0;
            elseif (this.ParamValue.(cand).pri == this.ParamValue.(str).pri)
              count++;
            endif
          endfor
          if (count > 0)
            this.error (sprintf (
			    "Multiple parameter names contain %s as starting substring",
			    toupper (substr)));
          else
            warning (sprintf ("Multiple matches for parameter %s. Matching priority selects %s", ...
			      toupper(substr), toupper(str)));
          endif
        elseif sum(res(:)) < 1
          str = '';
        else
          str = this.ParamValueNames{find(res)};
        endif
      endif
    endfunction
  endmethods
endclassdef

%!function p = create_p ()
%!  p = inputParser ();
%!  p.CaseSensitive = true;
%!  p.addRequired ("req1", @(x) ischar (x));
%!  p.addOptional ("op1", "val", @(x) any (strcmp (x, {"val", "foo"})));
%!  p.addOptional ("op2", 78, @(x) x > 50);
%!  p.addSwitch ("verbose");
%!  p.addParamValue ("line", "tree", @(x) any (strcmp (x, {"tree", "circle"})));
%!endfunction

## check normal use, only required are given
%!test
%! p = create_p ();
%! p.parse ("file");
%! r = p.Results;
%! assert (r.req1, "file");
%! assert (sort (p.UsingDefaults), sort ({"op1", "op2", "verbose", "line"}));
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!        {"file", "val", 78,    false,     "tree"});

## check normal use, but give values different than defaults
%!test
%! p = create_p ();
%! p.parse ("file", "foo", 80, "line", "circle", "verbose");
%! r = p.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## check optional is skipped and considered ParamValue if unvalidated string
%!test
%! p = create_p ();
%! p.parse ("file", "line", "circle");
%! r = p.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "val", 78,    false,     "circle"});

## check case insensitivity
%!test
%! p = create_p ();
%!  p.CaseSensitive = false;
%! p.parse ("file", "foo", 80, "LiNE", "circle", "vERbOSe");
%! r = p.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## check KeepUnmatched
%!test
%! p = create_p ();
%! p.KeepUnmatched = true;
%! p.parse ("file", "foo", 80, "line", "circle", "verbose", "extra", 50);
%! assert (p.Unmatched.extra, 50)

## check error when missing required
%!error <not enough input arguments>
%! p = create_p ();
%! p.parse ();

## check error when given required does not validate
%!error <failed validation of >
%! p = create_p ();
%! p.parse (50);

## check error when given optional does not validate
%!error <is not a valid parameter>
%! p = create_p ();
%! p.parse ("file", "no-val");

## check error when given ParamValue does not validate
%!error <failed validation of >
%! p = create_p ();
%! p.parse ("file", "foo", 51, "line", "round");

## check alternative method (obj, ...) API
%!function p2 = create_p2 ();
%!  p2 = inputParser;
%!  addRequired (p2, "req1", @(x) ischar (x));
%!  addOptional (p2, "op1", "val", @(x) any (strcmp (x, {"val", "foo"})));
%!  addOptional (p2, "op2", 78, @(x) x > 50);
%!  addSwitch (p2, "verbose");
%!  addParamValue (p2, "line", "tree", @(x) any (strcmp (x, {"tree", "circle"})));
%!endfunction

## check normal use, only required are given
%!test
%! p2 = create_p2 ();
%! parse (p2, "file");
%! r = p2.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "val", 78,    false,     "tree"});
%! assert (sort (p2.UsingDefaults), sort ({"op1", "op2", "verbose", "line"}));

## check normal use, but give values different than defaults
%!test
%! p2 = create_p2 ();
%! parse (p2, "file", "foo", 80, "line", "circle", "verbose");
%! r = p2.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## FIXME: This somehow works in Matlab
#%!test
#%! p = inputParser;
#%! p.addOptional ("op1", "val");
#%! p.addParamValue ("line", "tree");
#%! p.parse ("line", "circle");
#%! assert (p.Results, struct ("op1", "val", "line", "circle"));

%!function p3 = create_p3 ();
%!  p3 = inputParser;
%!  addRequired (p3, "req1", @(x) ischar (x));
%!  addOptional (p3, "op1", "val", @(x) any (strcmp (x, {"val", "foo"})));
%!  addOptional (p3, "op2", 78, @(x) x > 50);
%!  addSwitch (p3, "verbose");
%!  addParameter (p3, "line", "tree", @(x) any (strcmp (x, {"tree", "circle"})));
%!endfunction

## check normal use, only required are given
%!test
%! p3 = create_p3 ();
%! parse (p3, "file");
%! r = p3.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "val", 78,    false,     "tree"});
%! assert (sort (p3.UsingDefaults), sort ({"op1", "op2", "verbose", "line"}));

## check normal use, but give values different than defaults
%!test
%! p3 = create_p3 ();
%! parse (p3, "file", "foo", 80, "line", "circle", "verbose");
%! r = p3.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## check partial matching for addParameter
%!test
%! p3 = create_p3 ();
%! parse (p3, "file", "foo", 80, "li", "circle", "verbose");
%! r = p3.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## check that it is possible to disable partial matching
%!test
%! p3 = create_p3 ();
%! p3.KeepUnmatched = true;
%! p3.PartialMatching = false;
%! parse (p3, "file", "foo", 80, "li", "circle", "verbose");
%! r = p3.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line, p3.Unmatched.li},
%!         {"file", "foo", 80,    true,      "tree", "circle"});

## check addParamValue and partial matching
%!function p3a = create_p3a ();
%!  p3a = inputParser;
%!  p3a.PartialMatching = true;
%!  addRequired (p3a, "req1", @(x) ischar (x));
%!  addOptional (p3a, "op1", "val", @(x) any (strcmp (x, {"val", "foo"})));
%!  addOptional (p3a, "op2", 78, @(x) x > 50);
%!  addSwitch (p3a, "verbose");
%!  addParamValue (p3a, "line", "tree", @(x) any (strcmp (x, {"tree", "circle"})));
%!endfunction

## check partial matching for addParamValue
%!test
%! p3a = create_p3a ();
%! parse (p3a, "file", "foo", 80, "li", "circle", "verbose");
%! r = p3a.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line},
%!         {"file", "foo", 80,    true,      "circle"});

## check that it is possible to disable partial matching
%!test
%! p3a = create_p3a ();
%! p3a.KeepUnmatched = true;
%! p3a.PartialMatching = false;
%! parse (p3a, "file", "foo", 80, "li", "circle", "verbose");
%! r = p3a.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line, p3a.Unmatched.li},
%!         {"file", "foo", 80,    true,      "tree", "circle"});

%!function p4 = create_p4 ();
%!  p4 = inputParser;
%!  addRequired (p4, "req1", @(x) x > 0);
%!  addParameter (p4, "pp", 21, @(x) x > 10);
%!  addParameter (p4, "pq", 31, @(x) x > 15);
%!  addParamValue (p4, "qq", 41, @(x) x > 20);
%!  addParamValue (p4, "qp", 51, @(x) x > 25);
%!endfunction

## check error when partial matching is ambiguous for addparameter
%!test
%!error <Multiple parameter names contain >
%! p4 = create_p4 ();
%! p4.parse (4, 'p', 16, 'p', 11);

## check error when partial matching is ambiguous for addparamValue
%!test
%!error <Multiple parameter names contain >
%! p4 = create_p4 ();
%! p4.parse (4, 'q', 26, 'q', 41);

%!function p5 = create_p5 ();
%!  p5 = inputParser;
%!  addRequired (p5, "req1", @(x) true);
%!  addOptional (p5, "op1", "val", @(x) true);
%!  addOptional (p5, "op2", 78, @(x) true);
%!  addSwitch (p5, "verbose");
%!  addParameter (p5, "linear", 10, @(x) true);
%!  addParameter (p5, "line", 1, @(x) true);
%!endfunction

## check that partial matching does not collide with full names
%!test
%! p5 = create_p5 ();
%! p5.KeepUnmatched = true;
%! p5.PartialMatching = true;
%! parse (p5, "file", "foo", 80, "line", 2, "verbose");
%! r = p5.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line, r.linear},
%!         {"file", "foo", 80,    true,      2,      10});

%!test
%! p5 = create_p5 ();
%! p5.KeepUnmatched = true;
%! p5.PartialMatching = true;
%! parse (p5, "file", "foo", 80, "linear", 2, "verbose");
%! r = p5.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line, r.linear},
%!         {"file", "foo", 80,    true,      1,      2});

%!function p5a = create_p5a ();
%!  p5a = inputParser;
%!  addRequired (p5a, "req1", @(x) true);
%!  addOptional (p5a, "op1", "val", @(x) true);
%!  addOptional (p5a, "op2", 78, @(x) true);
%!  addSwitch (p5a, "verbose");
%!  addParamValue (p5a, "linear", 10, @(x) true);
%!  addParamValue (p5a, "line", 1, @(x) true);
%!endfunction

## check that partial matching does not collide with full names
%!test
%! p5a = create_p5a ();
%! p5a.KeepUnmatched = true;
%! p5a.PartialMatching = true;
%! parse (p5a, "file", "foo", 80, "line", 2, "verbose");
%! r = p5a.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line, r.linear},
%!         {"file", "foo", 80,    true,      2,      10});

%!test
%! p5a = create_p5a ();
%! p5a.KeepUnmatched = true;
%! p5a.PartialMatching = true;
%! parse (p5a, "file", "foo", 80, "linear", 2, "verbose");
%! r = p5a.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line, r.linear},
%!         {"file", "foo", 80,    true,      1,      2});

%!function p6 = create_p6 ();
%!  p6 = inputParser;
%!  addRequired (p6, "req1", @(x) true);
%!  addOptional (p6, "op1", "val", @(x) true);
%!  addOptional (p6, "op2", 78, @(x) true);
%!  addSwitch (p6, "verbose");
%!  addParameter (p6, "linear", 10, @(x) true, "PartialMatchPriority", 10);
%!  addParameter (p6, "line", 1, @(x) true, "PartialMatchPriority", 15);
%!endfunction

## check partial matching priority
%!test
%! p6 = create_p6 ();
%! p6.PartialMatching = true;
%! p6.KeepUnmatched = true;
%! parse (p6, "file", "foo", 80, "lin", 2, "verbose");
%! r = p6.Results;
%! assert ({r.req1, r.op1, r.op2, r.verbose, r.line, r.linear},
%!         {"file", "foo", 80,    true,      1,      2});
