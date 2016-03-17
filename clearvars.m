## Copyright (C) 2013 David Turner
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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} clearvars (@var{varargin})
## Clear the listed variables from memory
## @end deftypefn

## Author: David Turner <novalis@novalis.org>

function clearvars (varargin)

  vars = [[],[]];
  mode = 1; #-except changes this to 2
  regexp_mode = 'glob';
  r = 0;
  is_global = "";
  global_switch = "";
  for arg = 1:nargin
    cur_arg = varargin{arg};
    if strcmp(cur_arg, "-global")
      is_global = "\"global\", ";
      global_switch = " -global ";
      continue
    endif

    cur_arg = varargin{arg};
    if strcmp(cur_arg, "-except")
      mode = 2;
      r = 0;
      continue
    endif

    if strcmp(cur_arg, "-regexp")
      regexp_mode = "regexp";
      continue
    endif
    r += 1;

    vars(r, mode).type = regexp_mode;
    vars(r, mode).var_name = cur_arg;
  endfor

  if r == 0 && mode == 1
    evalin("caller", "clear");
    return;
  endif

  [r, c] = size(vars(1,:));
  matches = {};
  v =vars(1, :);

  if r && strjoin({vars(1, :).var_name},"")
    for arg = 1:r
      to_remove = vars(arg, 1);
      #get a list of matching vars
      if strcmp(to_remove.type, "glob")
        new_vars = evalin("caller", ["who (", is_global, "\"", to_remove.var_name, "\")"]);
      else
        new_vars = evalin("caller", ["who (\"-regexp\", ", is_global, "\"", to_remove.var_name, "\")"]);
      endif
        matches = union(matches, new_vars);
    endfor
  else
    if strcmp(is_global, "")
      matches = evalin("caller", "who");
    else
      matches = evalin("caller", "who(\"global\")");
    endif
  endif

  #handle the excepts
  [r, c] = size(vars);
  if c == 2
    excepts = vars(:, 2);
    except_vars = {};
    for i = 1:r
      except = excepts(i);
      if numfields(except) == 0
        break
      endif
      if strcmp(except.type, "glob")
        except_vars = union(except_vars, {except.var_name});
      else
        except_vars = union(except_vars, evalin("caller", ["who (\"-regexp\", ", is_global, "\"", except.var_name, "\")"]));
      end
    endfor
    matches = setdiff(matches, except_vars);
  endif

  if size(matches)
    for match = 1:size(matches)
      clear_var = matches(match);

      evalin("caller", strjoin(["clear", global_switch, clear_var], " "));
    endfor
  endif

endfunction


## basic usage
%!test
%! a1 = 1;
%! a2 = 2;
%! clearvars a1;
%! assert(!ismember("a1", who));
%! assert(ismember("a2", who));

## globs
%!test
%! a1 = 1;
%! a2 = 2;
%! clearvars a*;
%! assert(!ismember("a1", who));
%! assert(!ismember("a2", who));

## regexp
%!test
%! a1 = 1;
%! a2 = 2;
%! aa = 3;
%! clearvars -regexp a[0-9];
%! assert(!ismember("a1", who));
%! assert(!ismember("a2", who));
%! assert(ismember("aa", who));

## except
%!test
%! a1 = 1;
%! a2 = 2;
%! clearvars a* -except a2;
%! assert(!ismember("a1", who));
%! assert(ismember("a2", who));

## except regexp
%!test
%! a1 = 1;
%! a2 = 2;
%! aa = 3;
%! clearvars a* -except -regexp a[0-9];
%! assert(ismember("a1", who));
%! assert(ismember("a2", who));
%! assert(!ismember("aa", who));

## regexp except 
%!test
%! a1 = 1;
%! a2 = 2;
%! aa = 3;
%! clearvars -regexp a.* -except a[0-9];
%! assert(ismember("a1", who));
%! assert(ismember("a2", who));
%! assert(!ismember("aa", who));

## global
%!function f()
%!   assert(ismember("a3", who("global")));
%!   a1 = 1;
%!   a2 = 2;
%!   clearvars -global a*
%!   assert(ismember("a1", who));
%!   assert(ismember("a2", who));
%!   assert(!ismember("a3", who("global")));
%!endfunction
%!test
%! global a3 = 4
%! f();
