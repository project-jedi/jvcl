{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreterConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for English

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvInterpreterConst;

interface

const

{JvInterpreterParser}
 {$IFDEF Delphi}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF Delphi}
 {$IFDEF BCB}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF BCB}

 {JvInterpreterFm}
  SNoReportProc = 'Procedure "JvInterpreterRunReportPreview" not found';
  SNoReportProc2 = 'Procedure "JvInterpreterRunReportPreview2" not found';

 {JvInterpreter Error Descriptions}
  JvInterpreterErrors : array [0..47] of
    record
      ID: Integer;
      Description: String;
    end
    = (
      (ID:   0; Description: 'Ok'),
      (ID:   1; Description: 'Unknown error'),
      (ID:   2; Description: 'Internal interpreter error: %s'),
      (ID:   3; Description: 'User break'),
      (ID:   4; Description: 'Re-raising an exception only allowed in exception handler'),
      (ID:   5; Description: 'Error in unit ''%s'' on line %d : %s'),
      (ID:   6; Description: 'External error in unit ''%s'' on line %d : %s'),
      (ID:   7; Description: 'Access denied to ''%s'''),
      (ID:  31; Description: 'Record ''%s'' not defined'),

      (ID:  52; Description: 'Stack overflow'),
      (ID:  53; Description: 'Type mistmatch'),
      (ID:  55; Description: 'Function ''main'' undefined'),
      (ID:  56; Description: 'Unit ''%s'' not found'),
      (ID:  57; Description: 'Event ''%s'' not registered'),
      (ID:  58; Description: 'Dfm ''%s'' not found'),

      (ID: 101; Description: 'Error in remark'),
      (ID: 103; Description:'%s expected but %s found'),
      (ID: 104; Description: 'Undeclared identifer ''%s'''),
      (ID: 105; Description: 'Type of expression must be boolean'),
      (ID: 106; Description: 'Class type required'),
      (ID: 107; Description:' not allowed before else'),
      (ID: 108; Description: 'Type of expression must be integer'),
      (ID: 109; Description: 'Record, object or class type required'),
      (ID: 110; Description: 'Missing operator or semicolon'),
      (ID: 111; Description: 'Identifer redeclared: ''%s'''),

      (ID: 171; Description: 'Array index out of bounds'),
      (ID: 172; Description: 'Too many array bounds'),
      (ID: 173; Description: 'Not enough array bounds'),
      (ID: 174; Description: 'Invalid array dimension'),
      (ID: 175; Description: 'Invalid array range'),
      (ID: 176; Description: 'Array type required'),

      (ID: 181; Description: 'Too many actual parameters'),
      (ID: 182; Description: 'Not enough parameters'),
      (ID: 183; Description: 'Incompatible types: ''%s'' and ''%s'''),
      (ID: 184; Description: 'Error loading library ''%s'''),
      (ID: 185; Description: 'Invalid type of argument in call to function ''%s'''),
      (ID: 186; Description: 'Invalid type of result in call to function ''%s'''),
      (ID: 187; Description: 'Can''t get proc address for function ''%s'''),
      (ID: 188; Description: 'Invalid type of argument in call to function ''%s'''),
      (ID: 189; Description: 'Invalid type of result in call to function ''%s'''),
      (ID: 190; Description: 'Invalid call convention for function ''%s'''),

      (ID: 201; Description: 'Calling ''%s'' failed: ''%s'''),

      (ID: 301; Description: 'Expression'),
      (ID: 302; Description: 'Identifer'),
      (ID: 303; Description: 'Declaration'),
      (ID: 304; Description: 'end of file'),
      (ID: 305; Description: 'class declaration'),

      (ID: 401; Description: 'Implementation of unit not found')
    );

implementation

end.
