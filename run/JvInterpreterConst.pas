{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreterConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for English

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreterConst;

{$I jvcl.inc}

interface

uses
  JvResources;

const
  {JvInterpreterParser}
  StIdSymbols = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators = ['(', ')', ',', '.', ';'];

type
  TJvInterpreterErrorsDescr = record
    ID: Integer;
    Description: string;
  end;

const
  JvInterpreterErrors: array [0..52] of TJvInterpreterErrorsDescr =
    ((ID: 0; Description: RsEInterpreter0),
     (ID: 1; Description: RsEInterpreter1),
     (ID: 2; Description: RsEInterpreter2),
     (ID: 3; Description: RsEInterpreter3),
     (ID: 4; Description: RsEInterpreter4),
     (ID: 5; Description: RsEInterpreter5),
     (ID: 6; Description: RsEInterpreter6),
     (ID: 7; Description: RsEInterpreter7),
     (ID: 8; Description: RsEInterpreter8),
     (ID: 31; Description: RsEInterpreter31),

     (ID: 52; Description: RsEInterpreter52),
     (ID: 53; Description: RsEInterpreter53),
     (ID: 55; Description: RsEInterpreter55),
     (ID: 56; Description: RsEInterpreter56),
     (ID: 57; Description: RsEInterpreter57),
     (ID: 58; Description: RsEInterpreter58),

     (ID: 101; Description: RsEInterpreter101),
     (ID: 103; Description: RsEInterpreter103),
     (ID: 104; Description: RsEInterpreter104),
     (ID: 105; Description: RsEInterpreter105),
     (ID: 106; Description: RsEInterpreter106),
     (ID: 107; Description: RsEInterpreter107),
     (ID: 108; Description: RsEInterpreter108),
     (ID: 109; Description: RsEInterpreter109),
     (ID: 110; Description: RsEInterpreter110),
     (ID: 111; Description: RsEInterpreter111),

     (ID: 171; Description: RsEInterpreter171),
     (ID: 172; Description: RsEInterpreter172),
     (ID: 173; Description: RsEInterpreter173),
     (ID: 174; Description: RsEInterpreter174),
     (ID: 175; Description: RsEInterpreter175),
     (ID: 176; Description: RsEInterpreter176),

     (ID: 181; Description: RsEInterpreter181),
     (ID: 182; Description: RsEInterpreter182),
     (ID: 183; Description: RsEInterpreter183),
     (ID: 184; Description: RsEInterpreter184),
     (ID: 185; Description: RsEInterpreter185),
     (ID: 186; Description: RsEInterpreter186),
     (ID: 187; Description: RsEInterpreter187),
     (ID: 188; Description: RsEInterpreter188),
     (ID: 189; Description: RsEInterpreter189),
     (ID: 190; Description: RsEInterpreter190),

     (ID: 201; Description: RsEInterpreter201),

     (ID: 301; Description: RsEInterpreter301),
     (ID: 302; Description: RsEInterpreter302),
     (ID: 303; Description: RsEInterpreter303),
     (ID: 304; Description: RsEInterpreter304),
     (ID: 305; Description: RsEInterpreter305),
     (ID: 306; Description: RsEInterpreter306),
     (ID: 307; Description: RsEInterpreter307),
     (ID: 308; Description: RsEInterpreter308),
     (ID: 309; Description: RsEInterpreter309),

     (ID: 401; Description: RsEInterpreter401));

implementation

end.

