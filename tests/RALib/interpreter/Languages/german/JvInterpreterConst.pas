{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   

All Rights Reserved.

Contributor(s): Suat IMAM-OGLU (simam@t-online.de)

Last Modified: 2002-07-04

Description : Language specific constant for German

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

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
      (ID:   1; Description: 'Unbekannter Fehler'),
      (ID:   2; Description: 'Interner Fehler: %s'),
      (ID:   3; Description: 'Benutzerunterbrechung'),
      (ID:   4; Description: 'Wiederhervorrufen einer Exception ist nur im Exception-Handler möglich'),
      (ID:   5; Description: 'Fehler in Unit ''%s'' in Zeile %d: %s'),
      (ID:   6; Description: 'English: External error in unit ''%s'' on line %d : %s'),
      (ID:   7; Description: 'English: Access denied to ''%s'''),
      (ID:  31; Description: 'Record ''%s'' nicht definiert'),

      (ID:  52; Description: 'Speicherüberlauf'),
      (ID:  53; Description: 'Ungültige Typumwandlung'),
      (ID:  55; Description: 'Funktion ''main'' nicht definiert'),
      (ID:  56; Description: 'Unit ''%s'' nicht gefunden'),
      (ID:  57; Description: 'Ereignis ''%s'' nicht registriert'),
      (ID:  58; Description: 'Dfm ''%s'' not found'),

      (ID: 101; Description: 'Fehler im Kommentar'),
      (ID: 103; Description: '%s erwartet aber %s gefunden'),
      (ID: 104; Description: 'Undefinierter Bezeichner ''%s'''),
      (ID: 105; Description: 'Ausdruckstyp muss BOOLEAN sein'),
      (ID: 106; Description: 'Klassentyp erwartet'),
      (ID: 107; Description: ''';'' nicht erlaubt vor einem ELSE'),
      (ID: 108; Description: 'Ausdruckstyp muss INTEGER sein'),
      (ID: 109; Description: 'Record, Object oder Klassentyp erforderlich'),
      (ID: 110; Description: 'Operator oder Semikolon fehlt'),
      (ID: 111; Description: 'Bezeichner redefiniert: ''%s'''),

      (ID: 171; Description: 'Array-Index ausserhalb des gültigen Bereichs'),
      (ID: 172; Description: 'Zu viele Array-Bereiche'),
      (ID: 173; Description: 'Nicht genügend Array-Bereiche'),
      (ID: 174; Description: 'Ungültige Array-Dimension'),
      (ID: 175; Description: 'Ungültiger Array-Bereich'),
      (ID: 176; Description: 'Arraytyp erwartet'),

      (ID: 181; Description: 'Zu viele Parameter'),
      (ID: 182; Description: 'Nicht genügend wirkliche Parameter'),
      (ID: 183; Description: 'Inkompatible Typen: ''%s'' und ''%s'''),
      (ID: 184; Description: 'Fehler beim Laden der Bibliothek ''%s'''),
      (ID: 185; Description: 'Falscher Argumententyp im Aufruf der Funktion ''%s'''),
      (ID: 186; Description: 'Falscher Ergebnistyp im Aufruf der Funktion ''%s'''),
      (ID: 187; Description: 'Auf Prozessadresse der Funktion ''%s'' kann nicht zugegriffen werden'),
      (ID: 188; Description: 'Falscher Argumententyp im Aufruf der Funktion ''%s'''),
      (ID: 189; Description: 'Falscher Ergebnistyp im Aufruf der Funktion ''%s'''),
      (ID: 190; Description: 'Ungültige Aufrufkonvention der Funktion ''%s'''),

      (ID: 201; Description: 'Aufruf von ''%s'' verfehlt: ''%s'''),

      (ID: 301; Description: 'Ausdruck'),
      (ID: 302; Description: 'Bezeichner'),
      (ID: 303; Description: 'Deklaration'),
      (ID: 304; Description: 'Dateiende'),
      (ID: 305; Description: 'class declaration'),

      (ID: 401; Description: 'Implementation der Unit nicht gefunden')
    );

implementation

end.
