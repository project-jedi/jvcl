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

Contributor(s): Jaromir Solar (jarda@foresta.cz)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for Czech

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
  SNoReportProc = 'Procedura "JvInterpreterRunReportPreview" nenalezena';
  SNoReportProc2 = 'Procedura "JvInterpreterRunReportPreview2" nenalezena';

 {JvInterpreter Error Descriptions}
  JvInterpreterErrors : array [0..47] of
    record
      ID: Integer;
      Description: String;
    end
    = (
      (ID:   0; Description: 'Ok'),
      (ID:   1; Description: 'Neznámá chyba'),
      (ID:   2; Description: 'Interní chyba interpretu: %s'),
      (ID:   3; Description: 'Pøerušeno uživatelem'),
      (ID:   4; Description: 'Znovu vyvolání vyjímky je možno pouze v obsluze vyjímky'),
      (ID:   5; Description: 'Chyba v unitì ''%s'' na øádku %d : %s'),
      (ID:   6; Description: 'Externì v unitì ''%s'' na øádku %d : %s'),
      (ID:   7; Description: 'Odepøen pøístup do ''%s'''),
      (ID:  31; Description: 'Záznam ''%s'' nebyl definován'),

      (ID:  52; Description: 'Pøeteèení zásobníku'),
      (ID:  53; Description: 'Nevhodný typ'),
      (ID:  55; Description: 'Nedefinovaná ''main'' funkce'),
      (ID:  56; Description: 'Unita ''%s'' nebyla nalezena'),
      (ID:  57; Description: 'Událost ''%s'' nebyla registrována'),
      (ID:  58; Description: 'Dfm ''%s'' nebyla nalezena'),

      (ID: 101; Description: 'Chyba v poznámce'),
      (ID: 103; Description: 'Oèekáváno %s, ale nalezeno %s'),
      (ID: 104; Description: 'Nedeklarovaný identifikátor ''%s'''),
      (ID: 105; Description: 'Výraz musí být typu boolean'),
      (ID: 106; Description: 'Je oèekáván typ class'),
      (ID: 107; Description: ''';'' není podporovaný pøed else'),
      (ID: 108; Description: 'Výraz musí být typu integer'),
      (ID: 109; Description: 'Oèekáván typ záznam, object nebo class'),
      (ID: 110; Description: 'Chybìjící operátor nebo støedník'),
      (ID: 111; Description: 'Opìtovná deklarace identifikátoru: ''%s'''),

      (ID: 171; Description: 'Index pole je mimo jeho rozmìr'),
      (ID: 172; Description: 'Pøíliš mnoho rozmìrù pole'),
      (ID: 173; Description: 'Nedostatek rozmìrù pole'),
      (ID: 174; Description: 'Špatná velikost pole'),
      (ID: 175; Description: 'Špatný rozsah pole'),
      (ID: 176; Description: 'Oèekáván typ pole'),

      (ID: 181; Description: 'Pøíliš mnoho parametrù'),
      (ID: 182; Description: 'Nedostatek parametrù'),
      (ID: 183; Description: 'Nekompatibilní typy: ''%s'' a ''%s'''),
      (ID: 184; Description: 'Chyba pøi naèítání knihovny ''%s'''),
      (ID: 185; Description: 'Chybný typ argumentu pøi volání funkce ''%s'''),
      (ID: 186; Description: 'Chybný typ výsledku pøi volání funkce ''%s'''),
      (ID: 187; Description: 'Nemùžu získat adresu funkce ''%s'''),
      (ID: 188; Description: 'Chybný typ argumentu pøi volání funkce ''%s'''),
      (ID: 189; Description: 'Chybný typ výsledku pøi volání funkce ''%s'''),
      (ID: 190; Description: 'Chybná konvence volání funkce ''%s'''),

      (ID: 201; Description: 'Volání ''%s'' se nepovedlo: ''%s'''),

      (ID: 301; Description: 'Výraz'),
      (ID: 302; Description: 'Identifikátor'),
      (ID: 303; Description: 'Deklarace'),
      (ID: 304; Description: 'konec souboru'),
      (ID: 305; Description: 'deklarace tøídy'),

      (ID: 401; Description: 'Implementace unity nebyla nalezena')
    );
 
implementation

end.
