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

Contributor(s): KNIPPER John (knipjo@altavista.net)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for French

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
  SNoReportProc = 'Procédure "JvInterpreterRunReportPreview" non trouvée';
  SNoReportProc2 = 'Procédure "JvInterpreterRunReportPreview2" non trouvée';

 {JvInterpreter Error Descriptions}
  JvInterpreterErrors : array [0..47] of
    record
      ID: Integer;
      Description: String;
    end
    = (
      (ID:   0; Description: 'Ok'),
      (ID:   1; Description: 'Erreur inconnu'),
      (ID:   2; Description: 'Erreur interne de l''interpréteur: %s'),
      (ID:   3; Description: 'Interruption de l''utilisateur'),
      (ID:   4; Description: 'Re-provocation d''une exception autorisé seulement dans un gestionnaire d''exception'),
      (ID:   5; Description: 'Erreur dans l''unité ''%s'' à la ligne %d\n%s'),
      (ID:   6; Description: 'External in unit ''%s'' on line %d : %s'),
      (ID:   7; Description: 'English: Access denied to ''%s'''),
      (ID:  31; Description: 'Enregistrement ''%s'' non défini'),

      (ID:  52; Description: 'Débordement de pile'),
      (ID:  53; Description: 'Types incompatibles'),
      (ID:  55; Description: 'Fonction ''main'' non définie'),
      (ID:  56; Description: 'Unité ''%s'' non trouvé'),
      (ID:  57; Description: 'Evénement ''%s'' non enregistré'),
      (ID:  58; Description: 'Dfm ''%s'' not found'),

      (ID: 101; Description: 'Erreur dans le remarquage'),
      (ID: 103; Description: 's attendu mais %s trouvé'),
      (ID: 104; Description: 'Identificateur non déclaré : ''%s'''),
      (ID: 105; Description: 'Le type de l''expresion doit être BOOLEAN'),
      (ID: 106; Description: 'Type de la classe requit'),
      (ID: 107; Description: 'non autorisé devant else'),
      (ID: 108; Description: 'Le type de l''expresion doit être INTEGER'),
      (ID: 109; Description: 'Type d''enregistrement, d''objet ou de classe requit'),
      (ID: 110; Description: 'Opérateur ou '';'' manquant'),
      (ID: 111; Description: 'Identificateur redéclaré: ''%s'''),

      (ID: 171; Description: 'English: Array index out of bounds'),
      (ID: 172; Description: 'English: Too many array bounds'),
      (ID: 173; Description: 'English: Not enough array bounds'),
      (ID: 174; Description: 'English: Invalid array dimension'),
      (ID: 175; Description: 'English: Invalid array range'),
      (ID: 176; Description: 'English: Array type required'),

      (ID: 181; Description: 'Trop de paramètres'),
      (ID: 182; Description: 'Pas assez de paramètres'),
      (ID: 183; Description: 'Types incompatible : ''%s'' and ''%s'''),
      (ID: 184; Description: 'Erreur de chargement de la librairie ''%s'''),
      (ID: 185; Description: 'Type d''argument invalide dans l''appel de la fonction ''%s'''),
      (ID: 186; Description: 'Type de résultat invalide dans l''appel de la fonction ''%s'''),
      (ID: 187; Description: 'Impossible d''obtenir une adresse proc pour la fonction ''%s'''),
      (ID: 188; Description: 'Type d''argument invalide dans l''appel de la fonction ''%s'''),
      (ID: 189; Description: 'Type de résultat invalide dans l''appel de la fonction ''%s'''),
      (ID: 190; Description: 'Convention d''appel invalide pour la fonction ''%s'''),

      (ID: 201; Description: 'L''appel de ''%s'' à échouer: ''%s'''),

      (ID: 301; Description: 'Expression'),
      (ID: 302; Description: 'Identificateur'),
      (ID: 303; Description: 'Déclaration'),
      (ID: 304; Description: 'Fin de fichier'),
      (ID: 305; Description: 'class declaration'),

      (ID: 401; Description: 'Implémentation de l''unité non trouvé')
    );

implementation

end.

