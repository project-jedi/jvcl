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

unit RAConst;

interface

const

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Saisissez le type de la propriété';
  sRegAutoEditorTreeHint       = 'Propriété disponible';
  sRegAutoEditorListHint       = 'Propriété stockées';
  sRegAutoEditorBtnAddPropHint = 'Ajouter/supprimer une propriété';
  sRegAutoEditorSort           = 'Trier';

 {JvEditor}
  RAEditorCompletionChars = #8+'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

{IParser}
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

{$IFDEF RAINTER}
 {RAInter}
  RAIIdSymbols      = ['0'..'9', 'A'..'Z', 'a'..'z',  '_'];
  RAIIdFirstSymbols = ['A'..'Z', 'a'..'z', '_'];
{$ENDIF RAINTER}

{$IFDEF COMPILER2}
  SScrollBarRange = 'Indice de barre de défilement hors limite';
{$ENDIF}

 {JvDlg}
  SOk = 'OK';
  SCancel = 'Annulé';

 { Menu Designer }
  SMDMenuDesigner       = 'Editeur &de menu';
  SMDInsertItem         = '&Insérer';
  SMDDeleteItem         = '&Supprimer';
  SMDCreateSubmenuItem  = 'Créer un &Sous-Menu';

  SCantGetShortCut      = 'Fichier cible du raccourci %s non disponible';

 { RALib 1.23 }
  SPropertyNotExists    = 'La propriété "%s" n''existe pas';
  SInvalidPropertyType  = 'Le type de la propriété "%s" est incorrect';

 { RALib 1.55 }

 {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Propriétés de l''éditeur';
  SHLEdPropDlg_tsEditor = 'Général';
  SHLEdPropDlg_tsColors = 'Couleurs';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Options pré&définies';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Affectation par défaut';
  SHLEdPropDlg_gbEditor = 'Options de l''éditeur';
  SHLEdPropDlg_cbAutoIndent = '&Mode auto-indentation';
  SHLEdPropDlg_cbSmartTab = '&Tabulation intelligente';
  SHLEdPropDlg_cbBackspaceUnindents = '&Retour arrière désindenté';
  SHLEdPropDlg_cbGroupUndo = 'Défaire en &groupe';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Curseur après la f&in de fichier';
  SHLEdPropDlg_cbUndoAfterSave = 'Défaire après l''&enregistrement';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Co&nserver les espaces de fin';
  SHLEdPropDlg_cbDoubleClickLine = 'Double-cli&quer pour la ligne';
  SHLEdPropDlg_cbSytaxHighlighting = 'Mise en é&vidence de la syntaxe';
  SHLEdPropDlg_lblTabStops = 'Arrêts tabulations :';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Co&uleurs prédéfinies :';
  SHLEdPropDlg_lblElement = '&Elément :';
  SHLEdPropDlg_lblColor = '&Couleur :';
  SHLEdPropDlg_gbTextAttributes = 'Attributs texte';
  SHLEdPropDlg_gbUseDefaultsFor = 'Valeurs par défaut';
  SHLEdPropDlg_cbBold = '&Gras';
  SHLEdPropDlg_cbItalic = '&Italique';
  SHLEdPropDlg_cbUnderline = '&Souligné';
  SHLEdPropDlg_cbDefForeground = 'Pour le &texte';
  SHLEdPropDlg_cbDefBackground = 'Pour le &fond';
  SHLEdPropDlg_OptionCantBeChanged = 'Cette option ne peut être modifiée';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'propriété du JvHLEditor n''est pas définie';
  SHLEdPropDlg_RegAutoNotAssigned = 'propriété du RegAuto n''est pas définie';

implementation

end.


