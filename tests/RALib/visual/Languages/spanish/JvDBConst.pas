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

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Language specific constant for Spanish

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvDBConst;

interface

const

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Usted puede tipear el nombre de la propiedad aqui';
  sRegAutoEditorTreeHint       = 'Propiedades disponibles';
  sRegAutoEditorListHint       = 'Propiedades almacenadas';
  sRegAutoEditorBtnAddPropHint = 'Agregar/Remover propiedad';
  sRegAutoEditorSort           = 'Ordenar';

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
  SScrollBarRange = 'Valor de Scrollbar fuera de limite';
{$ENDIF}

 {JvDlg}
  SOk = 'OK';
  SCancel = 'Cancelar';

 { Menu Designer }
  SMDMenuDesigner       = '&Designador de Menu';
  SMDInsertItem         = '&Insertar';
  SMDDeleteItem         = '&Borrar';
  SMDCreateSubmenuItem  = 'Crear &SubMenu';

  SCantGetShortCut      = 'Nombre de archivo para acceso %s no disponible';


 { RALib 1.23 } 
  SPropertyNotExists    = 'Propiedad "%s" no existe';
  SInvalidPropertyType  = 'Propiedad "%s" tiene tipo invalido';

 { RALib 1.55 }

 {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Propiedades del editor';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Colores';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Configuracion del editor';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Mapeo de teclas por defecto';
  SHLEdPropDlg_gbEditor = 'Opciones del editor:';
  SHLEdPropDlg_cbAutoIndent = 'Modo &Auto indent';
  SHLEdPropDlg_cbSmartTab = 'Tab &Habil';
  SHLEdPropDlg_cbBackspaceUnindents = 'Des-Indent de &Backspace';
  SHLEdPropDlg_cbGroupUndo = '&Deshacer grupo';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Cursor mas alla de &EOF';
  SHLEdPropDlg_cbUndoAfterSave = 'D&eshacer despues de guardar';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Mantener rastros de blancos';
  SHLEdPropDlg_cbDoubleClickLine = '&Doble click la linea';
  SHLEdPropDlg_cbSytaxHighlighting = 'Usar resaltado de &sintaxis';
  SHLEdPropDlg_lblTabStops = 'Para con &Tab:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Configuracion de color para';
  SHLEdPropDlg_lblElement = '&Elemento:';
  SHLEdPropDlg_lblColor = '&Color:';
  SHLEdPropDlg_gbTextAttributes = 'Atributos de texto:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Usar por defecto para:';
  SHLEdPropDlg_cbBold = '&Negrita';
  SHLEdPropDlg_cbItalic = '&Italica';
  SHLEdPropDlg_cbUnderline = '&Subrayado';
  SHLEdPropDlg_cbDefForeground = '&Primer plano';
  SHLEdPropDlg_cbDefBackground = '&Fondo';
  SHLEdPropDlg_OptionCantBeChanged = 'Esta opcion no puede ser cambiada.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Propiedad JvHLEditor no asignada';
  SHLEdPropDlg_RegAutoNotAssigned = 'Propiedad RegAuto no asignada';

implementation

end.
