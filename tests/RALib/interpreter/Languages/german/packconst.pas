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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for German

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Abbrechen';                           //'Cancel';

 { Dataset Editor }
  SDEDatasetDesigner = 'Feld-Edi&tor...';                    //'&Fields editor...';

  SDEAddItem          = '&Felder hinzufügen...';             //'&Add fields...';
  SDEDeleteItem       = '&Löschen';                          //'&Delete';
  SDESelectAllItem    = 'Alles &markieren';                  //'Se&lect All';
  SDENewItem          = '&Neues Feld...';                    //'&New Field...';

  SDEAddFieldsCaption = 'Felder hinzufügen';                 //'Add fields';
  SDEAvailableFields  = 'Verfügbare Felder';                 //'Available fields';

  SDENewFieldCaption    = 'Neues Feld';                      //'New field';
  SDEFieldProperties    = 'Feldeigenschaften';               //'Field properties';
  SDEFieldNameLabel     = '&Name:';
  SDEFieldTypeLabel     = '&Typ:';                           //'&Type:';
  SDEComponentNameLabel = 'K&omponente:';                    //'C&omponent:';
  SDEFieldSizeLabel     = '&Grösse:';                         //'&Size:';
  SDEFieldKind          = 'Feldtyp';                         //'Field type';
  SDELookupGroup        = 'Nachschlage-Definition';          //'Lookup definition';
  SDEKeyFieldsLabel     = '&Schlüsselfelder:';               //'&Key Fields:';
  SDEDatasetLabel       = 'D&atenmenge:';                    //'D&ataset:';
  SDELookupKeysLabel    = 'S&chlüssel:';                     //'Look&up Keys:';
  SDEResultFieldLabel   = 'E&rgebnisfeld:';                  //'&Result Field:';
  SDEFieldKindItems     = '&Daten'#13'&Berechnet'#13'&Nachschlagen'; //'&Data'#13'&Calculated'#13'&Lookup';
  SDEFieldTypeMustBeSpecified = 'Typ muss angegeben werden'; //'Field type must be specified';

  SDBGridColEditor    = 'Spa&lteneditor...';                 //'&Columns Editor...';

 { Collection Editor }
  SCEEditCollection     = '%s wird bearbeitet';              //'Editing %s';
  SCEAdd                = 'Hin&zufügen';                     //'&Add';
  SCEDelete             = '&Löschen';                        //'&Delete';
  SCEMoveUp             = 'Nach &oben';                      //'Move &Up';
  SCEMoveDown           = 'Na&ch unten';                     //'Move Dow&n';
  SCESelectAllItem      = 'Alles &markieren';                //'Se&lect All';

 { Picture Editor }
  SPELoad               = '&Laden...';                       //'&Load...';
  SPESave               = '&Speichern...';                   //'&Save...';
  SPEClear              = 'Ent&fernen';                      //'&Clear';
  SPECopy               = '&Kopieren';                       //'C&opy';
  SPEPaste              = '&Einfügen';                       //'&Paste';

 { Menu Designer }
  SMDMenuDesigner       = 'Menü-&Designer';                  //'Menu &Designer';
  SMDInsertItem         = '&Einfügen';                       //'&Insert';
  SMDDeleteItem         = '&Löschen';                        //'&Delete';
  SMDCreateSubmenuItem  = '&Untermenü erstellen';            //'Create &Submenu';

implementation

end.
