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

Contributor(s): Jaromir Solar (jarda@foresta.cz).

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : JVCL implemetation of Delphi design-time packages

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit packconst;

interface

const
  SOK               = 'Ok';
  SCancel           = 'Storno';

 { Dataset Editor }
  SDEDatasetDesigner = '&Editor polí...';

  SDEAddItem          = '&Pøidat pole...';
  SDEDeleteItem       = '&Smazat';
  SDESelectAllItem    = 'Vybrat &vše';
  SDENewItem          = '&Nové pole...';

  SDEAddFieldsCaption = 'Pøidat pole';
  SDEAvailableFields  = 'Dostupná pole';

  SDENewFieldCaption    = 'Nové pole';
  SDEFieldProperties    = 'Vlastnosti pole';
  SDEFieldNameLabel     = '&Jméno:';
  SDEFieldTypeLabel     = '&Typ:';
  SDEComponentNameLabel = '&Komponenta:';
  SDEFieldSizeLabel     = '&Velikost:';
  SDEFieldKind          = 'Typ pole';
  SDELookupGroup        = 'Definice vyhledávání';
  SDEKeyFieldsLabel     = 'K&líèová pole:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Klíèe pro v&yhledávání:';
  SDEResultFieldLabel   = '&Výsledné pole:';
  SDEFieldKindItems     = '&Data'#13'&Poèítané'#13'&Vyhledávané';
  SDEFieldTypeMustBeSpecified = 'Musí být zadán typ pole';

  SDBGridColEditor    = 'Editor &sloupcù...';

 { Collection Editor }
  SCEEditCollection     = 'Editace %s';
  SCEAdd                = '&Pøidat';
  SCEDelete             = '&Smazat';
  SCEMoveUp             = 'Pøesunout &nahoru';
  SCEMoveDown           = 'Pøesunout &dolù';
  SCESelectAllItem      = 'Vy&brat vše';

 { Picture Editor }
  SPELoad               = '&Naèíst...';
  SPESave               = '&Uložit...';
  SPEClear              = '&Vyèistit';
  SPECopy               = '&Kopírovat';
  SPEPaste              = 'V&ložit';

 { Menu Designer }
  SMDMenuDesigner       = 'Návrháø &menu';
  SMDInsertItem         = '&Vložit';
  SMDDeleteItem         = '&Smazat';
  SMDCreateSubmenuItem  = 'Vytvoøit &podmenu';

implementation

end.
