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

Contributor(s): Tibor Bednar (tiborbmt@hotmail.com)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for Slovak

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Zruöiù';

 { Dataset Editor }
  SDEDatasetDesigner = '&Editor polÌ...';

  SDEAddItem          = '&Pridaù pole...';
  SDEDeleteItem       = '&Odstr·niù';
  SDESelectAllItem    = 'Vy&braù vöetko';
  SDENewItem          = '&NovÈ pole...';

  SDEAddFieldsCaption = 'Pridaù pole';
  SDEAvailableFields  = 'DostupnÈ polia';

  SDENewFieldCaption    = 'NovÈ pole';
  SDEFieldProperties    = 'Vlastnosti poæa';
  SDEFieldNameLabel     = '&N·zov:';
  SDEFieldTypeLabel     = '&Typ:';
  SDEComponentNameLabel = 'K&omponent:';
  SDEFieldSizeLabel     = '&Veækosù:';
  SDEFieldKind          = 'Typ poæa';
  SDELookupGroup        = 'DefinÌcia vyhæad·vania';
  SDEKeyFieldsLabel     = '&Kæ˙ËovÈ polia:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Kæ˙Ëe pre v&yhæad·vanie:';
  SDEResultFieldLabel   = '&V˝slednÈ pole:';
  SDEFieldKindItems     = '&D·ta'#13'&PoËÌtanÈ'#13'&Vyhæad·vanÈ';
  SDEFieldTypeMustBeSpecified = 'Typ poæa musÌ byù zadan˝';

  SDBGridColEditor    = 'Editor &stÂpcov...';

 { Collection Editor }
  SCEEditCollection     = 'Edit·cia %s';
  SCEAdd                = '&Pridaù';
  SCEDelete             = 'O&dstr·niù';
  SCEMoveUp             = 'Presun˙ù &hore';
  SCEMoveDown           = 'Presun˙ù do&le';
  SCESelectAllItem      = 'Vy&braù vöetko';

 { Picture Editor }
  SPELoad               = '&NaËÌtaù...';
  SPESave               = '&Uloûiù...';
  SPEClear              = '&Vymazaù';
  SPECopy               = '&KopÌrovaù';
  SPEPaste              = '&Prilepiù';

 { Menu Designer }
  SMDMenuDesigner       = 'N·vrh &menu';
  SMDInsertItem         = '&Vloûiù';
  SMDDeleteItem         = 'O&dstr·niù';
  SMDCreateSubmenuItem  = 'Vytvoriù &podmenu';

implementation

end.
