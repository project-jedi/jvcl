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

Description : JVCL implemetation of Delphi design-time packages


Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Annuler';

 { Dataset Editor }
  SDEDatasetDesigner = 'Editeur de &champs...';

  SDEAddItem          = '&Ajouter des champs...';
  SDEDeleteItem       = '&Supprimer';
  SDESelectAllItem    = 'Tous sé&lectionner';
  SDENewItem          = '&Nouveau champ...';

  SDEAddFieldsCaption = 'Ajouter des champs';
  SDEAvailableFields  = 'Champs disponible';

  SDENewFieldCaption    = 'Nouveau champ';
  SDEFieldProperties    = 'Propriétés du champs';
  SDEFieldNameLabel     = '&Nom:';
  SDEFieldTypeLabel     = '&Type:';
  SDEComponentNameLabel = 'C&omposant:';
  SDEFieldSizeLabel     = 'Ta&ille:';
  SDEFieldKind          = 'Type du champ';
  SDELookupGroup        = 'Lookup definition';
  SDEKeyFieldsLabel     = 'Champs Clés:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Look&up Keys:';
  SDEResultFieldLabel   = 'Champ résultat:';
  SDEFieldKindItems     = '&Données'#13'&Calculé'#13'&Lookup';
  SDEFieldTypeMustBeSpecified = 'Le type du champs doit être spécifié';

  SDBGridColEditor    = 'Editeur de colonnes...';

 { Collection Editor }
  SCEEditCollection     = 'Edition de %s';
  SCEAdd                = '&Ajouter';
  SCEDelete             = 'Effacer';
  SCEMoveUp             = 'Monter';
  SCEMoveDown           = 'Descendre';
  SCESelectAllItem      = 'Tous sé&lectionner';

 { Picture Editor }
  SPELoad               = '&Charger...';
  SPESave               = '&Sauvegarder...';
  SPEClear              = '&Effacer';
  SPECopy               = 'C&opier';
  SPEPaste              = 'Coller';

 { Menu Designer }
  SMDMenuDesigner       = 'Editeur &de menu';
  SMDInsertItem         = '&Insérer';
  SMDDeleteItem         = '&Effacer';
  SMDCreateSubmenuItem  = 'Créer un &sous-menu';

implementation

end.
