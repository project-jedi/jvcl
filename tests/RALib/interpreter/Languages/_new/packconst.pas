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

Description : Language specific constant for English

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Cancel';

 { Dataset Editor }
  SDEDatasetDesigner = '&Fields editor...';

  SDEAddItem          = '&Add fields...';
  SDEDeleteItem       = '&Delete';
  SDESelectAllItem    = 'Se&lect All';
  SDENewItem          = '&New Field...';

  SDEAddFieldsCaption = 'Add fields';
  SDEAvailableFields  = 'Available fields';

  SDENewFieldCaption    = 'New field';
  SDEFieldProperties    = 'Field properties';
  SDEFieldNameLabel     = '&Name:';
  SDEFieldTypeLabel     = '&Type:';
  SDEComponentNameLabel = 'C&omponent:';
  SDEFieldSizeLabel     = '&Size:';
  SDEFieldKind          = 'Field type';
  SDELookupGroup        = 'Lookup definition';
  SDEKeyFieldsLabel     = '&Key Fields:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Look&up Keys:';
  SDEResultFieldLabel   = '&Result Field:';
  SDEFieldKindItems     = '&Data'#13'&Calculated'#13'&Lookup';
  SDEFieldTypeMustBeSpecified = 'Field type must be specified';

  SDBGridColEditor    = '&Columns Editor...';

 { Collection Editor }
  SCEEditCollection     = 'Editing %s';
  SCEAdd                = '&Add';
  SCEDelete             = '&Delete';
  SCEMoveUp             = 'Move &Up';
  SCEMoveDown           = 'Move Dow&n';
  SCESelectAllItem      = 'Se&lect All';

 { Picture Editor }
  SPELoad               = '&Load...';
  SPESave               = '&Save...';
  SPEClear              = '&Clear';
  SPECopy               = 'C&opy';
  SPEPaste              = '&Paste';

 { Menu Designer }
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &Submenu';

implementation

end.
