{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderConsts.pas, released on --.

The Initial Developers of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Project JEDI
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-06-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProviderDsgnConsts;

interface

resourcestring
  SDataProviderEditorCaption = 'Editing %s%s...';
  SDataItemRootID = 'ROOT';
  SDataItemRootCaption = 'Root';
  SDataItemNoTextIntf = 'Item has no text support.';
  SDataItemIDNotFound = 'Item ID "%s" not found!';
  SDataItemNotFound = 'Item not found.';
  SDataProviderAddFailed = 'Failed to add a new item.';
  SDataProviderAddErrorReason = 'unable to add new item; %s.';
  SDataProviderDeleteErrorReason = 'Unable to delete item; %s.';
  
  SDataProviderNoManOrDsgn = 'neither IJvDataItemsManagement nor IJvDataItemsDesigner are supported';
  SDataProviderNoSubItems = 'item doesn''t support IJvDataItems';
  SDataProviderNoMan = 'IJvDataItemsManagement is not supported';

implementation

end.
