{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit DepWalkConsts;

interface

uses
  Graphics;
  
type
  TPrintFormat = (pfText,pfHTML,pfXML);
const
  cUnitParsedImageIndex = 0;
  cUnitUsedImageIndex   = 1;
  cPascalExt = '.pas';
  cIniFileExt = '.ini';
// icon offsets
  cStartX = 50;
  cStartY = 50;


resourcestring
  SParsedStatusFmt = '  Done (%d units parsed, %d units in diagram)';
  SStatusReady     = '  Ready';
  SParseErrorsFmt  = 'Errors were encountered:'#13#10#13#10'%s';

  SUsesColon = 'uses:';
  SNone      = '(none)';
  SUsedByColon = 'used by:';
  SDependencyWalkerTitle = 'Dependency Walker';
  SAddSkipListTitle = 'Add unit to skiplist';
  SAddSkipListCaption =  'Unit name:';
  SConfirmDelete = 'Confirm delete';
  SConfirmClear  = 'Confirm clear';
  
  SDelSelItemsPrompt = 'Delete selected items?';
  SDelSelItemFmt = 'Remove "%s" from diagram?';
  SCheckPaths    = 'Check your paths in the options dialog and try again.';

  SAboutText = 'Dependency Walker Demo - part of JVCL (http://jvcl.sourceforge.net)';
  SClearDiagramPrompt = 'Clear and create new diagram?';

  SFindTitle = 'Find';
  SFindNameColon = 'Name:';
  SFindNotFoundFmt = '"%s" not found.';
  SRestartForNewOptions = 'The new settings will not take effect until you create a new diagram (Ctrl+N).';
  SFileNotFoundFmt      = 'Unit %s not found.';
  SUnitNotFound = 'Unit not found.';


implementation

end.
