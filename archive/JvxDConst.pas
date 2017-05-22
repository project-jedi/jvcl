{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxConst.PAS, released on 2002-11-23.

The Initial Developers of the Original Code are: Peter Thörnqvist
All Rights Reserved.

Contributor(s):
  Polaris Software

Last Modified: 2002-11-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Notes:
This is a merging of the units JvCConst, JvDConst, JvGConst, JvLConst and JvTConst
(originally from RxLib). The old units declared the identidiers as id/string constants
and included a corresponding res file with a string table in it. This unit replaces those
id/string identifiers with resourcestrings, removing the need for the res files
and makes it easier to translate the messages (using ITE, ETM or other tools)
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvxDConst;

interface

resourcestring
  srJvHLEdPropDlgIni = 'JvHLEdPropDlg.ini';

  { see JvCtlReg }
  {$IFDEF COMPILER3_UP}
  srSamplesPalette = 'Samples';
  {$ENDIF}

  { JVCL versions of standard IDE palettes }
  srJvStandardPalette     = 'Jv Standard';
  srJvAdditionalPalette   = 'Jv Additional';
  srJvWin32Palette        = 'Jv Win32';
  srJvSystemPalette       = 'Jv System';
  srJvDataAccessPalette   = 'Jv Data Access';
  srJvDataControlsPalette = 'Jv Data Controls';
  srJvBDEPalette          = 'Jv BDE';
  srJvInternetPalette     = 'Jv Internet';
  srJvDialogsPalette      = 'Jv Dialogs';

  { other JVCL palettes }
  srJvCustomPalette       = 'Jv Custom';
  srJvLabelsPalette       = 'Jv Labels';
  srJvMultimediaPalette   = 'Jv Multimedia';
  srJvFormsPalette        = 'Jv Forms';
  srJvUtilsPalette        = 'Jv Utils';
  srJvConvertPalette      = 'Jv Convert';
  srJvPluginPalette       = 'Jv Plugin';
  srJvCompositesPalette   = 'Jv Composites';
  srJvControlsPalette     = 'Jv Controls';
  srJvInterpreterPalette  = 'Jv Interpreter';
  srJvXToolsPalette       = 'JvX Tools';
  srJvJFreeVCSPalette     = 'Jv JFreeVCS';

  { for RegisterActions }
  srJVCLActions     = 'JVCL';
  srJvDialogActions = 'Jv Dialog';

  { TImageList component editor }
  srSaveImageList      = 'Save to bitmap...';

  { TJvFormStorage component editor }
  srStorageDesigner    = 'Form Storage Designer...';

  { TJvPageManager component editor }
  srProxyEditor        = 'Edit Proxies...';
  srPageProxies        = '%s Page Proxies';
  srProxyName          = 'Page Proxy Name';
  srPageName           = 'Page Name';

  { TJvSpeedBar component editor }
  srSBItemNotCreate    = 'Cannot create a new Speedbar button';
  srConfirmSBDelete    = 'Are you sure you want to delete current section?';
  srSpeedbarDesigner   = 'Speedbar designer...';
  srNewSectionName     = 'Untitled (%d)';

  { TJvTimerList component editor }
  srEventNotCreate     = 'Cannot create a new event';
  srTimerDesigner      = 'Edit Events...';
  srTimerEvents        = '%s.Events';

  { TJvAnimatedImage component editor }
  srAniCurFilter       = 'Animated Cursors (*.ani)|*.ani|Any files (*.*)|*.*';
  srEditPicture        = 'Edit picture...';
  srLoadAniCursor      = 'Load from ANI...';

  { TJvIconList property editor }
  srLoadIcon           = 'Load Icon';

  { TJvxGradientCaption component editor }
  srCaptionDesigner    = 'Edit Captions...';
  srGradientCaptions   = 'Captions';

  { TJvMemoryTable & TJvMemoryData component editor }
  srBorrowStructure    = 'Borrow structure...';

  { various editors }
  SJvEditorString = 'Click to edit...';

  { JvID3_ component editor }
  SID3FrameEditor = 'Frame edi&tor...';
  SID3RemoveTag = '&Remove tag...';
  SID3CommitTag = '&Commit...';

implementation

end.
