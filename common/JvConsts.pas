{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvConsts;

interface

uses
  SysUtils;

const
  { JvEditor }
  JvEditorCompletionChars = #8+'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

  { various units }
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];

  {$IFDEF RAINTER}
  {RAInter}
  RAIIdSymbols      = StIdSymbols;
  RAIIdFirstSymbols = StIdFirstSymbols;
  {$ENDIF RAINTER}

  { Menu Designer }
  { (rom) disabled unused
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &SubMenu';
  }

  { RALib 1.23 }
  // (rom) now in JvJCLUtils.pas

  { RALib 1.55 }

const
  {$IFDEF DELPHI2}
  SDelphiKey = 'Software\Borland\Delphi\2.0';
  {$ENDIF}
  {$IFDEF BCB1}
  SDelphiKey = 'Software\Borland\C++Builder\1.0';
  {$ENDIF}
  {$IFDEF DELPHI3}
  SDelphiKey = 'Software\Borland\Delphi\3.0';
  {$ENDIF}
  {$IFDEF BCB3}
  SDelphiKey = 'Software\Borland\C++Builder\3.0';
  {$ENDIF}
  {$IFDEF DELPHI4}
  SDelphiKey = 'Software\Borland\Delphi\4.0';
  {$ENDIF}
  {$IFDEF BCB4}
  SDelphiKey = 'Software\Borland\C++Builder\4.0';
  {$ENDIF}
  {$IFDEF DELPHI5}
  SDelphiKey = 'Software\Borland\Delphi\5.0';
  {$ENDIF}
  {$IFDEF BCB5}
  SDelphiKey = 'Software\Borland\C++Builder\5.0';
  {$ENDIF}
  {$IFDEF DELPHI6}
  SDelphiKey = 'Software\Borland\Delphi\6.0';
  {$ENDIF}
  {$IFDEF BCB6}
  SDelphiKey = 'Software\Borland\C++Builder\6.0';
  {$ENDIF}
  {$IFDEF DELPHI7}
  SDelphiKey = 'Software\Borland\Delphi\7.0';
  {$ENDIF}
  {$IFDEF BCB7} // will it ever be released?
  SDelphiKey = 'Software\Borland\C++Builder\7.0';
  {$ENDIF}
  {$IFDEF DELPHI8}
  SDelphiKey = 'Software\Borland\Delphi\8.0';
  {$ENDIF}

resourcestring
  { JvPickDate }
  SNextYear              = 'Next Year|';
  SNextMonth             = 'Next Month|';
  SPrevYear              = 'Previous Year|';
  SPrevMonth             = 'Previous Month|';

  { JvVCLUtils }
  SNotImplemented        = 'Function not yet implemented';
  SFileNotExec           = 'File specified is not an executable file, dynamic-link library, or icon file';
  SLoadLibError          = 'Could not load library ''%s''';
  SDetails               = 'Details';

  // JvTConst
  { JvDualList }
  SDualListSrcCaption         = '&Source';
  SDualListDestCaption        = '&Destination';

  { JvClipView }
  SClipbrdUnknown             = 'Cannot display. Data in Clipboard is in an unknown format.';
  SClipbrdEmpty               = 'Clipboard is empty';

  { JvSpeedbar }
  SCustomizeSpeedbar          = 'Customize Speedbar';
  SAvailButtons               = '&Available buttons:';
  SSpeedbarCategories         = '&Categories:';
  SSpeedbarEditHint           = 'To add command buttons, drag and drop buttons onto the SpeedBar. To remove command buttons, drag them off of the SpeedBar.';

  { MathParser }
  SParseSyntaxError           = 'Syntax error';
  SParseNotCramp              = 'Invalid condition (no cramp)';
  SParseDivideByZero          = 'Divide by zero';
  SParseSqrError              = 'Invalid floating operation';
  SParseLogError              = 'Invalid floating operation';
  SParseInvalidFloatOperation = 'Invalid floating operation';

  // JvDConst
  { JvDBLists }
  SLocalDatabase          = 'Cannot perform this operation on a local database';

  { JvDBUtils }
  SRetryLogin             = 'Do you wish to retry the connect to database?';

  { JvDBFilter }
  SExprNotBoolean         = 'Field ''%s'' is not of type Boolean';
  SExprBadNullTest        = 'NULL only allowed with ''='' and ''<>''';
  SExprBadField           = 'Field ''%s'' cannot be used in a filter expression';

  { JvChPswDlg }
  SChangePassword         = 'Change password';
  SOldPasswordLabel       = '&Old password:';
  SNewPasswordLabel       = '&New password:';
  SConfirmPasswordLabel   = '&Confirm password:';
  SPasswordChanged        = 'Password has been changed';
  SPasswordNotChanged     = 'Password has not been changed';
  SPasswordsMismatch      = 'The new and confirmed passwords do not match';

  { JvDbExcpt }
  SDBExceptCaption        = 'Database Engine Error';
  SBDEErrorLabel          = 'BDE Error';
  SServerErrorLabel       = 'Server Error';
  SErrorMsgLabel          = 'Error message';
  SNextButton             = '&Next';
  SPrevButton             = '&Prev';

  { JvDBFilter expression parser }
  SExprIncorrect          = 'Incorrectly formed filter expression';
  SExprTermination        = 'Filter expression incorrectly terminated';
  SExprNameError          = 'Unterminated field name';
  SExprStringError        = 'Unterminated string constant';
  SExprInvalidChar        = 'Invalid filter expression character: ''%s''';
  SExprNoRParen           = ''')'' expected but %s found';
  SExprExpected           = 'Expression expected but %s found';
  SExprBadCompare         = 'Relational operators require a field and a constant';

  { JvDBUtils }
  SConfirmSave            = 'The data were changed. Save them?';

  { JvDBCtrl }
  SDeleteMultipleRecords  = 'Delete all selected records?';

  srJvHLEdPropDlgIni = 'JvHLEdPropDlg.ini';

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
  SID3CommitTag = '&Commit';
  SID3FileInfoTag = 'File &info';
  SID3FrameEditorTag = 'Frame edi&tor';
  SID3RemoveTag = '&Remove tag...';
  SID3RemoveTagConfirmation = 'Remove tag?';
  SID3Err_FileDoesNotExists = 'File %s does not exists';
  SID3Err_NoFileSpecified = 'No file specified';
  SID3Err_NoValidMPEGTag = 'This file has not a valid MPEG tag';

const
  { JvDataProvider constants }
  { Consumer attributes }
  DPA_RenderDisabledAsGrayed = 1;
  DPA_RendersSingleItem      = 2;
  DPA_ConsumerDisplaysList   = 3;

implementation

end.
