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

unit JvxRConst;

interface

resourcestring
  { JvToolEdit }
  SBrowse                = 'Browse';
  SDefaultFilter         = 'All files (*.*)|*.*';

  { JvPickDate }
  SDateDlgTitle          = 'Select a Date';
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

  // JvGConst
  SGIFImage            = 'CompuServe GIF Image';
  SChangeGIFSize       = 'Cannot change the Size of a GIF image';
  SNoGIFData           = 'No GIF Data to write';
  SUnrecognizedGIFExt  = 'Unrecognized extension block: %.2x';
  SWrongGIFColors      = 'Wrong number of colors; must be a power of 2';
  SBadGIFCodeSize      = 'GIF code size not in range 2 to 9';
  SGIFDecodeError      = 'GIF encoded data is corrupt';
  SGIFEncodeError      = 'GIF image encoding error';
  SGIFVersion          = 'Unknown GIF version';

  // JvDConst
  { JvDBLists }
  SLocalDatabase          = 'Cannot perform this operation on a local database';

  { JvDBUtils }
  SRetryLogin             = 'Do you wish to retry the connect to database?';

  { JvDBFilter }
  SExprNotBoolean         = 'Field ''%s'' is not of type Boolean';
  SExprBadNullTest        = 'NULL only allowed with ''='' and ''<>''';
  SExprBadField           = 'Field ''%s'' cannot be used in a filter expression';
  SCaptureFilter          = 'Cannot perform this operation when controls are captured';
  SNotCaptureFilter       = 'Cannot perform this operation when controls are not captured';

  { JvDBCtrl }
  SInactiveData           = 'Closed';
  SBrowseData             = 'Browse';
  SEditData               = 'Edit';
  SInsertData             = 'Insert';
  SSetKeyData             = 'Search';
  SCalcFieldsData         = 'Calculate';

  { LoginDlg }
  SRegistration           = 'Registration';
  SAppTitleLabel          = 'Application "%s"';
  SHintLabel              = 'Type your user name and password';
  SUserNameLabel          = '&User name:';
  SPasswordLabel          = '&Password:';
  SInvalidUserName        = 'Invalid user name or password';

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
  SDatabaseName           = 'Database name: %s';

  { LoginDlg }
  SUnlockCaption          = 'Unlock application';
  SUnlockHint             = 'Type your password';

  { JvDBCtrl }
  SDeleteMultipleRecords  = 'Delete all selected records?';

  { Polaris patch }
  SDateMinLimit           = 'Enter a date before "%s"';
  SDateMaxLimit           = 'Enter a date after "%s"';
  SDateOutOfRange         = '%s - Enter a date between "%s" and "%s"';
  SDateOutOfMin           = '%s - Enter a date after "%s"';
  SDateOutOfMax           = '%s - Enter a date before "%s"';

  { JvID3_ }

  SID3FrameNotFound          = 'Frame not found';
  SID3UnknownEncoding        = 'Unknown encoding';
  SID3UnknownVersion         = 'Unknown version';
  SID3DuplicateFrame         = 'Frame is a duplicate of another frame in the tag.';
  SID3AlreadyContainsFrame   = 'Tag already contains a ''%s'' frame.';
  SID3ControllerNotActive    = 'Controller is not active';
  SID3EncodingNotSupported   = 'Encoding not supported in this version.';
  SID3VersionNotSupported    = 'Version not supported.';
  SID3NoController           = 'No controller specified.';
  SID3InvalidLanguageValue   = '''%s'' is an invalid language value.';
  SID3InvalidPartInSetValue  = '''%s'' is an invalid ''part in set'' value.';
  SID3InvalidTimeValue       = '''%s'' is an invalid time value.'#13'Value must be of format ''HHMM''.';
  SID3InvalidDateValue       = '''%s'' is an invalid date value.'#13'Value must be of format ''DDMM''.';
  SID3ValueTooBig            = '''%d'' is an invalid value. Value is too big.';
  SID3StringTooLong          = '''%s'' is an invalid value. String is too long.';
  SID3InvalidCharinList      = 'Invalid char ''%s'' in string ''%s'' in list.';
  SID3InvalidFrameClass      = 'Frame class ''%s'' can''t be used to represent frame ID ''%s''';
  SID3FrameIDNotSupported    = 'Frame ID ''%s'' not supported by this frame';
  SID3FrameIDStrNotSupported = 'Frame ID string ''%s'' not supported by this frame';

implementation

end.
