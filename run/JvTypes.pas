{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTypes.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Thornqvist
Oliver Giesen
Gustavo Bianconi

Last Modified: 2003-10-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  (rom) all types from a single file should be put back in their file
  (p3) disagree, putting them here makes it more obvious when equivalent
       types are declared in different units (f ex progress event types)
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTypes;

interface

uses
{$IFDEF COMPLIB_VCL}
  Windows, Messages, Controls, Forms, Graphics, 
{$ENDIF}
{$IFDEF COMPLIB_CLX}
  Qt, QTypes, Types, QControls, QForms, QGraphics,
{$ENDIF}
  SysUtils, Classes;

const
  MaxPixelCount = 32767;
  // (rom) unused
  {$IFDEF COMPILER7_UP}
  DEFAULT_SYSCOLOR_MASK = $000000FF;
  {$ELSE}
  DEFAULT_SYSCOLOR_MASK = $80000000;
  {$ENDIF}

{$IFDEF COMPLIB_CLX}
// constants for Canvas.TextRect
  AlignLeft = 1 { $1 };
  AlignRight = 2 { $2 };
  AlignHCenter = 4 { $4 };
  AlignTop = 8 { $8 };
  AlignBottom = 16 { $10 };
  AlignVCenter = 32 { $20 };
  AlignCenter = 36 { $24 };
  SingleLine = 64 { $40 };
  DontClip = 128 { $80 };
  ExpandTabs = 256 { $100 };
  ShowPrefix = 512 { $200 };
  WordBreak = 1024 { $400 };
  ModifyString = 2048 { $800 };
  DontPrint = 4096 { $1000 };
  ClipPath = 8192 { $2000 };
  ClipName = 16382 { $4000 );
  CalcRect =  32764 { $8000 } ;
  pf24bit = pf32bit;

const
  { DrawTextEx() Format Flags }
  {$EXTERNALSYM DT_TOP}
  DT_TOP = 0;          // default
  {$EXTERNALSYM DT_LEFT}
  DT_LEFT = 0;         // default
  {$EXTERNALSYM DT_CENTER}
  DT_CENTER = 1;
  {$EXTERNALSYM DT_RIGHT}
  DT_RIGHT = 2;
  {$EXTERNALSYM DT_VCENTER}
  DT_VCENTER = 4;
  {$EXTERNALSYM DT_BOTTOM}
  DT_BOTTOM = 8;
  {$EXTERNALSYM DT_WORDBREAK}
  DT_WORDBREAK = $10;
  {$EXTERNALSYM DT_SINGLELINE}
  DT_SINGLELINE = $20;
  {$EXTERNALSYM DT_EXPANDTABS}
  DT_EXPANDTABS = $40;
//  {$EXTERNALSYM DT_TABSTOP}
//  DT_TABSTOP = $80;
  {$EXTERNALSYM DT_NOCLIP}
  DT_NOCLIP = $100;
//  {$EXTERNALSYM DT_EXTERNALLEADING}
//  DT_EXTERNALLEADING = $200;
  {$EXTERNALSYM DT_CALCRECT}
  DT_CALCRECT = $400;
  {$EXTERNALSYM DT_NOPREFIX}
  DT_NOPREFIX = $800;
//  {$EXTERNALSYM DT_INTERNAL}
//  DT_INTERNAL = $1000;
//  {$EXTERNALSYM DT_HIDEPREFIX}
//  DT_HIDEPREFIX = $00100000;
//  {$EXTERNALSYM DT_PREFIXONLY}
//  DT_PREFIXONLY = $00200000;

//  {$EXTERNALSYM DT_EDITCONTROL}
//  DT_EDITCONTROL = $2000;
  {$EXTERNALSYM DT_PATH_ELLIPSIS}
  DT_PATH_ELLIPSIS = $4000;
  {$EXTERNALSYM DT_END_ELLIPSIS}
  DT_END_ELLIPSIS = $8000;
  DT_ELLIPSIS = DT_END_ELLIPSIS;
  {$EXTERNALSYM DT_MODIFYSTRING}
  DT_MODIFYSTRING = $10000;
//  {$EXTERNALSYM DT_RTLREADING}
//  DT_RTLREADING = $20000;
//  {$EXTERNALSYM DT_WORD_ELLIPSIS}
//  DT_WORD_ELLIPSIS = $40000;
{$ENDIF COMPLIB_CLX}


type
  TJvRegKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers, hkPerformanceData,
    hkCurrentConfig, hkDynData);
  TJvRegKeys = set of TJvRegKey;

  // base JVCL Exception class to derive from
  EJVCLException = class(Exception);
  TJvLinkClickEvent = procedure(Sender: TObject; Link: string) of object;
//  TOnRegistryChangeKey = procedure(Sender: TObject; RootKey: HKEY; Path: string) of object;
//  TAngle = 0..360;
  TJvOutputMode = (omFile, omStream);
//  TLabelDirection = (sdLeftToRight, sdRightToLeft); // JvScrollingLabel
  TJvLabelRotateAngle = 0..360;

  TJvDoneFileEvent = procedure(Sender: TObject; FileName: string; FileSize: Integer; Url: string) of object;
  TJvDoneStreamEvent = procedure(Sender: TObject; Stream: TStream; StreamSize: Integer; Url: string) of object;
  TJvHTTPProgressEvent = procedure(Sender: TObject; UserData, Position: Integer; TotalSize: Integer; Url: string; var Continue: Boolean) of object;
  TJvFTPProgressEvent = procedure(Sender: TObject; Position: Integer; Url: string) of object;

{$IFDEF COMPLIB_CLX}
  HWnd = QWidgetH;
  HCursor = QCursorH;
  TControlClass = class of TControl;
  TMinMaxInfo = packed record
    ptReserved: TPoint;
    ptMaxSize: TPoint;
    ptMaxPosition: TPoint;
    ptMinTrackSize: TPoint;
    ptMaxTrackSize: TPoint;
  end;
  TWindowPlacement = packed record
    length: Cardinal;
    flags: Integer;
    showCmd: TWindowState;
    ptMinPosition: TPoint;
    ptMaxPosition: TPoint;
    rcNormalPosition: TRect;
  end;
  PWindowPlacement = ^TWindowPlacement;
  TTime = TDateTime;
  TDate = TDateTime;

  TRGBQuad = packed record
    rgbtReserved: Byte;
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

  TRGBTriple = TRGBQuad;
//  TRGBArray = array[0..MaxPixelCount - 1] of TRGBQuad;
{$ENDIF}

  PRGBArray = ^TRGBArray; // JvThumbImage, JvImageSplit, JvRegion
  TRGBArray = array [0..MaxPixelCount - 1] of TRGBTriple;
  TBalance = 0..100;
  TJvVolumeRec = record
    case Byte of
      0:
      (LongVolume: Longint);
      1:
      (LeftVolume,
       RightVolume: Word);
  end;

  TJvPoint = class(TPersistent)
  protected
    Fx: Integer;
    Fy: Integer;
  published
    property x: Integer read Fx write Fx;
    property y: Integer read Fy write Fy;
  end;

  TJvErrorEvent = procedure(Sender: TObject; ErrorMsg: string) of object;
  TJvWallpaperStyle = (wpTile, wpCenter, wpStretch);
  TJvTransformationKind = (ttWipeLeft, ttWipeRight, ttWipeUp, ttWipeDown,
    ttTurnLeft, ttTurnRight, ttTurnUp, ttTurnDown,
    ttWipeDownRight, ttWipeDownLeft, ttWipeUpRight, ttWipeUpLeft);
  TJvWaveLocation = (frFile, frResource, frRAM);

  TJvPopupPosition = (ppNone, ppForm, ppApplication);
//  TJvDirMask = (dmFileNameChange, dmDirnameChange, dmAttributesChange, dmSizeChange, dmLastWriteChange, dmSecurityChange); //JvDirectorySpy
//  TJvDirMasks = set of TJvDirMask;
//  EJvDirectoryError = class(EJVCLException); // JvDirectorySpy
//  TListEvent = procedure(Sender: TObject; Title: string; Handle: THandle) of object; // JvWindowsTitle

  TJvProgressEvent = procedure(Sender: TObject; Current, Total: Integer) of object;
  TJvNextPageEvent = procedure(Sender: TObject; PageNumber: Integer) of object;
  TJvBitmapStyle = (bsNormal, bsCentered, bsStretched);

//  TOnOpened = procedure(Sender: TObject; Value: string) of object; // archive
//  TOnOpenCanceled = procedure(Sender: TObject) of object; // archive

  TJvKeyFoundEvent = procedure(Sender: TObject; Key, Results, OriginalLine: string) of object;
  TJvParserInfoList = TStringList;
  TJvParserInfo = class
    StartTag: string;
    EndTag: string;
    MustBe: Integer;
    TakeText: Integer;
  end;

const
  CrLf = #13#10;
  Cr = #13;
  Lf = #10;
  Tab = #9;
  
{$IFNDEF COMPILER6_UP}
  SLineBreak = #13#10;
  PathDelim = '\';
  DriveDelim = ':';
  PathSep    = ';';
{$ENDIF}

type
  TJvGradStyle = (grFilled, grEllipse, grHorizontal, grVertical, grPyramid, grMount);
//  TOnDelete = procedure(Sender: TObject; Path: string) of object;
  TJvParentEvent = procedure(Sender: TObject; ParentWindow: THandle) of object;
//   TOnImage = procedure(Sender: TObject; Image: TBitmap) of object; // JvClipboardViewer
//  TOnText = procedure(Sender: TObject; Text: string) of object;
//  TJvRestart = (rsLogoff, rsShutdown, rsReboot, rsRestart, rsRebootSystem, rsExitAndExecApp);
//  TJvRunOption = (roNoBrowse, roNoDefault, roCalcDirectory, roNoLabel, roNoSeparateMem); // JvRunDlg
//  TJvRunOptions = set of TJvRunOption; // JvRunDlg
//   TJvFileKind = (ftFile, ftPrinter); // JvObjectPropertiesDlg

//  TSHFormatDrive = function(Handle: HWND; Drive, ID, Options: Word): LongInt; stdcall; // JvFormatDrive
//  TFormatOption = (shQuickFormat, shFull, shSystemFilesOnly); // JvFormatDrive
//  TButtonStyle = (bsAbortRetryIgnore, bsOk, bsOkCancel, bsRetryCancel, bsYesNo, bsYesNoCancel); // JvMessageBox
//  TButtonDisplay = (bdIconExclamation, bdIconWarning, bdIconInformation, bdIconAsterisk, bdIconQuestion, bdIconStop, bdIconError, bdIconHand); // JvMessageBox

//  TDefault = (dbButton1, dbButton2, dbButton3, dbButton4); // JvMessageBox
//  TModality = (bmApplModal, bmSystemModal, bmTaskModal); // JvMessageBox
//  TButtonOption = (boDefaultDesktopOnly, boHelp, boRight, boRtlReading, boSetForeground, boTopMost); // JvMessageBox
//  TButtonOptions = set of TButtonOption; // JvMessageBox
//  TButtonResult = (brAbort, brCancel, brIgnore, brNo, brOk, brRetry, brYes); // JvMessageBox
//   TMsgStyle = (msBeep, msIconAsterisk, msIconExclamation, msIconHand, msIconQuestion, msOk); // JvMessageBeep
  TJvDiskRes = (dsSuccess, dsCancel, dsSkipfile, dsError);
  TJvDiskStyle = (idfCheckFirst, idfNoBeep, idfNoBrowse, idfNoCompressed, idfNoDetails,
    idfNoForeground, idfNoSkip, idfOemDisk, idfWarnIfSkip);
  TJvDiskStyles = set of TJvDiskStyle;
  TJvDeleteStyle = (idNoBeep, idNoForeground);
  TJvDeleteStyles = set of TJvDeleteStyle;
//   TOnOk = procedure(Sender: TObject; Password: string; var Accept: Boolean) of object; // JvPasswordForm

//  TCoordChanged = procedure(Sender: TObject; Coord: string) of object;
  TJvNotifyParamsEvent = procedure(Sender: TObject; Params: Pointer) of object;
{$IFDEF COMPLIB_VCL}
  TJvFileInfoRec = record
    Attributes: DWORD;
    DisplayName: string;
    ExeType: Integer;
    Icon: HICON;
    Location: string;
    TypeName: string;
    SysIconIndex: Integer;
  end;
{$ENDIF COMPLIB_VCL}  
  TJvAnimation = (anLeftRight, anRightLeft, anRightAndLeft, anLeftVumeter, anRightVumeter);
  TJvAnimations = set of TJvAnimation;
  TJvDropEvent = procedure(Sender: TObject; Pos: TPoint; Value: TStringList) of object;
//   TOnFound = procedure(Sender: TObject; Path: string) of object; // JvSearchFile
//  TOnChangedDir = procedure(Sender: TObject; Directory: string) of object; // JvSearchFile
//  TOnAlarm = procedure(Sender: TObject; Keyword: string) of object; // JvAlarm
{  TAlarm = record
    Keyword: string;
    DateTime: TDateTime;
  end;
} // JvAlarm

// Bianconi - Moved from JvAlarms.pas
  TJvTriggerKind = (tkOneShot, tkEachSecond, tkEachMinute, tkEachHour, tkEachDay, tkEachMonth, tkEachYear);
// End of Bianconi


  TJvFourCC = array [0..3] of Char;
  PJvAniTag = ^TJvAniTag;
  TJvAniTag = packed record
    ckID: TJvFourCC;
    ckSize: Longint;
  end;

  TJvAniHeader = record
    dwSizeof: Longint;
    dwFrames: Longint;
    dwSteps: Longint;
    dwCX: Longint;
    dwCY: Longint;
    dwBitCount: Longint;
    dwPlanes: Longint;
    dwJIFRate: Longint;
    dwFlags: Longint;
  end;

  TJvChangeColorEvent = procedure(Sender: TObject; Foreground, Background: TColor) of object;

  TJvLayout = (lTop, lCenter, lBottom);
  TJvBevelStyle = (bsShape, bsLowered, bsRaised);

  {for OnLoseFocus the AFocusControl argument will point at the control that
   receives focus while for OnGetFocus it is the control that lost the focus}
  TJvFocusChangeEvent = procedure(const ASender: TObject;
    const AFocusControl: TWinControl) of object;

// JvJCLUtils
type
  TTickCount = Cardinal;

{**** string handling routines}

type
  TSetOfChar = TSysCharSet;
  TCharSet = TSysCharSet;

 {const Separators is used in GetWordOnPos, JvUtils.ReplaceStrings and SubWord}
const
  Separators: TSysCharSet = [#00, ' ', '-', #13, #10, '.', ',', '/', '\', '#', '"', '''',
    ':', '+', '%', '*', '(', ')', ';', '=', '{', '}', '[', ']', '{', '}', '<', '>'];

type
  TDateOrder = (doMDY, doDMY, doYMD);
  TDayOfWeekName = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
  TDaysOfWeek = set of TDayOfWeekName;

const
  DefaultDateOrder = doDMY;

{$IFDEF USE_FOUR_DIGIT_YEAR}
var
  FourDigitYear: Boolean;
{$ELSE}
function FourDigitYear: Boolean;
{$ENDIF USE_FOUR_DIGIT_YEAR}

const
  CenturyOffset: Byte = 60;
  NullDate: TDateTime = {-693594} 0;

const
  DigitChars = ['0'..'9'];
  Brackets = ['(', ')', '[', ']', '{', '}'];
  StdWordDelims = [#0..' ', ',', '.', ';', '/', '\', ':', '''', '"', '`'] + Brackets;
type
  // JvDriveCtrls / JvLookOut
  TJvImageSize = (isSmall, isLarge);
  TJvImageAlign = (iaLeft, iaCentered);

  TJvDriveType = (dtUnknown, dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk);
  TJvDriveTypes = set of TJvDriveType;

  // Defines how a property (like a HotTrackFont) follows changes in the component's normal Font
  TJvTrackFontOption = (
    hoFollowFont,  // makes HotTrackFont follow changes to the normal Font
    hoPreserveCharSet,  // don't change HotTrackFont.Charset
    hoPreserveColor,    // don't change HotTrackFont.Color
    hoPreserveHeight,   // don't change HotTrackFont.Height (affects Size as well)
    hoPreserveName,     // don't change HotTrackFont.Name
    hoPreservePitch,    // don't change HotTrackFont.Pitch
    hoPreserveStyle);   // don't change HotTrackFont.Style
  TJvTrackFontOptions = set of TJvTrackFontOption;

const
  DefaultTrackFontOptions = [hoFollowFont, hoPreserveColor, hoPreserveStyle];

// from JvListView.pas
type
  TJvSortMethod = (smAutomatic, smAlphabetic, smNonCaseSensitive, smNumeric, smDate, smTime, smDateTime, smCurrency);
  TJvListViewColumnSortEvent = procedure(Sender: TObject; Column: Integer; var AMethod:TJvSortMethod) of object;


const
  { Command message for JvSpeedbar editor }
  CM_SPEEDBARCHANGED = CM_BASE + 80;
  { Command message for TJvSpeedButton }
  CM_JVBUTTONPRESSED = CM_BASE + 81;
  { Command messages for TJvWindowHook }
  CM_RECREATEWINDOW  = CM_BASE + 82;
  CM_DESTROYHOOK     = CM_BASE + 83;
  { Notify message for TJvxTrayIcon }
  CM_TRAYICON        = CM_BASE + 84;

  { TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  PaletteMask = $02000000;

const
  crHand     = TCursor(14000);
  crDragHand = TCursor(14001);

implementation

{$IFDEF COMPLIB_CLX}
uses
  QWinCursors;
{$ENDIF}

initialization
  Screen.Cursors[crHand] := LoadCursor(hInstance, 'JV_HANDCUR');
  Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'JV_DRAGCUR');

end.

