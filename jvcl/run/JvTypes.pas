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
{$IFDEF VCL}
  Windows, Messages, Controls, Forms, Graphics,
{$ENDIF}
{$IFDEF VisualCLX}
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

{ Standard Windows colors that are not defined in Delphi 5}
  {$IFNDEF COMPILER6_UP}

  COLOR_MENUHILIGHT = 29;
  {$EXTERNALSYM COLOR_MENUHILIGHT}
  COLOR_MENUBAR = 30;
  {$EXTERNALSYM COLOR_MENUBAR}

  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($F0CAA6);
  clCream = TColor($F0FBFF);
  clMedGray = TColor($A4A0A0);
  clGradientActiveCaption = TColor(COLOR_GRADIENTACTIVECAPTION or $80000000);
  clGradientInactiveCaption = TColor(COLOR_GRADIENTINACTIVECAPTION or $80000000);
  clHotLight = TColor(COLOR_HOTLIGHT or $80000000);
  clMenuHighlight = TColor(COLOR_MENUHILIGHT or $80000000);
  clMenuBar = TColor(COLOR_MENUBAR or $80000000);
  {$ENDIF}

type
{$IFNDEF COMPILER6_UP}
  EOSError = class(EWin32Error);
  IInterface = IUnknown;
{$ENDIF}
  {$M+}
  IInvokable = interface(IInterface)
  end;
  {$M-}

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

{$IFDEF VisualCLX}
//  HWnd = QWidgetH;
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
    showCmd: Integer; // SW_Xxx
    ptMinPosition: TPoint;
    ptMaxPosition: TPoint;
    rcNormalPosition: TRect;
  end;
  PWindowPlacement = ^TWindowPlacement;
  TTime = TDateTime;
  TDate = TDateTime;
  TRGBQuad = packed record
    rgbReserved: Byte;
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
  end;
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
//  TRGBArray = array[0..MaxPixelCount - 1] of TRGBQuad;
{$ENDIF VisualCLX}

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
  sLineBreak = #13#10;
  PathDelim = '\';
  DriveDelim = ':';
  PathSep    = ';';

{ TStream seek origins }
type
  TSeekOrigin = Integer; // Delphi 6: TSeekOrigin = (soBeginning, soCurrent, soEnd);
const
  soBeginning = 0;
  soCurrent = 1;
  soEnd = 2;
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
{$IFDEF VCL}
  TJvFileInfoRec = record
    Attributes: DWORD;
    DisplayName: string;
    ExeType: Integer;
    Icon: HICON;
    Location: string;
    TypeName: string;
    SysIconIndex: Integer;
  end;
{$ENDIF VCL}  
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

uses
{$IFDEF VisualCLX}
  QWinCursors,
{$ENDIF}
  JvConsts; // includes cursor resources

initialization
  Screen.Cursors[crHand] := LoadCursor(hInstance, 'JV_HANDCUR');
  Screen.Cursors[crDragHand] := LoadCursor(hInstance, 'JV_DRAGCUR');

end.

