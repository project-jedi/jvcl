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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  (rom) all types from a single file should be put back in their file
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTypes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

const
  MaxPixelCount = 32767;
  // (rom) unused
  {$IFDEF COMPILER7_UP}
  // (rom) is this correct?
  DEFAULT_SYSCOLOR_MASK = $000000FF;
  {$ELSE}
  DEFAULT_SYSCOLOR_MASK = $80000000;
  {$ENDIF}

type
  TJvRegKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers, hkCurrentConfig, hkPerformanceData, hkDynData);
  TJvRegKeys = set of TJvRegKey;

  {$IFNDEF COMPILER5_UP}
  TAnchorKind = (akLeft, akTop, akRight, akBottom);
  TAnchors = set of TAnchorKind;
  {$ENDIF}
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
  TJvProgressEvent = procedure(Sender: TObject; Position: Integer; TotalSize: Integer; Url: string;
    var Continue: Boolean) of object;
  TJvFTPProgressEvent = procedure(Sender: TObject; Position: Integer; Url: string) of object;

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
  { (rom) unused
  TPeaks = record
    Left: Integer;
    Right: Integer;
    LeftShifted: Integer;
    RightShifted: Integer;
  end;
  }

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

  TJvPrintProgressEvent = procedure(Sender: TObject; Current, Total: Integer) of object;
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
  TJvFileInfoRec = record
    Attributes: DWORD;
    DisplayName: string;
    ExeType: Integer;
    Icon: HICON;
    Location: string;
    TypeName: string;
    SysIconIndex: Integer;
  end;
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

implementation

end.



