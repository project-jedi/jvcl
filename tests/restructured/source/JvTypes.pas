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

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTypes;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics;

type
  TOnUrlClick = procedure(Sender: TObject; Url: string) of object;
  TOnLinkClick = procedure(Sender: TObject; Link: string) of object;

type
  TCplApplet = function(hwndCPl: THandle; uMsg: DWORD; lParam1, lParam2: LongInt): LongInt; stdcall;
  TOnChangeKey = procedure(Sender: TObject; Rootkey: HKEY; Path: string) of object;

type
  TLabelDirection = (sdLeftToRight, sdRightToLeft);
  TAngle = 0..360;
  TDirection = (drFromLeft, drFromRight, drFromTop, drFromBottom);

type
  TJvOutputMode = (omFile, omStream);
  TOnDoneFile = procedure(Sender: TObject; FileName: string; FileSize: Integer; Url: string) of object;
  TOnDoneStream = procedure(Sender: TObject; Stream: TStream; StreamSize: Integer; Url: string) of object;
  TOnProgress = procedure(Sender: TObject; Position: Integer; TotalSize: Integer; Url: string;
    var Continue: Boolean) of object;
  TOnFtpProgress = procedure(Sender: TObject; Position: Integer; Url: string) of object;

const
  MaxPixelCount = 32768;

type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBTriple;
  TBalance = 0..100;
  TVolumeRec = record
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

type
  TOnError = procedure(Sender: TObject; ErrorMsg: string) of object;
  TWallpaperStyle = (wpTile, wpCenter, wpStretch);
  TTransformationKind = (ttWipeLeft, ttWipeRight, ttWipeUp, ttWipeDown,
    ttTurnLeft, ttTurnRight, ttTurnUp, ttTurnDown,
    ttWipeDownRight, ttWipeDownLeft, ttWipeUpRight, ttWipeUpLeft);
  TJvWaveLocation = (frFile, frResource, frRAM);
  TStars = record
    X: Integer;
    Y: Integer;
    Color: TColor;
    Speed: Integer;
  end;
  TJoyCap = (joHasZCoordinate, joHasRudder, joHasUCoordinate, joHasVCoordinate, joHasPointOfVue,
    joHasPointOfVDiscrete, joHasPointOfVContinuous);
  TJoyCaps = set of TJoyCap;
  TJoyButtonDown = procedure(Sender: TObject; X, Y: Integer; ButtonChanged: Integer; But1Pressed, But2Pressed,
    But3Pressed, But4Pressed: Boolean) of object;
  TJoyMove = procedure(Sender: TObject; X, Y: Integer; But1Pressed, But2Pressed, But3Pressed, But4Pressed: Boolean) of
    object;
  TJoyZMove = procedure(Sender: TObject; Z: Integer; But1Pressed, But2Pressed, But3Pressed, But4Pressed: Boolean) of
    object;
  TJoyErrorMsg = procedure(Sender: TObject; code: Integer; Msg: string) of object;
  TBright = 0..200;

type
  TPopupPosition = (ppNone, ppForm, ppApplication);
  TJvDirMask = (dmFileNameChange, dmDirnameChange, dmAttributesChange, dmSizeChange, dmLastWriteChange,
    dmSecurityChange);
  TJvDirMasks = set of TJvDirMask;
  EJvDirectoryError = class(Exception);
  TListEvent = procedure(Sender: TObject; Title: string; Handle: THandle) of object;

type
  TOnPrnProgress = procedure(Sender: TObject; Current, Total: Integer) of object;
  TOnNextPage = procedure(Sender: TObject; PageNumber: Integer) of object;
  TBitmapStyle = (bsNormal, bsCentered, bsStretched);

type
  TOnOpened = procedure(Sender: TObject; Value: string) of object;
  TOnOpenCanceled = procedure(Sender: TObject) of object;

type
  TOnKeyFound = procedure(Sender: TObject; Key, Results, OriginalLine: string) of object;
  TParserInfos = TStringList;
  TParserInf = class
    StartTag: string;
    EndTag: string;
    MustBe: Integer;
    TakeText: Integer;
  end;

  { (rom) unused and silly
  const
    cr = #13#10;
  }

type
  TGradStyle = (grFilled, grEllipse, grHorizontal, grVertical, grPyramid, grMount);
  TOnDelete = procedure(Sender: TObject; Path: string) of object;
  TOnParent = procedure(Sender: TObject; ParentWindow: THandle) of object;
  TOnImage = procedure(Sender: TObject; Image: TBitmap) of object;
  TOnText = procedure(Sender: TObject; Text: string) of object;
  TJvRestart = (rsLogoff, rsShutdown, rsReboot, rsRestart, rsRebootSystem, rsExitAndExecApp);
  TJvRunOption = (roNoBrowse, roNoDefault, roCalcDirectory, roNoLabel, roNoSeparateMem);
  TJvRunOptions = set of TJvRunOption;
  TJvFileKind = (ftFile, ftPrinter);

const
  SHFMT_ID_DEFAULT = $FFFF;
  SHFMT_OPT_QUICKFORMAT = $0000;
  SHFMT_OPT_FULL = $0001;
  SHFMT_OPT_SYSONLY = $0002;
  SHFMT_ERROR = -1;
  SHFMT_CANCEL = -2;
  SHFMT_NOFORMAT = -3;

type
  TSHFormatDrive = function(Handle: HWND; Drive, ID, Options: Word): LongInt; stdcall;
  TFormatOption = (shQuickFormat, shFull, shSystemFilesOnly);
  TButtonStyle = (bsAbortRetryIgnore, bsOk, bsOkCancel, bsRetryCancel, bsYesNo, bsYesNoCancel);
  TButtonDisplay = (bdIconExclamation, bdIconWarning, bdIconInformation, bdIconAsterisk,
    bdIconQuestion, bdIconStop, bdIconError, bdIconHand);
  TDefault = (dbButton1, dbButton2, dbButton3, dbButton4);
  TModality = (bmApplModal, bmSystemModal, bmTaskModal);
  TButtonOption = (boDefaultDesktopOnly, boHelp, boRight, boRtlReading, boSetForeground, boTopMost);
  TButtonOptions = set of TButtonOption;
  TButtonResult = (brAbort, brCancel, brIgnore, brNo, brOk, brRetry, brYes);
  TMsgStyle = (msBeep, msIconAsterisk, msIconExclamation, msIconHand, msIconQuestion, msOk);
  TDiskRes = (dsSuccess, dsCancel, dsSkipfile, dsError);
  TDiskStyle = (idfCheckFirst, idfNoBeep, idfNoBrowse, idfNoCompressed, idfNoDetails,
    idfNoForeground, idfNoSkip, idfOemDisk, idfWarnIfSkip);
  TDiskStyles = set of TDiskStyle;
  TDeleteStyle = (idNoBeep, idNoForeground);
  TDeleteStyles = set of TDeleteStyle;
  TOnOk = procedure(Sender: TObject; Password: string; var Accept: Boolean) of object;

const
  TVIS_CHECKED = $2000;

const
  CPL_DBLCLK = 5;
  { (rom) unused and silly
  Oper = 120 * Pi / 180;
  }

type
  TCoordChanged = procedure(Sender: TObject; Coord: string) of object;
  TNotifyEventParams = procedure(Sender: TObject; params: Pointer) of object;
  TFileInformation = record
    Attributes: DWORD;
    DisplayName: string;
    ExeType: Integer;
    Icon: HICON;
    Location: string;
    TypeName: string;
    SysIconIndex: Integer;
  end;
  TJvAnimations = (anLeftRight, anRightLeft, anRightAndLeft, anLeftVumeter, anRightVumeter);
  TJvAnimation = set of TJvAnimations;
  TDropEvent = procedure(Sender: TObject; Pos: TPoint; Value: TStringList) of object;
  TOnFound = procedure(Sender: TObject; Path: string) of object;
  TOnChangedDir = procedure(Sender: TObject; Directory: string) of object;
  TOnAlarm = procedure(Sender: TObject; Keyword: string) of object;
  TAlarm = record
    Keyword: string;
    DateTime: TDateTime;
  end;

type
  TOnChangeColor = procedure(Sender: TObject; Foreground, Background: TColor) of object;

implementation

end.
