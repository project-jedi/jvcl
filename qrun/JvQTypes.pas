{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTypes.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Thornqvist
Oliver Giesen
Gustavo Bianconi

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQTypes;

interface

uses
  SysUtils, Classes,
  
  
  Qt, QTypes, Types, QControls, QForms, QGraphics, QWindows, JvQWStrUtils,
  
  JvQConsts, JvQResources;

const
  MaxPixelCount = 32767;

{$HPPEMIT '#ifndef TDate'}


{$HPPEMIT '#define TDate TDateTime'}
{$HPPEMIT '#define TTime TDateTime'}
type
  TDate = type TDateTime;
  {$EXTERNALSYM TDate}
  TTime = type TDateTime;
  {$EXTERNALSYM TTime}

{$HPPEMIT '#endif'}

type
  

  // Base class for persistent properties that can show events.
  // By default, Delhpi and BCB don't show the events of a class
  // derived from TPersistent unless it also derives from
  // TComponent. However, up until version 5, you couldn't have
  // a Component as a Sub Component of another one, thus preventing
  // from having events for a sub property.
  // The design time editor associated with TJvPersistent will display
  // the events, thus mimicking a Sub Component.
  
  TJvPersistent = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  TJvRegKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers,
    hkPerformanceData, hkCurrentConfig, hkDynData);
  TJvRegKeys = set of TJvRegKey;

  // base JVCL Exception class to derive from
  EJVCLException = class(Exception);

  TJvLinkClickEvent = procedure(Sender: TObject; Link: string) of object;
  //  TOnRegistryChangeKey = procedure(Sender: TObject; RootKey: HKEY; Path: string) of object;
  //  TAngle = 0..360;
  TJvOutputMode = (omFile, omStream);
  //  TLabelDirection = (sdLeftToRight, sdRightToLeft); // JvScrollingLabel

  TJvDoneFileEvent = procedure(Sender: TObject; FileName: string; FileSize: Integer; Url: string) of object;
  TJvDoneStreamEvent = procedure(Sender: TObject; Stream: TStream; StreamSize: Integer; Url: string) of object;
  TJvHTTPProgressEvent = procedure(Sender: TObject; UserData, Position: Integer; TotalSize: Integer; Url: string; var Continue: Boolean) of object;
  TJvFTPProgressEvent = procedure(Sender: TObject; Position: Integer; Url: string) of object;

  // from JvComponent.pas
  TJvClipboardCommand = (caCopy, caCut, caPaste, caUndo);
  TJvClipboardCommands = set of TJvClipboardCommand;

  // used in JvSpeedButton, JvArrowButton, JvButton
  TJvCMButtonPressed = packed record
    Msg: Cardinal;
    Index: Integer;
    Control: TControl;
    Result: Longint;
  end;

  // used in JvButton
  TCMForceSize = record
    Msg: Cardinal;
    Sender: TControl;
    NewSize: TSmallPoint;
    Result: Longint;
  end;

  
  
  TJvRGBTriple = TRGBQuad; // VisualCLX does not support pf24bit
  

  PJvRGBArray = ^TJvRGBArray;
  TJvRGBArray = array [0..MaxPixelCount - 1] of TJvRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [0..MaxPixelCount - 1] of TRGBQuad;
  PRGBPalette = ^TRGBPalette;
  TRGBPalette = array [Byte] of TRGBQuad;

  { (rom) unused
  TJvPoint = class(TPersistent)
  protected
    FX: Integer;
    FY: Integer;
  published
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;
  }

  TJvErrorEvent = procedure(Sender: TObject; ErrorMsg: string) of object;
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
  // (rom) definitely needs improvement
  TJvParserInfo = class(TObject)
  public
    StartTag: string;
    EndTag: string;
    MustBe: Integer;
    TakeText: Integer;
  end;

  

  TJvGradientStyle = (grFilled, grEllipse, grHorizontal, grVertical, grPyramid, grMount);
  //  TOnDelete = procedure(Sender: TObject; Path: string) of object;
  TJvParentEvent = procedure(Sender: TObject; ParentWindow: THandle) of object;
  //  TOnImage = procedure(Sender: TObject; Image: TBitmap) of object; // JvClipboardViewer
  //  TOnText = procedure(Sender: TObject; Text: string) of object;
  //  TJvRestart = (rsLogoff, rsShutdown, rsReboot, rsRestart, rsRebootSystem, rsExitAndExecApp);
  //  TJvRunOption = (roNoBrowse, roNoDefault, roCalcDirectory, roNoLabel, roNoSeparateMem); // JvRunDlg
  //  TJvRunOptions = set of TJvRunOption; // JvRunDlg
  //  TJvFileKind = (ftFile, ftPrinter); // JvObjectPropertiesDlg

  //  TSHFormatDrive = function(Handle: HWND; Drive, ID, Options: Word): LongInt; stdcall; // JvFormatDrive
  //  TFormatOption = (shQuickFormat, shFull, shSystemFilesOnly); // JvFormatDrive
  //  TButtonStyle = (bsAbortRetryIgnore, bsOk, bsOkCancel, bsRetryCancel, bsYesNo, bsYesNoCancel); // JvMessageBox
  //  TButtonDisplay = (bdIconExclamation, bdIconWarning, bdIconInformation, bdIconAsterisk, bdIconQuestion, bdIconStop, bdIconError, bdIconHand); // JvMessageBox

  //  TDefault = (dbButton1, dbButton2, dbButton3, dbButton4); // JvMessageBox
  //  TModality = (bmApplModal, bmSystemModal, bmTaskModal); // JvMessageBox
  //  TButtonOption = (boDefaultDesktopOnly, boHelp, boRight, boRtlReading, boSetForeground, boTopMost); // JvMessageBox
  //  TButtonOptions = set of TButtonOption; // JvMessageBox
  //  TButtonResult = (brAbort, brCancel, brIgnore, brNo, brOk, brRetry, brYes); // JvMessageBox
  //  TMsgStyle = (msBeep, msIconAsterisk, msIconExclamation, msIconHand, msIconQuestion, msOk); // JvMessageBeep
  TJvDiskRes = (dsSuccess, dsCancel, dsSkipfile, dsError);
  TJvDiskStyle = (idfCheckFirst, idfNoBeep, idfNoBrowse, idfNoCompressed, idfNoDetails,
    idfNoForeground, idfNoSkip, idfOemDisk, idfWarnIfSkip);
  TJvDiskStyles = set of TJvDiskStyle;
  TJvDeleteStyle = (idNoBeep, idNoForeground);
  TJvDeleteStyles = set of TJvDeleteStyle;
  //   TOnOk = procedure(Sender: TObject; Password: string; var Accept: Boolean) of object; // JvPasswordForm

  //  TCoordChanged = procedure(Sender: TObject; Coord: string) of object;
  TJvNotifyParamsEvent = procedure(Sender: TObject; Params: Pointer) of object;

  

  TJvAnimation = (anLeftRight, anRightLeft, anRightAndLeft, anLeftVumeter, anRightVumeter);
  TJvAnimations = set of TJvAnimation;
  //   TOnFound = procedure(Sender: TObject; Path: string) of object; // JvSearchFile
  //  TOnChangedDir = procedure(Sender: TObject; Directory: string) of object; // JvSearchFile
  //  TOnAlarm = procedure(Sender: TObject; Keyword: string) of object; // JvAlarm
  {  TAlarm = record
      Keyword: string;
      DateTime: TDateTime;
    end;
  } // JvAlarm

  // Bianconi - Moved from JvAlarms.pas
  TJvTriggerKind =
    (tkOneShot, tkEachSecond, tkEachMinute, tkEachHour, tkEachDay, tkEachMonth, tkEachYear);
  // End of Bianconi

  TJvFourCC = array [0..3] of Char;
  PJvAniTag = ^TJvAniTag;
  TJvAniTag = packed record
    ckID: TJvFourCC;
    ckSize: Longint;
  end;

  TJvAniHeader = packed record
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
  TTickCount = Cardinal;

  {**** string handling routines}
  TSetOfChar = TSysCharSet;
  TCharSet = TSysCharSet;

  TDateOrder = (doMDY, doDMY, doYMD);
  TDayOfWeekName = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
  TDaysOfWeek = set of TDayOfWeekName;

const
  DefaultDateOrder = doDMY;

  CenturyOffset: Byte = 60;
  NullDate: TDateTime = 0; {-693594}

  
  
  NullHandle = nil;
  
  
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

type
  
  
  THintString = WideString;
  THintStringList = TWideStringList;
  

type
  // from JvListView.pas
  TJvSortMethod = (smAutomatic, smAlphabetic, smNonCaseSensitive, smNumeric, smDate, smTime, smDateTime, smCurrency);
  TJvListViewColumnSortEvent = procedure(Sender: TObject; Column: Integer; var AMethod:TJvSortMethod) of object;

  // from JvColorProvider.pas
  TColorType = (ctStandard, ctSystem, ctCustom);

  TDefColorItem = record
    Value: TColor;
    Constant: string;
    Description: string;
  end;

const
  ColCount = 20;
  
  
  SysColCount = 58;
  
  ColorValues: array [0 .. ColCount - 1] of TDefColorItem = (
    (Value: clBlack;      Constant: 'clBlack';      Description: RsClBlack),
    (Value: clMaroon;     Constant: 'clMaroon';     Description: RsClMaroon),
    (Value: clGreen;      Constant: 'clGreen';      Description: RsClGreen),
    (Value: clOlive;      Constant: 'clOlive';      Description: RsClOlive),
    (Value: clNavy;       Constant: 'clNavy';       Description: RsClNavy),
    (Value: clPurple;     Constant: 'clPurple';     Description: RsClPurple),
    (Value: clTeal;       Constant: 'clTeal';       Description: RsClTeal),
    (Value: clGray;       Constant: 'clGray';       Description: RsClGray),
    (Value: clSilver;     Constant: 'clSilver';     Description: RsClSilver),
    (Value: clRed;        Constant: 'clRed';        Description: RsClRed),
    (Value: clLime;       Constant: 'clLime';       Description: RsClLime),
    (Value: clYellow;     Constant: 'clYellow';     Description: RsClYellow),
    (Value: clBlue;       Constant: 'clBlue';       Description: RsClBlue),
    (Value: clFuchsia;    Constant: 'clFuchsia';    Description: RsClFuchsia),
    (Value: clAqua;       Constant: 'clAqua';       Description: RsClAqua),
    (Value: clWhite;      Constant: 'clWhite';      Description: RsClWhite),
    (Value: clMoneyGreen; Constant: 'clMoneyGreen'; Description: RsClMoneyGreen),
    (Value: clSkyBlue;    Constant: 'clSkyBlue';    Description: RsClSkyBlue),
    (Value: clCream;      Constant: 'clCream';      Description: RsClCream),
    (Value: clMedGray;    Constant: 'clMedGray';    Description: RsClMedGray)
  );

  SysColorValues: array [0 .. SysColCount - 1] of TDefColorItem = (
    
    
    (Value: clForeground;              Constant: 'clForeground';              Description: RsClForeground),
    (Value: clButton;                  Constant: 'clButton';                  Description: RsClButton),
    (Value: clLight;                   Constant: 'clLight';                   Description: RsClLight),
    (Value: clMidlight;                Constant: 'clMidlight';                Description: RsClMidlight),
    (Value: clDark;                    Constant: 'clDark';                    Description: RsClDark),
    (Value: clMid;                     Constant: 'clMid';                     Description: RsClMid),
    (Value: clText;                    Constant: 'clText';                    Description: RsClText),
    (Value: clBrightText;              Constant: 'clBrightText';              Description: RsClBrightText),
    (Value: clButtonText;              Constant: 'clButtonText';              Description: RsClButtonText),
    (Value: clBase;                    Constant: 'clBase';                    Description: RsClBase),
    (Value: clBackground;              Constant: 'clBackground';              Description: RsClBackground),
    (Value: clShadow;                  Constant: 'clShadow';                  Description: RsClShadow),
    (Value: clHighlight;               Constant: 'clHighlight';               Description: RsClHighlight),
    (Value: clHighlightedText;         Constant: 'clHighlightedText';         Description: RsClHighlightedText),

    (Value: clNormalForeground;        Constant: 'clNormalForeground';        Description: RsClNormalForeground),
    (Value: clNormalButton;            Constant: 'clNormalButton';            Description: RsClNormalButton),
    (Value: clNormalLight;             Constant: 'clNormalLight';             Description: RsClNormalLight),
    (Value: clNormalMidlight;          Constant: 'clNormalMidlight';          Description: RsClNormalMidlight),
    (Value: clNormalDark;              Constant: 'clNormalDark';              Description: RsClNormalDark),
    (Value: clNormalMid;               Constant: 'clNormalMid';               Description: RsClNormalMid),
    (Value: clNormalText;              Constant: 'clNormalText';              Description: RsClNormalText),
    (Value: clNormalBrightText;        Constant: 'clNormalBrightText';        Description: RsClNormalBrightText),
    (Value: clNormalButtonText;        Constant: 'clNormalButtonText';        Description: RsClNormalButtonText),
    (Value: clNormalBase;              Constant: 'clNormalBase';              Description: RsClNormalBase),
    (Value: clNormalBackground;        Constant: 'clNormalBackground';        Description: RsClNormalBackground),
    (Value: clNormalShadow;            Constant: 'clNormalShadow';            Description: RsClNormalShadow),
    (Value: clNormalHighlight;         Constant: 'clNormalHighlight';         Description: RsClNormalHighlight),
    (Value: clNormalHighlightedText;   Constant: 'clNormalHighlightedText';   Description: RsClNormalHighlightedText),

    (Value: clActiveForeground;        Constant: 'clActiveForeground';        Description: RsClActiveForeground),
    (Value: clActiveButton;            Constant: 'clActiveButton';            Description: RsClActiveButton),
    (Value: clActiveLight;             Constant: 'clActiveLight';             Description: RsClActiveLight),
    (Value: clActiveMidlight;          Constant: 'clActiveMidlight';          Description: RsClActiveMidlight),
    (Value: clActiveDark;              Constant: 'clActiveDark';              Description: RsClActiveDark),
    (Value: clActiveMid;               Constant: 'clActiveMid';               Description: RsClActiveMid),
    (Value: clActiveText;              Constant: 'clActiveText';              Description: RsClActiveText),
    (Value: clActiveBrightText;        Constant: 'clActiveBrightText';        Description: RsClActiveBrightText),
    (Value: clActiveButtonText;        Constant: 'clActiveButtonText';        Description: RsClActiveButtonText),
    (Value: clActiveBase;              Constant: 'clActiveBase';              Description: RsClActiveBase),
    (Value: clActiveBackground;        Constant: 'clActiveBackground';        Description: RsClActiveBackground),
    (Value: clActiveShadow;            Constant: 'clActiveShadow';            Description: RsClActiveShadow),
    (Value: clActiveHighlight;         Constant: 'clActiveHighlight';         Description: RsClActiveHighlight),
    (Value: clActiveHighlightedText;   Constant: 'clActiveHighlightedText';   Description: RsClActiveHighlightedText),

    (Value: clDisabledForeground;      Constant: 'clDisabledForeground';      Description: RsClDisabledForeground),
    (Value: clDisabledButton;          Constant: 'clDisabledButton';          Description: RsClDisabledButton),
    (Value: clDisabledLight;           Constant: 'clDisabledLight';           Description: RsClDisabledLight),
    (Value: clDisabledMidlight;        Constant: 'clDisabledMidlight';        Description: RsClDisabledMidlight),
    (Value: clDisabledDark;            Constant: 'clDisabledDark';            Description: RsClDisabledDark),
    (Value: clDisabledMid;             Constant: 'clDisabledMid';             Description: RsClDisabledMid),
    (Value: clDisabledText;            Constant: 'clDisabledText';            Description: RsClDisabledText),
    (Value: clDisabledBrightText;      Constant: 'clDisabledBrightText';      Description: RsClDisabledBrightText),
    (Value: clDisabledButtonText;      Constant: 'clDisabledButtonText';      Description: RsClDisabledButtonText),
    (Value: clDisabledBase;            Constant: 'clDisabledBase';            Description: RsClDisabledBase),
    (Value: clDisabledBackground;      Constant: 'clDisabledBackground';      Description: RsClDisabledBackground),
    (Value: clDisabledShadow;          Constant: 'clDisabledShadow';          Description: RsClDisabledShadow),
    (Value: clDisabledHighlight;       Constant: 'clDisabledHighlight';       Description: RsClDisabledHighlight),
    (Value: clDisabledHighlightedText; Constant: 'clDisabledHighlightedText'; Description: RsClDisabledHighlightedText),

    (Value: clDesktop;                 Constant: 'clDesktop';                 Description: RsClDesktop),
    (Value: clInfoBk;                  Constant: 'clInfoBk';                  Description: RsClInfoBk)
    
  );



type
  PCaptionChar = PWideChar;


type
  // equivalent of TPoint, but that can be a published property for BCB
  TJvPoint = class(TPersistent)
  private
    FY: Longint;
    FX: Longint;
    FOnChange: TNotifyEvent;
    procedure SetX(Value: Longint);
    procedure SetY(Value: Longint);
  protected
    procedure DoChange;
  public
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property X: Longint read FX write SetX;
    property Y: Longint read FY write SetY;
  end;

  // equivalent of TRect, but that can be a published property for BCB
  TJvRect = class(TPersistent)
  private
    FTopLeft: TJvPoint;
    FBottomRight: TJvPoint;
    FOnChange: TNotifyEvent;
    function GetBottom: Integer;
    function GetLeft: Integer;
    function GetRight: Integer;
    function GetTop: Integer;
    procedure SetBottom(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetRight(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetBottomRight(Value: TJvPoint);
    procedure SetTopLeft(Value: TJvPoint);
    procedure PointChange(Sender: TObject);
  protected
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property TopLeft    : TJvPoint     read FTopLeft     write SetTopLeft;
    property BottomRight: TJvPoint     read FBottomRight write SetBottomRight;
    property OnChange   : TNotifyEvent read FOnChange    write FOnChange;
  published
    property Left  : Integer read GetLeft   write SetLeft;
    property Top   : Integer read GetTop    write SetTop;
    property Right : Integer read GetRight  write SetRight;
    property Bottom: Integer read GetBottom write SetBottom;
  end;

implementation


constructor TJvPersistent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetSubComponent(True);
  Name := 'SubComponent';
end;


{ TJvRect }

procedure TJvRect.Assign(Source: TPersistent);
begin
  if Source is TJvRect then
  begin
    TopLeft := (Source as TJvRect).TopLeft;
    BottomRight := (Source as TJvRect).BottomRight;
    DoChange;
  end
  else
    inherited Assign(Source);
end;

constructor TJvRect.Create;
begin
  inherited Create;
  FTopLeft     := TJvPoint.Create;
  FBottomRight := TJvPoint.Create;

  FTopLeft.OnChange     := PointChange;
  FBottomRight.OnChange := PointChange;
end;

destructor TJvRect.Destroy;
begin
  FTopLeft.Free;
  FBottomRight.Free;
  inherited Destroy;
end;

procedure TJvRect.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvRect.GetBottom: Integer;
begin
  Result := FBottomRight.Y;
end;

function TJvRect.GetLeft: Integer;
begin
  Result := FTopLeft.X;
end;

function TJvRect.GetRight: Integer;
begin
  Result := FBottomRight.X;
end;

function TJvRect.GetTop: Integer;
begin
  Result := FTopLeft.Y;
end;

procedure TJvRect.PointChange(Sender: TObject);
begin
  DoChange;
end;

procedure TJvRect.SetBottom(Value: Integer);
begin
  FBottomRight.Y := Value;
end;

procedure TJvRect.SetBottomRight(Value: TJvPoint);
begin
  FBottomRight.Assign(Value);
end;

procedure TJvRect.SetLeft(Value: Integer);
begin
  FTopLeft.X := Value;
end;

procedure TJvRect.SetRight(Value: Integer);
begin
  FBottomRight.X := Value;
end;

procedure TJvRect.SetTop(Value: Integer);
begin
  FTopLeft.Y := Value;
end;

procedure TJvRect.SetTopLeft(Value: TJvPoint);
begin
  FTopLeft.Assign(Value);
end;

{ TJvPoint }

procedure TJvPoint.Assign(Source: TPersistent);
begin
  if Source is TJvPoint then
  begin
    FX := (Source as TJvPoint).X;
    FY := (Source as TJvPoint).Y;
    DoChange;
  end
  else
    inherited;
end;

procedure TJvPoint.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvPoint.SetX(Value: Longint);
begin
  FX := Value;
  DoChange;
end;

procedure TJvPoint.SetY(Value: Longint);
begin
  FY := Value;
  DoChange;
end;

end.

