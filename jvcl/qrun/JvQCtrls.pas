{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCtrls.PAS, released May 13, 2000.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 2000 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQCtrls;

interface

uses
  QWindows, QMessages, Classes, Types, QGraphics, QControls, QStdCtrls, QImgList,
  JvQButton;


const
  ODS_DISABLED = 1;
  ODS_SELECTED = 2;
  ODS_FOCUS    = 4;
  
type
  TDrawItemStruct = record
    itemState: Integer;
  end;


type
  TJvImgBtnLayout = (blImageLeft, blImageRight);

  TJvImgBtnKind = (bkCustom, bkOK, bkCancel, bkHelp, bkYes, bkNo, bkClose,
    bkAbort, bkRetry, bkIgnore, bkAll);

  TJvCustomImageButton = class;

  TJvImgBtnActionLink = class(TButtonActionLink)
  protected
    FClient: TJvCustomImageButton;
    procedure AssignClient(AClient: TObject); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  TJvImgBtnDrawEvent = procedure(Sender: TObject; const DrawItemStruct: TDrawItemStruct) of object;
  TJvImgBtnAnimIndexEvent = procedure(Sender: TObject; CurrentAnimateFrame: Byte;
    var ImageIndex: Integer) of object;

  TJvCustomImageButton = class(TJvCustomButton)
  private
    FAlignment: TAlignment;
    FAnimate: Boolean;
    FAnimateFrames: Integer;
    FAnimateInterval: Cardinal;
    FAnimating: Boolean;
    FCanvas: TCanvas;
    FCurrentAnimateFrame: Byte;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FIsFocused: Boolean;
    FKind: TJvImgBtnKind;
    FLayout: TJvImgBtnLayout;
    FOwnerDraw: Boolean;
    FSpacing: Integer;
    FMargin: Integer;
    FMouseInControl: Boolean;
    FOnButtonDraw: TJvImgBtnDrawEvent;
    FOnGetAnimateIndex: TJvImgBtnAnimIndexEvent;
    FImageVisible: Boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAnimate(const Value: Boolean);
    procedure SetAnimateFrames(const Value: Integer);
    procedure SetAnimateInterval(const Value: Cardinal);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageVisible(const Value: Boolean);
    procedure SetKind(const Value: TJvImgBtnKind);
    procedure SetLayout(const Value: TJvImgBtnLayout);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer); 
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
  protected 
    procedure DestroyWidget; override;
    procedure Paint; override; 
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CalcButtonParts(ButtonRect: TRect; var RectText, RectImage: TRect); 
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); dynamic;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetCustomCaption: string; dynamic;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetKindImageIndex: Integer;
    function GetRealCaption: string;override;
    procedure InvalidateImage;
    function IsImageVisible: Boolean;
    procedure Loaded; override;
    procedure SetButtonStyle(ADefault: Boolean); 
    procedure ShowNextFrame;
    procedure StartAnimate;
    procedure StopAnimate;
    procedure RestartAnimate;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    class procedure InitializeDefaultImageList;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Animate: Boolean read FAnimate write SetAnimate default False;
    property AnimateFrames: Integer read FAnimateFrames write SetAnimateFrames default 0;
    property AnimateInterval: Cardinal read FAnimateInterval write SetAnimateInterval default 200;
    property Color default clBtnFace;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ImageVisible: Boolean read FImageVisible write SetImageVisible default True;
    property Kind: TJvImgBtnKind read FKind write SetKind default bkCustom;
    property Layout: TJvImgBtnLayout read FLayout write SetLayout default blImageLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property OnButtonDraw: TJvImgBtnDrawEvent read FOnButtonDraw write FOnButtonDraw;
    property OnGetAnimateIndex: TJvImgBtnAnimIndexEvent read FOnGetAnimateIndex write FOnGetAnimateIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure DrawButtonImage(ImageBounds: TRect);
    procedure DrawButtonFocusRect(const RectContent: TRect);
    procedure DrawButtonFrame(const DrawItemStruct: TDrawItemStruct; var RectContent: TRect);
    procedure DrawButtonText(TextBounds: TRect; TextEnabled: Boolean);
    property Canvas: TCanvas read FCanvas;
    property CurrentAnimateFrame: Byte read FCurrentAnimateFrame;
    property MouseInControl: Boolean read FMouseInControl;
  end;

  TJvImgBtn = class(TJvCustomImageButton)
  published
    property Alignment;
    property Animate;
    property AnimateFrames;
    property AnimateInterval;
    property Color;
    property DropDownMenu;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;

    property HintColor;
    property Images;
    property ImageIndex;
    property ImageVisible;
    property Kind;
    property Layout;
    property Margin;
    property Spacing;
    property WordWrap;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OwnerDraw; 
    property OnGetAnimateIndex;
  end;

implementation

uses
  QConsts, SysUtils, QForms, QActnList, 
  JvQJCLUtils, JvQThemes, JvQFinalize;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvCtrls.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvCtrls.res}
{$ENDIF LINUX}

const
  sUnitName = 'JvCtrls';

const
  JvImgBtnModalResults: array [TJvImgBtnKind] of TModalResult =
    (mrNone, mrOk, mrCancel, mrNone, mrYes, mrNo, mrNone,
     mrAbort, mrRetry, mrIgnore, mrAll);

  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

var
  DefaultImgBtnImagesList: TImageList = nil;

//=== { TJvImgBtnActionLink } ================================================

procedure TJvImgBtnActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvCustomImageButton;
end;

function TJvImgBtnActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TJvImgBtnActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;

//=== { TJvCustomImageButton } ==========================================================

constructor TJvCustomImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FAlignment := taCenter;
  FAnimateInterval := 200;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageIndex := -1;
  FImageVisible := True;
  FKind := bkCustom;
  FLayout := blImageLeft;
  FMargin := -1;
  FSpacing := 4;
  Color := clBtnFace;
  InitializeDefaultImageList;
end;

destructor TJvCustomImageButton.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FreeAndNil(FCanvas);
end;

procedure TJvCustomImageButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images:= nil;
end;



procedure TJvCustomImageButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if ActionList <> nil then
        Self.SetImages(ActionList.Images);
      Self.SetImageIndex(ImageIndex);
      Invalidate;
    end;
end;

procedure TJvCustomImageButton.CalcButtonParts(ButtonRect: TRect; var RectText, RectImage: TRect);
var
  BlockWidth, ButtonWidth, ButtonHeight, BlockMargin, InternalSpacing: Integer;
begin
  SetRect(RectText, 0, 0, 0, 0);
  //  RectText.Right := ButtonRect.Right - ButtonRect.Left;
  DrawText(Canvas, PChar(GetRealCaption), -1, RectText, DT_CALCRECT or Alignments[FAlignment]);
  if IsImageVisible then
  begin
    with GetImageList do
      SetRect(RectImage, 0, 0, Width - 1, Height - 1);
    InternalSpacing := Spacing;
  end
  else
  begin
    SetRect(RectImage, 0, 0, 0, 0);
    InternalSpacing := 0;
  end;
  BlockWidth := RectImage.Right + InternalSpacing + RectText.Right;
  ButtonWidth := ButtonRect.Right - ButtonRect.Left;
  if Margin = -1 then
    BlockMargin := (ButtonWidth - BlockWidth) div 2
  else
    BlockMargin := Margin;
  case Layout of
    blImageLeft:
      begin
        OffsetRect(RectImage, BlockMargin, 0);
        OffsetRect(RectText, RectImage.Right + InternalSpacing, 0);
      end;
    blImageRight:
      begin
        OffsetRect(RectImage, ButtonWidth - BlockMargin - RectImage.Right, 0);
        OffsetRect(RectText, ButtonWidth - BlockWidth - BlockMargin, 0);
      end;
  end;
  ButtonHeight := ButtonRect.Bottom - ButtonRect.Top;
  OffsetRect(RectImage, ButtonRect.Left, (ButtonHeight - RectImage.Bottom) div 2 + ButtonRect.Top);
  OffsetRect(RectText, ButtonRect.Left, (ButtonHeight - RectText.Bottom) div 2 + ButtonRect.Top);
end;

procedure TJvCustomImageButton.Click;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
  case FKind of
    bkClose:
      begin
        Form := GetParentForm(Self);
        if Form <> nil then
          Form.Close
        else
          inherited Click;
      end;
    bkHelp:
      begin
        Control := Self;
        while (Control <> nil) and (Control.HelpContext = 0) do
          Control := Control.Parent;
        if Control <> nil then  
          Application.HelpSystem.ShowContextHelp(Control.HelpContext, Application.HelpFile) 
        else
          inherited Click;
      end;
  else
    inherited Click;
  end;
end;

procedure TJvCustomImageButton.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

procedure TJvCustomImageButton.FontChanged;
begin
  inherited FontChanged;
  Invalidate;
end;

procedure TJvCustomImageButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FMouseInControl and Enabled and (GetCapture = NullHandle) then
  begin
    FMouseInControl := True;
    inherited MouseEnter(Control); 
  end;
end;

procedure TJvCustomImageButton.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if FMouseInControl and Enabled and not Dragging then
  begin
    FMouseInControl := False;
    inherited MouseLeave(Control); 
  end;
end;



procedure TJvCustomImageButton.Paint;
var
  DrawItemStruct: TDrawItemStruct;
begin
  if csDestroying in ComponentState then
    Exit;

  with DrawItemStruct do
  begin
    itemState := 0;
    if Focused then
      itemState := ODS_FOCUS;
    if not Enabled then
      itemState := ODS_DISABLED;
    if Default then
      itemState := ODS_SELECTED;
  end;

  FCanvas.Handle := inherited Canvas.Handle;
  FCanvas.Start(False);
  try
    FCanvas.Font := Font;
    if FOwnerDraw and Assigned(FOnButtonDraw) then
      FOnButtonDraw(Self, DrawItemStruct)
    else
      DrawItem(DrawItemStruct);
  finally
    FCanvas.Stop;
    FCanvas.Handle := NullHandle;
  end;
end;


procedure TJvCustomImageButton.DrawButtonFocusRect(const RectContent: TRect);
begin
  if FIsFocused and not (csDestroying in ComponentState) then
  begin
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Brush.Color := clBtnFace;
    DrawFocusRect(FCanvas.Handle, RectContent);
  end;
end;

procedure TJvCustomImageButton.DrawButtonFrame(const DrawItemStruct: TDrawItemStruct; var RectContent: TRect);
var
  IsDown, IsEnabled, IsDefault: Boolean;
  R: TRect;
  Flags: DWORD; 
begin
  if csDestroying in ComponentState then
    Exit;
  with DrawItemStruct do
  begin
    IsEnabled := itemState and ODS_DISABLED = 0;
    IsDown := (itemState and ODS_SELECTED <> 0) and IsEnabled;
    IsDefault := itemState and ODS_FOCUS <> 0;
  end;
 
  begin
    R := ClientRect;

    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if IsDown then
      Flags := Flags or DFCS_PUSHED;
    if not IsEnabled then
      Flags := Flags or DFCS_INACTIVE;

    if FIsFocused or IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end;

    if IsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
      DrawFrameControl(FCanvas.Handle, R, DFC_BUTTON, Flags);
    FCanvas.Brush.Color := Color;
    FCanvas.FillRect(R);

    // Return content rect
    RectContent := ClientRect;
    InflateRect(RectContent, -4, -4);
  end;
end;

procedure TJvCustomImageButton.DrawButtonImage(ImageBounds: TRect);

var
  glyph: TBitmap;

begin
  if csDestroying in ComponentState then
    Exit;
  with ImageBounds do
    if IsImageVisible then  
      if Assigned(FImages) then
        FImages.Draw(FCanvas, Left, Top, GetImageIndex, itImage, Enabled)
      else
      begin
        Glyph := TBitmap.Create;
        DefaultImgBtnImagesList.GetBitmap(GetKindImageIndex, Glyph);
        Glyph.TransparentColor := clOlive;
        FCanvas.draw(Left, Top, Glyph);
        Glyph.Free;
      end; 
end;

procedure TJvCustomImageButton.DrawButtonText(TextBounds: TRect; TextEnabled: Boolean);
var
  Flags: DWORD;
  RealCaption: string;
begin
  Flags := DrawTextBiDiModeFlags(DT_VCENTER or Alignments[FAlignment]);
  RealCaption := GetRealCaption;
  with Canvas do
  begin
    Brush.Style := bsClear;
    if not TextEnabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Canvas, RealCaption, Length(RealCaption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Canvas, RealCaption, Length(RealCaption), TextBounds, Flags);
    end
    else
      DrawText(Canvas, RealCaption, Length(RealCaption), TextBounds, Flags);
  end;
end;

procedure TJvCustomImageButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  R, RectContent, RectText, RectImage: TRect;
begin
  DrawButtonFrame(DrawItemStruct, RectContent);

  //R := ClientRect;
  //InflateRect(R, -4, -4);
  R := RectContent;
  if (DrawItemStruct.itemState and ODS_SELECTED <> 0) and Enabled then
  begin 
      OffsetRect(R, 1, 1);
  end;

  CalcButtonParts(R, RectText, RectImage);
  DrawButtonText(RectText, Enabled);
  DrawButtonImage(RectImage);

  DrawButtonFocusRect(RectContent);
end;

function TJvCustomImageButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvImgBtnActionLink;
end;

function TJvCustomImageButton.GetCustomCaption: string;
const
  Captions: array [TJvImgBtnKind] of string =
    ('', SOKButton, SCancelButton, SHelpButton, SYesButton, SNoButton,
      SCloseButton, SAbortButton, SRetryButton, SIgnoreButton, SAllButton);
begin
  Result := Captions[FKind];
end;

function TJvCustomImageButton.GetImageIndex: Integer;
begin
  if FAnimating then
  begin
    Result := FImageIndex + FCurrentAnimateFrame - 1;
    if Assigned(FOnGetAnimateIndex) then
      FOnGetAnimateIndex(Self, FCurrentAnimateFrame, Result);
  end
  else
    Result := FImageIndex;
end;

function TJvCustomImageButton.GetImageList: TCustomImageList;
begin
  if Assigned(FImages) then
    Result := FImages
  else
    Result := DefaultImgBtnImagesList;
end;

function TJvCustomImageButton.GetKindImageIndex: Integer;
const
  ImageKindIndexes: array [TJvImgBtnKind] of Integer =
    (-1, 2, 4, 0, 3, 1, 5, 8, 6, 9, 7);
begin
  Result := ImageKindIndexes[FKind];
end;

function TJvCustomImageButton.GetRealCaption: string;
begin
  if (FKind <> bkCustom) and (Caption = '') then
    Result := GetCustomCaption
  else
    Result := inherited GetRealCaption;
end;

procedure TJvCustomImageButton.ImageListChange(Sender: TObject);
begin
  InvalidateImage;
end;

class procedure TJvCustomImageButton.InitializeDefaultImageList;

var
  ResBmp: TBitmap;

begin
  if not Assigned(DefaultImgBtnImagesList) then
  begin
    DefaultImgBtnImagesList := TImageList.CreateSize(18, 18);
    AddFinalizeObjectNil(sUnitName, TObject(DefaultImgBtnImagesList));  
    ResBmp := TBitmap.Create;
    try
      ResBmp.LoadFromResourceName(HInstance, 'JVIMGBTNDEFAULT');
      DefaultImgBtnImagesList.Add(ResBmp, nil);
    finally
      ResBmp.Free;
    end; 
  end;
end;

procedure TJvCustomImageButton.InvalidateImage;
begin
  Invalidate;
end;

function TJvCustomImageButton.IsImageVisible: Boolean;
begin
  Result := FImageVisible and
    ((Assigned(FImages) and (GetImageIndex <> -1)) or
    (not Assigned(FImages) and (FKind <> bkCustom)));
end;

procedure TJvCustomImageButton.Loaded;
begin
  inherited Loaded;
  if FAnimate then
    StartAnimate;
end;

procedure TJvCustomImageButton.RestartAnimate;
begin
  if FAnimating then
  begin
    StopAnimate;
    StartAnimate;
    InvalidateImage;
  end;
end;

procedure TJvCustomImageButton.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetAnimate(const Value: Boolean);
begin
  if FAnimate <> Value then
  begin
    FAnimate := Value;
    if Value then
      StartAnimate
    else
      StopAnimate;
    InvalidateImage;
  end;
end;

procedure TJvCustomImageButton.SetAnimateFrames(const Value: Integer);
begin
  if FAnimateFrames <> Value then
  begin
    FAnimateFrames := Value;
    RestartAnimate;
  end;
end;

procedure TJvCustomImageButton.SetAnimateInterval(const Value: Cardinal);
begin
  if FAnimateInterval <> Value then
  begin
    FAnimateInterval := Value;
    RestartAnimate;
  end;
end;

procedure TJvCustomImageButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> FIsFocused then
  begin
    FIsFocused := ADefault;
    Refresh;
  end;
end;

procedure TJvCustomImageButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    InvalidateImage;
  end;
end;

procedure TJvCustomImageButton.SetImages(const Value: TCustomImageList);
begin
  if FImages <> nil then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end
  else
    SetImageIndex(-1);
  InvalidateImage;
end;

procedure TJvCustomImageButton.SetImageVisible(const Value: Boolean);
begin
  if FImageVisible <> Value then
  begin
    FImageVisible := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetKind(const Value: TJvImgBtnKind);
begin
  if FKind <> Value then
  begin
    if Value <> bkCustom then
    begin
      Default := Value in [bkOK, bkYes];
      Cancel := Value in [bkCancel, bkNo];
      if not (csLoading in ComponentState) and (FKind = bkCustom) then
      begin
        Caption := '';
        Images := nil;
      end;
    end;
    ModalResult := JvImgBtnModalResults[Value];
    FKind := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetLayout(const Value: TJvImgBtnLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if (csDesigning in ComponentState) and (FAlignment <> taCenter) then
      case FLayout of
        blImageLeft: FAlignment := taLeftJustify;
        blImageRight: FAlignment := taRightJustify;
      end;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetMargin(const Value: Integer);
begin
  if (FMargin <> Value) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetOwnerDraw(const Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvCustomImageButton.ShowNextFrame;
begin
  Inc(FCurrentAnimateFrame);
  if FCurrentAnimateFrame > FAnimateFrames then
    FCurrentAnimateFrame := 1;
  InvalidateImage;
end;

procedure TJvCustomImageButton.StartAnimate;
begin
  if ComponentState * [csDesigning, csLoading] = [] then
  begin
    DoubleBuffered := True;
    FCurrentAnimateFrame := 0;
    ShowNextFrame;
    OSCheck(SetTimer(Handle, 1, FAnimateInterval, nil) <> 0);
    FAnimating := True;
  end;
end;

procedure TJvCustomImageButton.StopAnimate;
begin
  if FAnimating then
  begin
    KillTimer(Handle, 1);
    FCurrentAnimateFrame := 0;
    DoubleBuffered := False;
    FAnimating := False;
  end;
end;




procedure TJvCustomImageButton.DestroyWidget;
begin
  StopAnimate;
  inherited DestroyWidget;
end;


procedure TJvCustomImageButton.WMTimer(var Msg: TWMTimer);
begin
  if Msg.TimerID = 1 then
  begin
    ShowNextFrame;
    Msg.Result := 1;
  end
  else
    inherited;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

