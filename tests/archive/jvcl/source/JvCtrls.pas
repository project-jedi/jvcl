{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCtrls.PAS, released May 13, 2000.

The Initial Developer of the Original Code is Petr Vones (petr.v@mujmail.cz)
Portions created by Petr Vones are Copyright (C) 2000 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: Jun 18, 2000
Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit JvCtrls;

{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ImgList, ActnList, JclBase, JVCLVer, JvListBox;

type

  TJvListBox = class(TJvCustomListBox)
  public
    property Count;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property HorzExtent;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property OwnerData;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Alignment;
    property HotTrack;
    property HintColor;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;
    property OnSelectCancel;
    property OnChange;
    property OnDeleteString;
    property OnAddString;
    property OnVerticalScroll;
    property OnHorizontalScroll;
  end;

  TJvImgBtnLayout = (blImageLeft, blImageRight);

  TJvImgBtnKind = (bkCustom, bkOK, bkCancel, bkHelp, bkYes, bkNo, bkClose,
    bkAbort, bkRetry, bkIgnore, bkAll);

  TJvImgBtn = class;

  TJvImgBtnActionLink = class(TButtonActionLink)
  protected
    FClient: TJvImgBtn;
    procedure AssignClient(AClient: TObject); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  TJvImgBtnDrawEvent = procedure(Sender: TObject; const DrawItemStruct: TDrawItemStruct) of object;
  TJvImgBtnAnimIndexEvent = procedure(Sender: TObject; CurrentAnimateFrame: Byte;
    var ImageIndex: Integer) of object;

  TJvImgBtn = class(TButton)
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
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FImageVisible: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
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
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CalcButtonParts(ButtonRect: TRect; var RectText, RectImage: TRect);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); dynamic;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetCustomCaption: string; dynamic;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetKindImageIndex: Integer;
    function GetRealCaption: string;
    procedure InvalidateImage;
    function IsImageVisible: Boolean;
    procedure Loaded; override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure ShowNextFrame;
    procedure StartAnimate;
    procedure StopAnimate;
    procedure RestartAnimate;
    class procedure InitializeDefaultImageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure DrawButtonImage(ImageBounds: TRect);
    procedure DrawButtonFocusRect;
    procedure DrawButtonFrame(const DrawItemStruct: TDrawItemStruct);
    procedure DrawButtonText(TextBounds: TRect; TextEnabled: Boolean);
    property Canvas: TCanvas read FCanvas;
    property CurrentAnimateFrame: Byte read FCurrentAnimateFrame;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
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
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

uses
  Math, Consts,
  JclSysUtils, JvFunctions;

{$R *.res}

resourcestring
  RsLBVirtualCantBeSorted = 'ListBox doesn''t allow sorting in virtual mode';

const
  JvImgBtnLineSeparator = '|';
  JvImgBtnModalResults: array[TJvImgBtnKind] of TModalResult =
  (mrNone, mrOk, mrCancel, mrNone, mrYes, mrNo, mrNone, mrAbort, mrRetry,
    mrIgnore, mrAll);

  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

var
  DefaultImgBtnImagesList: TImageList = nil;

{ TJvImgBtnActionLink }

procedure TJvImgBtnActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TJvImgBtn;
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

{ TJvImgBtn }

procedure TJvImgBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
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

procedure TJvImgBtn.CalcButtonParts(ButtonRect: TRect; var RectText, RectImage: TRect);
var
  BlockWidth, ButtonWidth, ButtonHeight, BlockMargin, InternalSpacing: Integer;
begin
  SetRect(RectText, 0, 0, 0, 0);
  //  RectText.Right := ButtonRect.Right - ButtonRect.Left;
  DrawText(Canvas.Handle, PChar(GetRealCaption), -1, RectText, DT_CALCRECT or
    Alignments[FAlignment]);
  if IsImageVisible then
  begin
    with GetImageList do
      SetRect(RectImage, 0, 0, Width, Height);
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

procedure TJvImgBtn.Click;
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
          inherited;
      end;
    bkHelp:
      begin
        Control := Self;
        while (Control <> nil) and (Control.HelpContext = 0) do
          Control := Control.Parent;
        if Control <> nil then
          Application.HelpContext(Control.HelpContext)
        else
          inherited;
      end;
  else
    inherited;
  end;
end;

procedure TJvImgBtn.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvImgBtn.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvImgBtn.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    DoMouseEnter;
  end;
end;

procedure TJvImgBtn.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled and not Dragging then
  begin
    FMouseInControl := False;
    DoMouseLeave;
  end;
end;

procedure TJvImgBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  FCanvas.Handle := Message.DrawItemStruct^.hDC;
  try
    FCanvas.Font := Font;
    if FOwnerDraw and Assigned(FOnButtonDraw) then
      FOnButtonDraw(Self, Message.DrawItemStruct^)
    else
      DrawItem(Message.DrawItemStruct^);
  finally
    FCanvas.Handle := 0;
  end;
end;

procedure TJvImgBtn.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

constructor TJvImgBtn.Create(AOwner: TComponent);
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
end;

procedure TJvImgBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or BS_OWNERDRAW;
end;

procedure TJvImgBtn.CreateWnd;
begin
  inherited;
  if FAnimate then
    StartAnimate;
end;

destructor TJvImgBtn.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TJvImgBtn.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvImgBtn.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvImgBtn.DrawButtonFocusRect;
var
  R: TRect;
begin
  if FIsFocused then
  begin
    R := ClientRect;
    InflateRect(R, -4, -4);
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Brush.Color := clBtnFace;
    DrawFocusRect(FCanvas.Handle, R);
  end;
end;

procedure TJvImgBtn.DrawButtonFrame(const DrawItemStruct: TDrawItemStruct);
var
  IsDown, IsEnabled, IsDefault: Boolean;
  R: TRect;
  Flags: DWORD;
begin
  R := ClientRect;
  with DrawItemStruct do
  begin
    IsEnabled := itemState and ODS_DISABLED = 0;
    IsDown := (itemState and ODS_SELECTED <> 0) and IsEnabled;
    IsDefault := itemState and ODS_FOCUS <> 0;
  end;

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
end;

procedure TJvImgBtn.DrawButtonImage(ImageBounds: TRect);
begin
  with ImageBounds do
    if IsImageVisible then
    begin
      if Assigned(FImages) then
        FImages.Draw(FCanvas, Left, Top, GetImageIndex, Enabled)
      else
        DefaultImgBtnImagesList.Draw(FCanvas, Left, Top, GetKindImageIndex, Enabled);
    end;
end;

procedure TJvImgBtn.DrawButtonText(TextBounds: TRect; TextEnabled: Boolean);
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
      DrawText(Handle, PChar(RealCaption), Length(RealCaption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(RealCaption), Length(RealCaption), TextBounds, Flags);
    end
    else
      DrawText(Handle, PChar(RealCaption), Length(RealCaption), TextBounds, Flags);
  end;
end;

procedure TJvImgBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  R, RectText, RectImage: TRect;
begin
  DrawButtonFrame(DrawItemStruct);

  R := ClientRect;
  InflateRect(R, -4, -4);
  if (DrawItemStruct.itemState and ODS_SELECTED <> 0) and Enabled then
    OffsetRect(R, 1, 1);

  CalcButtonParts(R, RectText, RectImage);
  DrawButtonText(RectText, Enabled);
  DrawButtonImage(RectImage);

  DrawButtonFocusRect;
end;

function TJvImgBtn.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvImgBtnActionLink;
end;

function TJvImgBtn.GetCustomCaption: string;
const
  Captions: array[TJvImgBtnKind] of string =
  ('', SOKButton, SCancelButton, SHelpButton, SYesButton, SNoButton,
    SCloseButton, SAbortButton, SRetryButton, SIgnoreButton,
    SAllButton);
begin
  Result := Captions[FKind];
end;

function TJvImgBtn.GetImageIndex: Integer;
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

function TJvImgBtn.GetImageList: TCustomImageList;
begin
  if Assigned(FImages) then
    Result := FImages
  else
    Result := DefaultImgBtnImagesList;
end;

function TJvImgBtn.GetKindImageIndex: Integer;
const
  ImageKindIndexes: array[TJvImgBtnKind] of Integer =
  (-1, 2, 4, 0, 3, 1, 5, 8, 6, 9, 7);
begin
  Result := ImageKindIndexes[FKind];
end;

function TJvImgBtn.GetRealCaption: string;
begin
  if (FKind <> bkCustom) and (Caption = '') then
    Result := GetCustomCaption
  else
    Result := StringReplace(Caption, JvImgBtnLineSeparator, #10, [rfReplaceAll]);
end;

procedure TJvImgBtn.ImageListChange(Sender: TObject);
begin
  InvalidateImage;
end;

class procedure TJvImgBtn.InitializeDefaultImageList;
begin
  if not Assigned(DefaultImgBtnImagesList) then
  begin
    DefaultImgBtnImagesList := TImageList.CreateSize(18, 18);
    DefaultImgBtnImagesList.ResourceLoad(rtBitmap, 'JVIMGBTNDEFAULT', clOlive);
  end;
end;

procedure TJvImgBtn.InvalidateImage;
begin
  Invalidate;
end;

function TJvImgBtn.IsImageVisible: Boolean;
begin
  Result := FImageVisible and
    ((Assigned(FImages) and (GetImageIndex <> -1)) or
    (not Assigned(FImages) and (FKind <> bkCustom)));
end;

procedure TJvImgBtn.Loaded;
begin
  inherited;
  if FAnimate then
    StartAnimate;
end;

procedure TJvImgBtn.RestartAnimate;
begin
  if FAnimating then
  begin
    StopAnimate;
    StartAnimate;
    InvalidateImage;
  end;
end;

procedure TJvImgBtn.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvImgBtn.SetAnimate(const Value: Boolean);
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

procedure TJvImgBtn.SetAnimateFrames(const Value: Integer);
begin
  if FAnimateFrames <> Value then
  begin
    FAnimateFrames := Value;
    RestartAnimate;
  end;
end;

procedure TJvImgBtn.SetAnimateInterval(const Value: Cardinal);
begin
  if FAnimateInterval <> Value then
  begin
    FAnimateInterval := Value;
    RestartAnimate;
  end;
end;

procedure TJvImgBtn.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> FIsFocused then
  begin
    FIsFocused := ADefault;
    Refresh;
  end;
end;

procedure TJvImgBtn.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    InvalidateImage;
  end;
end;

procedure TJvImgBtn.SetImages(const Value: TCustomImageList);
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

procedure TJvImgBtn.SetImageVisible(const Value: Boolean);
begin
  if FImageVisible <> Value then
  begin
    FImageVisible := Value;
    Invalidate;
  end;
end;

procedure TJvImgBtn.SetKind(const Value: TJvImgBtnKind);
begin
  if FKind <> Value then
  begin
    if Value <> bkCustom then
    begin
      InitializeDefaultImageList;
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

procedure TJvImgBtn.SetLayout(const Value: TJvImgBtnLayout);
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

procedure TJvImgBtn.SetMargin(const Value: Integer);
begin
  if (FMargin <> Value) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TJvImgBtn.SetOwnerDraw(const Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    Invalidate;
  end;
end;

procedure TJvImgBtn.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TJvImgBtn.ShowNextFrame;
begin
  Inc(FCurrentAnimateFrame);
  if FCurrentAnimateFrame > FAnimateFrames then
    FCurrentAnimateFrame := 1;
  InvalidateImage;
end;

procedure TJvImgBtn.StartAnimate;
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

procedure TJvImgBtn.StopAnimate;
begin
  if FAnimating then
  begin
    KillTimer(Handle, 1);
    FCurrentAnimateFrame := 0;
    DoubleBuffered := False;
    FAnimating := False;
  end;
end;

procedure TJvImgBtn.WMDestroy(var Message: TWMDestroy);
begin
  StopAnimate;
  inherited;
end;

procedure TJvImgBtn.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

procedure TJvImgBtn.WMTimer(var Message: TWMTimer);
begin
  if Message.TimerID = 1 then
  begin
    ShowNextFrame;
    Message.Result := 1;
  end
  else
    inherited;
end;

initialization

finalization
  FreeAndNil(DefaultImgBtnImagesList);

end.

