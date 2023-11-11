{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOfficeColorButton.PAS, released on 2004-02-26.

The Initial Developer of the Original Code is dejoy [dejoy att ynl dott gov dott cn]
All Rights Reserved.

Contributor(s):dejoy.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A office color selection button look like Microsoft office Color picker, make to customable Highly.

Known Issues:

-----------------------------------------------------------------------------}
// $Id$

unit JvOfficeColorButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  JvConsts, JvTypes, JvExControls, JvExtComponent, JvSpeedButton,
  JvHotTrackPersistent,
  JvOfficeColorForm, JvOfficeColorPanel, JvOfficeDragBarForm;

const
  MinArrowWidth = 9 + 4;
  MinButtonHeight = 22;
  MinButtonWidth = 22;
  Tag_ArrowWidth = 11;

type
  TJvOfficeColorButtonProperties = class(TJvOfficeColorPanelProperties)
  private
    FShowDragBar: Boolean;
    FFloatWindowCaption: string;
    FEdgeWidth: Integer;
    FArrowWidth: Integer;
    FDragBarHeight: Integer;
    FDragBarSpace: Integer;
    FDragBarHint: string;
    FFilerTag:string;
    procedure SetShowDragBar(const Value: Boolean);
    procedure SetFloatWindowCaption(const Value: string);
    procedure SetArrowWidth(const Value: Integer);
    procedure SetEdgeWidth(const Value: Integer);
    procedure SetDragBarHeight(const Value: Integer);
    procedure SetDragBarSpace(const Value: Integer);
    procedure SetDragBarHint(const Value: string);
    procedure ReadData(Reader: TReader);
  protected
    procedure CreateDefaultText; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property EdgeWidth: Integer read FEdgeWidth write SetEdgeWidth default 4;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default MinArrowWidth;
    property ShowDragBar: Boolean read FShowDragBar write SetShowDragBar default True;
    property FloatWindowCaption: string read FFloatWindowCaption write SetFloatWindowCaption;
    property DragBarHint: string read FDragBarHint write SetDragBarHint;
    property DragBarHeight: Integer read FDragBarHeight write SetDragBarHeight default MinDragBarHeight;
    property DragBarSpace: Integer read FDragBarSpace write SetDragBarSpace default MinDragBarSpace;
  end;

  TJvOfficeColorButtonHotTrackOptions = TJvHotTrackOptions;
  TJvCustomOfficeColorButton = class(TJvCustomPanel, IJvHotTrack)
  private
    FMainButton: TJvColorSpeedButton;
    FArrowButton: TJvSpeedButton;
    FColorsForm: TJvOfficeColorForm;
    FProperties: TJvOfficeColorButtonProperties;
    FFlat: Boolean;
    FColorFormDropDown: Boolean;
    FFloatingBgColor: TColor;
    FDropingBgColor: TColor;
    FInited: Boolean;
    FColorsListChanged:Boolean;
    FOnColorChange: TNotifyEvent;
    FOnHoldCustomColor: TJvHoldCustomColorEvent;
    FOnDropDown: TNotifyEvent;
    FOnColorButtonClick: TNotifyEvent;
    FOnShowOwnerColorDialog: TNotifyEvent;
    FOnArrowClick: TNotifyEvent;
    FHotTrack: Boolean;
    FHotTrackOptions: TJvOfficeColorButtonHotTrackOptions;
    procedure SetFlat(const Value: Boolean);
    // Set Control Color
    procedure SetControlBgColor(const Value: TColor);
    function GetControlBgColor: TColor;
    //Set DropDown form Bg Color
    function GetDropingBgColor: TColor;
    procedure SetDropingBgColor(const Value: TColor);
    //Set floating DropDown form Bg Color
    procedure SetFloatingBgColor(const Value: TColor);
    procedure SetSelectedColor(const Value: TColor);
    function GetSelectedColor: TColor;
    function GetColorDlgCustomColors: TStrings;
    procedure SetColorDlgCustomColors(const Value: TStrings);
    function GetGlyph: TBitmap;
    procedure SetGlyph(const Value: TBitmap);
    function GetProperties: TJvOfficeColorButtonProperties;
    procedure SetProperties(const Value: TJvOfficeColorButtonProperties);
    function GetColorDialogOptions: TColorDialogOptions;
    procedure SetColorDialogOptions(const Value: TColorDialogOptions);

    {IJvHotTrack}
    function GetHotTrack: Boolean;
    function GetHotTrackOptions: TJvOfficeColorButtonHotTrackOptions;
    function GetHotTrackFont: TFont;
    function GetHotTrackFontOptions: TJvTrackFontOptions;

    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackFont(Value: TFont);
    procedure SetHotTrackFontOptions(Value: TJvTrackFontOptions);
    procedure SetHotTrackOptions(Value: TJvOfficeColorButtonHotTrackOptions);
    procedure IJvHotTrack_Assign(Source: IJvHotTrack);
    procedure IJvHotTrack.Assign = IJvHotTrack_Assign;

    function GetStandardColors: TStringList;
    function GetSystemColors: TStringList;
    function GetUserColors: TStringList;
    procedure SetStandardColors(const Value: TStringList);
    procedure SetSystemColors(const Value: TStringList);
    procedure SetUserColors(const Value: TStringList);
    function GetStandardColorDrawers: TList;
    function GetSystemColorDrawers: TList;
    function GetUserColorDrawers: TList;
    function GetColorPanel: TJvOfficeColorPanel;
    function GetButtonOfDefaultColor: TJvColorSpeedButton;
    function GetButtonOfNoneColor: TJvColorSpeedButton;
    function GetButtonOfCustomColor: TJvColorSpeedButton;
    function GetClickColorType: TJvClickColorType;
    procedure DoHotTrackOptionsChanged(Sender: TObject);
    procedure DoFormShowingChanged(Sender: TObject);
//    procedure DoFormKillFocus(Sender: TObject);
    procedure DoFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoFormWindowStyleChanged(Sender: TObject);
    procedure DoButtonMouseEnter(Sender: TObject);
    procedure DoButtonMouseLeave(Sender: TObject);
    procedure DoArrowButtonClick(Sender: TObject);
    procedure DoOnColorChange(Sender: TObject);
    procedure DoColorButtonClick(Sender: TObject);
    procedure DoClick(Sender: TObject);
    procedure DoColorsListChanged(Sender: TObject);
    procedure DoHoldedCustomColor(Sender: TObject;AColor: TColor);
    function GetAddInControls: TList;
    procedure AdjustChildBounds;
  protected
    procedure CMPopupCloseUp(var Msg: TMessage); message CM_POPUPCLOSEUP;
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure PopupCloseUp; virtual;
    procedure FocusKilled(NextWnd: THandle); override;

    procedure AdjustColorForm(X: Integer = 0; Y: Integer = 0); //Screen position
    procedure ShowColorForm(X: Integer = 0; Y: Integer = 0); virtual; //Screen position
    class function GetColorsPanelClass: TJvOfficeColorPanelClass;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure SetEnabled( Value: Boolean); override;
    procedure FontChanged; override;
    procedure Resize; override;

    function CreateStandardColors(ColorList: TStrings): Integer; virtual;
    function CreateSystemColors(ColorList: TStrings): Integer; virtual;
    //if you wnat to create published color list by default,override this procedure.
    function CreateUserColors(ColorList: TStrings): Integer; virtual;

    procedure DoPropertiesChanged(Sender: TObject; const PropName: string); virtual;

    //Do't change the following list, The result might unpredictability.
    property StandardColorDrawers: TList read GetStandardColorDrawers;
    property SystemColorDrawers: TList read GetSystemColorDrawers;
    property UserColorDrawers: TList read GetUserColorDrawers;
    property AddInControls: TList read GetAddInControls;

    property ButtonNoneColor: TJvColorSpeedButton read GetButtonOfNoneColor;
    property ButtonDefaultColor: TJvColorSpeedButton read GetButtonOfDefaultColor;
    property ButtonCustomColor: TJvColorSpeedButton read GetButtonOfCustomColor;

    property ColorPanel: TJvOfficeColorPanel read GetColorPanel;
    property ColorsForm: TJvOfficeColorForm read FColorsForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateStandardColorDrawers; virtual;
    procedure CreateSystemColorDrawers; virtual;
    procedure CreateUserColorDrawers; virtual;
    procedure RearrangeControls; virtual;
    procedure RefreshControls;

    procedure AdjustSize; override;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Color: TColor read GetControlBgColor write SetControlBgColor default clBtnFace;
    property BackColor: TColor read GetControlBgColor write SetControlBgColor default clBtnFace;
    property DropingBgColor: TColor read GetDropingBgColor write SetDropingBgColor default clBtnFace;
    property FloatingBgColor: TColor read FFloatingBgColor write SetFloatingBgColor default clBtnFace;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor default clBlack; // COLOR SELECTED IN THE BUTTON.
    property ColorDlgCustomColors: TStrings read GetColorDlgCustomColors write SetColorDlgCustomColors;
    property ClickColorType: TJvClickColorType read GetClickColorType ;
    property Properties: TJvOfficeColorButtonProperties read GetProperties write SetProperties;
    property ColorDialogOptions: TColorDialogOptions read GetColorDialogOptions write SetColorDialogOptions default [];
    property StandardColors: TStringList read GetStandardColors write SetStandardColors;
    property SystemColors: TStringList read GetSystemColors write SetSystemColors;
    property UserColors: TStringList read GetUserColors write SetUserColors;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read GetHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions;
    property HotTrackOptions: TJvOfficeColorButtonHotTrackOptions read GetHotTrackOptions write SetHotTrackOptions;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnArrowClick: TNotifyEvent read FOnArrowClick write FOnArrowClick;
    property OnColorButtonClick: TNotifyEvent read FOnColorButtonClick write FOnColorButtonClick;
    property OnHoldCustomColor: TJvHoldCustomColorEvent read FOnHoldCustomColor write
      FOnHoldCustomColor;
    property OnShowOwnerColorDialog: TNotifyEvent read FOnShowOwnerColorDialog write FOnShowOwnerColorDialog;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvOfficeColorButton = class(TJvCustomOfficeColorButton)
  private
    FFilerTag: string;
    procedure ReadData(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    // UserColors must be load fist on read from the DFM file.
    property UserColors;
    property ColorDialogOptions;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Flat;
    property BackColor;  // basic Control bg color.
    property DropingBgColor;// DropDown Color panel bg Color.
    property FloatingBgColor; //floating DropDown Color panel bg Color.
    property SelectedColor;

    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;
    property HotTrackOptions;
    property ColorDlgCustomColors;
    property Glyph;
    property Properties;
    property OnConstrainedResize;
    property OnResize;
    property OnDropDown;
    property OnArrowClick;
    property OnColorChange;
    property OnColorButtonClick;

//if OnShowOwnerColorDialog not nil,the default ColorDialog will not show,
//so you can show coustom ColorDialog yourself.
    property OnShowOwnerColorDialog;
    property OnClick;

    property Action;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Types, // expand inlines
  JvResources, JvJVCLUtils, JvThemes, JvComponent;

const
  cArrowWidth = 'ArrowWidth';
  cDragBarHeight = 'DragBarHeight';
  cDragBarHint = 'DragBarHint';
  cDragBarSpace = 'DragBarSpace';
  cFloatWindowCaption = 'FloatWindowCaption';
  cEdgeWidth = 'EdgeWidth';
  cShowDragBar = 'ShowDragBar';

type
  TColorSpeedButtonAccessProtected = class(TJvColorSpeedButton);
  TJvOfficeColorFormAccessProtected = class(TJvOfficeColorForm);
  TJvOfficeColorPanelAccessProtected = class(TJvOfficeColorPanel);

//=== { TJvColorArrowButton } ================================================

type
  TArrowColor = record
    Enabled: TColor;
    Disabled: TColor;
  end;

  TJvColorArrowButton = class(TJvSpeedButton)
  private
    FArrowColor: TArrowColor;
    FArrowDirection: TAnchorKind;
    procedure SetArrowColor(const Value: TArrowColor);
    procedure SetArrowDirection(const Value: TAnchorKind);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ArrowColor: TArrowColor read FArrowColor write SetArrowColor;
    property ArrowDirection: TAnchorKind read FArrowDirection write SetArrowDirection default akBottom;
  end;

constructor TJvColorArrowButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArrowDirection := akBottom;
  FArrowColor.Enabled := clBlack;
  FArrowColor.Disabled := clBtnShadow;
end;

procedure TJvColorArrowButton.SetArrowDirection(const Value: TAnchorKind);
begin
  if FArrowDirection <> Value then
  begin
    FArrowDirection := Value;
    Repaint;
  end;
end;

procedure TJvColorArrowButton.SetArrowColor(const Value: TArrowColor);
begin
  if (FArrowColor.Enabled <> Value.Enabled) and (FArrowColor.Disabled <> Value.Disabled) then
  begin
    FArrowColor := Value;
    Repaint;
  end;
end;

procedure TJvColorArrowButton.Paint;
var
  PaintRect: TRect;
begin
  inherited Paint;

  { calculate were to put arrow part }
  PaintRect := Rect(3, 0, Width - 3, Height);
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
    Dec(PaintRect.Left);
  {$ENDIF JVCLThemesEnabled}

  { Draw arrow }
  if Enabled then
    DrawArrow(Canvas, PaintRect, ArrowColor.Enabled, ArrowDirection)
  else
    DrawArrow(Canvas, PaintRect, ArrowColor.Disabled, ArrowDirection);
end;

//=== { TJvColorMainButton } =================================================

type
  TJvColorMainButton = class(TJvColorSpeedButton)
  protected
    function GetEdgeWidth: Integer; override;
  end;

function TJvColorMainButton.GetEdgeWidth: Integer;
begin
  Result := FEdgeWidth;
end;

//=== { TJvCustomOfficeColorButton } =========================================

constructor TJvCustomOfficeColorButton.Create(AOwner: TComponent);
var
  LArrowColor: TArrowColor;
  LOwner: TComponent;
  LColorsChanged: Boolean;
begin
  inherited Create(AOwner);
  FInited := False;
  FHotTrack := False;
  FFloatingBgColor := clBtnFace;
  FDropingBgColor := clBtnFace;
  TabStop := True;

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  BevelOuter := bvNone;
  Locked := True;
  Width := MinButtonWidth + MinArrowWidth;
  Height := MinButtonHeight;

  FHotTrackOptions := TJvOfficeColorButtonHotTrackOptions.Create(Self);
  FHotTrackOptions.OnChanged := DoHotTrackOptionsChanged;

  FMainButton := TJvColorMainButton.Create(Self);
  with FMainButton do
  begin
    Parent := Self;
    NumGlyphs := 2;
    DrawColor := clBlack;
    Tag := StandardColCount + 3;
    OnClick := DoClick;
  end;

  FArrowButton := TJvColorArrowButton.Create(Self);
  with TJvColorArrowButton(FArrowButton) do
  begin
    Parent := Self;
    GroupIndex := 2;
    AllowAllUp := True;
    Tag := StandardColCount + 4;
    ArrowDirection := akBottom;
    LArrowColor.Enabled := clBlack;
    LArrowColor.Disabled := clBtnShadow;
    ArrowColor := LArrowColor;
    OnClick := DoArrowButtonClick;
  end;

  FColorsForm := TJvOfficeColorForm.Create(Self, GetColorsPanelClass);
  with TJvOfficeColorFormAccessProtected(FColorsForm) do
  begin
    IsFocusable := False;
    FormStyle := fsStayOnTop;
    ToolWindowStyle := False;
    OnShowingChanged := DoFormShowingChanged;
//    OnKillFocus := DoFormKillFocus;
    OnClose := DoFormClose;
    OnWindowStyleChanged := DoFormWindowStyleChanged;

    ColorPanel.OnColorChange := DoOnColorChange;
    ColorPanel.OnColorButtonClick := DoColorButtonClick;
    ColorPanel.OnHoldCustomColor := DoHoldedCustomColor;
  end;

  FProperties := TJvOfficeColorButtonProperties.Create(Self);
  FProperties.Assign(ColorPanel.Properties);
  FProperties.OnChangedProperty := DoPropertiesChanged;

  FColorsListChanged:= False;
  StandardColors.BeginUpdate;
  StandardColors.OnChange := DoColorsListChanged;
  try
    CreateStandardColors(StandardColors);
  finally
    StandardColors.EndUpdate;
    StandardColors.OnChange := nil;
  end;
  if FColorsListChanged then // Changed the colors value, recreate buttons
    CreateStandardColorDrawers;
  LColorsChanged := FColorsListChanged;

  FColorsListChanged:= False;
  SystemColors.BeginUpdate;
  SystemColors.OnChange := DoColorsListChanged;
  try
    CreateSystemColors(SystemColors);
  finally
    SystemColors.EndUpdate;
    SystemColors.OnChange := nil;
  end;
  if FColorsListChanged then //Changed the colors value,recreate buttons
    CreateSystemColorDrawers;
  LColorsChanged := LColorsChanged or FColorsListChanged;

  LOwner := GetTopOwner(Self);
  // make sure that if this is not loading from DFM file or stream.
  if (LOwner <> nil) and (LOwner.ComponentState * [csReading, csLoading] = []) then
  begin
    FColorsListChanged := False;
    UserColors.BeginUpdate;
    UserColors.OnChange := DoColorsListChanged;
    UserColors.EndUpdate;

    CreateUserColors(UserColors);
    UserColors.OnChange := nil;
    if FColorsListChanged then // Changed the colors value, recreate buttons
      CreateUserColorDrawers;
    LColorsChanged := LColorsChanged or FColorsListChanged;
    FProperties.CreateDefaultText;
  end;
  FColorsListChanged := False;

  if LColorsChanged then //StandardColors or SystemColors Or userColors changed.
    RearrangeControls;

  //Font.Name := 'MS Shell Dlg 2';
  Flat := True;
  FMainButton.OnMouseEnter := DoButtonMouseEnter;
  FArrowButton.OnMouseEnter := DoButtonMouseEnter;
  FMainButton.OnMouseLeave := DoButtonMouseLeave;
  FArrowButton.OnMouseLeave := DoButtonMouseLeave;

  FInited := True;
end;

destructor TJvCustomOfficeColorButton.Destroy;
begin
  if FColorsForm.Visible then
  begin
    with TJvOfficeColorFormAccessProtected(FColorsForm) do
    begin
      OnShowingChanged := nil;
      OnKillFocus := nil;
      OnClose := nil;
      OnWindowStyleChanged := nil;

      ColorPanel.OnColorChange := nil;
      ColorPanel.OnColorButtonClick := nil;
      Hide;
    end;
  end;
  FreeAndNil(FProperties);
  FHotTrackOptions.Free;
  inherited Destroy;
end;

procedure TJvCustomOfficeColorButton.Resize;
begin
  AdjustChildBounds;
  inherited Resize;
end;

procedure TJvCustomOfficeColorButton.AdjustChildBounds;
begin
  with Properties do
  begin
    if ArrowWidth < MinArrowWidth then
      ArrowWidth := MinArrowWidth;
    if (Width - ArrowWidth) < MinButtonWidth then
      Width := MinButtonWidth + ArrowWidth;
    if Height < MinButtonHeight then
      Height := MinButtonHeight;

    FMainButton.SetBounds(0, 0, Width - FArrowWidth, Height);

    FArrowButton.SetBounds(FMainButton.Width, 0, ArrowWidth, Height);
  end;
end;

procedure TJvCustomOfficeColorButton.AdjustSize;
begin
  if FInited then
    AdjustChildBounds;
  inherited AdjustSize;
end;

class function TJvCustomOfficeColorButton.GetColorsPanelClass: TJvOfficeColorPanelClass;
begin
  Result := TJvOfficeColorPanel;
end;

procedure TJvCustomOfficeColorButton.CreateWnd;
begin
  inherited CreateWnd;
  AdjustSize;
end;

type
  TControlAccessProtected = class(TControl);

procedure TJvCustomOfficeColorButton.Loaded;
begin
  inherited Loaded;
  TControlAccessProtected(ColorsForm.ColorPanel).Loaded;
end;

procedure TJvCustomOfficeColorButton.SetEnabled( Value: Boolean);
begin
  inherited SetEnabled(Value);
  FMainButton.Enabled := Value;
  FArrowButton.Enabled := Value;
  ColorPanel.Enabled := Value;
end;

procedure TJvCustomOfficeColorButton.FontChanged;
begin
  inherited FontChanged;
  FColorsForm.Font.Assign(Font);
end;

function TJvCustomOfficeColorButton.CreateStandardColors(ColorList: TStrings): Integer;
begin
  Result := ColorList.Count;
  { Because in ColorPanel the same name function has inited, so do nothing here.
    If you want to change ColorList,make sure call Strings.BeginUpdate before change,
    and call Strings.EndUpdate after changed. }
end;

function TJvCustomOfficeColorButton.CreateSystemColors(ColorList: TStrings): Integer;
begin
  Result := ColorList.Count;
  { Because in ColorPanel the same name function has inited, so do nothing here.
    If you want to change ColorList, make sure call strings.BeginUpdate before change,
    and call Strings.EndUpdate after changed. }
end;

function TJvCustomOfficeColorButton.CreateUserColors(ColorList: TStrings): Integer;
begin
  Result := ColorList.Count;
  { Because in ColorPanel the same name function has inited, so do nothing here.
    If you want to change ColorList, make sure call strings.BeginUpdate before change,
    and call Strings.EndUpdate after changed. }
end;

procedure TJvCustomOfficeColorButton.RearrangeControls;
begin
  ColorPanel.RearrangeControls;
  AdjustSize;
end;

procedure TJvCustomOfficeColorButton.CreateStandardColorDrawers;
begin
  ColorPanel.CreateStandardColorDrawers;
end;

procedure TJvCustomOfficeColorButton.CreateSystemColorDrawers;
begin
  ColorPanel.CreateSystemColorDrawers;
end;

procedure TJvCustomOfficeColorButton.CreateUserColorDrawers;
begin
  ColorPanel.CreateUserColorDrawers;
end;

procedure TJvCustomOfficeColorButton.RefreshControls;
begin
  if Properties.ShowStandardColors then
    CreateStandardColorDrawers;
  if Properties.ShowSystemColors then
    CreateSystemColorDrawers;
  if Properties.ShowUserColors then
    CreateUserColorDrawers;
  RearrangeControls;
end;

procedure TJvCustomOfficeColorButton.DoArrowButtonClick(Sender: TObject);
begin
  SetFocus;
  if Sender = FArrowButton then
  begin
    if FColorsForm.Visible or FColorFormDropDown then
      PopupCloseUp
    else
    begin
      if Assigned(FOnDropDown) then
        FOnDropDown(Self);
      ShowColorForm;
      FColorFormDropDown := True;
    end
  end
  else
  begin
    TJvColorSpeedButton(Sender).Down := True;
    SetSelectedColor(TJvColorSpeedButton(Sender).DrawColor);
  end;
  if Assigned(FOnArrowClick) then
    FOnArrowClick(Self);
end;

procedure TJvCustomOfficeColorButton.DoColorButtonClick(Sender: TObject);
begin
  if Assigned(FOnColorButtonClick) then
    FOnColorButtonClick(Sender);

  if (ColorPanel.ClickColorType = cctCustomColor) and
   Assigned(FOnShowOwnerColorDialog) then
    ColorPanel.OnShowOwnerColorDialog := FOnShowOwnerColorDialog
  else
    ColorPanel.OnShowOwnerColorDialog := nil;

  if not FColorsForm.ToolWindowStyle then
  begin
    FColorsForm.Hide;  //Auto hide color form when form not floating
    FColorsForm.ToolWindowStyle := False;
    if FArrowButton.Down then
      FArrowButton.Down := False;
    FColorFormDropDown := False;
  end
  else
  if ColorPanel.ClickColorType = cctCustomColor then
    //set formStyle to fsNormal,or else the ColorFrom will stay on top of system select ColorDialog.
    FColorsForm.FormStyle := fsNormal;
end;

function TJvCustomOfficeColorButton.GetColorDlgCustomColors: TStrings;
begin
  Result := ColorPanel.ColorDlgCustomColors;
end;

function TJvCustomOfficeColorButton.GetSelectedColor: TColor;
begin
  Result := ColorPanel.SelectedColor;
end;

procedure TJvCustomOfficeColorButton.DoHotTrackOptionsChanged(Sender: TObject);
begin
  FMainButton.HotTrackOptions := FHotTrackOptions;
  FArrowButton.HotTrackOptions := FHotTrackOptions;
  ColorPanel.HotTrackOptions := FHotTrackOptions;
end;

procedure TJvCustomOfficeColorButton.DoOnColorChange(Sender: TObject);
begin
  FMainButton.DrawColor := SelectedColor;
  if FColorsForm.ToolWindowStyle and (FColorsForm.FormStyle <> fsStayOnTop) then
    FColorsForm.FormStyle := fsStayOnTop;
  if ColorPanel.ClickColorType = cctCustomColor then
    ColorPanel.OnShowOwnerColorDialog := nil;
  if Assigned(FOnColorChange) then
    FOnColorChange(Self);
end;

procedure TJvCustomOfficeColorButton.SetColorDlgCustomColors(const Value: TStrings);
begin
  ColorPanel.ColorDlgCustomColors.Assign(Value);
end;

procedure TJvCustomOfficeColorButton.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    FMainButton.Flat := Value;
    FArrowButton.Flat := Value;
    FColorsForm.Flat := Value;
  end;
end;

// NEW: Set Control Background Color
procedure TJvCustomOfficeColorButton.SetControlBgColor(const Value: TColor);
begin
  if Value <> FArrowButton.Color then
  begin
    inherited Color := Value;
    FMainButton.Color := Value;
    FArrowButton.Color := Value;
  end;
end;

function TJvCustomOfficeColorButton.GetControlBgColor: TColor;
begin
  Result := FArrowButton.Color;
end;

function TJvCustomOfficeColorButton.GetDropingBgColor: TColor;
begin
  Result := FDropingBgColor;
end;

procedure TJvCustomOfficeColorButton.SetDropingBgColor(const Value: TColor);
begin
  if FDropingBgColor <> Value then
  begin
    FDropingBgColor := Value;
    if not ColorsForm.ToolWindowStyle then
    begin
      FColorsForm.Color := Value;
      ColorPanel.BackColor := Value;
    end;
  end;
end;

procedure TJvCustomOfficeColorButton.SetFloatingBgColor(const Value: TColor);
begin
  if FFloatingBgColor <> Value then
  begin
    FFloatingBgColor := Value;
    if ColorsForm.ToolWindowStyle then
    begin
      FColorsForm.Color := FloatingBgColor;
      ColorPanel.BackColor := FloatingBgColor;
    end;
  end;
end;

procedure TJvCustomOfficeColorButton.SetSelectedColor(const Value: TColor);
begin
  if ColorPanel.SelectedColor <> Value then
    ColorPanel.SelectedColor := Value;
end;

procedure TJvCustomOfficeColorButton.AdjustColorForm(X: Integer; Y: Integer);
var
  Pt: TPoint;
begin
  if (X = 0) and (Y = 0) then
    Pt := ClientToScreen(Point(FMainButton.Left, FMainButton.Top))
  else
    Pt := Point(X, Y);

  FColorsForm.Left := Pt.X;
  if (FColorsForm.Left + FColorsForm.Width) > Screen.DesktopWidth then
    FColorsForm.Left := Screen.DesktopWidth - FColorsForm.Width;
  FColorsForm.Top := Pt.Y + Height;
  if (FColorsForm.Top + FColorsForm.Height) > Screen.DesktopHeight then
    FColorsForm.Top := Pt.Y - FColorsForm.Height;
end;

procedure TJvCustomOfficeColorButton.ShowColorForm(X: Integer; Y: Integer);
begin
  AdjustColorForm(X, Y);
  FColorsForm.ShowNoActivate(True);
  FColorFormDropDown := True;
end;

procedure TJvCustomOfficeColorButton.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FColorsForm) and
     Assigned(FColorsForm) and not FColorsForm.ContainsControl(Msg.Sender) then
    PopupCloseUp;
end;

procedure TJvCustomOfficeColorButton.CMPopupCloseUp(var Msg: TMessage);
begin
  PopupCloseUp;
end;

procedure TJvCustomOfficeColorButton.PopupCloseUp;
begin
  if Assigned(ColorsForm) and ColorsForm.Visible then
    FColorsForm.Hide;
  FColorFormDropDown := False;
  FArrowButton.Down := False;
end;

procedure TJvCustomOfficeColorButton.DoFormShowingChanged(Sender: TObject);
begin
  if not FColorsForm.Visible and not (csDesigning in ComponentState) then
  begin
    FArrowButton.Down := False;
    TColorSpeedButtonAccessProtected(FArrowButton).MouseLeave(FArrowButton);
    TColorSpeedButtonAccessProtected(FMainButton).MouseLeave(FMainButton);
  end;
end;

{procedure TJvCustomOfficeColorButton.DoFormKillFocus(Sender: TObject);
var
  R: TRect;
  P: TPoint;
begin
  R := FArrowButton.ClientRect;
  GetCursorPos(P);
  P := FArrowButton.ScreenToClient(P);
  if (not FColorsForm.ToolWindowStyle) and (not PtInRect(R, P)) then //mouse in ArrowButton
  begin
    FColorsForm.Hide;
    FColorsForm.ToolWindowStyle := False;
    if FArrowButton.Down then
      FArrowButton.Down := False;
    FColorFormDropDown := False;
  end;
end;}

procedure TJvCustomOfficeColorButton.FocusKilled(NextWnd: THandle);
var
  Sender: TWinControl;
  Focused: Boolean;
begin
  inherited FocusKilled(NextWnd);
  Focused := Screen.ActiveControl <> Self;
  if not Focused then
  begin
    Sender := FindControl(NextWnd);
    if (Sender <> Self) and (Sender <> FColorsForm) and
      Assigned(FColorsForm) and not FColorsForm.ContainsControl(Sender) then
    begin
      { MSDN : While processing this message (WM_KILLFOCUS), do not make any
               function calls that display or activate a window. }
      PostMessage(Handle, CM_POPUPCLOSEUP, 0, 0);
    end;
  end;
end;

procedure TJvCustomOfficeColorButton.DoFormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FColorsForm.ToolWindowStyle then
    FColorFormDropDown := False;
  if csDestroying in ComponentState then
    Action := caFree
  else
    Action := caHide;
end;

procedure TJvCustomOfficeColorButton.DoFormWindowStyleChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  if FColorsForm.ToolWindowStyle then
  begin
    FArrowButton.Down := False;
    ColorsForm.Color := FloatingBgColor;
    ColorPanel.BackColor := FloatingBgColor;
    TColorSpeedButtonAccessProtected(FArrowButton).MouseLeave(FArrowButton);
    TColorSpeedButtonAccessProtected(FMainButton).MouseLeave(FMainButton);
  end
  else
  begin
    ColorsForm.Color := DropingBgColor;
    ColorPanel.BackColor := DropingBgColor;
  end;
end;

procedure TJvCustomOfficeColorButton.DoButtonMouseEnter(Sender: TObject);
begin
  if FFlat and Enabled and not (csDesigning in ComponentState) then
  begin
    TColorSpeedButtonAccessProtected(FMainButton).MouseEnter(FMainButton);
    TColorSpeedButtonAccessProtected(FArrowButton).MouseEnter(FArrowButton);
  end;
end;

procedure TJvCustomOfficeColorButton.DoButtonMouseLeave(Sender: TObject);
begin
  if FFlat and Enabled and not (csDesigning in ComponentState) then
  begin
    if Sender = FMainButton then
    begin
      if FColorsForm.Visible then
        TColorSpeedButtonAccessProtected(FMainButton).MouseEnter(FMainButton)
      else
        TColorSpeedButtonAccessProtected(FArrowButton).MouseLeave(FArrowButton);
    end
    else
    if Sender = FArrowButton then
    begin
      if not FColorsForm.Visible then
        TColorSpeedButtonAccessProtected(FMainButton).MouseLeave(FMainButton)
      else
        TColorSpeedButtonAccessProtected(FArrowButton).MouseEnter(FArrowButton);
    end;
  end;
end;

function TJvCustomOfficeColorButton.GetGlyph: TBitmap;
begin
  Result := FMainButton.Glyph;
end;

procedure TJvCustomOfficeColorButton.SetGlyph(const Value: TBitmap);
begin
  if FMainButton.Glyph <> Value then
    FMainButton.Glyph := Value;
end;

function TJvCustomOfficeColorButton.GetColorDialogOptions: TColorDialogOptions;
begin
  Result := ColorPanel.ColorDialogOptions;
end;

procedure TJvCustomOfficeColorButton.SetColorDialogOptions(const Value: TColorDialogOptions);
begin
  ColorPanel.ColorDialogOptions := Value;
end;

function TJvCustomOfficeColorButton.GetProperties: TJvOfficeColorButtonProperties;
begin
  Result := FProperties;
end;

procedure TJvCustomOfficeColorButton.SetProperties(const Value: TJvOfficeColorButtonProperties);
begin
  if (FProperties <> Value) and (Value <> nil)  then
  begin
    FProperties.Assign(Value);
    ColorPanel.Properties.Assign(Value);
  end;
end;

procedure TJvCustomOfficeColorButton.DoPropertiesChanged(Sender: TObject; const PropName: string);
begin
  if SameText(PropName, cShowDragBar) then
  begin
    if FColorsForm.ShowDragBar <> Properties.ShowDragBar then
      FColorsForm.ShowDragBar := Properties.ShowDragBar;
    if not Properties.ShowDragBar and
      TJvOfficeColorFormAccessProtected(FColorsForm).DropDownMoved then
      AdjustColorForm;
  end
  else
  if SameText(PropName, cFloatWindowCaption) then
    FColorsForm.Caption := Properties.FloatWindowCaption
  else
  if SameText(PropName, cDragBarHeight) then
  begin
    FColorsForm.DragBarHeight := Properties.DragBarHeight;
    AdjustColorForm;
  end
  else
  if SameText(PropName, cDragBarHint) then
    FColorsForm.DragBarHint := Properties.DragBarHint
  else
  if SameText(PropName, cDragBarSpace) then
  begin
    FColorsForm.DragBarSpace := Properties.DragBarSpace;
    AdjustColorForm;
  end
  else
  if SameText(PropName, cArrowWidth) then
    AdjustSize
  else
  if SameText(PropName, cEdgeWidth) then
    FMainButton.EdgeWidth := Properties.EdgeWidth
  else
  begin
    with ColorsForm.ColorPanel,Properties do
    begin
      Assign(Self.Properties);
      OnChangedProperty(Properties,PropName);
    end;

    TJvOfficeColorFormAccessProtected(FColorsForm).AdjustForm;
  end;
end;

procedure TJvCustomOfficeColorButton.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    FMainButton.HotTrack := Value;
    FArrowButton.HotTrack := Value;
    ColorPanel.HotTrack := Value;
  end;
end;

procedure TJvCustomOfficeColorButton.SetHotTrackFont(Value: TFont);
begin
  ColorPanel.HotTrackFont := Value;
end;

function TJvCustomOfficeColorButton.GetHotTrackFont: TFont;
begin
  Result := ColorPanel.HotTrackFont;
end;

procedure TJvCustomOfficeColorButton.SetHotTrackFontOptions(Value: TJvTrackFontOptions);
begin
  ColorPanel.HotTrackFontOptions := Value;
end;

procedure TJvCustomOfficeColorButton.SetHotTrackOptions(Value: TJvOfficeColorButtonHotTrackOptions);
begin
  if FHotTrackOptions <> Value then
  begin
    FHotTrackOptions.Assign(Value);
    FMainButton.HotTrackOptions := Value;
    FArrowButton.HotTrackOptions := Value;
  end;
end;

procedure TJvCustomOfficeColorButton.IJvHotTrack_Assign(Source: IJvHotTrack);
begin
  if (Source <> nil) and (IJvHotTrack(Self) <> Source) then
  begin
    HotTrack := Source.HotTrack;
    HotTrackFont :=Source.HotTrackFont;
    HotTrackFontOptions := Source.HotTrackFontOptions;
    HotTrackOptions := Source.HotTrackOptions;
  end;
end;

function TJvCustomOfficeColorButton.GetHotTrack: Boolean;
begin
  Result := FHotTrack;
end;

function TJvCustomOfficeColorButton.GetHotTrackOptions: TJvOfficeColorButtonHotTrackOptions;
begin
  Result := FHotTrackOptions;
end;

function TJvCustomOfficeColorButton.GetStandardColors: TStringList;
begin
  Result := ColorPanel.StandardColors;
end;

function TJvCustomOfficeColorButton.GetUserColors: TStringList;
begin
  Result := ColorPanel.UserColors;
end;

procedure TJvCustomOfficeColorButton.SetStandardColors(const Value: TStringList);
begin
  ColorPanel.StandardColors := Value;
end;

procedure TJvCustomOfficeColorButton.SetUserColors(const Value: TStringList);
begin
  ColorPanel.UserColors := Value;
end;

function TJvCustomOfficeColorButton.GetStandardColorDrawers: TList;
begin
  Result := TJvOfficeColorPanelAccessProtected(ColorPanel).StandardColorDrawers;
end;

function TJvCustomOfficeColorButton.GetSystemColorDrawers: TList;
begin
  Result := TJvOfficeColorPanelAccessProtected(ColorPanel).SystemColorDrawers;
end;

function TJvCustomOfficeColorButton.GetSystemColors: TStringList;
begin
  Result := ColorPanel.SystemColors;
end;

function TJvCustomOfficeColorButton.GetUserColorDrawers: TList;
begin
  Result := TJvOfficeColorPanelAccessProtected(ColorPanel).UserColorDrawers;
end;

function TJvCustomOfficeColorButton.GetAddInControls: TList;
begin
  Result := TJvOfficeColorPanelAccessProtected(ColorPanel).AddInControls;
end;

procedure TJvCustomOfficeColorButton.SetSystemColors(const Value: TStringList);
begin
  ColorPanel.SystemColors := Value;
end;

function TJvCustomOfficeColorButton.GetHotTrackFontOptions: TJvTrackFontOptions;
begin
  Result := ColorPanel.HotTrackFontOptions;
end;

function TJvCustomOfficeColorButton.GetColorPanel: TJvOfficeColorPanel;
begin
  Result := TJvOfficeColorPanel(ColorsForm.ColorPanel);
end;

function TJvCustomOfficeColorButton.GetButtonOfDefaultColor: TJvColorSpeedButton;
begin
  Result := TJvOfficeColorPanelAccessProtected(ColorPanel).ButtonDefaultColor;
end;

function TJvCustomOfficeColorButton.GetButtonOfNoneColor: TJvColorSpeedButton;
begin
  Result := TJvOfficeColorPanelAccessProtected(ColorPanel).ButtonNoneColor;
end;

function TJvCustomOfficeColorButton.GetButtonOfCustomColor: TJvColorSpeedButton;
begin
  Result := TJvOfficeColorPanelAccessProtected(ColorPanel).ButtonCustomColor;
end;

function TJvCustomOfficeColorButton.GetClickColorType: TJvClickColorType;
begin
  Result := ColorPanel.ClickColorType;
end;

procedure TJvCustomOfficeColorButton.DoClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TJvCustomOfficeColorButton.DoColorsListChanged(Sender: TObject);
begin
  FColorsListChanged := True;
end;

procedure TJvCustomOfficeColorButton.DoHoldedCustomColor(Sender: TObject;AColor: TColor);
begin
  if Assigned(FOnHoldCustomColor) then
    FOnHoldCustomColor(Self, AColor);

  // after hold custom color, realign the form
  if Properties.HoldCustomColor and (ColorPanel.ClickColorType = cctCustomColor) then
    FColorsForm.AdjustForm;
end;

//=== { TJvOfficeColorButtonProperties } =====================================

constructor TJvOfficeColorButtonProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FShowDragBar := True;
  FEdgeWidth := 4;
  FArrowWidth := MinArrowWidth;
  FDragBarHeight := MinDragBarHeight;
  FDragBarSpace := MinDragBarSpace;
  FDragBarHint := RsDragToFloating;
end;

procedure TJvOfficeColorButtonProperties.CreateDefaultText;
begin
  BeginUpdate;
  try
    inherited CreateDefaultText;
    DragBarHint := RsDragToFloat;
    FloatWindowCaption := RsColorWindow;
  finally
    EndUpdate;
  end;
end;

const
  cDragCaption = 'DragCaption';

procedure TJvOfficeColorButtonProperties.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  BeginUpdate;
  try
    FFilerTag := cDragCaption;
    Filer.DefineProperty(FFilerTag, ReadData, nil, False);
  finally
    EndUpdate;
  end;
end;

procedure TJvOfficeColorButtonProperties.ReadData(Reader: TReader);
begin
  if SameText(FFilerTag,cDragCaption) then
    FloatWindowCaption:= Reader.ReadString;
end;

procedure TJvOfficeColorButtonProperties.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    inherited Assign(Source);
    if Source is TJvOfficeColorButtonProperties then
    with TJvOfficeColorButtonProperties(Source) do
    begin
      Self.ShowDragBar := ShowDragBar;
      Self.FloatWindowCaption := FloatWindowCaption;
      Self.EdgeWidth := EdgeWidth;
      Self.ArrowWidth := ArrowWidth;
      Self.DragBarHeight := DragBarHeight;
      Self.DragBarHint := DragBarHint;
      Self.DragBarSpace := DragBarSpace;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvOfficeColorButtonProperties.SetArrowWidth(const Value: Integer);
begin
  if FArrowWidth <> Value then
  begin
    Changing;
    ChangingProperty(cArrowWidth);
    FArrowWidth := Value;
    ChangedProperty(cArrowWidth);
    Changed;
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarHeight(const Value: Integer);
begin
  if FDragBarHeight <> Value then
  begin
    Changing;
    ChangingProperty(cDragBarHeight);
    FDragBarHeight := Value;
    ChangedProperty(cDragBarHeight);
    Changed;
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarSpace(const Value: Integer);
begin
  if FDragBarSpace <> Value then
  begin
    Changing;
    ChangingProperty(cDragBarSpace);
    FDragBarSpace := Value;
    ChangedProperty(cDragBarSpace);
    Changed;
  end;
end;

procedure TJvOfficeColorButtonProperties.SetDragBarHint(const Value: string);
begin
  if FDragBarHint<>Value then
  begin
    Changing;
    ChangingProperty(cDragBarHint);
    FDragBarHint := Value;
    ChangedProperty(cDragBarHint);
    Changed;
  end;
end;

procedure TJvOfficeColorButtonProperties.SetFloatWindowCaption(const Value: string);
begin
  if FFloatWindowCaption <> Value then
  begin
    Changing;
    ChangingProperty(cFloatWindowCaption);
    FFloatWindowCaption := Value;
    ChangedProperty(cFloatWindowCaption);
    Changed;
  end;
end;

procedure TJvOfficeColorButtonProperties.SetEdgeWidth(const Value: Integer);
begin
  if FEdgeWidth <> Value then
  begin
    Changing;
    ChangingProperty(cEdgeWidth);
    FEdgeWidth := Value;
    ChangedProperty(cEdgeWidth);
    Changed;
  end;
end;

procedure TJvOfficeColorButtonProperties.SetShowDragBar(const Value: Boolean);
begin
  if FShowDragBar <> Value then
  begin
    Changing;
    ChangingProperty(cShowDragBar);
    FShowDragBar := Value;
    ChangedProperty(cShowDragBar);
    Changed;
  end;
end;

//=== { TJvOfficeColorButton } ===============================================

procedure TJvOfficeColorButton.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  { For backwards compatibility }
  FFilerTag := 'Color';
  Filer.DefineProperty(FFilerTag, ReadData, nil, False);
  FFilerTag := 'CustomColors';
  Filer.DefineProperty(FFilerTag, ReadData, nil, False);
  FFilerTag := 'Options';
  Filer.DefineProperty(FFilerTag, ReadData, nil, False);
end;

procedure TJvOfficeColorButton.ReadData(Reader: TReader);
begin
  if SameText(FFilerTag, 'Color') then
    BackColor := JvReaderReadColor(Reader)
  else
  if SameText(FFilerTag, 'CustomColors') then
    JvReaderReadStrings(Reader,ColorDlgCustomColors)
  else
  if SameText(FFilerTag, 'Options') then
    ColorDialogOptions := JvReaderReadColorDialogOptions(Reader)
  ;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

