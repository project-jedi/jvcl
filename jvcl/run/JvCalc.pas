{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCalc.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCalc;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Controls, Forms, StdCtrls, Menus, ExtCtrls,
  JvBaseDlg, JvComponent;

const
  DefCalcPrecision = 15;

type
  TJvCalcState = (csFirst, csValid, csError);
  TJvCalculatorForm = class;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvCalculator = class(TJvCommonDialog)
  private
    FValue: Double;
    FMemory: Double;
    FTitle: string;
    FFlat: Boolean;
    FPrecision: Byte;
    FBeepOnError: Boolean;
    FHelpContext: THelpContext;
    FCalc: TJvCalculatorForm;
    FOnChange: TNotifyEvent;
    FOnCalcKey: TKeyPressEvent;
    FOnDisplayChange: TNotifyEvent;
    function GetDisplay: Double;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function TitleStored: Boolean;
    procedure ReadCtl3D(Reader: TReader);
  protected
    procedure Change; dynamic;
    procedure CalcKey(var Key: Char); dynamic;
    procedure DisplayChange; dynamic;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;

    property CalcDisplay: Double read GetDisplay;
    property Memory: Double read FMemory;
  published
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property Precision: Byte read FPrecision write FPrecision default DefCalcPrecision;
    property Title: string read GetTitle write SetTitle stored TitleStored;
    property Value: Double read FValue write FValue;
    property OnCalcKey: TKeyPressEvent read FOnCalcKey write FOnCalcKey;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
    property Flat: Boolean read FFlat write FFlat default False;
  end;

  TJvCalculatorForm = class(TJvForm)
  private
    FMainPanel: TPanel;
    FCalcPanel: TPanel;
    FDisplayPanel: TPanel;
    FDisplayLabel: TLabel;
    FPasteItem: TMenuItem;
    FParentWnd: HWND;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PopupMenuPopup(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure PasteItemClick(Sender: TObject);
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure CalcKey(Sender: TObject; var Key: Char);
    procedure DisplayChange(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AParentWnd: HWND); reintroduce; overload;
  end;

function CreateCalculatorForm(AOwner: TComponent; AHelpContext: THelpContext; AOwnerWnd: HWND): TJvCalculatorForm;
function CreatePopupCalculator(AOwner: TComponent; ABiDiMode: TBiDiMode = bdLeftToRight): TWinControl;
procedure SetupPopupCalculator(PopupCalc: TWinControl; APrecision: Byte; ABeepOnError: Boolean);

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
  Variants, SysUtils, Math, Graphics, Buttons, Clipbrd,
  JvToolEdit, JvSpeedButton, JvExExtCtrls,
  JvJVCLUtils, JvJCLUtils, JvConsts, JvResources, JclSysUtils;

{$R JvCalc.Res} // (ahuser) the filename should be fixed

type
  TCalcBtnKind =
    (cbNone, cbNum0, cbNum1, cbNum2, cbNum3, cbNum4, cbNum5, cbNum6,
    cbNum7, cbNum8, cbNum9, cbSgn, cbDcm, cbDiv, cbMul, cbSub,
    cbAdd, cbSqr, cbPcnt, cbRev, cbEql, cbBck, cbClr, cbMP,
    cbMS, cbMR, cbMC, cbOk, cbCancel);

  TCalcPanelLayout = (clDialog, clPopup);

  TCustomLabelAccessProtected = class(TCustomLabel);

  TJvCalcButton = class(TJvSpeedButton)
  private
    FKind: TCalcBtnKind;
    FFontChanging: Boolean;
  protected
    procedure ParentFontChanged; override;
  public
    constructor CreateKind(AOwner: TComponent; AKind: TCalcBtnKind);
    property Kind: TCalcBtnKind read FKind;
  end;

  TJvCalculatorPanel = class(TJvExPanel)
  private
    FText: string;
    FStatus: TJvCalcState;
    FOperator: Char;
    FOperand: Double;
    FMemory: Double;
    FPrecision: Byte;
    FBeepOnError: Boolean;
    FMemoryPanel: TPanel;
    FMemoryLabel: TLabel;
    FOnError: TNotifyEvent;
    FOnOk: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FOnResult: TNotifyEvent;
    FOnTextChange: TNotifyEvent;
    FOnCalcKey: TKeyPressEvent;
    FOnDisplayChange: TNotifyEvent;
    FControl: TControl;
    procedure SetText(const Value: string);
    procedure CheckFirst;
    procedure CalcKey(Key: Char);
    procedure Clear;
    procedure Error;
    procedure SetDisplay(R: Double);
    function GetDisplay: Double;
    procedure UpdateMemoryLabel;
    function FindButton(Key: Char): TJvSpeedButton;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure BtnClick(Sender: TObject);
  protected
    procedure TextChange; virtual;
  public
    constructor CreateLayout(AOwner: TComponent; ALayout: TCalcPanelLayout);
    procedure CalcKeyPress(Sender: TObject; var Key: Char);
    procedure Copy;
    procedure Paste;
    property DisplayValue: Double read GetDisplay write SetDisplay;
    property Text: string read FText;
    property OnOkClick: TNotifyEvent read FOnOk write FOnOk;
    property OnCancelClick: TNotifyEvent read FOnCancel write FOnCancel;
    property OnResultClick: TNotifyEvent read FOnResult write FOnResult;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property OnCalcKey: TKeyPressEvent read FOnCalcKey write FOnCalcKey;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
  end;

  TJvLocCalculator = class(TJvCalculatorPanel)
  protected
    procedure EnabledChanged; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvPopupCalculator = class(TJvPopupWindow)
  private
    FCalcPanel: TJvLocCalculator;
    procedure TextChange(Sender: TObject);
    procedure ResultClick(Sender: TObject);
  protected
    procedure KeyPress(var Key: Char); override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPopupText: string; override;
  end;

const
  BtnPos: array [TCalcPanelLayout, TCalcBtnKind] of TPoint =
  (((X: - 1; Y: - 1), (X: 47; Y: 104), (X: 47; Y: 80), (X: 85; Y: 80),
    (X: 123; Y: 80), (X: 47; Y: 56), (X: 85; Y: 56), (X: 123; Y: 56),
    (X: 47; Y: 32), (X: 85; Y: 32), (X: 123; Y: 32), (X: 85; Y: 104),
    (X: 123; Y: 104), (X: 161; Y: 32), (X: 161; Y: 56), (X: 161; Y: 80),
    (X: 161; Y: 104), (X: 199; Y: 32), (X: 199; Y: 56), (X: 199; Y: 80),
    (X: 199; Y: 104), (X: 145; Y: 6), (X: 191; Y: 6), (X: 5; Y: 104),
    (X: 5; Y: 80), (X: 5; Y: 56), (X: 5; Y: 32),
    (X: 47; Y: 6), (X: 85; Y: 6)),
    {PopUp}
     {cbNone         cbNum0         cbNum1         cbNum2}
    ((X: - 1; Y: - 1), (X: 6; Y: 75), (X: 6; Y: 52), (X: 29; Y: 52),
    {cbNum3         cbNum4         cbNum5          cbNum6}
    (X: 52; Y: 52), (X: 6; Y: 29), (X: 29; Y: 29), (X: 52; Y: 29),
    {cbNum7       cbNum8         cbNum9         cbSgn}
    (X: 6; Y: 6), (X: 29; Y: 6), (X: 52; Y: 6), (X: 52; Y: 75),
    {cbDcm           cbDiv         cbMul           cbSub}
    (X: 29; Y: 75), (X: 75; Y: 6), (X: 75; Y: 29), (X: 75; Y: 52),
    {cbAdd          cbSqr           cbPcnt          cbRev}
//Polaris    (X: 75; Y: 75), (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1),
    (X: 75; Y: 75), (X: 98; Y: 6), (X: 98; Y: 29), (X: 98; Y: 52),
    {cbEql          cbBck           cbClr          cbMP}
//Polaris    (X: 52; Y: 98), (X: 29; Y: 98), (X: 6; Y: 98), (X: -1; Y: -1),
    (X: 98; Y: 75), (X: 29; Y: 98), (X: 6; Y: 98), (X: - 1; Y: - 1),
    {cbMS           cbMR            cbMC}
    (X: - 1; Y: - 1), (X: - 1; Y: - 1), (X: - 1; Y: - 1),
    {cbOk           cbCancel}
    (X: - 1; Y: - 1), (X: - 1; Y: - 1)));

  {((X: - 1; Y: - 1), (X: 6; Y: 75), (X: 6; Y: 52), (X: 29; Y: 52),
  (X: 52; Y: 52), (X: 6; Y: 29), (X: 29; Y: 29), (X: 52; Y: 29),
  (X: 6; Y: 6), (X: 29; Y: 6), (X: 52; Y: 6), (X: 52; Y: 75),
  (X: 29; Y: 75), (X: 75; Y: 6), (X: 75; Y: 29), (X: 75; Y: 52),
  (X: 75; Y: 75), (X: - 1; Y: - 1), (X: - 1; Y: - 1), (X: - 1; Y: - 1),
  (X: 52; Y: 98), (X: 29; Y: 98), (X: 6; Y: 98), (X: - 1; Y: - 1),
  (X: - 1; Y: - 1), (X: - 1; Y: - 1), (X: - 1; Y: - 1),
  (X: - 1; Y: - 1), (X: - 1; Y: - 1)));}

  ResultKeys = [Cr, '=', '%'];

//=== Local procedures =======================================================

procedure SetDefaultFont(AFont: TFont; Layout: TCalcPanelLayout);

var
  NonClientMetrics: TNonClientMetrics;

begin
  {$IFDEF RTL210_UP}
  NonClientMetrics.cbSize := TNonClientMetrics.SizeOf;
  {$ELSE}
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  {$ENDIF RTL210_UP}
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, NonClientMetrics.cbSize, @NonClientMetrics, 0) then
    AFont.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont)
  else
  begin
    AFont.Color := clWindowText;
    AFont.Name := 'MS Sans Serif';
    AFont.Size := 8;
  end;
  AFont.Style := [fsBold];
  {
  if Layout = clDialog then
  begin
  end
  else
  begin
  end;
  }
end;

function CreateCalcBtn(AParent: TWinControl; AKind: TCalcBtnKind;
  AOnClick: TNotifyEvent; ALayout: TCalcPanelLayout): TJvCalcButton;
const
  BtnCaptions: array [cbSgn..cbMC] of string =
    ('', ',', '/', '*', '-', '+', 'sqrt', '%', '1/x', '=', '<-', 'C',
     'MP', 'MS', 'MR', 'MC');
begin
  Result := TJvCalcButton.CreateKind(AParent, AKind);
  with Result do
  try
    if Kind in [cbNum0..cbNum9] then
      Caption := IntToStr(Tag)
    else
    if Kind = cbDcm then
      Caption := JclFormatSettings.DecimalSeparator
    else
    if Kind in [cbSgn..cbMC] then
      Caption := BtnCaptions[Kind];
    Left := BtnPos[ALayout, Kind].X;
    Top := BtnPos[ALayout, Kind].Y;
    if ALayout = clDialog then
    begin
      Width := 36;
      Height := 22;
    end
    else
    begin
      Width := 21;
      Height := 21;
    end;
    Style := bsNew;
    OnClick := AOnClick;
    ParentFont := True;
    Parent := AParent;
  except
    Free;
    raise;
  end;
end;

//=== Global procedures ======================================================

function CreateCalculatorForm(AOwner: TComponent; AHelpContext: THelpContext; AOwnerWnd: HWND): TJvCalculatorForm;
begin
  Result := TJvCalculatorForm.Create(AOwner, AOwnerWnd);
  with Result do
  try
    HelpContext := AHelpContext;
    if HelpContext <> 0 then
      BorderIcons := BorderIcons + [biHelp];
    if Screen.PixelsPerInch <> cDefaultPixelsPerInch then
    begin { scale to screen res }
      ScaleBy(Screen.PixelsPerInch, cDefaultPixelsPerInch);
      SetDefaultFont(Font, clDialog);
      Left := (Screen.Width div 2) - (Width div 2);
      Top := (Screen.Height div 2) - (Height div 2);
    end;
  except
    Free;
    raise;
  end;
end;

function CreatePopupCalculator(AOwner: TComponent; ABiDiMode: TBiDiMode = bdLeftToRight): TWinControl;
begin
  Result := TJvPopupCalculator.Create(AOwner);
  Result.BiDiMode := ABiDiMode;
end;

procedure SetupPopupCalculator(PopupCalc: TWinControl; APrecision: Byte;
  ABeepOnError: Boolean);
begin
  if (PopupCalc <> nil) and (PopupCalc is TJvPopupCalculator) then
    if TJvPopupCalculator(PopupCalc).FCalcPanel <> nil then
      with TJvPopupCalculator(PopupCalc).FCalcPanel do
      begin
        FPrecision := Max(2, APrecision);
        FBeepOnError := ABeepOnError;
      end;
end;

//=== { TJvCalcButton } ======================================================

constructor TJvCalcButton.CreateKind(AOwner: TComponent; AKind: TCalcBtnKind);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FKind := AKind;
  if FKind in [cbNum0..cbClr] then
    Tag := Ord(Kind) - 1
  else
    Tag := -1;
end;

procedure TJvCalcButton.ParentFontChanged;

  function BtnColor(Kind: TCalcBtnKind): TColor;
  begin
    if Kind in [cbSqr, cbPcnt, cbRev, cbMP..cbMC] then
      Result := clNavy
    else
    if Kind in [cbDiv, cbMul, cbSub, cbAdd, cbEql] then
      Result := clPurple
    else
    if Kind in [cbBck, cbClr] then
      Result := clMaroon
    else
      Result := clBtnText;
  end;

begin
  if not FFontChanging then
    inherited;
  if ParentFont and not FFontChanging then
  begin
    FFontChanging := True;
    try
      Font.Color := BtnColor(FKind);
      ParentFont := True;
    finally
      FFontChanging := False;
    end;
  end;
end;

//=== { TJvCalculator } ======================================================

constructor TJvCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := RsCalculatorCaption;
  FFlat := False;
  FPrecision := DefCalcPrecision;
  FBeepOnError := True;
end;

procedure TJvCalculator.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Ctl3D', ReadCtl3D, nil, False);
end;

destructor TJvCalculator.Destroy;
begin
  FOnChange := nil;
  FOnDisplayChange := nil;
  inherited Destroy;
end;

procedure TJvCalculator.CalcKey(var Key: Char);
begin
  if Assigned(FOnCalcKey) then
    FOnCalcKey(Self, Key);
end;

procedure TJvCalculator.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCalculator.DisplayChange;
begin
  if Assigned(FOnDisplayChange) then
    FOnDisplayChange(Self);
end;

function TJvCalculator.Execute(ParentWnd: HWND): Boolean;
begin
  if csDesigning in ComponentState then
    FCalc := CreateCalculatorForm(Application, HelpContext, ParentWnd)
  else
    FCalc := CreateCalculatorForm(Self, HelpContext, ParentWnd);
  with FCalc do
  try
    Ctl3D := not FFlat;
    Caption := Self.Title;
    TJvCalculatorPanel(FCalcPanel).FMemory := Self.FMemory;
    TJvCalculatorPanel(FCalcPanel).UpdateMemoryLabel;
    TJvCalculatorPanel(FCalcPanel).FPrecision := Max(2, Self.Precision);
    TJvCalculatorPanel(FCalcPanel).FBeepOnError := Self.BeepOnError;
    if Self.FValue <> 0 then
    begin
      TJvCalculatorPanel(FCalcPanel).DisplayValue := Self.FValue;
      TJvCalculatorPanel(FCalcPanel).FStatus := csFirst;
      TJvCalculatorPanel(FCalcPanel).FOperator := '=';
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      Self.FMemory := TJvCalculatorPanel(FCalcPanel).FMemory;
      if TJvCalculatorPanel(FCalcPanel).DisplayValue <> Self.FValue then
      begin
        Self.FValue := TJvCalculatorPanel(FCalcPanel).DisplayValue;
        Change;
      end;
    end;
  finally
    Free;
    FCalc := nil;
  end;
end;

function TJvCalculator.GetDisplay: Double;
begin
  if Assigned(FCalc) then
    Result := TJvCalculatorPanel(FCalc.FCalcPanel).GetDisplay
  else
    Result := FValue;
end;

function TJvCalculator.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TJvCalculator.ReadCtl3D(Reader: TReader);
begin
  Flat := not Reader.ReadBoolean;
end;

procedure TJvCalculator.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

function TJvCalculator.TitleStored: Boolean;
begin
  Result := Title <> RsCalculatorCaption;
end;

//=== { TJvCalculatorForm } ==================================================

constructor TJvCalculatorForm.Create(AOwner: TComponent);
var
  Control: TWinControl;
  Popup: TPopupMenu;
  Items: array [0..1] of TMenuItem;
begin
  inherited CreateNew(AOwner, 0); // for BCB
  BorderIcons := [biSystemMenu];
  BorderStyle := bsDialog;
  PixelsPerInch := cDefaultPixelsPerInch;
  Caption := RsCalculatorCaption;
  ClientHeight := 159;
  ClientWidth := 242;
  SetDefaultFont(Font, clDialog);
  KeyPreview := True;
  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP};  
  OnKeyPress := FormKeyPress;
  Items[0] := NewItem(RsCopyItem, scCtrl + VK_INSERT, False, True, CopyItemClick, 0, '');
  Items[1] := NewItem(RsPasteItem, scShift + VK_INSERT, False, True, PasteItemClick, 0, '');
  FPasteItem := Items[1];
  Popup := NewPopupMenu(Self, 'PopupMenu', paLeft, True, Items);
  Popup.OnPopup := PopupMenuPopup;
  { MainPanel }
  FMainPanel := TPanel.Create(Self);
  with FMainPanel do
  begin
    Align := alClient;
    Parent := Self;
    BevelOuter := bvLowered;
    ParentColor := True;
    PopupMenu := Popup;
  end;
  { DisplayPanel }
  FDisplayPanel := TPanel.Create(Self);
  with FDisplayPanel do
  begin
    SetBounds(6, 6, 230, 23);
    Parent := FMainPanel;
    BevelOuter := bvLowered;
    Color := clWindow;
    Ctl3D := False;
  end;
  Control := TPanel.Create(Self);
  with TPanel(Control) do
  begin
    SetBounds(1, 1, 228, 21);
    Align := alClient;
    Parent := FDisplayPanel;
    BevelOuter := bvNone;
    BorderStyle := bsSingle;
    Ctl3D := False;
    ParentCtl3D := False;
    ParentColor := True;
  end;
  FDisplayLabel := TLabel.Create(Self);
  with FDisplayLabel do
  begin
    AutoSize := False;
    Alignment := taRightJustify;
    SetBounds(5, 2, 217, 15);
    Parent := TPanel(Control);
    Caption := '0';
  end;
  { CalcPanel }
  FCalcPanel := TJvCalculatorPanel.CreateLayout(Self, clDialog);
  with TJvCalculatorPanel(FCalcPanel) do
  begin
    Align := alBottom;
    Parent := FMainPanel;
    OnOkClick := Self.OkClick;
    OnCancelClick := Self.CancelClick;
    OnCalcKey := Self.CalcKey;
    OnDisplayChange := Self.DisplayChange;
    FControl := FDisplayLabel;
  end;
end;

constructor TJvCalculatorForm.Create(AOwner: TComponent; AParentWnd: HWND);
begin
  FParentWnd := AParentWnd;
  Create(AOwner);
end;

procedure TJvCalculatorForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FParentWnd <> 0 then
    Params.WndParent := FParentWnd;
end;

procedure TJvCalculatorForm.CalcKey(Sender: TObject; var Key: Char);
begin
  if (Owner <> nil) and (Owner is TJvCalculator) then
    TJvCalculator(Owner).CalcKey(Key);
end;

procedure TJvCalculatorForm.CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;


procedure TJvCalculatorForm.CMCtl3DChanged(var Msg: TMessage);
const
  Ctl3DBevel: array [Boolean] of TPanelBevel = (bvNone, bvLowered);
begin
  inherited;
  if FDisplayPanel <> nil then
    FDisplayPanel.BevelOuter := Ctl3DBevel[Ctl3D];
  if FMainPanel <> nil then
    FMainPanel.BevelOuter := Ctl3DBevel[Ctl3D];
end;


procedure TJvCalculatorForm.CopyItemClick(Sender: TObject);
begin
  TJvCalculatorPanel(FCalcPanel).Copy;
end;

procedure TJvCalculatorForm.DisplayChange(Sender: TObject);
begin
  if (Owner <> nil) and (Owner is TJvCalculator) then
    TJvCalculator(Owner).DisplayChange;
end;

procedure TJvCalculatorForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  TJvCalculatorPanel(FCalcPanel).CalcKeyPress(Sender, Key);
end;

procedure TJvCalculatorForm.OkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TJvCalculatorForm.PasteItemClick(Sender: TObject);
begin
  TJvCalculatorPanel(FCalcPanel).Paste;
end;

procedure TJvCalculatorForm.PopupMenuPopup(Sender: TObject);
begin
  FPasteItem.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

//=== { TJvCalculatorPanel } =================================================

constructor TJvCalculatorPanel.CreateLayout(AOwner: TComponent;
  ALayout: TCalcPanelLayout);
const
  BtnGlyphs: array [cbSgn..cbCancel] of Integer = (2 {Sgn}, -1, -1, 3 {Mul},
    4 {Sub}, 5 {Add}, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1 {Ok}, 0 {Cancel});
var
  Bmp: TBitmap;
  I: TCalcBtnKind;
begin
  inherited Create(AOwner);
  if ALayout = clPopup then
    ControlStyle := ControlStyle + [csReplicatable];
  ParentColor := False;
  Color := clBtnFace;
  if ALayout = clDialog then
  begin
    Height := 129;
    Width := 240;
  end
  else
  begin
    Height := 124;
    //    Width := 98;
    Width := 131; // Polaris
  end;
  SetDefaultFont(Font, ALayout);
  ParentFont := False;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ParentColor := True;
  ParentCtl3D := True;
  if ALayout = clDialog then
    Bmp := TBitmap.Create
  else
    Bmp := nil;
  try
    if Bmp <> nil then
//      Bmp.Handle := LoadBitmap(HInstance, 'JvCalculatorPanelBUTTONS');
      Bmp.LoadFromResourceName(HInstance, 'JvCalculatorPanelBUTTONS');
    for I := cbNum0 to cbCancel do
    begin
      if BtnPos[ALayout, I].X > 0 then
        with CreateCalcBtn(Self, I, BtnClick, ALayout) do
        begin
          if ALayout = clDialog then
          begin
            if (Kind in [cbBck, cbClr]) then
              Width := 44;
            if (Kind in [cbSgn..cbCancel]) then
              if BtnGlyphs[Kind] >= 0 then
              begin
                Caption := '';
                AssignBitmapCell(Bmp, Glyph, 6, 1, BtnGlyphs[Kind]);
              end;
          end
          else
          begin
            //Polaris            if Kind in [cbEql] then Width := 44;
            case Kind of
              cbSqr..cbRev:
                Width := 31;
              cbAdd:
                Height := 44;
              cbEql:
                begin
                  Height := 44;
                  Width := 31;
                end;
              cbBck:
                Width := 44;
            end;
          end;
        end;
    end;
    if ALayout = clDialog then
    begin
      { Memory panel }
      FMemoryPanel := TPanel.Create(Self);
      with FMemoryPanel do
      begin
        SetBounds(6, 7, 34, 20);
        BevelInner := bvLowered;
        BevelOuter := bvNone;
        ParentColor := True;
        Parent := Self;
      end;
      FMemoryLabel := TLabel.Create(Self);
      with FMemoryLabel do
      begin
        SetBounds(3, 3, 26, 14);
        Alignment := taCenter;
        AutoSize := False;
        Parent := FMemoryPanel;
        Font.Style := [];
      end;
    end;
  finally
    Bmp.Free;
  end;
  FText := '0';
  FMemory := 0.0;
  FPrecision := DefCalcPrecision;
  FBeepOnError := True;
end;

procedure TJvCalculatorPanel.BtnClick(Sender: TObject);
begin
  case TJvCalcButton(Sender).Kind of
    cbNum0..cbNum9:
      CalcKey(Char(TComponent(Sender).Tag + Ord('0')));
    cbSgn:
      CalcKey('_');
    cbDcm:
      CalcKey(JclFormatSettings.DecimalSeparator);
    cbDiv:
      CalcKey('/');
    cbMul:
      CalcKey('*');
    cbSub:
      CalcKey('-');
    cbAdd:
      CalcKey('+');
    cbSqr:
      CalcKey('Q');
    cbPcnt:
      CalcKey('%');
    cbRev:
      CalcKey('R');
    cbEql:
      CalcKey('=');
    cbBck:
      CalcKey(Backspace);
    cbClr:
      CalcKey('C');
    cbMP:
      if FStatus in [csValid, csFirst] then
      begin
        FStatus := csFirst;
        FMemory := FMemory + GetDisplay;
        UpdateMemoryLabel;
      end;
    cbMS:
      if FStatus in [csValid, csFirst] then
      begin
        FStatus := csFirst;
        FMemory := GetDisplay;
        UpdateMemoryLabel;
      end;
    cbMR:
      if FStatus in [csValid, csFirst] then
      begin
        FStatus := csFirst;
        CheckFirst;
        SetDisplay(FMemory);
      end;
    cbMC:
      begin
        FMemory := 0.0;
        UpdateMemoryLabel;
      end;
    cbOk:
      begin
        if FStatus <> csError then
        begin
          DisplayValue := DisplayValue; { to raise exception on error }
          if Assigned(FOnOk) then
            FOnOk(Self);
        end
        else
        if FBeepOnError then
          Beep;
      end;
    cbCancel:
      if Assigned(FOnCancel) then
        FOnCancel(Self);
  end;
end;

procedure TJvCalculatorPanel.CalcKey(Key: Char);
var
  R: Double;
begin
  Key := UpCase(Key);
  if (FStatus = csError) and (Key <> 'C') then
    Key := #0;
  if Assigned(FOnCalcKey) then
    FOnCalcKey(Self, Key);
  if CharInSet(Key, [JclFormatSettings.DecimalSeparator, '.', ',']) then
  begin
    CheckFirst;
    if Pos(JclFormatSettings.DecimalSeparator, Text) = 0 then
      SetText(Text + JclFormatSettings.DecimalSeparator);
    Exit;
  end;
  case Key of
    'R':
      if FStatus in [csValid, csFirst] then
      begin
        FStatus := csFirst;
        if GetDisplay = 0 then
          Error
        else
          SetDisplay(1.0 / GetDisplay);
      end;
    'Q':
      if FStatus in [csValid, csFirst] then
      begin
        FStatus := csFirst;
        if GetDisplay < 0 then
          Error
        else
          SetDisplay(Sqrt(GetDisplay));
      end;
    '0'..'9':
      begin
        CheckFirst;
        if Text = '0' then
          SetText('');
        if Pos('E', Text) = 0 then
        begin
          if Length(Text) < Max(2, FPrecision) + Ord(Boolean(Pos('-', Text))) then
            SetText(Text + Key)
          else
          if FBeepOnError then
            Beep;
        end;
      end;
    Backspace:
      begin
        CheckFirst;
        if (Length(Text) = 1) or ((Length(Text) = 2) and (Text[1] = '-')) then
          SetText('0')
        else
          SetText(System.Copy(Text, 1, Length(Text) - 1));
      end;
    '_':
      SetDisplay(-GetDisplay);
    '+', '-', '*', '/', '=', '%', Cr:
      begin
        if FStatus = csValid then
        begin
          FStatus := csFirst;
          R := GetDisplay;
          if Key = '%' then
            case FOperator of
              '+', '-': R := FOperand * R / 100.0;
              '*', '/': R := R / 100.0;
            end;
          case FOperator of
            '+': SetDisplay(FOperand + R);
            '-': SetDisplay(FOperand - R);
            '*': SetDisplay(FOperand * R);
            '/':
              if R = 0 then
                Error
              else
                SetDisplay(FOperand / R);
          end;
        end;
        FOperator := Key;
        FOperand := GetDisplay;
        if CharInSet(Key, ResultKeys) then
          if Assigned(FOnResult) then
            FOnResult(Self);
      end;
    Esc, 'C':
      Clear;
    CtrlC:
      Copy;
    CtrlV:
      Paste;
  end;
end;

procedure TJvCalculatorPanel.CalcKeyPress(Sender: TObject; var Key: Char);
var
  Btn: TJvSpeedButton;
begin
  Btn := FindButton(Key);
  if Btn <> nil then
    Btn.ButtonClick
  else
    CalcKey(Key);
end;

procedure TJvCalculatorPanel.CheckFirst;
begin
  if FStatus = csFirst then
  begin
    FStatus := csValid;
    SetText('0');
  end;
end;

procedure TJvCalculatorPanel.Clear;
begin
  FStatus := csFirst;
  SetDisplay(0.0);
  FOperator := '=';
end;


procedure TJvCalculatorPanel.CMCtl3DChanged(var Msg: TMessage);
const
  Ctl3DStyle: array [Boolean] of TButtonStyle = (bsWin31, bsNew);
  Ctl3DBevel: array [Boolean] of TPanelBevel = (bvNone, bvLowered);
  Ctl3DBorder: array [Boolean] of TBorderStyle = (bsSingle, bsNone);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TJvSpeedButton then
      TJvSpeedButton(Components[I]).Style := Ctl3DStyle[Ctl3D]
    else
    if Components[I] = FMemoryPanel then
    begin
      FMemoryPanel.BevelInner := Ctl3DBevel[Ctl3D];
      FMemoryPanel.BorderStyle := Ctl3DBorder[Ctl3D];
    end;
  end;
end;


procedure TJvCalculatorPanel.Copy;
begin
  Clipboard.AsText := Text;
end;

procedure TJvCalculatorPanel.Error;
begin
  FStatus := csError;
  SetText(RsError);
  if FBeepOnError then
    Beep;
  if Assigned(FOnError) then
    FOnError(Self);
end;

function TJvCalculatorPanel.FindButton(Key: Char): TJvSpeedButton;
const
  ButtonChars = '0123456789_./*-+Q%R='#8'C';
var
  I: Integer;
  BtnTag: Longint;
begin
  if CharInSet(Key, [JclFormatSettings.DecimalSeparator, '.', ',']) then
    Key := '.'
  else
  if Key = Cr then
    Key := '='
  else
  if Key = Esc then
    Key := 'C';
  BtnTag := Pos(UpCase(Key), ButtonChars) - 1;
  if BtnTag >= 0 then
    for I := 0 to ControlCount - 1 do
    begin
      if Controls[I] is TJvSpeedButton then
      begin
        Result := TJvSpeedButton(Controls[I]);
        if Result.Tag = BtnTag then
          Exit;
      end;
    end;
  Result := nil;
end;

function TJvCalculatorPanel.GetDisplay: Double;
begin
  if FStatus = csError then
    Result := 0.0
  else
    Result := StrToFloat(Trim(Text));
end;

procedure TJvCalculatorPanel.Paste;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  try
    SetDisplay(StrToFloat(Trim(ReplaceStr(Clipboard.AsText, JclFormatSettings.CurrencyString, ''))));
  except
    SetText('0');
  end;
end;

procedure TJvCalculatorPanel.SetDisplay(R: Double);
var
  S: string;
begin
  S := FloatToStrF(R, ffGeneral, Max(2, FPrecision), 0);
  if Text <> S then
  begin
    SetText(S);
    if Assigned(FOnDisplayChange) then
      FOnDisplayChange(Self);
  end;
end;

procedure TJvCalculatorPanel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    TextChange;
  end;
end;

procedure TJvCalculatorPanel.TextChange;
begin
  if Assigned(FControl) then
  begin
    if FControl is TCustomLabel then
      TCustomLabelAccessProtected(FControl).Caption := Text
    else
    // Note that JvDBCalcEdit will not set its text if it is readonly
    if FControl is TCustomEdit then
      TCustomEdit(FControl).Text := Text;
  end;
  if Assigned(FOnTextChange) then
    FOnTextChange(Self);
end;

procedure TJvCalculatorPanel.UpdateMemoryLabel;
begin
  if FMemoryLabel <> nil then
    if FMemory <> 0.0 then
      FMemoryLabel.Caption := 'M'
    else
      FMemoryLabel.Caption := '';
end;

//=== { TJvLocCalculator } ===================================================

constructor TJvLocCalculator.Create(AOwner: TComponent);
begin
  inherited CreateLayout(AOwner, clPopup);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];
  ControlStyle := ControlStyle + [csReplicatable];
  Enabled := False;
  TabStop := False;
end;


procedure TJvLocCalculator.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and not (WS_TABSTOP or WS_DISABLED);
    AddBiDiModeExStyle(ExStyle);
  end;
end;


procedure TJvLocCalculator.EnabledChanged;
begin
  inherited EnabledChanged;
  if HandleAllocated and not (csDesigning in ComponentState) then
    EnableWindow(Handle, True);
end;

//=== { TJvPopupCalculator } =================================================

constructor TJvPopupCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 127;
  //  Width := 104;
  Width := 137; // Polaris
  Color := clBtnFace;
  SetDefaultFont(Font, clPopup);
  if csDesigning in ComponentState then
    Exit;
  FCalcPanel := TJvLocCalculator.Create(Self);
  with FCalcPanel do
  begin
    Parent := Self;
    Align := alClient;
    BevelOuter := bvRaised;
    FPrecision := DefCalcPrecision;
    { (rb) Fix to update the text of a TJvCalcEdit }
    if AOwner is TControl then
      FControl := TControl(AOwner);
    Visible := True;
    OnTextChange := Self.TextChange;
    OnResultClick := Self.ResultClick;
  end;
end;

function TJvPopupCalculator.GetPopupText: string;
begin
  Result := FCalcPanel.Text;
end;

function TJvPopupCalculator.GetValue: Variant;
begin
  if csDesigning in ComponentState then
    Result := 0
  else
  begin
    if FCalcPanel.FStatus <> csError then
    begin
      { to raise exception on error }
      FCalcPanel.DisplayValue := FCalcPanel.DisplayValue;
      Result := FCalcPanel.DisplayValue;
    end
    else
    begin
      if FCalcPanel.FBeepOnError then
        Beep;
      Result := 0;
    end;
  end;
end;

procedure TJvPopupCalculator.KeyPress(var Key: Char);
begin
  if FCalcPanel <> nil then
    FCalcPanel.CalcKeyPress(Self, Key);
  inherited KeyPress(Key);
end;

procedure TJvPopupCalculator.ResultClick(Sender: TObject);
begin
  if FCalcPanel.FStatus <> csError then
  begin
    FCalcPanel.DisplayValue := FCalcPanel.DisplayValue;
    CloseUp(True);
  end;
end;

procedure TJvPopupCalculator.SetValue(const Value: Variant);
begin
  if not (csDesigning in ComponentState) then
    with FCalcPanel do
    begin
      try
        if VarIsNullEmpty(Value) then
          DisplayValue := 0
        else
          DisplayValue := Value;
      except
        DisplayValue := 0;
      end;
      FStatus := csFirst;
      FOperator := '=';
    end;
end;

procedure TJvPopupCalculator.TextChange(Sender: TObject);
begin
  InvalidateEditor;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.