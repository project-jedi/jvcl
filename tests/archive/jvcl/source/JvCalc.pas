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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCalc;

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ExtCtrls, Buttons, Clipbrd,
  JvxCtrls, JvBaseDlg;

const
  DefCalcPrecision = 15;

type
  TJvCalcState = (csFirst, csValid, csError);
  TJvCalculatorForm = class;

  TJvCalculator = class(TJvCommonDialogF)
  private
    FValue: Double;
    FMemory: Double;
    FTitle: string;
    FCtl3D: Boolean;
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
  protected
    procedure Change; dynamic;
    procedure CalcKey(var Key: Char); dynamic;
    procedure DisplayChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    property CalcDisplay: Double read GetDisplay;
    property Memory: Double read FMemory;
  published
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property Ctl3D: Boolean read FCtl3D write FCtl3D default True;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property Precision: Byte read FPrecision write FPrecision default DefCalcPrecision;
    property Title: string read GetTitle write SetTitle stored TitleStored;
    property Value: Double read FValue write FValue;
    property OnCalcKey: TKeyPressEvent read FOnCalcKey write FOnCalcKey;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
  end;

  TJvCalculatorForm = class(TForm)
  private
    FMainPanel: TPanel;
    FCalcPanel: TPanel;
    FDisplayPanel: TPanel;
    FDisplayLabel: TLabel;
    FPasteItem: TMenuItem;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PopupMenuPopup(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure PasteItemClick(Sender: TObject);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure CalcKey(Sender: TObject; var Key: Char);
    procedure DisplayChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

function CreateCalculatorForm(AOwner: TComponent; AHelpContext: THelpContext): TJvCalculatorForm;
{$IFDEF COMPILER4_UP}
function CreatePopupCalculator(AOwner: TComponent;
  ABiDiMode: TBiDiMode = bdLeftToRight): TWinControl;
{$ELSE}
function CreatePopupCalculator(AOwner: TComponent): TWinControl;
{$ENDIF}
procedure SetupPopupCalculator(PopupCalc: TWinControl; APrecision: Byte;
  ABeepOnError: Boolean);

implementation

uses
  Math,
  {$IFNDEF WIN32}
  JvStr16,
  {$ENDIF}
  JvVCLUtils, JvStrUtils, JvToolEdit;

{$IFDEF WIN32}
{$R *.Res}
{$ELSE}
{$R *.R16}
{$ENDIF}

const
  SCalculator = 'Calculator';
  SError = 'Error';

type
  TCalcBtnKind =
    (cbNone, cbNum0, cbNum1, cbNum2, cbNum3, cbNum4, cbNum5, cbNum6,
    cbNum7, cbNum8, cbNum9, cbSgn, cbDcm, cbDiv, cbMul, cbSub,
    cbAdd, cbSqr, cbPcnt, cbRev, cbEql, cbBck, cbClr, cbMP,
    cbMS, cbMR, cbMC, cbOk, cbCancel);

  TCalcPanelLayout = (clDialog, clPopup);

procedure SetDefaultFont(AFont: TFont; Layout: TCalcPanelLayout);
{$IFDEF WIN32}
var
  NonClientMetrics: TNonClientMetrics;
{$ENDIF}
begin
  {$IFDEF WIN32}
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    AFont.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont)
  else
  {$ENDIF}
    with AFont do
    begin
      Color := clWindowText;
      Name := 'MS Sans Serif';
      Size := 8;
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

function CreateCalculatorForm(AOwner: TComponent; AHelpContext: THelpContext): TJvCalculatorForm;
begin
  Result := TJvCalculatorForm.Create(AOwner);
  with Result do
  try
    HelpContext := AHelpContext;
    {$IFDEF WIN32}
    if HelpContext <> 0 then
      BorderIcons := BorderIcons + [biHelp];
    {$ENDIF}
    if Screen.PixelsPerInch <> 96 then
    begin { scale to screen res }
      ScaleBy(Screen.PixelsPerInch, 96);
      SetDefaultFont(Font, clDialog);
      Left := (Screen.Width div 2) - (Width div 2);
      Top := (Screen.Height div 2) - (Height div 2);
    end;
  except
    Free;
    raise;
  end;
end;

//=== TJvCalcButton ==========================================================

type
  TJvCalcButton = class(TJvSpeedButton)
  private
    FKind: TCalcBtnKind;
    FFontChanging: Boolean;
  protected
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
  public
    constructor CreateKind(AOwner: TComponent; AKind: TCalcBtnKind);
    property Kind: TCalcBtnKind read FKind;
  end;

constructor TJvCalcButton.CreateKind(AOwner: TComponent; AKind: TCalcBtnKind);
begin
  inherited Create(AOwner);
  {$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  FKind := AKind;
  if FKind in [cbNum0..cbClr] then
    Tag := Ord(Kind) - 1
  else
    Tag := -1;
end;

procedure TJvCalcButton.CMParentFontChanged(var Message: TMessage);

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

  ResultKeys = [#13, '=', '%'];

function CreateCalcBtn(AParent: TWinControl; AKind: TCalcBtnKind;
  AOnClick: TNotifyEvent; ALayout: TCalcPanelLayout): TJvCalcButton;
const
  BtnCaptions: array [cbSgn..cbMC] of PChar =
    ('±', ',', '/', '*', '-', '+', 'sqrt', '%', '1/x', '=', '<-', 'C',
     'MP', 'MS', 'MR', 'MC');
begin
  Result := TJvCalcButton.CreateKind(AParent, AKind);
  with Result do
  try
    if Kind in [cbNum0..cbNum9] then
      Caption := IntToStr(Tag)
    else
    if Kind = cbDcm then
      Caption := DecimalSeparator
    else
    if Kind in [cbSgn..cbMC] then
      Caption := StrPas(BtnCaptions[Kind]);
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

//=== TJvCalculatorPanel =====================================================

type
  TJvCalculatorPanel = class(TPanel)
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
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure BtnClick(Sender: TObject);
  protected
    procedure TextChanged; virtual;
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

constructor TJvCalculatorPanel.CreateLayout(AOwner: TComponent;
  ALayout: TCalcPanelLayout);
var
  Bmp: TBitmap;
  I: TCalcBtnKind;
const
  BtnGlyphs: array [cbSgn..cbCancel] of Integer = (2 {Sgn}, -1, -1, 3 {Mul},
    4 {Sub}, 5 {Add}, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1 {Ok}, 0 {Cancel});
begin
  inherited Create(AOwner);
  {$IFDEF WIN32}
  if ALayout = clPopup then
    ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
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
      Bmp.Handle := LoadBitmap(HInstance, 'JVCALCBTNS');
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

procedure TJvCalculatorPanel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    TextChanged;
  end;
end;

type
  TAccessLabel = class(TCustomLabel);

procedure TJvCalculatorPanel.TextChanged;
begin
  if Assigned(FControl) then
  begin
    if FControl is TCustomLabel then
      TAccessLabel(FControl).Caption := FText
    else
    { (rb) Fix to update the text of a TJvCalcEdit }
    if FControl is TCustomEdit then
      TCustomEdit(FControl).Text := FText;
  end;
  if Assigned(FOnTextChange) then
    FOnTextChange(Self);
end;

procedure TJvCalculatorPanel.Error;
begin
  FStatus := csError;
  SetText(SError);
  if FBeepOnError then
    MessageBeep(0);
  if Assigned(FOnError) then
    FOnError(Self);
end;

procedure TJvCalculatorPanel.SetDisplay(R: Double);
var
  S: string;
begin
  S := FloatToStrF(R, ffGeneral, Max(2, FPrecision), 0);
  if FText <> S then
  begin
    SetText(S);
    if Assigned(FOnDisplayChange) then
      FOnDisplayChange(Self);
  end;
end;

function TJvCalculatorPanel.GetDisplay: Double;
begin
  if FStatus = csError then
    Result := 0.0
  else
    Result := StrToFloat(Trim(FText));
end;

procedure TJvCalculatorPanel.CheckFirst;
begin
  if FStatus = csFirst then
  begin
    FStatus := csValid;
    SetText('0');
  end;
end;

procedure TJvCalculatorPanel.CMCtl3DChanged(var Message: TMessage);
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

procedure TJvCalculatorPanel.UpdateMemoryLabel;
begin
  if FMemoryLabel <> nil then
    if FMemory <> 0.0 then
      FMemoryLabel.Caption := 'M'
    else
      FMemoryLabel.Caption := '';
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
  if Key in [DecimalSeparator, '.', ','] then
  begin
    CheckFirst;
    if Pos(DecimalSeparator, FText) = 0 then
      SetText(FText + DecimalSeparator);
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
        if FText = '0' then
          SetText('');
        if Pos('E', FText) = 0 then
        begin
          if Length(FText) < Max(2, FPrecision) + Ord(Boolean(Pos('-', FText))) then
            SetText(FText + Key)
          else
          if FBeepOnError then
            MessageBeep(0);
        end;
      end;
    #8:
      begin
        CheckFirst;
        if (Length(FText) = 1) or ((Length(FText) = 2) and (FText[1] = '-')) then
          SetText('0')
        else
          SetText(System.Copy(FText, 1, Length(FText) - 1));
      end;
    '_':
      SetDisplay(-GetDisplay);
    '+', '-', '*', '/', '=', '%', #13:
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
        if Key in ResultKeys then
          if Assigned(FOnResult) then
            FOnResult(Self);
      end;
    #27, 'C':
      Clear;
    ^C:
      Copy;
    ^V:
      Paste;
  end;
end;

procedure TJvCalculatorPanel.Clear;
begin
  FStatus := csFirst;
  SetDisplay(0.0);
  FOperator := '=';
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

function TJvCalculatorPanel.FindButton(Key: Char): TJvSpeedButton;
const
  ButtonChars = '0123456789_./*-+Q%R='#8'C';
var
  I: Integer;
  BtnTag: Longint;
begin
  if Key in [DecimalSeparator, '.', ','] then
    Key := '.'
  else
  if Key = #13 then
    Key := '='
  else
  if Key = #27 then
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

procedure TJvCalculatorPanel.BtnClick(Sender: TObject);
begin
  case TJvCalcButton(Sender).Kind of
    cbNum0..cbNum9:
      CalcKey(Char(TComponent(Sender).Tag + Ord('0')));
    cbSgn:
      CalcKey('_');
    cbDcm:
      CalcKey(DecimalSeparator);
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
      CalcKey(#8);
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
          MessageBeep(0);
      end;
    cbCancel:
      if Assigned(FOnCancel) then
        FOnCancel(Self);
  end;
end;

procedure TJvCalculatorPanel.Copy;
begin
  Clipboard.AsText := FText;
end;

procedure TJvCalculatorPanel.Paste;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  try
    SetDisplay(StrToFloat(Trim(ReplaceStr(Clipboard.AsText,
      CurrencyString, ''))));
  except
    SetText('0');
  end;
end;

//=== TJvLocCalculator =======================================================

type
  TJvLocCalculator = class(TJvCalculatorPanel)
  private
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TJvLocCalculator.Create(AOwner: TComponent);
begin
  inherited CreateLayout(AOwner, clPopup);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];
  {$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  Enabled := False;
  TabStop := False;
end;

procedure TJvLocCalculator.CMEnabledChanged(var Message: TMessage);
begin
  if HandleAllocated and not (csDesigning in ComponentState) then
    EnableWindow(Handle, True);
end;

procedure TJvLocCalculator.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and not (WS_TABSTOP or WS_DISABLED);
    {$IFDEF COMPILER4_UP}
    AddBiDiModeExStyle(ExStyle);
    {$ENDIF}
  end;
end;

//=== TJvPopupCalculator =====================================================

type
  TJvPopupCalculator = class(TJvPopupWindow)
  private
    FCalcPanel: TJvLocCalculator;
    procedure TextChange(Sender: TObject);
    procedure ResultClick(Sender: TObject);
  protected
    procedure KeyPress(var Key: Char); override;
    {$IFDEF WIN32}
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    {$ELSE}
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    function GetPopupText: string; override;
  end;

{$IFDEF COMPILER4_UP}
function CreatePopupCalculator(AOwner: TComponent;
  ABiDiMode: TBiDiMode = bdLeftToRight): TWinControl;
{$ELSE}
function CreatePopupCalculator(AOwner: TComponent): TWinControl;
{$ENDIF}
begin
  Result := TJvPopupCalculator.Create(AOwner);
  if (AOwner <> nil) and not (csDesigning in AOwner.ComponentState) and
    (Screen.PixelsPerInch <> 96) then
  begin { scale to screen res }
    Result.ScaleBy(Screen.PixelsPerInch, 96);
    { The ScaleBy method does not scale the font well, so set the
      font back to the original info. }
    TJvPopupCalculator(Result).FCalcPanel.ParentFont := True;
    SetDefaultFont(TJvPopupCalculator(Result).Font, clPopup);
    {$IFDEF COMPILER4_UP}
    Result.BiDiMode := ABiDiMode;
    {$ENDIF}
  end;
end;

procedure SetupPopupCalculator(PopupCalc: TWinControl; APrecision: Byte;
  ABeepOnError: Boolean);
begin
  if (PopupCalc = nil) or not (PopupCalc is TJvPopupCalculator) then
    Exit;
  if TJvPopupCalculator(PopupCalc).FCalcPanel <> nil then
    with TJvPopupCalculator(PopupCalc).FCalcPanel do
    begin
      FPrecision := Max(2, APrecision);
      FBeepOnError := ABeepOnError;
    end;
end;

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

procedure TJvPopupCalculator.KeyPress(var Key: Char);
begin
  if FCalcPanel <> nil then
    FCalcPanel.CalcKeyPress(Self, Key);
  inherited KeyPress(Key);
end;

{$IFDEF WIN32}

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
        MessageBeep(0);
      Result := 0;
    end;
  end;
end;

procedure TJvPopupCalculator.SetValue(const Value: Variant);
begin
  if not (csDesigning in ComponentState) then
    with FCalcPanel do
    begin
      try
        if VarIsNull(Value) or VarIsEmpty(Value) then
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

{$ELSE}

function TJvPopupCalculator.GetValue: string;
var
  D: Double;
begin
  if (csDesigning in ComponentState) or (FCalcPanel = nil) then
    Result := '0'
  else
  begin
    if FCalcPanel.FStatus <> csError then
    begin
      { to raise exception on error }
      FCalcPanel.DisplayValue := FCalcPanel.DisplayValue;
      D := FCalcPanel.DisplayValue;
    end
    else
    begin
      if FCalcPanel.FBeepOnError then
        MessageBeep(0);
      D := 0;
    end;
    Result := FloatToStrF(D, ffGeneral, Max(2, FCalcPanel.FPrecision), 0);
  end;
end;

procedure TJvPopupCalculator.SetValue(const Value: string);
begin
  if not (csDesigning in ComponentState) then
  begin
    with FCalcPanel do
    begin
      if Value = '' then
        DisplayValue := 0
      else
      try
        DisplayValue := StrToFloat(Value);
      except
        DisplayValue := 0;
      end;
      FStatus := csFirst;
      FOperator := '=';
    end;
  end;
end;

{$ENDIF}

function TJvPopupCalculator.GetPopupText: string;
begin
  Result := FCalcPanel.Text;
end;

procedure TJvPopupCalculator.ResultClick(Sender: TObject);
begin
  if FCalcPanel.FStatus <> csError then
  begin
    FCalcPanel.DisplayValue := FCalcPanel.DisplayValue;
    CloseUp(True);
  end;
end;

procedure TJvPopupCalculator.TextChange(Sender: TObject);
begin
  InvalidateEditor;
end;

//=== TJvCalculator ==========================================================

constructor TJvCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := SCalculator;
  FCtl3D := True;
  FPrecision := DefCalcPrecision;
  FBeepOnError := True;
end;

destructor TJvCalculator.Destroy;
begin
  FOnChange := nil;
  FOnDisplayChange := nil;
  inherited Destroy;
end;

function TJvCalculator.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TJvCalculator.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

function TJvCalculator.TitleStored: Boolean;
begin
  Result := Title <> SCalculator;
end;

function TJvCalculator.GetDisplay: Double;
begin
  if Assigned(FCalc) then
    Result := TJvCalculatorPanel(FCalc.FCalcPanel).GetDisplay
  else
    Result := FValue;
end;

procedure TJvCalculator.CalcKey(var Key: Char);
begin
  if Assigned(FOnCalcKey) then
    FOnCalcKey(Self, Key);
end;

procedure TJvCalculator.DisplayChange;
begin
  if Assigned(FOnDisplayChange) then
    FOnDisplayChange(Self);
end;

procedure TJvCalculator.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvCalculator.Execute: Boolean;
begin
  if csDesigning in ComponentState then
    FCalc := CreateCalculatorForm(Application, HelpContext)
  else
    FCalc := CreateCalculatorForm(Self, HelpContext);
  with FCalc do
  try
    Ctl3D := FCtl3D;
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

//=== TJvCalculatorForm ======================================================

constructor TJvCalculatorForm.Create(AOwner: TComponent);
var
  Control: TWinControl;
  Popup: TPopupMenu;
  Items: array [0..1] of TMenuItem;
begin
  {$IFDEF CBUILDER}
  inherited CreateNew(AOwner, 0);
  {$ELSE}
  inherited CreateNew(AOwner);
  {$ENDIF}
  BorderIcons := [biSystemMenu];
  BorderStyle := bsDialog;
  Caption := SCalculator;
  ClientHeight := 159;
  ClientWidth := 242;
  SetDefaultFont(Font, clDialog);
  KeyPreview := True;
  PixelsPerInch := 96;
  Position := poScreenCenter;
  OnKeyPress := FormKeyPress;
  Items[0] := NewItem('&Copy', scCtrl + vk_Insert, False, True, CopyItemClick, 0, '');
  Items[1] := NewItem('&Paste', scShift + vk_Insert, False, True, PasteItemClick, 0, '');
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
    ParentColor := True;
    ParentCtl3D := False;
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

procedure TJvCalculatorForm.CMCtl3DChanged(var Message: TMessage);
const
  Ctl3DBevel: array [Boolean] of TPanelBevel = (bvNone, bvLowered);
begin
  inherited;
  if FDisplayPanel <> nil then
    FDisplayPanel.BevelOuter := Ctl3DBevel[Ctl3D];
  if FMainPanel <> nil then
    FMainPanel.BevelOuter := Ctl3DBevel[Ctl3D];
end;

procedure TJvCalculatorForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  TJvCalculatorPanel(FCalcPanel).CalcKeyPress(Sender, Key);
end;

procedure TJvCalculatorForm.CopyItemClick(Sender: TObject);
begin
  TJvCalculatorPanel(FCalcPanel).Copy;
end;

procedure TJvCalculatorForm.PasteItemClick(Sender: TObject);
begin
  TJvCalculatorPanel(FCalcPanel).Paste;
end;

procedure TJvCalculatorForm.OkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TJvCalculatorForm.CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TJvCalculatorForm.CalcKey(Sender: TObject; var Key: Char);
begin
  if (Owner <> nil) and (Owner is TJvCalculator) then
    TJvCalculator(Owner).CalcKey(Key);
end;

procedure TJvCalculatorForm.DisplayChange(Sender: TObject);
begin
  if (Owner <> nil) and (Owner is TJvCalculator) then
    TJvCalculator(Owner).DisplayChange;
end;

procedure TJvCalculatorForm.PopupMenuPopup(Sender: TObject);
begin
  FPasteItem.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

end.

