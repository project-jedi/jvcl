{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHLEdPropDlg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvHLEdPropDlg
description : Properties dialog for TJvHLEditor component

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
unit JvHLEditorPropertyForm;

interface

uses
  Windows, Messages, SysUtils, ComCtrls, Classes, Graphics, Controls,
  Forms, StdCtrls, ExtCtrls,
  JvEditor, JvHLEditor, JvComponent, JvFormPlacement;

type
  TJvHLEdPropDlg = class;

  TJvHLEditorParamsForm = class(TForm)
    Pages: TPageControl;
    bCancel: TButton;
    bOK: TButton;
    tsEditor: TTabSheet;
    lblEditorSpeedSettings: TLabel;
    cbKeyboardLayout: TComboBox;
    gbEditor: TGroupBox;
    cbUndoAfterSave: TCheckBox;
    cbDoubleClickLine: TCheckBox;
    cbKeepTrailingBlanks: TCheckBox;
    cbSytaxHighlighting: TCheckBox;
    cbAutoIndent: TCheckBox;
    cbSmartTab: TCheckBox;
    cbBackspaceUnindents: TCheckBox;
    cbGroupUndo: TCheckBox;
    cbCursorBeyondEOF: TCheckBox;
    eTabStops: TEdit;
    lblTabStops: TLabel;
    tsColors: TTabSheet;
    lblColorSpeedSettingsFor: TLabel;
    cbColorSettings: TComboBox;
    lblElement: TLabel;
    lbElements: TListBox;
    lblColor: TLabel;
    gbTextAttributes: TGroupBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    gbUseDefaultsFor: TGroupBox;
    cbDefForeground: TCheckBox;
    cbDefBackground: TCheckBox;
    Label6: TLabel;
    Panel1: TPanel;
    Cell0: TPanel;
    Cell4: TPanel;
    Cell8: TPanel;
    Cell12: TPanel;
    Cell1: TPanel;
    Cell5: TPanel;
    Cell9: TPanel;
    Cell13: TPanel;
    Cell2: TPanel;
    Cell6: TPanel;
    Cell10: TPanel;
    Cell14: TPanel;
    Cell3: TPanel;
    Cell7: TPanel;
    Cell11: TPanel;
    Cell15: TPanel;
    raColorSamples: TJvFormStorage;
    procedure FormCreate(Sender: TObject);
    procedure NotImplemented(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure lbElementsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ColorChanged(Sender: TObject);
    procedure cbColorSettingsChange(Sender: TObject);
    procedure DefClick(Sender: TObject);
    procedure CellMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    RAHLEditor1: TJvHLEditor;
    FHighlighter: THighlighter;
    SC: TJvSymbolColor;
    InChanging: Boolean;
    Params: TJvHLEdPropDlg;
    procedure LoadLocale;
    function ColorToIndex(const AColor: TColor): Integer;
    function GetColorIndex(const ColorName: string): Integer;
    function GetForegroundIndex: Integer;
    function GetBackgroundIndex: Integer;
    procedure SetColorIndex(const Index: Integer;
      const ColorName, OtherColorName: string);
    procedure SetForegroundIndex(const Index: Integer);
    procedure SetBackgroundIndex(const Index: Integer);
    function GetColorColor(const ColorName: string): TColor;
    function GetForegroundColor: TColor;
    function GetBackgroundColor: TColor;
    function GetCell(const Index: Integer): TPanel;
  public
    procedure ParamsToControls;
    procedure ControlsToParams;
  end;

  TJvHLEdActivePage = 0..1;
  TJvHLEdReadFrom = (rfRegAuto, rfHLEditor);
  TJvHLEdPages = set of (epEditor, epColors);
  TOnDialogPopup = procedure(Sender: TObject; Form: TForm) of object;
  TOnDialogClosed = procedure(Sender: TObject; Form: TForm; Apply: Boolean) of object;

  TJvHLEdPropDlg = class(TComponent)
  private
    FRAHLEditor: TJvHLEditor;
    FRegAuto: TJvFormStorage;
    FColorSamples: TStrings;
    FHighlighterCombo: Boolean;
    FActivePage: TJvHLEdActivePage;
    FReadFrom: TJvHLEdReadFrom;
    FPages: TJvHLEdPages;
    FRegAutoSection: string; { ini section for regauto }
    FOnDialogPopup: TOnDialogPopup;
    FOnDialogClosed: TOnDialogClosed;
    procedure SetColorSamples(const Value: TStrings);
    function IsPagesStored: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure Restore;
    procedure LoadHighlighterColors(ARAHLEditor: TJvHLEditor; AHighlighter: THighlighter);
    procedure SaveHighlighterColors(ARAHLEditor: TJvHLEditor; AHighlighter: THighlighter);
    function Execute: Boolean;
    procedure LoadCurrentHighlighterColors;
    procedure SaveCurrentHighlighterColors;
  published
    property JvHLEditor: TJvHLEditor read FRAHLEditor write FRAHLEditor;
    property RegAuto: TJvFormStorage read FRegAuto write FRegAuto;
    property ColorSamples: TStrings read FColorSamples write SetColorSamples;
    property HighlighterCombo: Boolean read FHighlighterCombo write FHighlighterCombo default True;
    property ActivePage: TJvHLEdActivePage read FActivePage write FActivePage default 0;
    property ReadFrom: TJvHLEdReadFrom read FReadFrom write FReadFrom default rfRegAuto;
    property Pages: TJvHLEdPages read FPages write FPages stored IsPagesStored;
    property RegAutoSection: string read FRegAutoSection write FRegAutoSection;
    property OnDialogPopup: TOnDialogPopup read FOnDialogPopup write FOnDialogPopup;
    property OnDialogClosed: TOnDialogClosed read FOnDialogClosed write FOnDialogClosed;
  end;

const
  HighLighters: array[THighLighter] of string =
  ('None', 'Pascal', 'CBuilder', 'Sql', 'Python', 'Java', 'VB', 'Html',
    'Perl', 'Ini', 'CocoR', 'Php'
    {$IFDEF HL_NOT_QUITE_C}, 'NQC'{$ENDIF HL_NOT_QUITE_C}, 'User Defined'
    );

implementation

uses
  Math,
  JvConsts, JvJVCLUtils, JvJCLUtils;

{$R *.DFM}

function Pixels(Control: TControl; APixels: Integer): Integer;
var
  Form: TForm;
begin
  Result := APixels;
  if Control is TForm then
    Form := TForm(Control)
  else
    Form := TForm(GetParentForm(Control));
  if Form.Scaled then
    Result := Result * Form.PixelsPerInch div 96;
end;

//=== TJvSampleViewer ========================================================

type
  TJvSampleViewer = class(TJvHLEditor)
  private
    TmpEd: TJvHLEditor;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure WndProc(var Msg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TJvSampleViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TmpEd := TJvHLEditor.Create(Self);
  TmpEd.Visible := False;
  TmpEd.Parent := Self;
end;

procedure TJvSampleViewer.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    {WM_LBUTTONDOWN,} WM_LBUTTONUP, WM_RBUTTONDOWN, WM_RBUTTONUP,
    WM_MOUSEMOVE, WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK:
      { nothing - prevent user interact };
  else
    inherited WndProc(Msg);
  end;
end;

procedure TJvSampleViewer.WMLButtonDown(var Msg: TWMLButtonDown);
var
  XX, YY: Integer;
  F: Integer;
  Str: WideString;
begin
  { also prevent user interact }
  { detect symbol type }
  Mouse2Caret(Msg.XPos, Msg.YPos, XX, YY);
  if (XX = RightMargin) or (XX - 1 = RightMargin) then
    F := 13
  else
  begin
    TmpEd.Lines := Lines;
    TmpEd.HighLighter := HighLighter;
    { color values corresponds to lbElements ListBox }
    TmpEd.Font.Color := 0;
    with TmpEd.Colors do
    begin
      Comment.ForeColor := 1;
      Reserved.ForeColor := 2;
      Identifier.ForeColor := 3;
      Symbol.ForeColor := 4;
      Strings.ForeColor := 5;
      Number.ForeColor := 6;
      Preproc.ForeColor := 7;
      Declaration.ForeColor := 8;
      FunctionCall.ForeColor := 9;
      Statement.ForeColor := 10;
      PlainText.ForeColor := 11;
    end;
    TmpEd.SelForeColor := 12;
    Str := TmpEd.Lines[YY];
    TJvSampleViewer(TmpEd).GetLineAttr(Str, YY, 0, JvEditor.Max_X - 1);
    F := TJvSampleViewer(TmpEd).LineAttrs[XX].FC;
  end;
  (Owner as TJvHLEditorParamsForm).lbElements.ItemIndex := F;
  (Owner as TJvHLEditorParamsForm).lbElementsClick((Owner as TJvHLEditorParamsForm).lbElements);
end;

//=== TJvHLEdPropDlg =========================================================

constructor TJvHLEdPropDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHighlighterCombo := True;
  FColorSamples := TStringList.Create;
  with TJvHLEditorParamsForm.Create(Application) do
  try
//    FColorSamples.Assign(raColorSamples.Strings);
   finally
     Free;
   end;
  FPages := [epEditor, epColors];
end;

destructor TJvHLEdPropDlg.Destroy;
begin
  FColorSamples.Free;
  inherited Destroy;
end;

procedure TJvHLEdPropDlg.Save;
const
  cParams = 'Params';
var
  S: string;
begin
  if FRegAuto = nil then
    raise Exception.Create(SHLEdPropDlg_RegAutoNotAssigned);
  S := AddSlash2(FRegAutoSection) + cParams;
  with FRegAuto do
  begin
    StoredValue['DoubleClickLine'] := FRAHLEditor.DoubleClickLine;
    StoredValue['UndoAfterSave'] := FRAHLEditor.UndoAfterSave;
    StoredValue['KeepTrailingBlanks'] := FRAHLEditor.KeepTrailingBlanks;
    StoredValue['AutoIndent'] := FRAHLEditor.AutoIndent;
    StoredValue['SmartTab'] := FRAHLEditor.SmartTab;
    StoredValue['BackspaceUnindents'] := FRAHLEditor.BackspaceUnindents;
    StoredValue['GroupUndo'] := FRAHLEditor.GroupUndo;
    StoredValue['CursorBeyondEOF'] := FRAHLEditor.CursorBeyondEOF;
    StoredValue['SyntaxHighlighting'] := FRAHLEditor.SyntaxHighlighting;
    StoredValue['TabStops'] := FRAHLEditor.TabStops;
    StoredValue['RightMargin'] := FRAHLEditor.RightMargin;
  end;
end;

procedure TJvHLEdPropDlg.Restore;
const
  cParams = 'Params';
var
  S: string;
begin
  if FRegAuto = nil then
    raise Exception.Create(SHLEdPropDlg_RegAutoNotAssigned);
  S := AddSlash2(FRegAutoSection) + cParams;
  with FRegAuto do
  begin
    FRAHLEditor.DoubleClickLine := DefaultValue['DoubleClickLine', FRAHLEditor.DoubleClickLine];
    FRAHLEditor.UndoAfterSave := DefaultValue['UndoAfterSave', FRAHLEditor.UndoAfterSave];
    FRAHLEditor.KeepTrailingBlanks := DefaultValue['KeepTrailingBlanks', FRAHLEditor.KeepTrailingBlanks];
    FRAHLEditor.AutoIndent := DefaultValue['AutoIndent', FRAHLEditor.AutoIndent];
    FRAHLEditor.SmartTab := DefaultValue['SmartTab', FRAHLEditor.SmartTab];
    FRAHLEditor.BackspaceUnindents := DefaultValue['BackspaceUnindents', FRAHLEditor.BackspaceUnindents];
    FRAHLEditor.GroupUndo := DefaultValue['GroupUndo', FRAHLEditor.GroupUndo];
    FRAHLEditor.CursorBeyondEOF := DefaultValue['CursorBeyondEOF', FRAHLEditor.CursorBeyondEOF];
    FRAHLEditor.SyntaxHighlighting := DefaultValue['SyntaxHighlighting', FRAHLEditor.SyntaxHighlighting];
    FRAHLEditor.TabStops := DefaultValue['TabStops', FRAHLEditor.TabStops];
    FRAHLEditor.RightMargin := DefaultValue['RightMargin', FRAHLEditor.RightMargin];
  end
end;

procedure TJvHLEdPropDlg.SaveHighlighterColors(ARAHLEditor: TJvHLEditor; AHighlighter: THighlighter);
var
  Section: string;

  procedure SaveColor(AColor: TJvSymbolColor; const Prefix: string);
  begin
    FRegAuto.StoredValue[Section + Prefix] := ColorToString(AColor.ForeColor) + ', ' + ColorToString(AColor.BackColor) +
      ', ' + IntToStr(byte(AColor.Style));
  end;

begin
  if FRegAuto = nil then
    raise Exception.Create(SHLEdPropDlg_RegAutoNotAssigned);
  Section := AddSlash2(FRegAutoSection) + HighLighters[AHighLighter];
  FRegAuto.StoredValue[Section + 'BackColor'] := ColorToString(ARAHLEditor.Color);
  FRegAuto.StoredValue[Section + 'FontName'] := ARAHLEditor.Font.Name;
  FRegAuto.StoredValue[Section + 'Charset'] := IntToStr(ARAHLEditor.Font.Charset);
  FRegAuto.StoredValue[Section + 'FontSize'] := ARAHLEditor.Font.Size;
  FRegAuto.StoredValue[Section + 'RightMarginColor'] := ColorToString(ARAHLEditor.RightMarginColor);
  SaveColor(ARAHLEditor.Colors.Number, 'Number');
  SaveColor(ARAHLEditor.Colors.Strings, 'Strings');
  SaveColor(ARAHLEditor.Colors.Symbol, 'Symbol');
  SaveColor(ARAHLEditor.Colors.Comment, 'Comment');
  SaveColor(ARAHLEditor.Colors.Reserved, 'Reserved');
  SaveColor(ARAHLEditor.Colors.Identifier, 'Identifier');
  SaveColor(ARAHLEditor.Colors.Preproc, 'Preproc');
  SaveColor(ARAHLEditor.Colors.FunctionCall, 'FunctionCall');
  SaveColor(ARAHLEditor.Colors.Declaration, 'Declaration');
  SaveColor(ARAHLEditor.Colors.Statement, 'Statement');
  SaveColor(ARAHLEditor.Colors.PlainText, 'PlainText');
end;

procedure TJvHLEdPropDlg.LoadHighlighterColors(ARAHLEditor: TJvHLEditor; AHighlighter: THighlighter);
var
  Section: string;

  procedure LoadColor(AColor: TJvSymbolColor; DefaultForeColor,
    DefaultBackColor: TColor; DefaultStyle: TFontStyles; const Prefix: string);
  var
    S, S1: string;
  begin
    S := FRegAuto.DefaultValue[Section + Prefix, ColorToString(DefaultForeColor) + ', ' + ColorToString(DefaultBackColor)
      + ', ' + IntToStr(byte(DefaultStyle))];
    S1 := Trim(SubStr(S, 0, ','));
    if S1 <> '' then
      AColor.ForeColor := StringToColor(S1)
    else
      AColor.ForeColor := DefaultForeColor;
    S1 := Trim(SubStr(S, 1, ','));
    if S1 <> '' then
      AColor.BackColor := StringToColor(S1)
    else
      AColor.BackColor := DefaultBackColor;
    S1 := Trim(SubStr(S, 2, ','));
    if S1 <> '' then
      AColor.Style := TFontStyles(byte(StrToInt(S1)))
    else
      AColor.Style := DefaultStyle;
  end;

begin
  if FRegAuto = nil then
    raise Exception.Create(SHLEdPropDlg_RegAutoNotAssigned);
  Section := AddSlash2(FRegAutoSection) + HighLighters[AHighLighter];
  LoadColor(ARAHLEditor.Colors.Number, clNavy, clWindow, [], 'Number');
  LoadColor(ARAHLEditor.Colors.Strings, clMaroon, clWindow, [], 'Strings');
  LoadColor(ARAHLEditor.Colors.Symbol, clBlue, clWindow, [], 'Symbol');
  LoadColor(ARAHLEditor.Colors.Comment, clOlive, clWindow, [fsItalic], 'Comment');
  LoadColor(ARAHLEditor.Colors.Reserved, clWindowText, clWindow, [fsBold], 'Reserved');
  LoadColor(ARAHLEditor.Colors.Identifier, clWindowText, clWindow, [], 'Identifier');
  LoadColor(ARAHLEditor.Colors.Preproc, clGreen, clWindow, [], 'Preproc');
  LoadColor(ARAHLEditor.Colors.FunctionCall, clWindowText, clWindow, [], 'FunctionCall');
  LoadColor(ARAHLEditor.Colors.Declaration, clWindowText, clWindow, [], 'Declaration');
  LoadColor(ARAHLEditor.Colors.Statement, clWindowText, clWindow, [], 'Statement');
  LoadColor(ARAHLEditor.Colors.PlainText, clWindowText, clWindow, [], 'PlainText');
  ARAHLEditor.Color := StringToColor(FRegAuto.ReadString(Section + 'BackColor', 'clWindow'));
  ARAHLEditor.Font.Name := FRegAuto.ReadString(Section + 'FontName', 'Courier New');
  ARAHLEditor.Font.Charset := FRegAuto.ReadInteger(Section + 'Charset', DEFAULT_CHARSET);
  ARAHLEditor.Font.Size := FRegAuto.ReadInteger(Section + 'FontSize', 10);
  ARAHLEditor.RightMarginColor := StringToColor(FRegAuto.ReadString(Section + 'RightMarginColor', 'clSilver'));
end;

function TJvHLEdPropDlg.Execute: Boolean;
var
  F: Integer;
  Form: TJvHLEditorParamsForm;
begin
  if FRAHLEditor = nil then
    raise Exception.Create(SHLEdPropDlg_RAHLEditorNotAssigned);
  Form := TJvHLEditorParamsForm.Create(Application);
  with Form do
  try
    FHighlighter := FRAHLEditor.Highlighter;
    Params := Self;
//    raColorSamples.Strings := FColorSamples;
    ParamsToControls;
    if FReadFrom = rfHLEditor then
      RAHLEditor1.Assign(FRAHLEditor);

    tsEditor.TabVisible := epEditor in FPages;
    tsColors.TabVisible := epColors in FPages;

    F := FActivePage;

    if Assigned(FOnDialogPopup) then
      FOnDialogPopup(Self, Form);

    if FRegAuto <> nil then
      F := FRegAuto.ReadInteger(AddSlash2(FRegAutoSection) + 'Params' + 'ActivePage', F);
    F := Max(Min(F, Pages.PageCount - 1), 0);
    if not Pages.Pages[F].TabVisible then
      Pages.ActivePage := Pages.FindNextPage(Pages.Pages[F], True, True)
    else
      Pages.ActivePage := Pages.Pages[F];

    Result := ShowModal = mrOk;
    if Result then
    begin
      ControlsToParams;
      FRAHLEditor.Assign(RAHLEditor1);
    end;

    if (FRegAuto <> nil) and (Pages.ActivePage <> nil) then
         FRegAuto.WriteInteger(AddSlash2(FRegAutoSection) + 'Params' +
           'ActivePage', Pages.ActivePage.PageIndex);

    if Assigned(FOnDialogClosed) then
      FOnDialogClosed(Self, Form, Result);
  finally
    Free;
  end;
end;

procedure TJvHLEdPropDlg.LoadCurrentHighlighterColors;
begin
  LoadHighlighterColors(FRAHLEditor, FRAHLEditor.Highlighter);
end;

procedure TJvHLEdPropDlg.SaveCurrentHighlighterColors;
begin
  SaveHighlighterColors(FRAHLEditor, FRAHLEditor.Highlighter);
end;

procedure TJvHLEdPropDlg.SetColorSamples(const Value: TStrings);
begin
  FColorSamples.Assign(Value);
end;

function TJvHLEdPropDlg.IsPagesStored: Boolean;
begin
  Result := FPages = [epEditor, epColors];
end;

//=== TJvHLEditorParamsForm ==================================================

procedure TJvHLEditorParamsForm.ParamsToControls;
var
  I: Integer;
begin
  cbDoubleClickLine.Checked := Params.FRAHLEditor.DoubleClickLine;
  cbUndoAfterSave.Checked := Params.FRAHLEditor.UndoAfterSave;
  cbKeepTrailingBlanks.Checked := Params.FRAHLEditor.KeepTrailingBlanks;
  cbAutoIndent.Checked := Params.FRAHLEditor.AutoIndent;
  cbSmartTab.Checked := Params.FRAHLEditor.SmartTab;
  cbBackspaceUnindents.Checked := Params.FRAHLEditor.BackspaceUnindents;
  cbGroupUndo.Checked := Params.FRAHLEditor.GroupUndo;
  cbCursorBeyondEOF.Checked := Params.FRAHLEditor.CursorBeyondEOF;
  cbSytaxHighlighting.Checked := Params.FRAHLEditor.SyntaxHighlighting;
  eTabStops.Text := Params.FRAHLEditor.TabStops;
  cbColorSettings.ItemIndex := Integer(FHighlighter);
  cbColorSettingsChange(nil);
  RAHLEditor1.RightMargin := Params.FRAHLEditor.RightMargin;
  cbColorSettings.Visible := Params.FHighlighterCombo;
  lblColorSpeedSettingsFor.Visible := Params.FHighlighterCombo;
  if not Params.FHighlighterCombo then
  begin
    for I := 0 to tsColors.ControlCount - 1 do
      tsColors.Controls[I].Top := tsColors.Controls[I].Top - Pixels(tsColors, 24);
    RAHLEditor1.Height := RAHLEditor1.Height + Pixels(tsColors, 24);
  end;
end;

procedure TJvHLEditorParamsForm.ControlsToParams;
begin
  Params.FRAHLEditor.DoubleClickLine := cbDoubleClickLine.Checked;
  Params.FRAHLEditor.UndoAfterSave := cbUndoAfterSave.Checked;
  Params.FRAHLEditor.KeepTrailingBlanks := cbKeepTrailingBlanks.Checked;
  Params.FRAHLEditor.AutoIndent := cbAutoIndent.Checked;
  Params.FRAHLEditor.SmartTab := cbSmartTab.Checked;
  Params.FRAHLEditor.BackspaceUnindents := cbBackspaceUnindents.Checked;
  Params.FRAHLEditor.GroupUndo := cbGroupUndo.Checked;
  Params.FRAHLEditor.CursorBeyondEOF := cbCursorBeyondEOF.Checked;
  Params.FRAHLEditor.SyntaxHighlighting := cbSytaxHighlighting.Checked;
  Params.FRAHLEditor.TabStops := eTabStops.Text;
  if Params.FRegAuto <> nil then
    Params.SaveHighlighterColors(RAHLEditor1, RAHLEditor1.HighLighter);
end;

procedure TJvHLEditorParamsForm.FormCreate(Sender: TObject);
begin
  LoadLocale;
  RAHLEditor1 := TJvSampleViewer.Create(Self);
  RAHLEditor1.Parent := tsColors;
  RAHLEditor1.SetBounds(8, 176, 396, 110);
  RAHLEditor1.TabStop := False;
  cbKeyboardLayout.ItemIndex := 0;
  cbColorSettings.ItemIndex := 0;
  lbElements.ItemIndex := 0;
  lbElementsClick(nil);
end;

procedure TJvHLEditorParamsForm.NotImplemented(Sender: TObject);
begin
  //(Sender as TCheckBox).Checked := True;
  //raise Exception.Create(SHLEdPropDlg_OptionCantBeChanged);
end;

{ Color tab }

{ Color grid }

function TJvHLEditorParamsForm.GetCell(const Index: Integer): TPanel;
begin
  Result := FindComponent('Cell' + IntToStr(Index)) as TPanel;
  if Result = nil then
    raise Exception.Create('Grid cell not found');
end;

function TJvHLEditorParamsForm.ColorToIndex(const AColor: TColor): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to 15 do
    if GetCell(I).Color = AColor then
    begin
      Result := I;
      Exit;
    end;
end;

function TJvHLEditorParamsForm.GetColorIndex(const ColorName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to 15 do
    if (GetCell(I).Caption = 'FB') or (GetCell(I).Caption = ColorName) then
    begin
      Result := I;
      Exit;
    end;
end;

function TJvHLEditorParamsForm.GetForegroundIndex: Integer;
begin
  Result := GetColorIndex('FC');
end;

function TJvHLEditorParamsForm.GetBackgroundIndex: Integer;
begin
  Result := GetColorIndex('BC');
end;

function TJvHLEditorParamsForm.GetColorColor(const ColorName: string): TColor;
var
  Index: Integer;
begin
  Index := GetColorIndex(ColorName);
  if Index > -1 then
    Result := GetCell(Index).Color
  else
    Result := clBlack;
end;

function TJvHLEditorParamsForm.GetForegroundColor: TColor;
begin
  Result := GetColorColor('FC');
end;

function TJvHLEditorParamsForm.GetBackgroundColor: TColor;
begin
  Result := GetColorColor('BC');
end;

procedure TJvHLEditorParamsForm.SetColorIndex(const Index: Integer;
  const ColorName, OtherColorName: string);
var
  I: Integer;
begin
  for I := 0 to 15 do
    if (GetCell(I).Caption = 'FB') or (GetCell(I).Caption = ColorName) then
      GetCell(I).Caption := ColorName
    else
      GetCell(I).Caption := '';
  if Index > -1 then
    if GetCell(Index).Caption = ColorName then
      GetCell(Index).Caption := 'FB'
    else
      GetCell(Index).Caption := OtherColorName;
end;

procedure TJvHLEditorParamsForm.SetForegroundIndex(const Index: Integer);
begin
  SetColorIndex(Index, 'BC', 'FC');
end;

procedure TJvHLEditorParamsForm.SetBackgroundIndex(const Index: Integer);
begin
  SetColorIndex(Index, 'FC', 'BC');
end;

procedure TJvHLEditorParamsForm.CellMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    SetForegroundIndex((Sender as TPanel).Tag)
  else if Button = mbRight then
    SetBackgroundIndex((Sender as TPanel).Tag);
  ColorChanged(Sender);
end;

{ color grid ### }

procedure TJvHLEditorParamsForm.lbElementsClick(Sender: TObject);
var
  FC, BC: TColor;
  ST: TFontStyles;
begin
  InChanging := True;
  try
    SC := nil;
    ST := [];
    FC := clWindowText;
    BC := clWindow;
    case lbElements.ItemIndex of
      0: { Whitespace }
        begin
          FC := RAHLEditor1.Font.Color;
          BC := RAHLEditor1.Color;
        end;
      1: { Comment }
        SC := RAHLEditor1.Colors.Comment;
      2: { Reserved word }
        SC := RAHLEditor1.Colors.Reserved;
      3: { Identifier }
        SC := RAHLEditor1.Colors.Identifier;
      4: { Symbol }
        SC := RAHLEditor1.Colors.Symbol;
      5: { String }
        SC := RAHLEditor1.Colors.Strings;
      6: { Number }
        SC := RAHLEditor1.Colors.Number;
      7: { Preprocessor }
        SC := RAHLEditor1.Colors.Preproc;
      8: { Declaration }
        SC := RAHLEditor1.Colors.Declaration;
      9: { Function call }
        SC := RAHLEditor1.Colors.FunctionCall;
      10: { VB Statement }
        SC := RAHLEditor1.Colors.Statement;
      11: { PlainText }
        SC := RAHLEditor1.Colors.PlainText;
      12: { Marked block }
        begin
          FC := RAHLEditor1.SelForeColor;
          BC := RAHLEditor1.SelBackColor;
        end;
      13: { Right margin }
        begin
          FC := RAHLEditor1.RightMarginColor;
          BC := -1;
        end;
    end;
    if SC <> nil then
    begin
      FC := SC.ForeColor;
      BC := SC.BackColor;
      ST := SC.Style;
    end;
    cbDefForeground.Checked := ((lbElements.ItemIndex < 12) and (FC = clWindowText)) or
      ((lbElements.ItemIndex = 12) and (FC = clHighlightText));
    cbDefBackground.Checked := ((lbElements.ItemIndex < 12) and (BC = clWindow)) or
      ((lbElements.ItemIndex = 12) and (BC = clHighlight));
    if not cbDefForeground.Checked then
      SetForegroundIndex(ColorToIndex(FC))
    else
      SetForegroundIndex(-1);
    if not cbDefBackground.Checked then
      SetBackgroundIndex(ColorToIndex(BC))
    else
      SetBackgroundIndex(-1);
    cbBold.Checked := fsBold in ST;
    cbItalic.Checked := fsItalic in ST;
    cbUnderline.Checked := fsUnderline in ST;
  finally
    InChanging := False;
  end;
end;

procedure TJvHLEditorParamsForm.ColorChanged(Sender: TObject);
var
  FC, BC: TColor;
  ST: TFontStyles;
begin
  if InChanging then
    Exit;
  InChanging := True;
  try
    ST := [];
    if GetForegroundIndex <> -1 then
      cbDefForeground.Checked := False;
    if GetBackgroundIndex <> -1 then
      cbDefBackground.Checked := False;
    if cbDefForeground.Checked then
    begin
      if lbElements.ItemIndex = 12 then
        FC := clHighlightText
      else
        FC := clWindowText;
      SetForegroundIndex(-1);
    end
    else
      FC := GetForegroundColor;
    if cbDefBackground.Checked then
    begin
      if lbElements.ItemIndex = 12 then { marked block }
        BC := clHighlight
      else
        BC := clWindow;
      SetBackgroundIndex(-1);
    end
    else
      BC := GetBackgroundColor;
    if cbBold.Checked then
      Include(ST, fsBold);
    if cbItalic.Checked then
      Include(ST, fsItalic);
    if cbUnderline.Checked then
      Include(ST, fsUnderline);
    if SC <> nil then
    begin
      SC.Style := ST;
      SC.ForeColor := FC;
      SC.BackColor := BC;
    end
    else if lbElements.ItemIndex = 12 then { marked block }
    begin
      RAHLEditor1.SelForeColor := FC;
      RAHLEditor1.SelBackColor := BC;
    end
    else if lbElements.ItemIndex = 0 then { whitespace }
      RAHLEditor1.Color := BC
    else if lbElements.ItemIndex = 13 then { right margin }
      RAHLEditor1.RightMarginColor := FC;
    RAHLEditor1.Invalidate;
  finally
    InChanging := False;
  end;
end;

procedure TJvHLEditorParamsForm.DefClick(Sender: TObject);
begin
  if InChanging then
    Exit;
  if cbDefForeground.Checked then
    SetForegroundIndex(-1);
  if cbDefBackground.Checked then
    SetBackgroundIndex(-1);
  ColorChanged(nil);
end;

procedure TJvHLEditorParamsForm.lbElementsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox).Canvas do { draw on control canvas, not on the form }
  begin
    FillRect(Rect); { clear the rectangle }
    TextOut(Rect.Left, Rect.Top, (Control as TListBox).Items[Index]) { display the text }
  end;
end;

procedure TJvHLEditorParamsForm.cbColorSettingsChange(Sender: TObject);
var
  I: Integer;
begin
    if (Sender <> nil) and (Params.FRegAuto <> nil) then
      Params.SaveHighlighterColors(RAHLEditor1, RAHLEditor1.HighLighter);
//  raColorSamples.ReadSection(cbColorSettings.Text, RAHLEditor1.Lines);
  RAHLEditor1.Highlighter := THighlighter(cbColorSettings.ItemIndex);
  if RAHLEditor1.HighLighter = hlIni then
    for I := 0 to RAHLEditor1.Lines.Count - 1 do
      RAHLEditor1.Lines[I] := Copy(RAHLEditor1.Lines[I], 2, 10000);
  if Params.FRegAuto <> nil then
     Params.LoadHighlighterColors(RAHLEditor1, RAHLEditor1.HighLighter);
  lbElementsClick(nil);
end;

procedure TJvHLEditorParamsForm.LoadLocale;
begin
  Caption := SHLEdPropDlg_Caption;
  tsEditor.Caption := SHLEdPropDlg_tsEditor;
  tsColors.Caption := SHLEdPropDlg_tsColors;
  lblEditorSpeedSettings.Caption := SHLEdPropDlg_lblEditorSpeedSettings;
  cbKeyboardLayout.Items[0] := SHLEdPropDlg_cbKeyboardLayoutDefault;
  gbEditor.Caption := SHLEdPropDlg_gbEditor;
  cbAutoIndent.Caption := SHLEdPropDlg_cbAutoIndent;
  cbSmartTab.Caption := SHLEdPropDlg_cbSmartTab;
  cbBackspaceUnindents.Caption := SHLEdPropDlg_cbBackspaceUnindents;
  cbGroupUndo.Caption := SHLEdPropDlg_cbGroupUndo;
  cbCursorBeyondEOF.Caption := SHLEdPropDlg_cbCursorBeyondEOF;
  cbUndoAfterSave.Caption := SHLEdPropDlg_cbUndoAfterSave;
  cbKeepTrailingBlanks.Caption := SHLEdPropDlg_cbKeepTrailingBlanks;
  cbDoubleClickLine.Caption := SHLEdPropDlg_cbDoubleClickLine;
  cbSytaxHighlighting.Caption := SHLEdPropDlg_cbSytaxHighlighting;
  lblTabStops.Caption := SHLEdPropDlg_lblTabStops;
  lblColorSpeedSettingsFor.Caption := SHLEdPropDlg_lblColorSpeedSettingsFor;
  lblElement.Caption := SHLEdPropDlg_lblElement;
  lblColor.Caption := SHLEdPropDlg_lblColor;
  gbTextAttributes.Caption := SHLEdPropDlg_gbTextAttributes;
  gbUseDefaultsFor.Caption := SHLEdPropDlg_gbUseDefaultsFor;
  cbBold.Caption := SHLEdPropDlg_cbBold;
  cbItalic.Caption := SHLEdPropDlg_cbItalic;
  cbUnderline.Caption := SHLEdPropDlg_cbUnderline;
  cbDefForeground.Caption := SHLEdPropDlg_cbDefForeground;
  cbDefBackground.Caption := SHLEdPropDlg_cbDefBackground;
  bOK.Caption := SOk;
  bCancel.Caption := SCancel;
end;
(*
  object raColorSamples: TJvRegAuto
    RegPath = 'Software\nbs\RANotepad'
    Storage = raIniStrings
    IniFile = '$HOME/.JvInterpreterTest'
    IniStrings.Strings = (
      '[Default]'
      'Plain text'
      'Selected text'
      ''
      '[Pascal]'
      '{ Syntax highlighting }'
      'procedure TMain.RAHLEditor1ChangeStatus(Sender: TObject);'
      'const'
      '  Modi: array[boolean] of string[10] = ('#39#39', '#39'Modified'#39');'
      '  Modes: array[boolean] of string[10] = ('#39'Overwrite'#39', '#39'Insert'#39');'
      'begin'
      '  with StatusBar, RAHLEditor1 do'
      '  begin'
      '    Panels[0].Text := IntToStr(CaretY) + '#39':'#39' + IntToStr(CaretX);'
      '    Panels[1].Text := Modi[Modified];'
      '    if ReadOnly then'
      '      Panels[2].Text := '#39'ReadOnly'#39
      '    else if Recording then'
      '      Panels[2].Text := '#39'Recording'#39
      '    else'
      '      Panels[2].Text := Modes[InsertMode];'
      '    miFileSave.Enabled := Modified;'
      '  end;'
      'end;'
      '[]'
      ''
      '[CBuilder]'
      '/* Syntax highlighting */'
      '#include "zlib.h"'
      ''
      '#define local static'
      ''
      'local int crc_table_empty = 1;'
      ''
      'local void make_crc_table()'
      '{'
      '  uLong c;'
      '  int n, k;'
      '  uLong poly;            /* polynomial exclusive-or pattern */'
      '  /* terms of polynomial defining this crc (except x^32): */'
      '  static Byte p[] = {0,1,2,4,5,7,8,10,11,12,16,22,23,26};'
      ''
      '  /* make exclusive-or pattern from polynomial (0xedb88320L) */'
      '  poly = 0L;'
      '  for (n = 0; n < sizeof(p)/sizeof(Byte); n++)'
      '    poly |= 1L << (31 - p[n]);'
      ''
      '  for (n = 0; n < 256; n++)'
      '  {'
      '    c = (uLong)n;'
      '    for (k = 0; k < 8; k++)'
      '      c = c & 1 ? poly ^ (c >> 1) : c >> 1;'
      '    crc_table[n] = c;'
      '  }'
      '  crc_table_empty = 0;'
      '}'
      '[]'
      ''
      '[Sql]'
      '/* Syntax highlighting */'
      'declare external function Copy'
      '  cstring(255), integer, integer'
      '  returns cstring(255)'
      '  entry_point "Copy" module_name "nbsdblib";'
      '[]'
      ''
      '[Python]'
      '# Syntax highlighting'
      ''
      'from Tkinter import *'
      'from Tkinter import _cnfmerge'
      ''
      'class Dialog(Widget):'
      '  def __init__(self, master=None, cnf={}, **kw):'
      '    cnf = _cnfmerge((cnf, kw))'
      '    self.widgetName = '#39'__dialog__'#39
      '    Widget._setup(self, master, cnf)'
      '    self.num = self.tk.getint('
      '      apply(self.tk.call,'
      '            ('#39'tk_dialog'#39', self._w,'
      '             cnf['#39'title'#39'], cnf['#39'text'#39'],'
      '             cnf['#39'bitmap'#39'], cnf['#39'default'#39'])'
      '            + cnf['#39'strings'#39']))'
      '    try: Widget.destroy(self)'
      '    except TclError: pass'
      '  def destroy(self): pass'
      '[]'
      ''
      '[Java]'
      '/* Syntax highlighting */'
      'public class utils {'
      
        '  public static String GetPropsFromTag(String str, String props)' +
        ' {'
      '    int bi;'
      '    String Res = "";'
      '    bi = str.indexOf(props);'
      '    if (bi > -1) {'
      '      str = str.substring(bi);'
      '      bi  = str.indexOf("\"");'
      '      if (bi > -1) {'
      '        str = str.substring(bi+1);'
      '        Res = str.substring(0, str.indexOf("\""));'
      '      } else Res = "true";'
      '    }'
      '    return Res;'
      '  }'
      '[]'
      ''
      '[Html]'
      '<html>'
      '<head>'
      '<meta name="GENERATOR" content="Microsoft FrontPage 3.0">'
      '<title>JVCLmp;A Library home page</title>'
      '</head>'
      ''
      
        '<body background="zertxtr.gif" bgcolor="#000000" text="#FFFFFF" ' +
        'link="#FF0000"'
      'alink="#FFFF00">'
      ''
      
        '<p align="left">Download last JVCLmp;A Library version now - <fo' +
        'nt face="Arial"'
      
        'color="#00FFFF"><a href="http://www.torry.ru/vcl/packs/ralib.zip' +
        '"><small>ralib110.zip</small></a>'
      
        '</font><font face="Arial" color="#008080"><small><small>(575 Kb)' +
        '</small></small></font>.</p>'
      ''
      '</body>'
      '</html>'
      '[]'
      ''
      '[Perl]'
      '#!/usr/bin/perl'
      '# Syntax highlighting'
      ''
      'require "webtester.pl";'
      ''
      '$InFile = "/usr/foo/scripts/index.shtml";'
      '$OutFile = "/usr/foo/scripts/sitecheck.html";'
      '$MapFile = "/usr/foo/scripts/sitemap.html";'
      ''
      'sub MainProg {'
      #9'require "find.pl";'
      #9'&Initialize;'
      #9'&SiteCheck;'
      #9'if ($MapFile) { &SiteMap; }'
      #9'exit;'
      '}'
      '[Ini]'
      ' ; Syntax highlighting'
      ' [drivers]'
      ' wave=mmdrv.dll'
      ' timer=timer.drv'
      ''
      ' plain text'
      '[Coco/R]'
      'TOKENS'
      '  NUMBER = digit { digit } .'
      '  EOL = eol .'
      ''
      'PRODUCTIONS'
      ''
      'ExprPostfix   ='
      '                       (. Output := '#39#39'; .)'
      '      Expression<Output>  EOL'
      '                       (. ShowOutput(Output); .)'
      '    .'
      '[]')
*)
end.

