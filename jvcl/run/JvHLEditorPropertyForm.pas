{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHLEdPropDlg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvHLEdPropDlg
description : Properties dialog for TJvHLEditor component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
unit JvHLEditorPropertyForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, ComCtrls, Controls,
  Forms, StdCtrls, ExtCtrls,
  JvFormPlacement, JvEditorCommon, JvComponent, JvHLEditor;

type
  TJvHLEdPropDlg = class;

  TJvHLEditorParamsForm = class(TJvForm)
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
    procedure FormCreate(Sender: TObject);
    procedure NotImplemented(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure lbElementsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ColorChange(Sender: TObject);
    procedure cbColorSettingsChange(Sender: TObject);
    procedure DefClick(Sender: TObject);
    procedure CellMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    JvHLEditorPreview: TJvHLEditor;
    FHighlighter: TJvHighlighter;
    SC: TJvSymbolColor;
    InChanging: Boolean;
    Params: TJvHLEdPropDlg;
    FColorSamples: TStringList;
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
    function GetColorSamples: TStrings;
    procedure SetColorSamples(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParamsToControls;
    procedure ControlsToParams;
    property ColorSamples: TStrings read GetColorSamples write SetColorSamples;
  end;

  TJvHLEdActivePage = 0..1;
  TJvHLEdReadFrom = (rfStorage, rfHLEditor);
  TJvHLEdPages = set of (epEditor, epColors);
  TOnDialogPopup = procedure(Sender: TObject; Form: TForm) of object;
  TOnDialogClosed = procedure(Sender: TObject; Form: TForm; Apply: Boolean) of object;

  TJvHLEdPropDlg = class(TComponent)
  private
    FJvHLEditor: TJvCustomEditorBase;
    FStorage: TJvFormStorage;
    FColorSamples: TStringList;
    FHighlighterCombo: Boolean;
    FActivePage: TJvHLEdActivePage;
    FReadFrom: TJvHLEdReadFrom;
    FPages: TJvHLEdPages;
    FStorageSection: string; { ini section for FStorage }
    FOnDialogPopup: TOnDialogPopup;
    FOnDialogClosed: TOnDialogClosed;
    function GetColorSamples: TStrings;
    procedure SetColorSamples(Value: TStrings);
    function IsPagesStored: Boolean;
    procedure SetJvHLEditor(const Value: TJvCustomEditorBase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure Restore;
    procedure LoadHighlighterColors(AJvHLEditor: TJvCustomEditorBase; AHighlighter: TJvHighlighter);
    procedure SaveHighlighterColors(AJvHLEditor: TJvCustomEditorBase; AHighlighter: TJvHighlighter);
    function Execute: Boolean;
    procedure LoadCurrentHighlighterColors;
    procedure SaveCurrentHighlighterColors;
  published
    property JvHLEditor: TJvCustomEditorBase read FJvHLEditor write SetJvHLEditor;
    property Storage: TJvFormStorage read FStorage write FStorage;
    property ColorSamples: TStrings read GetColorSamples write SetColorSamples;
    property HighlighterCombo: Boolean read FHighlighterCombo write FHighlighterCombo default True;
    property ActivePage: TJvHLEdActivePage read FActivePage write FActivePage default 0;
    property ReadFrom: TJvHLEdReadFrom read FReadFrom write FReadFrom default rfStorage;
    property Pages: TJvHLEdPages read FPages write FPages stored IsPagesStored;
    property StorageSection: string read FStorageSection write FStorageSection;
    property OnDialogPopup: TOnDialogPopup read FOnDialogPopup write FOnDialogPopup;
    property OnDialogClosed: TOnDialogClosed read FOnDialogClosed write FOnDialogClosed;
  end;

const
  Highlighters: array [TJvHighlighter] of PChar =
   ('None', 'Pascal', 'CBuilder', 'Sql', 'Python', 'Java', 'VB', 'Html',
    'Perl', 'Ini', 'CocoR', 'PHP', 'NQC', 'C#', 'User Defined');

  HighlighterNames: array [TJvHighlighter] of PChar =
   ('Default', 'Pascal', 'CBuilder', 'Sql', 'Python', 'Java', 'VB', 'Html',
    'Perl', 'Ini', 'Coco/R', 'PHP', 'NQC', 'C#', 'Custom');

implementation

uses
  Math,
  JvJVCLUtils, JvJCLUtils, JvResources, JvTypes;

{$R *.dfm}

function GetHardCodedExamples: string; forward;

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

//=== { TJvSampleViewer } ====================================================

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
    {WM_LBUTTONDOWN,}WM_LBUTTONUP, WM_RBUTTONDOWN, WM_RBUTTONUP,
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
  Str: string;
begin
  { also prevent user interact }
  { detect symbol type }
  Mouse2Caret(Msg.XPos, Msg.YPos, XX, YY);
  if Cardinal(YY) > Cardinal(TmpEd.Lines.Count) then
    Exit;
  if (XX = RightMargin) or (XX - 1 = RightMargin) then
    F := 13
  else
  begin
    TmpEd.Lines := Lines;
    TmpEd.Highlighter := Highlighter;
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
    TJvSampleViewer(TmpEd).GetLineAttr(Str, YY, 0, Max_X - 1);
    F := TJvSampleViewer(TmpEd).LineAttrs[XX].FC;
  end;
  (Owner as TJvHLEditorParamsForm).lbElements.ItemIndex := F;
  (Owner as TJvHLEditorParamsForm).lbElementsClick((Owner as TJvHLEditorParamsForm).lbElements);
end;

//=== { TJvHLEdPropDlg } =====================================================

constructor TJvHLEdPropDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHighlighterCombo := True;
  FColorSamples := TStringList.Create;
  FColorSamples.Text := GetHardCodedExamples;
  FPages := [epColors];
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
  HLed: IJvHLEditor;
begin
  if Storage <> nil then
  begin
//    raise Exception.CreateRes(@RsEHLEdPropDlg_RegAutoNotAssigned);
    FJvHLEditor.GetInterface(IJvHLEditor, HLed);
    S := AddSlash2(FStorageSection) + cParams;
    with Storage do
    begin
      StoredValue['DoubleClickLine'] := FJvHLEditor.DoubleClickLine;
      StoredValue['UndoAfterSave'] := FJvHLEditor.UndoAfterSave;
      StoredValue['KeepTrailingBlanks'] := FJvHLEditor.KeepTrailingBlanks;
      StoredValue['AutoIndent'] := FJvHLEditor.AutoIndent;
      StoredValue['SmartTab'] := FJvHLEditor.SmartTab;
      StoredValue['BackspaceUnindents'] := FJvHLEditor.BackSpaceUnindents;
      StoredValue['GroupUndo'] := FJvHLEditor.GroupUndo;
      StoredValue['CursorBeyondEOF'] := FJvHLEditor.CursorBeyondEOF;
      StoredValue['SyntaxHighlighting'] := HLed.SyntaxHighlighting;
      StoredValue['TabStops'] := FJvHLEditor.TabStops;
      StoredValue['RightMargin'] := FJvHLEditor.RightMargin;
    end;
  end;
end;

procedure TJvHLEdPropDlg.Restore;
const
  cParams = 'Params';
var
  S: string;
  HLed: IJvHLEditor;
begin
  if Storage <> nil then
  begin
//    raise Exception.CreateRes(@RsEHLEdPropDlg_RegAutoNotAssigned);
    S := AddSlash2(StorageSection) + cParams;
    FJvHLEditor.GetInterface(IJvHLEditor, HLed);
    with Storage do
    begin
      FJvHLEditor.DoubleClickLine := DefaultValue['DoubleClickLine', FJvHLEditor.DoubleClickLine];
      FJvHLEditor.UndoAfterSave := DefaultValue['UndoAfterSave', FJvHLEditor.UndoAfterSave];
      FJvHLEditor.KeepTrailingBlanks := DefaultValue['KeepTrailingBlanks', FJvHLEditor.KeepTrailingBlanks];
      FJvHLEditor.AutoIndent := DefaultValue['AutoIndent', FJvHLEditor.AutoIndent];
      FJvHLEditor.SmartTab := DefaultValue['SmartTab', FJvHLEditor.SmartTab];
      FJvHLEditor.BackSpaceUnindents := DefaultValue['BackspaceUnindents', FJvHLEditor.BackSpaceUnindents];
      FJvHLEditor.GroupUndo := DefaultValue['GroupUndo', FJvHLEditor.GroupUndo];
      FJvHLEditor.CursorBeyondEOF := DefaultValue['CursorBeyondEOF', FJvHLEditor.CursorBeyondEOF];
      HLed.SyntaxHighlighting := DefaultValue['SyntaxHighlighting', HLed.SyntaxHighlighting];
      FJvHLEditor.TabStops := DefaultValue['TabStops', FJvHLEditor.TabStops];
      FJvHLEditor.RightMargin := DefaultValue['RightMargin', FJvHLEditor.RightMargin];
    end;
  end;
end;

type
  TJvCustomEditorBaseAccessProtected = class(TJvCustomEditorBase);

procedure TJvHLEdPropDlg.SaveHighlighterColors(AJvHLEditor: TJvCustomEditorBase;
  AHighlighter: TJvHighlighter);
var
  Section: string;

  procedure SaveColor(AColor: TJvSymbolColor; const Prefix: string);
  begin
    Storage.StoredValue[Section + Prefix] :=
      ColorToString(AColor.ForeColor) + ', ' + ColorToString(AColor.BackColor) +
      ', ' + IntToStr(byte(AColor.Style));
  end;

var
  ed: TJvCustomEditorBaseAccessProtected;
  HLed: IJvHLEditor;
begin
  if Storage <> nil then
  begin
//    raise Exception.CreateRes(@RsEHLEdPropDlg_RegAutoNotAssigned);
    ed := TJvCustomEditorBaseAccessProtected(AJvHLEditor);
    FJvHLEditor.GetInterface(IJvHLEditor, HLed);
    Section := AddSlash2(Storage.AppStoragePath) + AddSlash2(StorageSection) +
      Highlighters[AHighlighter];
    Storage.StoredValue[Section + 'BackColor'] := ColorToString(ed.Color);
    Storage.StoredValue[Section + 'FontName'] := ed.Font.Name;
    Storage.StoredValue[Section + 'Charset'] := IntToStr(ed.Font.CharSet);
    Storage.StoredValue[Section + 'FontSize'] := ed.Font.Size;
    Storage.StoredValue[Section + 'RightMarginColor'] := ColorToString(ed.RightMarginColor);
    SaveColor(HLed.Colors.Number, 'Number');
    SaveColor(HLed.Colors.Strings, 'Strings');
    SaveColor(HLed.Colors.Symbol, 'Symbol');
    SaveColor(HLed.Colors.Comment, 'Comment');
    SaveColor(HLed.Colors.Reserved, 'Reserved');
    SaveColor(HLed.Colors.Identifier, 'Identifier');
    SaveColor(HLed.Colors.Preproc, 'Preproc');
    SaveColor(HLed.Colors.FunctionCall, 'FunctionCall');
    SaveColor(HLed.Colors.Declaration, 'Declaration');
    SaveColor(HLed.Colors.Statement, 'Statement');
    SaveColor(HLed.Colors.PlainText, 'PlainText');
  end;
end;

procedure TJvHLEdPropDlg.LoadHighlighterColors(AJvHLEditor: TJvCustomEditorBase;
  AHighlighter: TJvHighlighter);
var
  Section: string;

  procedure LoadColor(AColor: TJvSymbolColor; DefaultForeColor,
    DefaultBackColor: TColor; DefaultStyle: TFontStyles; const Prefix: string);
  var
    S, S1: string;
  begin
    S := Storage.DefaultValue[Section + Prefix, ColorToString(DefaultForeColor) + ', ' + ColorToString(DefaultBackColor)
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

var
  ed: TJvCustomEditorBaseAccessProtected;
  HLed: IJvHLEditor;
begin
  if Storage <> nil then
  begin
//    raise Exception.CreateRes(@RsEHLEdPropDlg_RegAutoNotAssigned);
    ed := TJvCustomEditorBaseAccessProtected(AJvHLEditor);
    FJvHLEditor.GetInterface(IJvHLEditor, HLed);
    Section := AddSlash2(StorageSection) + Highlighters[AHighlighter];
    LoadColor(HLed.Colors.Number, clNavy, clWindow, [], 'Number');
    LoadColor(HLed.Colors.Strings, clMaroon, clWindow, [], 'Strings');
    LoadColor(HLed.Colors.Symbol, clBlue, clWindow, [], 'Symbol');
    LoadColor(HLed.Colors.Comment, clOlive, clWindow, [fsItalic], 'Comment');
    LoadColor(HLed.Colors.Reserved, clWindowText, clWindow, [fsBold], 'Reserved');
    LoadColor(HLed.Colors.Identifier, clWindowText, clWindow, [], 'Identifier');
    LoadColor(HLed.Colors.Preproc, clGreen, clWindow, [], 'Preproc');
    LoadColor(HLed.Colors.FunctionCall, clWindowText, clWindow, [], 'FunctionCall');
    LoadColor(HLed.Colors.Declaration, clWindowText, clWindow, [], 'Declaration');
    LoadColor(HLed.Colors.Statement, clWindowText, clWindow, [], 'Statement');
    LoadColor(HLed.Colors.PlainText, clWindowText, clWindow, [], 'PlainText');
    try
      AJvHLEditor.Color := StringToColor(Storage.ReadString(Section + 'BackColor', 'clWindow'));
    except
      on E: EConvertError do
        AJvHLEditor.RightMarginColor := clWindow;
    end;
    ed.Font.Name := Storage.ReadString(Section + 'FontName', 'Courier New');
    ed.Font.CharSet := Storage.ReadInteger(Section + 'Charset', DEFAULT_CHARSET);
    ed.Font.Size := Storage.ReadInteger(Section + 'FontSize', 10);
    try
      ed.RightMarginColor := StringToColor(Storage.ReadString(Section + 'RightMarginColor', 'clSilver'));
    except
      on E: EConvertError do
        AJvHLEditor.RightMarginColor := clSilver;
    end;
  end;
end;

function TJvHLEdPropDlg.Execute: Boolean;
var
  F: Integer;
  Form: TJvHLEditorParamsForm;
  HLed: IJvHLEditor;
begin
  if FJvHLEditor = nil then
    raise EJVCLException.CreateRes(@RsEHLEdPropDlg_RAHLEditorNotAssigned);
  Form := TJvHLEditorParamsForm.Create(Application);
  Form.ColorSamples.Assign(ColorSamples);
  with Form do
  try
    FJvHLEditor.GetInterface(IJvHLEditor, HLed);
    FHighlighter := HLed.Highlighter;
    Params := Self;
    ParamsToControls;
    if FReadFrom = rfHLEditor then
      JvHLEditorPreview.Assign(FJvHLEditor);

    tsEditor.TabVisible := epEditor in FPages;
    tsColors.TabVisible := epColors in FPages;

    F := FActivePage;

    if Assigned(FOnDialogPopup) then
      FOnDialogPopup(Self, Form);

    if Storage <> nil then
      F := Storage.ReadInteger(AddSlash2(StorageSection) + 'Params' + 'ActivePage', F);
    F := Max(Min(F, Pages.PageCount - 1), 0);
    if not Pages.Pages[F].TabVisible then
      Pages.ActivePage := Pages.FindNextPage(Pages.Pages[F], True, True)
    else
      Pages.ActivePage := Pages.Pages[F];

    Result := ShowModal = mrOK;
    if Result then
    begin
      ControlsToParams;
      FJvHLEditor.Assign(JvHLEditorPreview);
    end;

    if (Storage <> nil) and (Pages.ActivePage <> nil) then
      Storage.WriteInteger(AddSlash2(StorageSection) + 'Params' +
        'ActivePage', Pages.ActivePage.PageIndex);

    if Assigned(FOnDialogClosed) then
      FOnDialogClosed(Self, Form, Result);
  finally
    Free;
  end;
end;

procedure TJvHLEdPropDlg.LoadCurrentHighlighterColors;
var
  HLed: IJvHLEditor;
begin
  if FJvHLEditor.GetInterface(IJvHLEditor, HLed) then
    LoadHighlighterColors(FJvHLEditor, HLed.Highlighter);
end;

procedure TJvHLEdPropDlg.SaveCurrentHighlighterColors;
var
  HLed: IJvHLEditor;
begin
  if FJvHLEditor.GetInterface(IJvHLEditor, HLed) then
    SaveHighlighterColors(FJvHLEditor, HLed.Highlighter);
end;

function TJvHLEdPropDlg.GetColorSamples: TStrings;
begin
  Result := FColorSamples;
end;

procedure TJvHLEdPropDlg.SetColorSamples(Value: TStrings);
begin
  FColorSamples.Assign(Value);
end;

function TJvHLEdPropDlg.IsPagesStored: Boolean;
begin
  Result := FPages <> [epColors];
end;

procedure TJvHLEdPropDlg.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = JvHLEditor then
      FJvHLEditor := nil // do not call SetJvHLEditor
    else
    if AComponent = Storage then
      FStorage := nil;
  end;
end;

procedure TJvHLEdPropDlg.SetJvHLEditor(const Value: TJvCustomEditorBase);
var
  HLed: IJvHLEditor;
begin
  if Value <> FJvHLEditor then
  begin
    if Value <> nil then
    begin
      if Value.GetInterface(IJvHLEditor, HLed) then
        FJvHLEditor := Value;
    end
    else
      FJvHLEditor := nil;
  end;
end;

//=== { TJvHLEditorParamsForm } ==============================================

constructor TJvHLEditorParamsForm.Create(AOwner: TComponent);
var
  hl: TJvHighlighter;
begin
  inherited Create(AOwner);
  FColorSamples := TStringList.Create;
  cbColorSettings.Clear;
  for hl:=Low(TJvHighlighter) to Pred(High(TJvHighlighter)) do
    cbColorSettings.Items.AddObject(HighlighterNames[hl], nil);
end;

destructor TJvHLEditorParamsForm.Destroy;
begin
  FColorSamples.Free;
  inherited Destroy;
end;

function TJvHLEditorParamsForm.GetColorSamples: TStrings;
begin
  Result := FColorSamples;
end;

procedure TJvHLEditorParamsForm.SetColorSamples(Value: TStrings);
begin
  FColorSamples.Assign(Value);
end;

procedure TJvHLEditorParamsForm.ParamsToControls;
var
  I: Integer;
  HLed: IJvHLEditor;
begin
  Params.FJvHLEditor.GetInterface(IJvHLEditor, HLed);

  cbDoubleClickLine.Checked := Params.FJvHLEditor.DoubleClickLine;
  cbUndoAfterSave.Checked := Params.FJvHLEditor.UndoAfterSave;
  cbKeepTrailingBlanks.Checked := Params.FJvHLEditor.KeepTrailingBlanks;
  cbAutoIndent.Checked := Params.FJvHLEditor.AutoIndent;
  cbSmartTab.Checked := Params.FJvHLEditor.SmartTab;
  cbBackspaceUnindents.Checked := Params.FJvHLEditor.BackSpaceUnindents;
  cbGroupUndo.Checked := Params.FJvHLEditor.GroupUndo;
  cbCursorBeyondEOF.Checked := Params.FJvHLEditor.CursorBeyondEOF;
  cbSytaxHighlighting.Checked := HLed.SyntaxHighlighting;
  eTabStops.Text := Params.FJvHLEditor.TabStops;
  cbColorSettings.ItemIndex := Integer(FHighlighter);
  cbColorSettingsChange(nil);
  JvHLEditorPreview.RightMargin := Params.FJvHLEditor.RightMargin;
  cbColorSettings.Visible := Params.FHighlighterCombo;
  lblColorSpeedSettingsFor.Visible := Params.FHighlighterCombo;
  if not Params.FHighlighterCombo then
  begin
    for I := 0 to tsColors.ControlCount - 1 do
      tsColors.Controls[I].Top := tsColors.Controls[I].Top - Pixels(tsColors, 24);
    JvHLEditorPreview.Height := JvHLEditorPreview.Height + Pixels(tsColors, 24);
  end;
end;

procedure TJvHLEditorParamsForm.ControlsToParams;
var
  HLed: IJvHLEditor;
begin
  Params.FJvHLEditor.GetInterface(IJvHLEditor, HLed);

  Params.FJvHLEditor.DoubleClickLine := cbDoubleClickLine.Checked;
  Params.FJvHLEditor.UndoAfterSave := cbUndoAfterSave.Checked;
  Params.FJvHLEditor.KeepTrailingBlanks := cbKeepTrailingBlanks.Checked;
  Params.FJvHLEditor.AutoIndent := cbAutoIndent.Checked;
  Params.FJvHLEditor.SmartTab := cbSmartTab.Checked;
  Params.FJvHLEditor.BackSpaceUnindents := cbBackspaceUnindents.Checked;
  Params.FJvHLEditor.GroupUndo := cbGroupUndo.Checked;
  Params.FJvHLEditor.CursorBeyondEOF := cbCursorBeyondEOF.Checked;
  HLed.SyntaxHighlighting := cbSytaxHighlighting.Checked;
  Params.FJvHLEditor.TabStops := eTabStops.Text;
  if Params.Storage <> nil then
    Params.SaveHighlighterColors(JvHLEditorPreview, JvHLEditorPreview.Highlighter);
end;

procedure TJvHLEditorParamsForm.FormCreate(Sender: TObject);
begin
  LoadLocale;
  JvHLEditorPreview := TJvSampleViewer.Create(Self);
  JvHLEditorPreview.Parent := tsColors;
  JvHLEditorPreview.SetBounds(8, 176, 396, 110);
  JvHLEditorPreview.TabStop := False;
  cbKeyboardLayout.ItemIndex := 0;
  cbColorSettings.ItemIndex := 0;
  lbElements.ItemIndex := 0;
  lbElementsClick(nil);
end;

procedure TJvHLEditorParamsForm.NotImplemented(Sender: TObject);
begin
  //(Sender as TCheckBox).Checked := True;
  //raise Exception.CreateRes(@RsEHLEdPropDlg_OptionCantBeChanged);
end;

{ Color tab }

{ Color grid }

function TJvHLEditorParamsForm.GetCell(const Index: Integer): TPanel;
begin
  Result := FindComponent('Cell' + IntToStr(Index)) as TPanel;
  if Result = nil then
    raise EJVCLException.CreateRes(@RsEHLEdPropDlg_GridCellNotFound);
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
  else
  if Button = mbRight then
    SetBackgroundIndex((Sender as TPanel).Tag);
  ColorChange(Sender);
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
          FC := JvHLEditorPreview.Font.Color;
          BC := JvHLEditorPreview.Color;
        end;
      1: { Comment }
        SC := JvHLEditorPreview.Colors.Comment;
      2: { Reserved word }
        SC := JvHLEditorPreview.Colors.Reserved;
      3: { Identifier }
        SC := JvHLEditorPreview.Colors.Identifier;
      4: { Symbol }
        SC := JvHLEditorPreview.Colors.Symbol;
      5: { String }
        SC := JvHLEditorPreview.Colors.Strings;
      6: { Number }
        SC := JvHLEditorPreview.Colors.Number;
      7: { Preprocessor }
        SC := JvHLEditorPreview.Colors.Preproc;
      8: { Declaration }
        SC := JvHLEditorPreview.Colors.Declaration;
      9: { Function call }
        SC := JvHLEditorPreview.Colors.FunctionCall;
      10: { VB Statement }
        SC := JvHLEditorPreview.Colors.Statement;
      11: { PlainText }
        SC := JvHLEditorPreview.Colors.PlainText;
      12: { Marked block }
        begin
          FC := JvHLEditorPreview.SelForeColor;
          BC := JvHLEditorPreview.SelBackColor;
        end;
      13: { Right margin }
        begin
          FC := JvHLEditorPreview.RightMarginColor;
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

procedure TJvHLEditorParamsForm.ColorChange(Sender: TObject);
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
    else
    if lbElements.ItemIndex = 12 then { marked block }
    begin
      JvHLEditorPreview.SelForeColor := FC;
      JvHLEditorPreview.SelBackColor := BC;
    end
    else
    if lbElements.ItemIndex = 0 then { whitespace }
      JvHLEditorPreview.Color := BC
    else
    if lbElements.ItemIndex = 13 then { right margin }
      JvHLEditorPreview.RightMarginColor := FC;
    JvHLEditorPreview.Invalidate;
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
  ColorChange(nil);
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

procedure ReadColorSampleSection(Ini: TStrings; const Section: string; Lines: TStrings);
var
  I: Integer;
  S: string;
  InSection: Boolean;
begin
  Lines.Clear;
  InSection := False;
  for I := 0 to Ini.Count - 1 do
  begin
    S := Ini[I];
    if (S <> '') and (S[1] = '[') and (S[Length(S)] = ']') then
    begin
      if CompareText(Copy(S, 2, Length(S) - 2), Section) = 0 then
        InSection := True
      else
      if InSection then
        Break;
      Continue;
    end;
    if InSection then
      Lines.Add(Ini[I]);
  end;
end;

procedure TJvHLEditorParamsForm.cbColorSettingsChange(Sender: TObject);
var
  I: Integer;
begin
  if (Sender <> nil) and (Params.Storage <> nil) then
    Params.SaveHighlighterColors(JvHLEditorPreview, JvHLEditorPreview.Highlighter);

  if cbColorSettings.Visible then
    ReadColorSampleSection(ColorSamples, cbColorSettings.Text, JvHLEditorPreview.Lines)
  else
    ReadColorSampleSection(ColorSamples, HighlighterNames[FHighlighter], JvHLEditorPreview.Lines);

  JvHLEditorPreview.Highlighter := TJvHighlighter(cbColorSettings.ItemIndex);
  if JvHLEditorPreview.Highlighter = hlIni then
    for I := 0 to JvHLEditorPreview.Lines.Count - 1 do
      JvHLEditorPreview.Lines[I] := Copy(JvHLEditorPreview.Lines[I], 2, 10000);
  if Params.Storage <> nil then
    Params.LoadHighlighterColors(JvHLEditorPreview, JvHLEditorPreview.Highlighter);
  lbElementsClick(nil);
end;

procedure TJvHLEditorParamsForm.LoadLocale;
begin
  Caption := RsHLEdPropDlg_Caption;
  tsEditor.Caption := RsHLEdPropDlg_tsEditor;
  tsColors.Caption := RsHLEdPropDlg_tsColors;
  lblEditorSpeedSettings.Caption := RsHLEdPropDlg_lblEditorSpeedSettings;
  cbKeyboardLayout.Items[0] := RsHLEdPropDlg_cbKeyboardLayoutDefault;
  gbEditor.Caption := RsHLEdPropDlg_gbEditor;
  cbAutoIndent.Caption := RsHLEdPropDlg_cbAutoIndent;
  cbSmartTab.Caption := RsHLEdPropDlg_cbSmartTab;
  cbBackspaceUnindents.Caption := RsHLEdPropDlg_cbBackspaceUnindents;
  cbGroupUndo.Caption := RsHLEdPropDlg_cbGroupUndo;
  cbCursorBeyondEOF.Caption := RsHLEdPropDlg_cbCursorBeyondEOF;
  cbUndoAfterSave.Caption := RsHLEdPropDlg_cbUndoAfterSave;
  cbKeepTrailingBlanks.Caption := RsHLEdPropDlg_cbKeepTrailingBlanks;
  cbDoubleClickLine.Caption := RsHLEdPropDlg_cbDoubleClickLine;
  cbSytaxHighlighting.Caption := RsHLEdPropDlg_cbSytaxHighlighting;
  lblTabStops.Caption := RsHLEdPropDlg_lblTabStops;
  lblColorSpeedSettingsFor.Caption := RsHLEdPropDlg_lblColorSpeedSettingsFor;
  lblElement.Caption := RsHLEdPropDlg_lblElement;
  lblColor.Caption := RsHLEdPropDlg_lblColor;
  gbTextAttributes.Caption := RsHLEdPropDlg_gbTextAttributes;
  gbUseDefaultsFor.Caption := RsHLEdPropDlg_gbUseDefaultsFor;
  cbBold.Caption := RsHLEdPropDlg_cbBold;
  cbItalic.Caption := RsHLEdPropDlg_cbItalic;
  cbUnderline.Caption := RsHLEdPropDlg_cbUnderline;
  cbDefForeground.Caption := RsHLEdPropDlg_cbDefForeground;
  cbDefBackground.Caption := RsHLEdPropDlg_cbDefBackground;
  bOK.Caption := RsButtonOKCaption;
  bCancel.Caption := RsButtonCancelCaption;
end;

function GetHardCodedExamples: string;
begin
  Result :=
    '[Default]'#10 +
    'Plain text'#10 +
    'Selected text'#10 +
    ''#10 +
    '[Pascal]'#10 +
    '{ Syntax highlighting }'#10 +
    '{$DEFINE DELPHI}'#10 +
    'procedure TMain.JvHLEditorPreviewChangeStatus(Sender: TObject);'#10 +
    'const'#10 +
    '  Modi: array [Boolean] of string[10] = ('#39#39', '#39'Modified'#39');'#10 +
    '  Modes: array [Boolean] of string[10] = ('#39'Overwrite'#39', '#39'Insert'#39');'#10 +
    'begin'#10 +
    '  with StatusBar, JvHLEditorPreview do'#10 +
    '  begin'#10 +
    '    Panels[0].Text := IntToStr(CaretY) + '#39':'#39' + IntToStr(CaretX);'#10 +
    '    Panels[1].Text := Modi[Modified];'#10 +
    '    if ReadOnly then'#10 +
    '      Panels[2].Text := '#39'ReadOnly'#39 +
    '    else'#10 +
    '    if Recording then'#10 +
    '      Panels[2].Text := '#39'Recording'#39 +
    '    else'#10 +
    '      Panels[2].Text := Modes[InsertMode];'#10 +
    '    miFileSave.Enabled := Modified;'#10 +
    '  end;'#10 +
    'end;'#10 +
    '[]'#10 +
    ''#10 +
    '[CBuilder]'#10 +
    '/* Syntax highlighting */'#10 +
    '#include "zlib.h"'#10 +
    ''#10 +
    '#define local static'#10 +
    ''#10 +
    'local int crc_table_empty = 1;'#10 +
    ''#10 +
    'local void make_crc_table()'#10 +
    '{'#10 +
    '  uLong c;'#10 +
    '  int n, k;'#10 +
    '  uLong poly;            /* polynomial exclusive-or pattern */'#10 +
    '  /* terms of polynomial defining this crc (except x^32): */'#10 +
    '  static Byte p[] = {0,1,2,4,5,7,8,10,11,12,16,22,23,26};'#10 +
    ''#10 +
    '  /* make exclusive-or pattern from polynomial (0xedb88320L) */'#10 +
    '  poly = 0L;'#10 +
    '  for (n = 0; n < sizeof(p)/sizeof(Byte); n++)'#10 +
    '    poly |= 1L << (31 - p[n]);'#10 +
    ''#10 +
    '  for (n = 0; n < 256; n++)'#10 +
    '  {'#10 +
    '    c = (uLong)n;'#10 +
    '    for (k = 0; k < 8; k++)'#10 +
    '      c = c & 1 ? poly ^ (c >> 1) : c >> 1;'#10 +
    '    crc_table[n] = c;'#10 +
    '  }'#10 +
    '  crc_table_empty = 0;'#10 +
    '}'#10 +
    '[]'#10 +
    ''#10 +
    '[VB]'#10 +
    'Rem Syntax highlighting'#10 +
    'Sub Main()'#10 +
    '  Dim S as String'#10 +
    '  If S = "" Then'#10 +
    '   '#39' Do something'#10 +
    '   MsgBox "Hallo World"'#10 +
    '  End If'#10 +
    'End Sub'#10 +
    '[]'#10 +
    ''#10 +
    '[Sql]'#10 +
    '/* Syntax highlighting */'#10 +
    'declare external function Copy'#10 +
    '  cstring(255), integer, integer'#10 +
    '  returns cstring(255)'#10 +
    '  entry_point "Copy" module_name "nbsdblib";'#10 +
    '[]'#10 +
    ''#10 +
    '[Python]'#10 +
    '# Syntax highlighting'#10 +
    ''#10 +
    'from Tkinter import *'#10 +
    'from Tkinter import _cnfmerge'#10 +
    ''#10 +
    'class Dialog(Widget):'#10 +
    '  def __init__(self, master=None, cnf={}, **kw):'#10 +
    '    cnf = _cnfmerge((cnf, kw))'#10 +
    '    self.widgetName = '#39'__dialog__'#39 +
    '    Widget._setup(self, master, cnf)'#10 +
    '    self.num = self.tk.getint('#10 +
    '      apply(self.tk.call,'#10 +
    '            ('#39'tk_dialog'#39', self._w,'#10 +
    '             cnf['#39'title'#39'], cnf['#39'text'#39'],'#10 +
    '             cnf['#39'bitmap'#39'], cnf['#39'default'#39'])'#10 +
    '            + cnf['#39'strings'#39']))'#10 +
    '    try: Widget.destroy(self)'#10 +
    '    except TclError: pass'#10 +
    '  def destroy(self): pass'#10 +
    '[]'#10 +
    ''#10 +
    '[Java]'#10 +
    '/* Syntax highlighting */'#10 +
    'public class utils {'#10 +
    '  public static String GetPropsFromTag(String str, String props) {'#10 +
    '    int bi;'#10 +
    '    String Res = "";'#10 +
    '    bi = str.indexOf(props);'#10 +
    '    if (bi > -1) {'#10 +
    '      str = str.substring(bi);'#10 +
    '      bi  = str.indexOf("\"");'#10 +
    '      if (bi > -1) {'#10 +
    '        str = str.substring(bi+1);'#10 +
    '        Res = str.substring(0, str.indexOf("\""));'#10 +
    '      } else Res = "true";'#10 +
    '    }'#10 +
    '    return Res;'#10 +
    '  }'#10 +
    '[]'#10 +
    ''#10 +
    '[Html]'#10 +
    '<html>'#10 +
    '<head>'#10 +
    '<meta name="GENERATOR" content="Microsoft FrontPage 3.0">'#10 +
    '<title>JVCLmp;A Library home page</title>'#10 +
    '</head>'#10 +
    ''#10 +
    '<body background="zertxtr.gif" bgcolor="#000000" text="#FFFFFF" link="#FF0000"'#10 +
    'alink="#FFFF00">'#10 +
    ''#10 +
    '<p align="left">Download last JVCLmp;A Library version now - <font face="Arial"'#10 +
    'color="#00FFFF"><a href="http://www.torry.ru/vcl/packs/ralib.zip"><small>ralib110.zip</small></a>'#10 +
    '</font><font face="Arial" color="#008080"><small><small>(575 Kb)</small></small></font>.</p>'#10 +
    ''#10 +
    '</body>'#10 +
    '</html>'#10 +
    '[]'#10 +
    ''#10 +
    '[Perl]'#10 +
    '#!/usr/bin/perl'#10 +
    '# Syntax highlighting'#10 +
    ''#10 +
    'require "webtester.pl";'#10 +
    ''#10 +
    '$InFile = "/usr/foo/scripts/index.shtml";'#10 +
    '$OutFile = "/usr/foo/scripts/sitecheck.html";'#10 +
    '$MapFile = "/usr/foo/scripts/sitemap.html";'#10 +
    ''#10 +
    'sub MainProg {'#10 +
    #9'require "find.pl";'#10 +
    #9'&Initialize;'#10 +
    #9'&SiteCheck;'#10 +
    #9'if ($MapFile) { &SiteMap; }'#10 +
    #9'exit;'#10 +
    '}'#10 +
    '[Ini]'#10 +
    ' ; Syntax highlighting'#10 +
    ' [drivers]'#10 +
    ' wave=mmdrv.dll'#10 +
    ' timer=timer.drv'#10 +
    ''#10 +
    ' plain text'#10 +
    '[Coco/R]'#10 +
    'TOKENS'#10 +
    '  NUMBER = digit { digit } .'#10 +
    '  EOL = eol .'#10 +
    ''#10 +
    'PRODUCTIONS'#10 +
    ''#10 +
    'ExprPostfix   ='#10 +
    '                       (. Output := '#39#39'; .)'#10 +
    '      Expression<Output>  EOL'#10 +
    '                       (. ShowOutput(Output); .)'#10 +
    '    .'#10 +
    '[]';
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
      'procedure TMain.JvHLEditorPreviewChangeStatus(Sender: TObject);'
      'const'
      '  Modi: array [Boolean] of string[10] = ('#39#39', '#39'Modified'#39');'
      '  Modes: array [Boolean] of string[10] = ('#39'Overwrite'#39', '#39'Insert'#39');'
      'begin'
      '  with StatusBar, JvHLEditorPreview do'
      '  begin'
      '    Panels[0].Text := IntToStr(CaretY) + '#39':'#39' + IntToStr(CaretX);'
      '    Panels[1].Text := Modi[Modified];'
      '    if ReadOnly then'
      '      Panels[2].Text := '#39'ReadOnly'#39
      '    else'
      '    if Recording then'
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
      '[VB]'
      'Rem Syntax highlighting'
      'Sub Main()'
      '  Dim S as String'
      '  If S = "" Then'
      '   '#39' Do something'
      '   MsgBox "Hallo"'
      '  End If'
      'End Sub'
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

