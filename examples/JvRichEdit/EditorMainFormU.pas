{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit EditorMainFormU;

interface

uses
  Windows, Messages, Forms, StdCtrls, Dialogs, Mask,
  ImgList, Controls, Menus, JvMenus, ComCtrls, ExtCtrls, Classes,
  JvComponent, JvAppEvent, JvMaskEdit, JvSpin, JvCombobox, JvColorCombo,
  JvSpeedBar, JvRichEdit, JvClipboardMonitor, JvExMask, JvExStdCtrls,
  JvExExtCtrls, XPColorMenuItemPainter;

type
  TEditorMainForm = class(TForm)
    MainMenu: TJvMainMenu;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    FilePrintItem: TMenuItem;
    FileExitItem: TMenuItem;
    EditUndoItem: TMenuItem;
    EditCutItem: TMenuItem;
    EditCopyItem: TMenuItem;
    EditPasteItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    Ruler: TPanel;
    FontDialog: TFontDialog;
    FirstInd: TLabel;
    LeftInd: TLabel;
    RulerLine: TBevel;
    RightInd: TLabel;
    Editor: TJvRichEdit;
    StatusBar: TStatusBar;
    ToolbarImages: TImageList;
    InsertMenu: TMenuItem;
    InsertImageItem: TMenuItem;
    InsertObjectItem: TMenuItem;
    EditRedoItem: TMenuItem;
    FormatMenu: TMenuItem;
    FormatFontItem: TMenuItem;
    FormatParagraphItem: TMenuItem;
    N3: TMenuItem;
    EditFindItem: TMenuItem;
    EditReplaceItem: TMenuItem;
    ColorMenu: TJvPopupMenu;
    EditPasteSpecial: TMenuItem;
    SpeedBar: TJvSpeedBar;
    FormatBar: TJvSpeedBar;
    SpeedbarSection1: TJvSpeedBarSection;
    SpeedbarSection2: TJvSpeedBarSection;
    NewBtn: TJvSpeedItem;
    OpenBtn: TJvSpeedItem;
    SaveBtn: TJvSpeedItem;
    PrintBtn: TJvSpeedItem;
    CutBtn: TJvSpeedItem;
    CopyBtn: TJvSpeedItem;
    PasteBtn: TJvSpeedItem;
    UndoBtn: TJvSpeedItem;
    RedoBtn: TJvSpeedItem;
    FindBtn: TJvSpeedItem;
    FontName: TJvFontComboBox;
    FontSize: TJvSpinEdit;
    SpeedbarSection3: TJvSpeedBarSection;
    BoldBtn: TJvSpeedItem;
    ItalicBtn: TJvSpeedItem;
    UnderlineBtn: TJvSpeedItem;
    ColorBtn: TJvSpeedItem;
    LeftBtn: TJvSpeedItem;
    CenterBtn: TJvSpeedItem;
    RightBtn: TJvSpeedItem;
    SubscriptBtn: TJvSpeedItem;
    SuperscriptBtn: TJvSpeedItem;
    BulletsBtn: TJvSpeedItem;
    N5: TMenuItem;
    EditObjPropsItem: TMenuItem;
    EditPopupMenu: TJvPopupMenu;
    CutItm: TMenuItem;
    CopyItm: TMenuItem;
    PasteItm: TMenuItem;
    BackgroundBtn: TJvSpeedItem;
    BackgroundMenu: TJvPopupMenu;
    N6: TMenuItem;
    ProtectedItem: TMenuItem;
    DisabledItem: TMenuItem;
    HiddenItem: TMenuItem;
    EditFindNextItem: TMenuItem;
    EditSelectAllItem: TMenuItem;
    FileSaveSelItem: TMenuItem;
    App: TJvAppEvents;
    FormatTabsItem: TMenuItem;
    JustifyBtn: TJvSpeedItem;
    procedure SelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FilePrint(Sender: TObject);
    procedure FileExit(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure SelectFont(Sender: TObject);
    procedure RulerResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BoldButtonClick(Sender: TObject);
    procedure ItalicButtonClick(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure AlignButtonClick(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure UnderlineButtonClick(Sender: TObject);
    procedure BulletsButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RulerItemMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RightIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure RichEditChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InsertObject(Sender: TObject);
    procedure EditRedo(Sender: TObject);
    procedure InsertBitmap(Sender: TObject);
    procedure FormatParaAttributes(Sender: TObject);
    procedure EditorURLClick(Sender: TObject; const URLText: string;
      Button: TMouseButton);
    procedure FormActivate(Sender: TObject);
    procedure EditFindClick(Sender: TObject);
    procedure EditReplaceClick(Sender: TObject);
    procedure MainMenuGetImageIndex(Sender: TMenu; Item: TMenuItem;
      State: TMenuOwnerDrawState; var ImageIndex: Integer);
    procedure ColorMenuPopup(Sender: TObject);
    procedure SubscriptClick(Sender: TObject);
    procedure EditPasteSpecialClick(Sender: TObject);
    procedure EditObjPropsItemClick(Sender: TObject);
    procedure EditPopupMenuGetImageIndex(Sender: TMenu; Item: TMenuItem;
      State: TMenuOwnerDrawState; var ImageIndex: Integer);
    procedure BackgroundMenuPopup(Sender: TObject);
    procedure ProtectedItemClick(Sender: TObject);
    procedure DisabledItemClick(Sender: TObject);
    procedure EditorProtectChange(Sender: TObject; StartPos,
      EndPos: Integer; var AllowChange: Boolean);
    procedure HiddenItemClick(Sender: TObject);
    procedure EditFindNextItemClick(Sender: TObject);
    procedure EditorTextNotFound(Sender: TObject; const FindText: string);
    procedure EditSelectAll(Sender: TObject);
    procedure FileSaveSelected(Sender: TObject);
    procedure FormatParaTabs(Sender: TObject);
  private
    FFileName: string;
    FUpdating: Boolean;
    FDragOfs: Integer;
    FLineOfs: Integer;
    FLineDC: HDC;
    FLinePen: HPen;
    FLineVisible: Boolean;
    FDragging: Boolean;
    FProtectChanging: Boolean;
    FClipboardMonitor: TJvClipboardMonitor;
    FOpenPictureDialog: TOpenDialog;
    function IndentToRuler(Indent: Integer; IsRight: Boolean): Integer;
    function RulerToIndent(RulerPos: Integer; IsRight: Boolean): Integer;
    procedure DrawLine;
    procedure CalcLineOffset(Control: TControl);
    function CurrText: TJvTextAttributes;
    procedure SetFileName(const FileName: string);
    procedure EditFindDialogClose(Sender: TObject; Dialog: TFindDialog);
    procedure ColorItemClick(Sender: TObject);
    procedure BackgroundItemClick(Sender: TObject);
    procedure CheckFileSave;
    procedure SetupRuler;
    procedure SetEditRect;
    procedure UpdateCursorPos;
    procedure FocusEditor;
    procedure ClipboardChanged(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure PerformFileOpen(const AFileName: string);
    procedure SetModified(Value: Boolean);
  end;

var
  EditorMainForm: TEditorMainForm;

implementation

uses
  Graphics, SysUtils, Math, ShellAPI, ClipBrd, ExtDlgs, Jpeg,
  JvGIF, JvWinDialogs, JvJVCLUtils,
  ParagraphFormatFormU, TabsFormU;

{$R *.DFM}

const
  RulerAdj = 4 / 3;

  GutterWid: Integer = 6;

  UndoNames: array[TUndoName] of string =
    ('', 'typing', 'delete', 'drag and drop', 'cut', 'paste');

  ColorValues: array[0..16] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
    clWindowText);
  BackValues: array[0..16] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
    clWindow);

function ColorName(Color: TColor): string;
begin
  if (Color = clWindowText) or (Color = clWindow) or (Color = clDefault) then
    Result := 'Automatic'
  else
  begin
    Result := ColorToString(Color);
    if Pos('cl', Result) = 1 then
      Delete(Result, 1, 2);
  end;
end;

//=== TEditorMainForm ========================================================

procedure TEditorMainForm.AlignButtonClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  Editor.Paragraph.Alignment := TParaAlignment(TComponent(Sender).Tag);
end;

procedure TEditorMainForm.BackgroundItemClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := True;
    CurrText.BackColor := Tag;
  end;
end;

procedure TEditorMainForm.BackgroundMenuPopup(Sender: TObject);
var
  I: Integer;
  C: TColor;
begin
  C := CurrText.BackColor;
  for I := 0 to 16 do
    BackgroundMenu.Items[I].Checked := (C = BackValues[I]);
end;

procedure TEditorMainForm.BoldButtonClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  if BoldBtn.Down then
    CurrText.Style := CurrText.Style + [fsBold]
  else
    CurrText.Style := CurrText.Style - [fsBold];
end;

procedure TEditorMainForm.BulletsButtonClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  Editor.Paragraph.Numbering := TJvNumbering(BulletsBtn.Down);
end;

procedure TEditorMainForm.CalcLineOffset(Control: TControl);
var
  P: TPoint;
begin
  with Control do
    P := ClientToScreen(Point(0, 0));
  P := Editor.ScreenToClient(P);
  FLineOfs := P.X + FDragOfs;
end;

procedure TEditorMainForm.CheckFileSave;
var
  SaveResp: Integer;
begin
  if not Editor.Modified then
    Exit;
  SaveResp := MessageDlg(Format('Save changes to %s?', [FFileName]),
    mtConfirmation, mbYesNoCancel, 0);
  try
    case SaveResp of
      mrYes: FileSave(Self);
      mrNo: {Nothing};
      mrCancel: Abort;
    end;
  finally
    FocusEditor;
  end;
end;

procedure TEditorMainForm.ClipboardChanged(Sender: TObject);
var
  E: Boolean;
begin
  { check to see if we can paste what's on the clipboard }
  E := Editor.CanPaste;
  PasteBtn.Enabled := E;
  EditPasteItem.Enabled := E;
  EditPasteSpecial.Enabled := E;
  PasteItm.Enabled := E;
end;

procedure TEditorMainForm.ColorItemClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := True;
    CurrText.Color := Tag;
  end;
end;

procedure TEditorMainForm.ColorMenuPopup(Sender: TObject);
var
  I: Integer;
  C: TColor;
begin
  C := CurrText.Color;
  for I := 0 to 16 do
    ColorMenu.Items[I].Checked := (C = ColorValues[I]);
end;

function TEditorMainForm.CurrText: TJvTextAttributes;
begin
  if Editor.SelLength > 0 then
    Result := Editor.SelAttributes
  else
    Result := Editor.WordAttributes;
end;

procedure TEditorMainForm.DisabledItemClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  CurrText.Disabled := not CurrText.Disabled;
  DisabledItem.Checked := CurrText.Disabled;
end;

procedure TEditorMainForm.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(0, 0);
  Inc(P.X, FLineOfs);
  with P, Editor do
  begin
    MoveToEx(FLineDC, X, Y, nil);
    LineTo(FLineDC, X, Y + ClientHeight);
  end;
end;

procedure TEditorMainForm.EditCopy(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TEditorMainForm.EditCut(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TEditorMainForm.EditFindClick(Sender: TObject);
begin
  with Editor do
    FindDialog(SelText);
end;

procedure TEditorMainForm.EditFindDialogClose(Sender: TObject; Dialog: TFindDialog);
begin
  FocusEditor;
end;

procedure TEditorMainForm.EditFindNextItemClick(Sender: TObject);
begin
  if not Editor.FindNext then
    Beep;
  FocusEditor;
end;

procedure TEditorMainForm.EditObjPropsItemClick(Sender: TObject);
begin
  Editor.ObjectPropertiesDialog;
end;

procedure TEditorMainForm.EditorProtectChange(Sender: TObject; StartPos,
  EndPos: Integer; var AllowChange: Boolean);
begin
  AllowChange := FProtectChanging;
end;

procedure TEditorMainForm.EditorTextNotFound(Sender: TObject;
  const FindText: string);
begin
  MessageDlg(Format('Text "%s" not found.', [FindText]), mtWarning, [mbOK], 0);
end;

procedure TEditorMainForm.EditorURLClick(Sender: TObject; const URLText: string;
  Button: TMouseButton);
begin
  if Button = mbLeft then
    ShellExecute(Handle, nil, PChar(URLText), nil, nil, SW_SHOW);
end;

procedure TEditorMainForm.EditPaste(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TEditorMainForm.EditPasteSpecialClick(Sender: TObject);
begin
  try
    Editor.PasteSpecialDialog;
  finally
    FocusEditor;
  end;
end;

procedure TEditorMainForm.EditPopupMenuGetImageIndex(Sender: TMenu;
  Item: TMenuItem; State: TMenuOwnerDrawState; var ImageIndex: Integer);
begin
  if (Item = CutItm) or (Item = CopyItm) or (Item = PasteItm) then
    ImageIndex := Item.Tag;
end;

procedure TEditorMainForm.EditRedo(Sender: TObject);
begin
  Editor.Redo;
  RichEditChange(nil);
  SelectionChange(nil);
end;

procedure TEditorMainForm.EditReplaceClick(Sender: TObject);
begin
  with Editor do
    ReplaceDialog(SelText, '');
end;

procedure TEditorMainForm.EditSelectAll(Sender: TObject);
begin
  Editor.SelectAll;
end;

procedure TEditorMainForm.EditUndo(Sender: TObject);
begin
  Editor.Undo;
  RichEditChange(nil);
  SelectionChange(nil);
end;

procedure TEditorMainForm.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TEditorMainForm.FileNew(Sender: TObject);
begin
  CheckFileSave;
  SetFileName('Untitled');
  FProtectChanging := True;
  try
    Editor.Lines.Clear;
    Editor.Modified := False;
    Editor.ReadOnly := False;
    SetModified(False);
    with Editor do
    begin
      DefAttributes.Assign(Font);
      SelAttributes.Assign(Font);
    end;
    SelectionChange(nil);
  finally
    FProtectChanging := False;
  end;
end;

procedure TEditorMainForm.FileOpen(Sender: TObject);
begin
  CheckFileSave;
  if OpenDialog.Execute then
  begin
    Editor.Refresh;
    PerformFileOpen(OpenDialog.FileName);
    Editor.ReadOnly := ofReadOnly in OpenDialog.Options;
  end;
end;

procedure TEditorMainForm.FilePrint(Sender: TObject);
begin
  if PrintDialog.Execute then
    Editor.Print(FFileName);
end;

procedure TEditorMainForm.FileSave(Sender: TObject);
begin
  if FFileName = 'Untitled' then
    FileSaveAs(Sender)
  else
  begin
    Editor.Lines.SaveToFile(FFileName);
    Editor.Modified := False;
    SetModified(False);
    RichEditChange(nil);
  end;
end;

procedure TEditorMainForm.FileSaveAs(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Editor.Lines.SaveToFile(SaveDialog.FileName);
    SetFileName(SaveDialog.FileName);
    Editor.Modified := False;
    SetModified(False);
    RichEditChange(nil);
  end;
  FocusEditor;
end;

procedure TEditorMainForm.FileSaveSelected(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    with Editor do
    try
      StreamMode := [smSelection];
      Lines.SaveToFile(SaveDialog.FileName);
    finally
      StreamMode := [];
    end;
    RichEditChange(nil);
  end;
  FocusEditor;
end;

procedure TEditorMainForm.FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  Editor.Paragraph.FirstIndent := Max(0, RulerToIndent(FirstInd.Left + FDragOfs,
    False));
  LeftIndMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TEditorMainForm.FocusEditor;
begin
  with Editor do
    if CanFocus then
      SetFocus;
end;

procedure TEditorMainForm.FontNameChange(Sender: TObject);
begin
  if FUpdating then
    Exit;
  CurrText.Name := FontName.FontName;
end;

procedure TEditorMainForm.FontSizeChange(Sender: TObject);
begin
  if FUpdating then
    Exit;
  if FontSize.AsInteger > 0 then
    CurrText.Size := FontSize.AsInteger;
end;

procedure TEditorMainForm.FormActivate(Sender: TObject);
begin
  FocusEditor;
end;

procedure TEditorMainForm.FormatParaAttributes(Sender: TObject);
begin
  FormatParagraph(Editor.Paragraph);
  FocusEditor;
end;

procedure TEditorMainForm.FormatParaTabs(Sender: TObject);
begin
  FormatTabs(Editor.Paragraph);
  FocusEditor;
end;

procedure TEditorMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  try
    CheckFileSave;
  except
    CanClose := False;
  end;
end;

procedure TEditorMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
const
  SPictureFilter = '%s|%s|%s|%s';
begin
  // The TComponent destructor should handle the destruction of the
  // painters because we indicate the form (self) as the owner of
  // the objects. But we are extra careful and destroy them ourselves
  // in FormDestroy anyway.
  ColorMenu.ItemPainter := TJvXPColorMenuItemPainter.Create(Self);
  BackgroundMenu.ItemPainter := TJvXPColorMenuItemPainter.Create(Self);

  Editor.RegisterMSTextConverters;
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog.Filter := Editor.Filter(ckImport);
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  SaveDialog.Filter := Editor.Filter(ckExport);
  SetFileName('Untitled');
  SetupRuler;
  HandleNeeded;
  SelectionChange(Self);
  Editor.OnCloseFindDialog := EditFindDialogClose;
  FOpenPictureDialog := TOpenPictureDialog.Create(Self);
  with FOpenPictureDialog do
    Filter := Format(SPictureFilter, [
      GraphicFilter(TBitmap),
      GraphicFilter(TMetafile),
      GraphicFilter(TJPEGImage),
      GraphicFilter(TJvGIFImage)
      ]);
  with ColorMenu.Items do
  begin
    while Count > 0 do
      Items[Count - 1].Free;
    for I := 0 to 16 do
    begin
      Item := NewItem(ColorName(ColorValues[I]), scNone,
        False, True, ColorItemClick, 0, '');
      Item.RadioItem := True;
      Item.Tag := ColorValues[I];
      Add(Item);
    end;
  end;
  with BackgroundMenu.Items do
  begin
    while Count > 0 do
      Items[Count - 1].Free;
    for I := 0 to 16 do
    begin
      Item := NewItem(ColorName(BackValues[I]), scNone,
        False, True, BackgroundItemClick, 0, '');
      Item.RadioItem := True;
      Item.Tag := BackValues[I];
      Add(Item);
    end;
  end;
  FClipboardMonitor := TJvClipboardMonitor.Create(Self);
  FClipboardMonitor.OnChange := ClipboardChanged;
  SuperscriptBtn.Enabled := RichEditVersion >= 2;
  SubscriptBtn.Enabled := RichEditVersion >= 2;
  BackgroundBtn.Enabled := RichEditVersion >= 2;
  DisabledItem.Enabled := RichEditVersion >= 2;
  HiddenItem.Enabled := RichEditVersion >= 2;
  JustifyBtn.Enabled := RichEditVersion >= 3;
end;

procedure TEditorMainForm.FormDestroy(Sender: TObject);
begin
  ColorMenu.ItemPainter.Free;
  ColorMenu.ItemPainter := nil;
  BackgroundMenu.ItemPainter.Free;
  BackgroundMenu.ItemPainter := nil;
  { remove ourselves from the viewer chain }
  FClipboardMonitor.Free;
end;

procedure TEditorMainForm.FormPaint(Sender: TObject);
begin
  SetEditRect;
end;

procedure TEditorMainForm.FormResize(Sender: TObject);
begin
  SetEditRect;
  SelectionChange(Sender);
end;

procedure TEditorMainForm.FormShow(Sender: TObject);
var
  Res: TResourceStream;
begin
  UpdateCursorPos;
  DragAcceptFiles(Handle, True);
  RichEditChange(nil);
  FocusEditor;
  ClipboardChanged(nil);
  { check if we should load a file from the command line }
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    PerformFileOpen(ParamStr(1))
  else
  begin
    StartWait;
    try
      Res := TResourceStream.Create(HInstance, 'OVERVIEWRTF', 'RTF');
      try
        Editor.Lines.LoadFromStream(Res);
        SetFileName('Untitled');
        Editor.Modified := False;
        SetModified(False);
      finally
        Res.Free;
      end;
    except
      { ignore exceptions }
    end;
    StopWait;
  end;
end;

procedure TEditorMainForm.HelpAbout(Sender: TObject);
begin
  with TJvShellAboutDialog.Create(Application) do
  try
    Caption := 'RichEdit 2.0 Control Demo';
    OtherText := 'RX Library Demo Program';
    Execute;
  finally
    Free;
  end;
  FocusEditor;
end;

procedure TEditorMainForm.HiddenItemClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  CurrText.Hidden := not CurrText.Hidden;
  HiddenItem.Checked := CurrText.Hidden;
end;

function TEditorMainForm.IndentToRuler(Indent: Integer; IsRight: Boolean): Integer;
var
  R: TRect;
  P: TPoint;
begin
  Indent := Trunc(Indent * RulerAdj);
  with Editor do
  begin
    SendMessage(Handle, EM_GETRECT, 0, Longint(@R));
    if IsRight then
    begin
      P := R.BottomRight;
      P.X := P.X - Indent;
    end
    else
    begin
      P := R.TopLeft;
      P.X := P.X + Indent;
    end;
    P := ClientToScreen(P);
  end;
  with Ruler do
    P := ScreenToClient(P);
  Result := P.X;
end;

procedure TEditorMainForm.InsertBitmap(Sender: TObject);
var
  Pict: TPicture;
begin
  with FOpenPictureDialog do
  begin
    if Execute then
    begin
      Pict := TPicture.Create;
      try
        Pict.LoadFromFile(FileName);
        Clipboard.Assign(Pict);
        Editor.PasteFromClipboard;
      finally
        Pict.Free;
      end;
    end;
  end;
end;

procedure TEditorMainForm.InsertObject(Sender: TObject);
begin
  Editor.InsertObjectDialog;
end;

procedure TEditorMainForm.ItalicButtonClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  if ItalicBtn.Down then
    CurrText.Style := CurrText.Style + [fsItalic]
  else
    CurrText.Style := CurrText.Style - [fsItalic];
end;

procedure TEditorMainForm.LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  if FLineVisible then
    DrawLine;
  DeleteObject(SelectObject(FLineDC, FLinePen));
  ReleaseDC(Editor.Handle, FLineDC);
  Editor.Paragraph.LeftIndent := Max(-Editor.Paragraph.FirstIndent,
    RulerToIndent(LeftInd.Left + FDragOfs, False) -
    Editor.Paragraph.FirstIndent);
  SelectionChange(Sender);
end;

procedure TEditorMainForm.MainMenuGetImageIndex(Sender: TMenu; Item: TMenuItem;
  State: TMenuOwnerDrawState; var ImageIndex: Integer);
begin
  if Item.Tag >= 0 then
    ImageIndex := Item.Tag;
end;

procedure TEditorMainForm.PerformFileOpen(const AFileName: string);
begin
  FProtectChanging := True;
  try
    Editor.Lines.LoadFromFile(AFileName);
  finally
    FProtectChanging := False;
  end;
  SetFileName(AFileName);
  Editor.SetFocus;
  Editor.Modified := False;
  SetModified(False);
end;

procedure TEditorMainForm.ProtectedItemClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  FProtectChanging := True;
  try
    CurrText.Protected := not CurrText.Protected;
    ProtectedItem.Checked := CurrText.Protected;
  finally
    FProtectChanging := False;
  end;
end;

procedure TEditorMainForm.RichEditChange(Sender: TObject);
begin
  SetModified(Editor.Modified);
  { Undo }
  UndoBtn.Enabled := Editor.CanUndo;
  EditUndoItem.Enabled := UndoBtn.Enabled;
  EditUndoItem.Caption := '&Undo ' + UndoNames[Editor.UndoName];
  { Redo }
  EditRedoItem.Enabled := Editor.CanRedo;
  RedoBtn.Enabled := EditRedoItem.Enabled;
  EditRedoItem.Caption := '&Redo ' + UndoNames[Editor.RedoName];
end;

procedure TEditorMainForm.RightIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  if FLineVisible then
    DrawLine;
  DeleteObject(SelectObject(FLineDC, FLinePen));
  ReleaseDC(Editor.Handle, FLineDC);
  Editor.Paragraph.RightIndent := Max(0, RulerToIndent(RightInd.Left + FDragOfs,
    True));
  SelectionChange(Sender);
end;

procedure TEditorMainForm.RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragOfs := (TControl(Sender).Width div 2);
  TControl(Sender).Left := Max(0, TControl(Sender).Left + X - FDragOfs);
  FLineDC := GetDCEx(Editor.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  FLinePen := SelectObject(FLineDC, CreatePen(PS_DOT, 1, ColorToRGB(clWindowText)));
  SetROP2(FLineDC, R2_XORPEN);
  CalcLineOffset(TControl(Sender));
  DrawLine;
  FDragging := True;
end;

procedure TEditorMainForm.RulerItemMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging then
  begin
    DrawLine;
    TControl(Sender).Left := Min(Max(0, TControl(Sender).Left + X - FDragOfs),
      Ruler.ClientWidth - FDragOfs * 2);
    CalcLineOffset(TControl(Sender));
    DrawLine;
  end;
end;

procedure TEditorMainForm.RulerResize(Sender: TObject);
begin
  RulerLine.Width := Ruler.ClientWidth - (RulerLine.Left * 2);
end;

function TEditorMainForm.RulerToIndent(RulerPos: Integer; IsRight: Boolean): Integer;
var
  R: TRect;
  P: TPoint;
begin
  P.Y := 0;
  P.X := RulerPos;
  with Ruler do
    P := ClientToScreen(P);
  with Editor do
  begin
    P := ScreenToClient(P);
    SendMessage(Handle, EM_GETRECT, 0, Longint(@R));
    if IsRight then
      Result := R.BottomRight.X - P.X
    else
      Result := P.X - R.TopLeft.X;
  end;
  Result := Trunc(Result / RulerAdj);
end;

procedure TEditorMainForm.SelectFont(Sender: TObject);
begin
  FontDialog.Font.Assign(Editor.SelAttributes);
  if FontDialog.Execute then
    CurrText.Assign(FontDialog.Font);
  FocusEditor;
end;

procedure TEditorMainForm.SelectionChange(Sender: TObject);
begin
  with Editor.Paragraph do
  try
    FUpdating := True;
    FirstInd.Left := IndentToRuler(FirstIndent, False) - (FirstInd.Width div 2);
    LeftInd.Left := IndentToRuler(LeftIndent + FirstIndent, False) - (LeftInd.Width div 2);
    RightInd.Left := IndentToRuler(RightIndent, True) - (RightInd.Width div 2);
    BoldBtn.Down := fsBold in CurrText.Style;
    ItalicBtn.Down := fsItalic in CurrText.Style;
    UnderlineBtn.Down := fsUnderline in CurrText.Style;
    BulletsBtn.Down := Boolean(Numbering);
    SuperscriptBtn.Down := CurrText.SubscriptStyle = ssSuperscript;
    SubscriptBtn.Down := CurrText.SubscriptStyle = ssSubscript;
    FontSize.AsInteger := CurrText.Size;
    FontName.FontName := CurrText.Name;
    ProtectedItem.Checked := CurrText.Protected;
    DisabledItem.Checked := CurrText.Disabled;
    HiddenItem.Checked := CurrText.Hidden;
    case Ord(Alignment) of
      0: LeftBtn.Down := True;
      1: RightBtn.Down := True;
      2: CenterBtn.Down := True;
      3: JustifyBtn.Down := True;
    end;
    UpdateCursorPos;
  finally
    FUpdating := False;
  end;
end;

procedure TEditorMainForm.SetEditRect;
var
  R: TRect;
  Offs: Integer;
begin
  with Editor do
  begin
    if SelectionBar then
      Offs := 3
    else
      Offs := 0;
    R := Rect(GutterWid + Offs, 0, ClientWidth - GutterWid, ClientHeight);
    SendMessage(Handle, EM_SETRECT, 0, Longint(@R));
  end;
end;

procedure TEditorMainForm.SetFileName(const FileName: string);
begin
  FFileName := FileName;
  Editor.Title := ExtractFileName(FileName);
  Caption := Format('%s - %s', [ExtractFileName(FileName), Application.Title]);
end;

procedure TEditorMainForm.SetModified(Value: Boolean);
begin
  if Value then
    StatusBar.Panels[1].Text := 'Modified'
  else
    StatusBar.Panels[1].Text := '';
end;

procedure TEditorMainForm.SetupRuler;
var
  I: Integer;
  S: string;
begin
  SetLength(S, 201);
  I := 1;
  while I < 200 do
  begin
    S[I] := #9;
    S[I + 1] := '|';
    Inc(I, 2);
  end;
  Ruler.Caption := S;
end;

procedure TEditorMainForm.ShowHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then
  begin
    StatusBar.SimplePanel := True;
    StatusBar.SimpleText := Application.Hint;
  end
  else
    StatusBar.SimplePanel := False;
end;

procedure TEditorMainForm.SubscriptClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  if SuperscriptBtn.Down then
    CurrText.SubscriptStyle := ssSuperscript
  else
    if SubscriptBtn.Down then
    CurrText.SubscriptStyle := ssSubscript
  else
    CurrText.SubscriptStyle := ssNone;
end;

procedure TEditorMainForm.UnderlineButtonClick(Sender: TObject);
begin
  if FUpdating then
    Exit;
  if UnderlineBtn.Down then
    CurrText.Style := CurrText.Style + [fsUnderline]
  else
    CurrText.Style := CurrText.Style - [fsUnderline];
end;

procedure TEditorMainForm.UpdateCursorPos;
var
  CharPos: TPoint;
begin
  CharPos := Editor.CaretPos;
  StatusBar.Panels[0].Text := Format('Line: %3d  Col: %3d',
    [CharPos.Y + 1, CharPos.X + 1]);
  { update the status of the cut and copy command }
  CopyBtn.Enabled := Editor.SelLength > 0;
  EditCopyItem.Enabled := CopyBtn.Enabled;
  CopyItm.Enabled := CopyBtn.Enabled;
  CutBtn.Enabled := EditCopyItem.Enabled;
  CutItm.Enabled := CutBtn.Enabled;
  FileSaveSelItem.Enabled := CopyBtn.Enabled;
  EditCutItem.Enabled := EditCopyItem.Enabled;
  EditObjPropsItem.Enabled := Editor.SelectionType = [stObject];
end;

procedure TEditorMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  cFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, cFileName, MAX_PATH) > 0 then
    begin
      Application.BringToFront;
      CheckFileSave;
      PerformFileOpen(cFileName);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

end.

