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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvPrvwDoc, ComCtrls, StdCtrls, ExtCtrls, Menus, jpeg,
  JvPrvwRender, JvExStdCtrls, JvRichEdit{, GIFImage};

type
  TfrmMain = class(TForm)
    pnlBottom: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edCols: TEdit;
    udCols: TUpDown;
    edRows: TEdit;
    udRows: TUpDown;
    edShadow: TEdit;
    udShadowWidth: TUpDown;
    Label4: TLabel;
    edScale: TEdit;
    udZoom: TUpDown;
    PrinterSetupDialog1: TPrinterSetupDialog;
    cbPreview: TComboBox;
    Label5: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Printer1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    View1: TMenuItem;
    First1: TMenuItem;
    Previous1: TMenuItem;
    Next1: TMenuItem;
    Last1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Options1: TMenuItem;
    mnuMargins: TMenuItem;
    PageControl1: TPageControl;
    tabPreview: TTabSheet;
    tabOriginal: TTabSheet;
    OpenDialog1: TOpenDialog;
    reOriginal: TJvRichEdit;
    PrintDialog1: TPrintDialog;
    Print1: TMenuItem;
    Label6: TLabel;
    cbScaleMode: TComboBox;
    StatusBar1: TStatusBar;
    N3: TMenuItem;
    mnuPreview: TMenuItem;
    Control1: TMenuItem;
    Clear1: TMenuItem;
    Label7: TLabel;
    edVert: TEdit;
    udVertSpacing: TUpDown;
    Label8: TLabel;
    edHorz: TEdit;
    udHorzSpacing: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure udColsClick(Sender: TObject; Button: TUDBtnType);
    procedure udRowsClick(Sender: TObject; Button: TUDBtnType);
    procedure udShadowWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure udZoomClick(Sender: TObject; Button: TUDBtnType);
    procedure cbPreviewChange(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Printer1Click(Sender: TObject);
    procedure mnuMarginsClick(Sender: TObject);
    procedure First1Click(Sender: TObject);
    procedure Previous1Click(Sender: TObject);
    procedure Next1Click(Sender: TObject);
    procedure Last1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure cbScaleModeChange(Sender: TObject);
    procedure Control1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure udVertSpacingClick(Sender: TObject; Button: TUDBtnType);
    procedure udHorzSpacingClick(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }
    procedure OpenRTFFile(const Filename: string);
    procedure OpenImages(Files: TStrings);
    procedure OpenTxtFile(const Filename: string);
    procedure OpenImage(const Filename: string);
    procedure DoChange(Sender: TObject);
    procedure DoAfterScroll(Sender: TObject);
    procedure DoScrollHint(Sender: TObject; AScrollPos: integer; var AHint: string);
    procedure BuildRTFPreview;
    procedure BuildTXTPreview;
    procedure BuildImagePreview;
    procedure BuildControlMenu;
  public
    { Public declarations }
    pd: TJvPreviewControl;
    JvRTF: TJvPreviewRenderJvRichEdit;
    JvTxt: TJvPreviewRenderStrings;
    JvImg: TJvPreviewRenderGraphics;
  end;


var
  frmMain: TfrmMain;

implementation
uses
  Printers;

{$R *.dfm}


procedure TfrmMain.Print1Click(Sender: TObject);
var jp: TJvPreviewPrinter;
begin
  PrintDialog1.PrintRange := prAllPages;
  if pd.PageCount < 1 then
    PrintDialog1.Options := PrintDialog1.Options - [poPageNums]
  else
  begin
    PrintDialog1.Options := PrintDialog1.Options + [poPageNums];
    PrintDialog1.FromPage := 1;
    PrintDialog1.ToPage := pd.PageCount;
  end;
  if PrintDialog1.Execute then
  begin
    jp := TJvPreviewPrinter.Create(nil);
    try
      jp.Assign(PrintDialog1);
      jp.Printer := Printer;
      jp.PrintPreview := pd;
      jp.Print;
    finally
      jp.Free;
    end;
  end;
end;

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  OpenDialog1.Filter := OpenDialog1.Filter + '|' + GraphicFilter(TGraphic);
  JvImg := TJvPreviewRenderGraphics.Create(self);
  JvRTF := TJvPreviewRenderJvRichEdit.Create(self);
  JvTxt := TJvPreviewRenderStrings.Create(self);

  pd := TJvPreviewControl.Create(self);
  pd.Name := 'JvPreviewDoc1';
  pd.Parent := tabPreview;
  pd.Align := alClient;
  pd.TabStop := true;
  pd.BeginUpdate;
  pd.OnChange := DoChange;
  try
    pd.Options.DrawMargins := mnuMargins.Checked;
    pd.Options.Rows := udRows.Position;
    pd.Options.Cols := udCols.Position;
    pd.Options.Shadow.Offset := udShadowWidth.Position;
    pd.Options.Scale := udZoom.Position;
    pd.OnAfterScroll := DoAfterScroll;
    pd.OnScrollHint := DoScrollHint;

    cbPreview.ItemIndex := 1; // printer
    cbPreviewChange(nil);
    cbScaleMode.ItemIndex := 0; // full page
    cbScaleModeChange(nil);

  finally
    pd.EndUpdate;
  end;

  BuildControlMenu;
end;

procedure TfrmMain.DoChange(Sender: TObject);
begin
  udCols.Position := pd.Options.Cols;
  udRows.Position := pd.Options.Rows;
  udShadowWidth.Position := pd.Options.Shadow.Offset;
  udZoom.Position := pd.Options.Scale;
  mnuMargins.Checked := pd.Options.DrawMargins;
  cbScaleMode.ItemIndex := Ord(pd.Options.ScaleMode);
  udVertSpacing.Position := pd.Options.VertSpacing;
  udHorzSpacing.Position := pd.Options.HorzSpacing;
  Statusbar1.Panels[0].Text := ExtractFilename(OpenDialog1.Filename);
  Statusbar1.Panels[1].Text := Format('%d pages', [pd.PageCount]);
  Statusbar1.Panels[2].Text := Format('Cols: %d, Rows: %d, Row %d', [pd.TotalCols, pd.VisibleRows, pd.TopRow]);
end;

procedure TfrmMain.udColsClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Cols := udCols.Position;
  udCols.Position := pd.Options.Cols;
end;

procedure TfrmMain.udRowsClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Rows := udRows.Position;
  udRows.Position := pd.Options.Rows;
end;

procedure TfrmMain.udShadowWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Shadow.Offset := udShadowWidth.Position;
  udShadowWidth.Position := pd.Options.Shadow.Offset;
end;

procedure TfrmMain.udZoomClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.Scale := udZoom.Position;
  udZoom.Position := pd.Options.Scale;
end;

procedure TfrmMain.cbPreviewChange(Sender: TObject);
var Ext: string;
begin
  case cbPreview.ItemIndex of
    0:
      pd.DeviceInfo.ReferenceHandle := 0; // reset to default (screen)
    1:
      pd.DeviceInfo.ReferenceHandle := Printer.Handle;
  end;
  // set at least 0.5 inch margins
  pd.DeviceInfo.OffsetLeft := Max(pd.DeviceInfo.InchToXPx(0.5), pd.DeviceInfo.OffsetLeft);
  pd.DeviceInfo.OffsetRight := Max(pd.DeviceInfo.InchToXPx(0.5), pd.DeviceInfo.OffsetRight);
  pd.DeviceInfo.OffsetTop := Max(pd.DeviceInfo.InchToYPx(0.5), pd.DeviceInfo.OffsetTop);
  pd.DeviceInfo.OffsetBottom := Max(pd.DeviceInfo.InchToYPx(0.5), pd.DeviceInfo.OffsetBottom);
  Ext := AnsiLowerCase(ExtractFileExt(OpenDialog1.Filename));
  case OpenDialog1.FilterIndex of
    1: BuildRTFPreview;
    2: BuildTxtPreview;
  else if Pos(Ext, AnsiLowerCase(GraphicFilter(TGraphic))) > 0 then
    BuildImagePreview
  else
    BuildRTFPreview;
  end;
end;

procedure TfrmMain.OpenRTFFile(const Filename: string);
begin
  Screen.Cursor := crHourGlass;
  try
    reOriginal.Lines.LoadFromFile(OpenDialog1.Filename);
    BuildRTFPreview;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.OpenTxtFile(const Filename: string);
begin
  Screen.Cursor := crHourGlass;
  try
    reOriginal.Lines.Clear;
    reOriginal.Font.Name := 'Courier New';
    reOriginal.Font.Size := 10;
    reOriginal.Font.Color := clWindowText;
    reOriginal.DefAttributes.Assign(reOriginal.Font);
    reOriginal.Lines.LoadFromFile(OpenDialog1.Filename);
    BuildTxtPreview;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.OpenImage(const Filename: string);
begin
  with JvImg.Images.Add do
    Picture.LoadFromFile(Filename);
end;

procedure TfrmMain.OpenImages(Files: TStrings);
var i: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    for i := 0 to Files.Count - 1 do
      OpenImage(Files[i]);
    BuildImagePreview;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.Open1Click(Sender: TObject);
var Ext: string;
begin
  if OpenDialog1.Execute then
  begin
    Ext := AnsiLowerCase(ExtractFileExt(OpenDialog1.Filename));
    case OpenDialog1.FilterIndex of
      1: OpenRTFFile(OpenDialog1.Filename);
      2: OpenTxtFile(OpenDialog1.Filename);
    else if Pos(Ext, AnsiLowerCase(GraphicFilter(TGraphic))) > 0 then
      OpenImages(OpenDialog1.Files)
    else
      OpenRTFFile(OpenDialog1.Filename);
    end; // case
  end; // if
end;


procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.Printer1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  // update preview
  cbPreviewChange(Sender);
end;

procedure TfrmMain.mnuMarginsClick(Sender: TObject);
begin
  mnuMargins.Checked := not mnuMargins.Checked;
  pd.Options.DrawMargins := mnuMargins.Checked;
end;

procedure TfrmMain.First1Click(Sender: TObject);
begin
  pd.First;
end;

procedure TfrmMain.Previous1Click(Sender: TObject);
begin
  pd.Prior;
end;

procedure TfrmMain.Next1Click(Sender: TObject);
begin
  pd.Next;
end;

procedure TfrmMain.Last1Click(Sender: TObject);
begin
  pd.Last;
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  ShowMessage('JvPreviewControl Demo');
end;

procedure TfrmMain.cbScaleModeChange(Sender: TObject);
begin
  pd.Options.ScaleMode := TJvPreviewScaleMode(cbScaleMode.ItemIndex);
  cbScaleMode.ItemIndex := Ord(pd.Options.ScaleMode);
end;

procedure TfrmMain.DoAfterScroll(Sender: TObject);
begin
  Statusbar1.Panels[2].Text := Format('Cols: %d, Rows: %d, Row %d', [pd.TotalCols, pd.VisibleRows, pd.TopRow]);
end;

procedure TfrmMain.BuildRTFPreview;
begin
  with JvRTF do
  begin
    RichEdit := reOriginal;
    PrintPreview := pd;
    CreatePreview(false);
  end;
end;

procedure TfrmMain.BuildImagePreview;
begin
  with JvImg do
  begin
    PrintPreview := pd;
    CreatePreview(false);
  end;
end;
                 
procedure TfrmMain.BuildTXTPreview;
begin
  with JvTxt do
  begin
    PrintPreview := pd;
    Strings := reOriginal.Lines;
    Font := reOriginal.Font;
    Font.Size := 12;
    CreatePreview(false);
  end;
end;

procedure TfrmMain.Control1Click(Sender: TObject);
begin
  with TJvPreviewRenderControl.Create(nil) do
  try
    pd.First;
    PrintPreview := pd;
    Control := TControl((Sender as TMenuItem).Tag);
    CreatePreview(false);
  finally
    Free;
  end;
end;

procedure TfrmMain.BuildControlMenu;
var m:TMenuItem;i:integer;
begin
  mnuPreview.Clear;
  for i := -1 to ComponentCount-1 do
    if (i < 0) or (Components[i] is TControl) then
    begin
      m := TMenuItem.Create(self);
      if i < 0 then
      begin
        m.Tag := integer(self);
        m.Caption := self.Name;
      end
      else
      begin
        m.Tag := integer(Components[i]);
        m.Caption := Components[i].Name;
      end;
      m.OnClick := Control1Click;
      mnuPreview.Add(m);
    end;
end;

procedure TfrmMain.Clear1Click(Sender: TObject);
begin
  pd.Clear;
  JvImg.Images.Clear;
  reOriginal.Lines.Clear;
  JvTxt.Strings.Clear;
end;

procedure TfrmMain.udVertSpacingClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.VertSpacing := udVertSpacing.Position;
  udVertSpacing.Position := pd.Options.VertSpacing;
end;

procedure TfrmMain.udHorzSpacingClick(Sender: TObject; Button: TUDBtnType);
begin
  pd.Options.HorzSpacing := udHorzSpacing.Position;
  udHorzSpacing.Position := pd.Options.HorzSpacing;
end;

procedure TfrmMain.DoScrollHint(Sender: TObject; AScrollPos: integer;
  var AHint: string);
begin
  AHint := Format('Page %d', [Cardinal(pd.TopRow) * pd.Options.Cols + 1]);
end;

end.

