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

{$I jvcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls, ImgList, ComCtrls, Menus,
  JvToolEdit,
  jpeg, JvPCX, JvGIF, JvAni, JvCursor, 
    // if you have units that supports other image formats, add them here *before* including JvItemViewer
//  GraphicEx, // http://www.delphi-gems.com/Graphics.php#GraphicEx
  JvCustomItemViewer,
  JvImagesViewer,
  JvImageListViewer,
  JvOwnerDrawViewer,
  JvComponent,
  JvInspector, JvExMask;

type
  TfrmMain = class(TForm)
    pnlSettings: TPanel;
    edDirectory: TJvDirectoryEdit;
    lblFolder: TLabel;
    lblFilemask: TLabel;
    edFileMask: TEdit;
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    pgViewers: TPageControl;
    tabIFViewer: TTabSheet;
    tabILViewer: TTabSheet;
    tabODViewer: TTabSheet;
    PopupMenu1: TPopupMenu;
    Reload1: TMenuItem;
    Viewfromfile1: TMenuItem;
    Viewfrompicture1: TMenuItem;
    N1: TMenuItem;
    Splitter1: TSplitter;
    btnUpdate: TButton;
    Rename1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    chkDisconnect: TCheckBox;
    SelectAll1: TMenuItem;
    procedure btnUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Reload1Click(Sender: TObject);
    procedure Viewfromfile1Click(Sender: TObject);
    procedure Viewfrompicture1Click(Sender: TObject);
    procedure pgViewersChange(Sender: TObject);
    procedure edDirectoryChange(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure chkDisconnectClick(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
  private
    FDragIndex: Integer;
    procedure BuildColorList;
    procedure SetDisplayDragImage(AControl: TControl);
    procedure DoITV2DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoITV2DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoITV2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoITV2GetCaption(Sender: TObject; ImageINdex: Integer; var ACaption: string);
    procedure DoITVClick(Sender: TObject);
    procedure DoITVDblClick(Sender: TObject);
    procedure DITVLoadBegin(Sender: TObject);
    procedure DoITVLoadEnd(Sender: TObject);
    procedure DoITVLoadProgress(Sender: TObject; Item: TJvImageItem; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
    procedure DoITV3DrawItem(Sender: TObject; AIndex: Integer; AState: TCustomDrawState; ACanvas: TCanvas; ItemRect,
      TextRect: TRect);
    procedure DoITV3Click(Sender: TObject);
    procedure ViewItem(Item: TJvImageItem; LoadFromFile: Boolean);
    procedure DoITV3ItemHint(Sender: TObject; Index: Integer;
    var HintInfo: THintInfo; var Handled: Boolean);
  public
    ITV: TJvImagesViewer;
    ITV2: TJvImageListViewer;
    ITV3: TJvOwnerDrawViewer;
    AInspector: TJvInspector;
    APainter: TJvInspectorBorlandPainter;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JvConsts, // for clMoneyGreen
  CommCtrl, Consts,
  {$IFNDEF COMPILER6_UP}
  FileCtrl,
  {$ENDIF}
  JclRegistry,
  ViewerFrm;

{$R *.dfm}

//=== TfrmMain ===============================================================

procedure TfrmMain.DoITV3DrawItem(Sender: TObject; AIndex: Integer; AState: TCustomDrawState; ACanvas:
  TCanvas; ItemRect, TextRect: TRect);
var
  AColor: TColor;
begin
  AColor := TColor(ITV3.Items[AIndex].Data);
  ACanvas.Brush.Color := AColor;
  ACanvas.FillRect(ItemRect);
  ACanvas.Pen.Style := psSolid;
  if [cdsSelected, cdsHot] * AState <> [] then
  begin
    ACanvas.Pen.Color := clHighlight;
    ACanvas.Pen.Width := 2;
    Inc(ItemRect.Left);
    Inc(ItemRect.Top);
    ACanvas.Rectangle(ItemRect);
    Dec(ItemRect.Left);
    Dec(ItemRect.Top);
  end
  else
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Width := 1;
    ACanvas.Rectangle(ItemRect);
  end;
end;

procedure TfrmMain.btnUpdateClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ITV.Directory := edDirectory.Text;
    ITV.FileMask := edFileMask.Text;
    AInspector.InspectObject := nil;
    AInspector.BeginUpdate;
    ITV.LoadImages;
    AInspector.InspectObject := ITV;
    AInspector.EndUpdate;
    StatusBar1.Panels[0].Text := Format(' %d images found and loaded', [ITV.Count]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Randomize;
  SetDisplayDragImage(Self);
  APainter := TJvInspectorBorlandPainter.Create(Self);
  AInspector := TJvInspector.Create(Self);
  AInspector.Parent := Self;
  AInspector.Left := -100;
  AInspector.Width := StatusBar1.Panels[0].Width - 2;
  AInspector.Parent := Self;
  AInspector.Align := alLeft;
  AInspector.Painter := APainter;

  ITV := TJvImagesViewer.Create(Self);
  ITV.Align := alClient;
  ITV.PopupMenu := PopupMenu1;
//  ITV.Cursor := crHandPoint;
  ITV.Options.RightClickSelect := True;
  ITV.Options.ImagePadding := 8;
  ITV.Options.MultiSelect := True;
  ITV.Options.HotTrack := True;
//  ITV.Options.Smooth := True; // don't use smooth with images - looks ugly when scrolling

  ITV.OnDblClick := DoITVDblClick;
  ITV.OnClick := DoITVClick;
  ITV.OnLoadBegin := DITVLoadBegin;
  ITV.OnLoadEnd := DoITVLoadEnd;
  ITV.OnLoadProgress := DoITVLoadProgress;

  ITV.Parent := tabIFViewer;
  ITV.Color := clWindow;
  if edFileMask.Text = '' then
    edFileMask.Text := ITV.Filemask;

  ITV2 := TJvImageListViewer.Create(Self);
  ITV2.Align := alClient;
  ITV2.Options.Width := ImageList1.Width * 2;
  ITV2.Options.Height := ImageList1.Height * 2;
  ITV2.Options.FillCaption := False;
  ITV2.Options.BrushPattern.Active := False;
//  ITV2.Options.BrushPattern.OddColor := clHighlight;

  ITV2.Images := ImageList1;
  ITV2.Parent := tabILViewer;
//  ITV2.Options.BrushPattern.OddColor := clHighlight;
  //  ITV2.Options.BrushPattern.Active := False;
  ITV2.OnMouseDown := DoITV2MouseDown;
  ITV2.OnDragOver := DoITV2DragOver;
  ITV2.OnDragDrop := DoITV2DragDrop;
  ITV2.OnGetCaption := DoITV2GetCaption;
  ITV2.Color := clWindow;
  ITV2.Options.ShowCaptions := True;

  ITV3 := TJvOwnerDrawViewer.Create(Self);
  ITV3.Options.Smooth := True; // Smooth looks OK here, because these items renders faster
  ITV3.Options.HotTrack := False;
  ITV3.Options.Width := 18;
  ITV3.Options.Height := 18;
  ITV3.Options.VertSpacing := 2;
  ITV3.Options.HorzSpacing := 2;
  ITV3.Align := alClient;
  ITV3.OnDrawItem := DoITV3DrawItem;
  ITV3.OnClick := DoITV3Click;
  ITV3.OnItemHint := DoITV3ItemHint;
  ITV3.ShowHint := true;

//  ITV3.Count := tbThumbSize.Position;
  ITV3.Parent := tabODViewer;
  ITV3.Color := clWindow;

  // add colors to TJvOwnerDrawViewer
  BuildColorList;

  if edDirectory.Text = '' then
  begin
    // this triggers the OnChange event
    edDirectory.Text := RegReadString(HKEY_CURRENT_USER,
      'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', 'My Pictures');
    if edDirectory.Text = '' then
      edDirectory.Text := GetCurrentDir;
  end;
  pgViewersChange(nil);
end;

procedure TfrmMain.DoITVClick(Sender: TObject);
begin
  if ITV.SelectedIndex > -1 then
    StatusBar1.Panels[1].Text := '  ' + ITV.Items[ITV.SelectedIndex].FileName
  else
    StatusBar1.Panels[1].Text := '';
end;

procedure TfrmMain.DITVLoadBegin(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
end;

procedure TfrmMain.BuildColorList;
var
  I, J: Cardinal;
begin
  // example of storing stuff in item's Data property
  ITV3.Count := $3FFF;
  Randomize;
  for I := 0 to $3FFE do
  begin
    J := ($3FFE - I) + 500;
    ITV3.Items[I].Data := Pointer(RGB(Random(J) mod 256, Random(J) mod 256, Random(J) mod 256));
  end;
end;

procedure TfrmMain.DoITV2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragIndex := ITV2.ItemAtPos(X, Y, True);
    if FDragIndex > -1 then
      ITV2.BeginDrag(False, 10);
  end;
  //  ITV2.Invalidate;
end;

procedure TfrmMain.DoITV2DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
//var
//  I: Integer;
begin
  Accept := Source = ITV2;
  //  I := ITV2.ItemAtPos(X, Y);
  //  if I > -1 then
  //    ITV2.SelectedIndex := I;
end;

procedure TfrmMain.DoITV2DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  I: Integer;
begin
  I := ITV2.ItemAtPos(X, Y, False);
  if I >= ITV2.Images.Count then
    I := ITV2.Images.Count - 1;
  if (I > -1) and (I <> FDragIndex) then
    ITV2.Images.Move(FDragIndex, I);
  ITV2.SelectedIndex := I;
end;

procedure TfrmMain.Reload1Click(Sender: TObject);
begin
  if ITV.SelectedIndex >= 0 then
  begin
    ITV.Items[ITV.SelectedIndex].Picture := nil;
    ITV.Invalidate;
  end;
end;

procedure TfrmMain.DoITVDblClick(Sender: TObject);
begin
  Viewfrompicture1Click(Sender);
end;

procedure TfrmMain.ViewItem(Item: TJvImageItem; LoadFromFile: Boolean);
begin
  if LoadFromFile and FileExists(Item.FileName) then
    TfrmImageViewer.View(Item.FileName, ITV.Options.Transparent, ITV.Color)
  else
    TfrmImageViewer.View(Item.Picture, ITV.Options.Transparent, ITV.Color);
end;

procedure TfrmMain.Viewfromfile1Click(Sender: TObject);
var
  Item: TJvImageItem;
begin
  if ITV.Focused and (ITV.SelectedIndex >= 0) then
  begin
    Item := ITV.Items[ITV.SelectedIndex];
    ViewItem(Item, True);
  end;
end;

procedure TfrmMain.Viewfrompicture1Click(Sender: TObject);
var
  Item: TJvImageItem;
begin
  if ITV.Focused and (ITV.SelectedIndex >= 0) then
  begin
    Item := ITV.Items[ITV.SelectedIndex];
    ViewItem(Item, False);
  end;
end;

procedure TfrmMain.DoITVLoadProgress(Sender: TObject;
  Item: TJvImageItem; Stage: TProgressStage; PercentDone: Byte;
  RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if PercentDone >= 100 then
    StatusBar1.Panels[1].Text := ''
  else
    StatusBar1.Panels[1].Text := Format(' Loading "%s", %d%% done...', [Item.FileName, PercentDone]);
  StatusBar1.Update;
end;

procedure TfrmMain.DoITVLoadEnd(Sender: TObject);
var
  I: Integer;
begin
  Screen.Cursor := crDefault;
  pgViewersChange(Sender);
  for I := 0 to ITV.Count - 1 do
    if Assigned(ITV.Items[I].Picture) and Assigned(ITV.Items[I].Picture.Graphic) and
      (ITV.Items[I].Picture.Graphic is TJvAni) then
      TJvAni(ITV.Items[I].Picture.Graphic).Animated := True;
end;

procedure EnableControls(AControl: TControl; Enable: Boolean);
var
  I: Integer;
begin
  AControl.Enabled := Enable;
  if AControl is TWinControl then
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
      EnableControls(TWinControl(AControl).Controls[I], Enable);
end;

procedure TfrmMain.pgViewersChange(Sender: TObject);
begin
  case pgViewers.ActivePageIndex of
    0:
      begin
        EnableControls(pnlSettings, True);
        Statusbar1.Panels[1].Text := ' Double-click to view full size, right-click for popup menu';
        AInspector.InspectObject := ITV;
      end;
    1:
      begin
        EnableControls(pnlSettings, False);
        Statusbar1.Panels[1].Text := ' Drag and drop images to rearrange';
        AInspector.InspectObject := ITV2;
      end;
    2:
      begin
        EnableControls(pnlSettings, False);
        Statusbar1.Panels[1].Text := ' Click color square to see its color value in status bar';
        AInspector.InspectObject := ITV3;
      end;
  end;
end;

procedure TfrmMain.DoITV3Click(Sender: TObject);
begin
  if (ITV3.SelectedIndex >= 0) and (ITV3.SelectedIndex < ITV3.Count) then
    StatusBar1.Panels[0].Text := ColorToString(TColor(ITV3.Items[ITV3.SelectedIndex].Data));
end;

procedure TfrmMain.DoITV2GetCaption(Sender: TObject; ImageIndex: Integer;
  var ACaption: string);
begin
  if ITV2.Options.ShowCaptions then
  begin
    if Odd(ImageIndex) then
      ACaption := Format('#%d', [ImageIndex])
    else
      ACaption := Format('$%x', [ImageIndex])
  end;
end;

procedure TfrmMain.edDirectoryChange(Sender: TObject);
begin
  if DirectoryExists(edDirectory.Text) then
    btnUpdate.Click;
end;

procedure TfrmMain.Rename1Click(Sender: TObject);
var
  S: string;
  AItem: TJvImageItem;
begin
  if ITV.SelectedIndex < 0 then
    Exit;
  AItem := ITV.Items[ITV.SelectedIndex];
  S := AItem.FileName;
  if InputQuery('Rename', 'New name', S) and not AnsiSameText(AItem.FileName, S) then
  begin
    S := ExpandUNCFileName(S);
    if RenameFile(ITV.ITems[ITV.SelectedIndex].FileName, S) then
    begin
      AItem.FileName := S;
      AItem.Caption := ExtractFileName(S);
    end
    else
      ShowMessage('Could not rename file!');
  end;
end;

procedure TfrmMain.Delete1Click(Sender: TObject);
var
  AItem: TJvImageItem;
begin
  if ITV.SelectedIndex < 0 then
    Exit;
  if MessageDlg('Are you sure you want to delete the selected file?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYEs then
  begin
    AItem := ITV.Items[ITV.SelectedIndex];
    if not DeleteFile(AItem.FileName) then
      ShowMessage('Could not delete the file!')
    else
      AItem.Delete;
  end;
end;

procedure TfrmMain.chkDisconnectClick(Sender: TObject);
begin
  if chkDisconnect.Checked then
  begin
    AInspector.InspectObject := nil;
    AInspector.Visible := False;
  end
  else
  begin
    AInspector.Visible := True;
    pgViewersChange(Sender);
  end;
end;

procedure TfrmMain.SetDisplayDragImage(AControl: TControl);
var
  I: Integer;
begin
  AControl.ControlStyle := AControl.ControlStyle + [csDisplayDragImage];
  if AControl is TWinControl then
    for I := 0 to TWinControl(AControl).ControlCOunt - 1 do
      SetDisplayDragImage(TWinControl(AControl).Controls[I]);
end;

procedure TfrmMain.SelectAll1Click(Sender: TObject);
begin
  ITV.SelectAll;
end;


procedure TfrmMain.DoITV3ItemHint(Sender: TObject; Index: Integer;
  var HintInfo: THintInfo; var Handled: Boolean);
var
  AColor: TColor;
begin
  AColor := TColor(ITV3.Items[Index].Data);
  HintInfo.HintColor := AColor;
  HintInfo.HintStr := ColorToString(AColor);
  Handled := true;
end;

end.

