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

{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

{$I jvcl.inc}

unit GIFMain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, JvSpeedbar, Grids,
  ComCtrls, ExtDlgs{$IFNDEF BCB}, Jpeg{$ENDIF},
  JvGIF, JvLabel, Mask, JvToolEdit, ImgList,
  JvComponent, JvFormPlacement, JvExControls, JvExExtCtrls, JvExMask;

type
  TAnimatorForm = class(TForm)
    JvSpeedbar: TJvSpeedBar;
    SpeedbarSection1: TjvSpeedbarSection;
    NewBtn: TjvSpeedItem;
    OpenBtn: TjvSpeedItem;
    SaveBtn: TjvSpeedItem;
    SaveAsBtn: TjvSpeedItem;
    InsertBtn: TjvSpeedItem;
    SpeedbarSection2: TjvSpeedbarSection;
    SpeedbarSection3: TjvSpeedbarSection;
    Storage: TJvFormStorage;
    CutBtn: TjvSpeedItem;
    CopyBtn: TjvSpeedItem;
    PasteBtn: TjvSpeedItem;
    DeleteBtn: TjvSpeedItem;
    SpeedbarSection4: TjvSpeedbarSection;
    AboutBtn: TjvSpeedItem;
    UpBtn: TjvSpeedItem;
    DownBtn: TjvSpeedItem;
    PreviewBtn: TjvSpeedItem;
    Pages: TPageControl;
    OptionsTab: TTabSheet;
    AnimationTab: TTabSheet;
    ImageTab: TTabSheet;
    Progress: TProgressBar;
    Bevel: TBevel;
    ImageScroll: TScrollBar;
    Frame1: TPanel;
    Frame2: TPanel;
    Frame3: TPanel;
    Frame4: TPanel;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    PaintBox3: TPaintBox;
    PaintBox4: TPaintBox;
    No1: TLabel;
    No2: TLabel;
    No3: TLabel;
    No4: TLabel;
    Status: TLabel;
    ColorDepthCombo: TComboBox;
    Label1: TLabel;
    ThumbnailsBox: TCheckBox;
    ThumbnailsLabel: TJvLabel;
    Frame1Lbl: TLabel;
    Frame2Lbl: TLabel;
    Frame3Lbl: TLabel;
    Frame4Lbl: TLabel;
    TrailingComment: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    FrameComment: TMemo;
    Label4: TLabel;
    Label5: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    ScreenW: TLabel;
    ScreenH: TLabel;
    Label6: TLabel;
    ImageCount: TLabel;
    Bevel3: TBevel;
    BackColor: TJvComboEdit;
    Label7: TLabel;
    Label8: TLabel;
    FrameW: TLabel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    FrameH: TLabel;
    Label11: TLabel;
    ImageLeft: TEdit;
    ImageLeftBtn: TUpDown;
    Label9: TLabel;
    Label10: TLabel;
    ImageTop: TEdit;
    ImageTopBtn: TUpDown;
    Label12: TLabel;
    DelayTime: TEdit;
    DelayTimeBtn: TUpDown;
    DisposalCombo: TComboBox;
    Label13: TLabel;
    TransColor: TJvComboEdit;
    TransBox: TCheckBox;
    TransColorLabel: TJvLabel;
    Label14: TLabel;
    GIFVersion: TLabel;
    Bevel6: TBevel;
    FlatBtns: TCheckBox;
    AlwaysOnTop: TCheckBox;
    AlwaysOnTopLabel: TJvLabel;
    GrayscaleBtn: TjvSpeedItem;
    ToolbarImages: TImageList;
    RepeatCntBtn: TUpDown;
    RepeatCnt: TEdit;
    LoopBox: TCheckBox;
    LoopLbl: TJvLabel;
    RepeatForever: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ImageScrollChange(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NewBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SaveAsBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure CutBtnClick(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ThumbnailsBoxClick(Sender: TObject);
    procedure ColorDepthComboChange(Sender: TObject);
    procedure CommentChange(Sender: TObject);
    procedure BackColorButtonClick(Sender: TObject);
    procedure TopLeftChange(Sender: TObject);
    procedure DelayTimeChange(Sender: TObject);
    procedure DisposalComboChange(Sender: TObject);
    procedure TransColorButtonClick(Sender: TObject);
    procedure TransBoxClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedBarDblClick(Sender: TObject);
    procedure FlatBtnsClick(Sender: TObject);
    procedure AlwaysOnTopClick(Sender: TObject);
    procedure CheckLabelMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GrayscaleBtnClick(Sender: TObject);
    procedure LoopChange(Sender: TObject);
  private
    { Private declarations }
    FImage: TJvGIFImage;
    FTopIndex: Integer;
    FFileName: string;
    FModified: Boolean;
    FLoading: Boolean;
    FUpdating: Boolean;
    FAborting: Boolean;
    FProgressPos: Integer;
    {$IFDEF COMPILER3_UP}
    FOpenDlg: TOpenPictureDialog;
    FSaveDlg: TSavePictureDialog;
    {$ELSE}
    FOpenDlg: TOpenDialog;
    FSaveDlg: TSaveDialog;
    {$ENDIF}
    procedure GraphicProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
    procedure SetSelectedIndex(Index: Integer; Force: Boolean);
    procedure ImageChanged(Sender: TObject);
    procedure EnableButtons;
    procedure EnableTabs;
    procedure UpdateControls;
    procedure UpdateCaption;
    procedure SelectBackColor;
    procedure SelectTransColor;
    procedure ConfirmSave;
    procedure GrayscaleImage;
    procedure InvalidateImage(UpdateAll: Boolean);
  protected
    function GetPalette: HPALETTE; override;
  end;

var
  AnimatorForm: TAnimatorForm;

implementation

uses
  Clipbrd, JvJVCLUtils, JvJCLUtils, JvHints, GIFPrvw, GIFPal, Math;

{$R *.DFM}

const
  SReady = 'Ready';

function EnablePaste: Boolean; near;
begin
  Result := Clipboard.HasFormat(CF_GIF) or Clipboard.HasFormat(CF_BITMAP);
end;

{ TAnimatorForm }

procedure TAnimatorForm.GraphicProgress(Sender: TObject;
  Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: string);
begin
  if Stage in [psStarting, psEnding] then
  begin
    FProgressPos := 0;
    Progress.Position := 0;
    Application.ProcessMessages;
  end
  else
  if Stage = psRunning then
  begin
    if PercentDone >= FProgressPos + 10 then
    begin
      Progress.Position := PercentDone;
      FProgressPos := PercentDone;
    end;
  end;
  if Msg <> '' then
  begin
    Status.Caption := Msg;
    Status.Update;
  end;
  if FLoading and FAborting then
  begin
    FAborting := False;
    Abort;
  end;
end;

procedure TAnimatorForm.UpdateCaption;
begin
  Caption := Application.Title;
  if (FFileName <> '') then
    Caption := Caption + ' - ' + ExtractFileName(FFileName);
end;

procedure TAnimatorForm.UpdateControls;
begin
  FUpdating := True;
  try
    ScreenW.Caption := IntToStr(FImage.ScreenWidth);
    ScreenH.Caption := IntToStr(FImage.ScreenHeight);
    ImageCount.Caption := IntToStr(FImage.Count);
    BackColor.Color := PaletteColor(FImage.BackgroundColor);
    LoopBox.Checked := FImage.Looping;
    RepeatForever.Checked := FImage.RepeatCount = 0;
    RepeatCnt.Text := IntToStr(FImage.RepeatCount);
    TrailingComment.Lines := FImage.Comment;
    case FImage.Version of
      gv87a: GIFVersion.Caption := '87a';
      gv89a: GIFVersion.Caption := '89a';
    else
      GIFVersion.Caption := '';
    end;
    if FImage.FrameIndex >= 0 then
    begin
      with FImage.Frames[FImage.FrameIndex] do
      begin
        FrameComment.Lines := Comment;
        FrameW.Caption := IntToStr(Width);
        FrameH.Caption := IntToStr(Height);
        ImageLeftBtn.Position := Origin.X;
        ImageTopBtn.Position := Origin.Y;
        DelayTimeBtn.Position :=
          FImage.Frames[FImage.FrameIndex].AnimateInterval div 10;
        DelayTime.Text := IntToStr(DelayTimeBtn.Position);
        if DisposalMethod in [dmUndefined..dmRestorePrevious] then
          DisposalCombo.ItemIndex := Ord(DisposalMethod)
        else
          DisposalCombo.ItemIndex := 0;
        TransBox.Checked := TransparentColor <> clNone;
        TransColor.Enabled := TransBox.Checked;
        TransColorLabel.Enabled := TransBox.Checked;
        TransColor.Color := PaletteColor(TransparentColor);
      end;
    end
    else
    begin
      FrameComment.Lines.Clear;
      FrameW.Caption := '';
      FrameH.Caption := '';
    end;
  finally
    FUpdating := False;
  end;
end;

function TAnimatorForm.GetPalette: HPALETTE;
begin
  Result := FImage.Palette;
  if Result = 0 then
    Result := inherited GetPalette;
end;

procedure TAnimatorForm.EnableButtons;
var
  IsSelected: Boolean;
begin
  IsSelected := not FImage.Empty and (FImage.FrameIndex >= 0);
  SaveBtn.Enabled := (FFileName <> '') and IsSelected;
  SaveAsBtn.Enabled := IsSelected;
  CutBtn.Enabled := IsSelected;
  CopyBtn.Enabled := IsSelected;
  DeleteBtn.Enabled := IsSelected;
  GrayscaleBtn.Enabled := not FImage.Empty;
  PasteBtn.Enabled := EnablePaste;
  UpBtn.Enabled := IsSelected and (FImage.FrameIndex > 0);
  DownBtn.Enabled := IsSelected and (FImage.FrameIndex < FImage.Count - 1);
  PreviewBtn.Enabled := IsSelected;
end;

procedure TAnimatorForm.GrayscaleImage;
begin
  if not FImage.Empty then
  begin
    FImage.Grayscale(True);
    FModified := True;
  end;
end;

procedure TAnimatorForm.ImageChanged(Sender: TObject);
var
  Enable: Boolean;
begin
  with ImageScroll do
  begin
    Min := 0;
    Max := Math.Max(0, FImage.Count - 1);
    Enable := (Max > Min) and not FImage.Empty;
    if not Enable then
      ActiveControl := Pages;
    Enabled := Enable;
  end;
  SetSelectedIndex(Max(0, FImage.FrameIndex), True);
  EnableTabs;
  if not FLoading then
  begin
    if Active and HandleAllocated then
      PostMessage(Handle, WM_QUERYNEWPALETTE, 0, 0);
    UpdateControls;
    EnableButtons;
  end;
end;

procedure TAnimatorForm.SetSelectedIndex(Index: Integer; Force: Boolean);
var
  I: Integer;
  Labl: TLabel;
begin
  if Force or (Index <> FImage.FrameIndex) then
  begin
    I := FTopIndex;
    Index := Min(FImage.Count - 1, Max(Index, -1));
    if (FImage.Count > 0) then
      FTopIndex := Max(FTopIndex, 0);
    while (FTopIndex < Index - 3) do
      Inc(FTopIndex);
    if Index < FTopIndex then
      FTopIndex := Index;
    FImage.FrameIndex := Index;
    if FImage.FrameIndex <> ImageScroll.Position then
      ImageScroll.Position := FImage.FrameIndex;
    InvalidateImage(I <> FTopIndex);
  end;
  for I := 1 to 4 do
  begin
    Labl := TLabel(FindComponent(Format('Frame%dLbl', [I])));
    if Labl <> nil then
      Labl.Visible := (FTopIndex + Labl.Tag < FImage.Count) and
        (FImage.Count > 0);
    Labl := TLabel(FindComponent(Format('No%d', [I])));
    if Labl <> nil then
    begin
      Labl.Caption := IntToStr(FTopIndex + Labl.Tag + 1);
      Labl.Visible := (FTopIndex + Labl.Tag < FImage.Count) and
        (FImage.Count > 0);
    end;
  end;
end;

procedure TAnimatorForm.EnableTabs;
var
  Empty: Boolean;
begin
  Empty := FImage.Empty;
  AnimationTab.Enabled := not Empty;
  AnimationTab.TabVisible := not Empty;
  ImageTab.Enabled := not Empty;
  ImageTab.TabVisible := not Empty;
  if Empty then
    Pages.ActivePage := OptionsTab;
end;

procedure TAnimatorForm.InvalidateImage(UpdateAll: Boolean);
begin
  if not FLoading then
  begin
    PaintBox1.Invalidate;
    PaintBox2.Invalidate;
    PaintBox3.Invalidate;
    PaintBox4.Invalidate;
  end;
end;

procedure TAnimatorForm.SelectBackColor;
var
  AColor, NewColor: TColor;
begin
  AColor := FImage.BackgroundColor;
  NewColor := SelectColor(FImage.Palette, AColor);
  if NewColor <> AColor then
  begin
    FImage.BackgroundColor := NewColor;
    FModified := True;
  end;
end;

procedure TAnimatorForm.SelectTransColor;
var
  Frame: TJvGIFFrame;
  AColor, NewColor: TColor;
begin
  if FImage.FrameIndex >= 0 then
  begin
    Frame := FImage.Frames[FImage.FrameIndex];
    AColor := Frame.TransparentColor;
    NewColor := SelectColor(FImage.Palette, AColor);
    if NewColor <> AColor then
    begin
      Frame.TransparentColor := NewColor;
      FModified := True;
    end;
  end
  else
    MessageBeep(MB_ICONEXCLAMATION);
end;

procedure TAnimatorForm.ConfirmSave;
var
  Res: TModalResult;
begin
  if FModified then
  begin
    Res := MessageDlg('Save changes to current animation?', mtWarning,
      [mbYes, mbNo, mbCancel], 0);
    case Res of
      mrYes: SaveBtnClick(nil);
      mrCancel: Abort;
    end;
  end;
end;

procedure TAnimatorForm.FormCreate(Sender: TObject);
begin
  { Registry settings }
  // (rom) deactivated not part of JVCL anymore
  //RegUseAppTitle := True;
  //DefCompanyName := 'RX';
  { Initialization }
  SetHintStyle(hsRectangle, 3, False, taCenter);
  UpdateCaption;
  FTopIndex := -1;
  FImage := TJvGIFImage.Create;
  EnableTabs;
  EnableButtons;
  ColorDepthCombo.ItemIndex := Ord(DefaultMappingMethod);
  {$IFDEF COMPILER3_UP}
  FOpenDlg := TOpenPictureDialog.Create(Self);
  FSaveDlg := TSavePictureDialog.Create(Self);
  {$ELSE}
  FOpenDlg := TOpenDialog.Create(Self);
  FSaveDlg := TSaveDialog.Create(Self);
  {$ENDIF}
  with FOpenDlg do
  begin
    Name := 'OpenDialog';
    Options := [ofHideReadOnly, ofFileMustExist];
    DefaultExt := GraphicExtension(TJvGIFImage);
    Filter := GraphicFilter(TGraphic);
    InitialDir := ExtractFileDir(Application.ExeName) + '\data';
  end;
  with FSaveDlg do
  begin
    Name := 'SaveDialog';
    Options := [ofHideReadOnly, ofOverwritePrompt];
    DefaultExt := GraphicExtension(TJvGIFImage);
    Filter := GraphicFilter(TJvGIFImage);
  end;
  { Add dialogs to storage }
//!!!  with Storage.StoredProps do begin
//!!!    AddObject(CreateStoredItem(FOpenDlg.Name, 'InitialDir'), FOpenDlg);
//!!!    AddObject(CreateStoredItem(FSaveDlg.Name, 'InitialDir'), FSaveDlg);
//!!!  end;
  Application.OnActivate := FormActivate;
  FImage.OnProgress := GraphicProgress;
  FImage.OnChange := ImageChanged;
  Status.Caption := SReady;
  FModified := False;
end;

procedure TAnimatorForm.FormDestroy(Sender: TObject);
begin
  FImage.Free;
end;

procedure TAnimatorForm.FormActivate(Sender: TObject);
begin
  PasteBtn.Enabled := EnablePaste;
end;

procedure TAnimatorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  ConfirmSave;
end;

procedure TAnimatorForm.PreviewBtnClick(Sender: TObject);
begin
  PreviewGIF(FImage);
end;

procedure TAnimatorForm.OpenBtnClick(Sender: TObject);
begin
  ConfirmSave;
  FOpenDlg.Filter := GraphicFilter(TJvGIFImage);
  try
    if FOpenDlg.Execute then
    begin
      StartWait;
      FLoading := True;
      try
        FOpenDlg.InitialDir := ExtractFilePath(FOpenDlg.FileName);
        Status.Caption := 'Press <Esc> to cancel';
        Application.ProcessMessages;
        try
          FImage.LoadFromFile(FOpenDlg.FileName);
          FImage.DecodeAllFrames;
          FModified := False;
        except
          FImage.Clear;
          FFileName := '';
          raise;
        end;
        FFileName := FOpenDlg.FileName;
        UpdateControls;
      finally
        FLoading := False;
        FAborting := False;
        FModified := False;
        StopWait;
        Status.Caption := SReady;
        InvalidateImage(True);
        UpdateCaption;
        EnableButtons;
      end;
    end;
  finally
    FOpenDlg.Filter := GraphicFilter(TGraphic);
  end;
end;

procedure TAnimatorForm.PaintBoxPaint(Sender: TObject);
var
  ImageIndex, Size: Integer;
  W, H, L, T: Integer;
  DrawRect: TRect;
  Frame: TJvGIFFrame;
begin
  ImageIndex := FTopIndex + TPaintBox(Sender).Tag;
  if (ImageIndex >= 0) and (ImageIndex < FImage.Count) then
  begin
    DrawRect := TPaintBox(Sender).ClientRect;
    Frame := FImage.Frames[ImageIndex];
    with TPaintBox(Sender).Canvas do
    begin
      if ImageIndex = FImage.FrameIndex then
      begin
        Pen.Color := clActiveCaption;
        Pen.Width := 3;
        with DrawRect do
          Rectangle(Left, Top, Right, Bottom);
      end;
      InflateRect(DrawRect, -3, -3);
      if ThumbnailsBox.Checked then
      begin
        W := FImage.ScreenWidth;
        H := FImage.ScreenHeight;
        L := Frame.Origin.X;
        T := Frame.Origin.Y;
      end
      else
      begin
        W := Frame.Width;
        H := Frame.Height;
        L := 0;
        T := 0;
      end;

      if (W <= RectWidth(DrawRect)) and
        (H <= RectHeight(DrawRect)) then
      begin
        DrawRect := Bounds(L, T, Frame.Width, Frame.Height);
      end
      else
      begin
        Size := DrawRect.Top + MulDiv(H, RectWidth(DrawRect), W);
        if Size > DrawRect.Bottom then
        begin
          L := MulDiv(L, RectHeight(DrawRect), H);
          T := MulDiv(T, RectHeight(DrawRect), H);
          W := MulDiv(Frame.Width, RectHeight(DrawRect), H);
          H := MulDiv(Frame.Height, RectHeight(DrawRect), H);
          DrawRect := Bounds(L, T, W, H);
        end
        else
        begin
          L := MulDiv(L, RectWidth(DrawRect), W);
          T := MulDiv(T, RectWidth(DrawRect), W);
          H := MulDiv(Frame.Height, RectWidth(DrawRect), W);
          W := MulDiv(Frame.Width, RectWidth(DrawRect), W);
          DrawRect := Bounds(L, T, W, H);
        end;
      end;
      OffsetRect(DrawRect, 3, 3);
      Frame.Draw(TPaintBox(Sender).Canvas, DrawRect, False);
    end;
  end;
end;

procedure TAnimatorForm.ImageScrollChange(Sender: TObject);
begin
  SetSelectedIndex(ImageScroll.Position, False);
end;

procedure TAnimatorForm.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ImageIndex: Integer;
begin
  if (FImage.Count > 0) then
  begin
    if ImageScroll.Enabled then
      ActiveControl := ImageScroll;
    ImageIndex := FTopIndex + TPaintBox(Sender).Tag;
    if (ImageIndex >= 0) and (ImageIndex < FImage.Count) then
      SetSelectedIndex(ImageIndex, True);
  end;
end;

procedure TAnimatorForm.NewBtnClick(Sender: TObject);
begin
  ConfirmSave;
  if not FImage.Empty then
    FImage.Clear;
  FFileName := '';
  UpdateCaption;
  Status.Caption := SReady;
  FModified := False;
end;

procedure TAnimatorForm.SaveBtnClick(Sender: TObject);
begin
  if FFileName = '' then
    SaveAsBtnClick(Sender)
  else
  begin
    StartWait;
    try
      FImage.SaveToFile(FFileName);
      FModified := False;
    finally
      StopWait;
      Status.Caption := SReady;
    end;
  end;
end;

procedure TAnimatorForm.SaveAsBtnClick(Sender: TObject);
begin
  if FFileName <> '' then
  begin
    FSaveDlg.FileName := FFileName;
    FSaveDlg.InitialDir := ExtractFilePath(FSaveDlg.FileName);
  end;
  if not FImage.Empty and FSaveDlg.Execute then
  begin
    Application.ProcessMessages;
    StartWait;
    try
      FSaveDlg.InitialDir := ExtractFilePath(FSaveDlg.FileName);
      FImage.SaveToFile(FSaveDlg.FileName);
      FModified := False;
      FFileName := FSaveDlg.FileName;
      UpdateCaption;
    finally
      StopWait;
      Status.Caption := SReady;
    end;
  end;
end;

procedure TAnimatorForm.InsertBtnClick(Sender: TObject);
var
  Pic: TPicture;
begin
  if FOpenDlg.Execute then
  begin
    Application.ProcessMessages;
    StartWait;
    Pic := TPicture.Create;
    try
      FOpenDlg.InitialDir := ExtractFilePath(FOpenDlg.FileName);
      {$IFDEF COMPILER3_UP}
      Pic.OnProgress := GraphicProgress;
      {$ENDIF}
      Pic.LoadFromFile(FOpenDlg.FileName);
      if (Pic.Graphic <> nil) and not Pic.Graphic.Empty then
      begin
        FImage.AddFrame(Pic.Graphic);
        FModified := True;
        ImageChanged(FImage);
        StartWait;
        try
          FImage.EncodeAllFrames;
        finally
          StopWait;
        end;
      end;
    finally
      Pic.Free;
      StopWait;
      Status.Caption := SReady;
    end;
  end;
end;

procedure TAnimatorForm.PasteBtnClick(Sender: TObject);
var
  Temp: TPicture;
begin
  if not EnablePaste then
    Exit;
  Temp := TPicture.Create;
  try
    Temp.Assign(Clipboard);
    if (Temp.Graphic <> nil) and not Temp.Graphic.Empty then
    begin
      FImage.AddFrame(Temp.Graphic);
      FModified := True;
      ImageChanged(FImage);
      StartWait;
      try
        FImage.EncodeAllFrames;
      finally
        StopWait;
      end;
    end;
  finally
    Temp.Free;
    Status.Caption := SReady;
  end;
end;

procedure TAnimatorForm.CutBtnClick(Sender: TObject);
begin
  CopyBtnClick(Sender);
  DeleteBtnClick(Sender);
end;

procedure TAnimatorForm.CopyBtnClick(Sender: TObject);
var
  Temp: TJvGIFImage;
begin
  if not FImage.Empty and (FImage.FrameIndex >= 0) then
  begin
    Temp := TJvGIFImage.Create;
    try
      Temp.Assign(FImage.Frames[FImage.FrameIndex]);
      Clipboard.Assign(Temp);
      PasteBtn.Enabled := EnablePaste;
    finally
      Temp.Free;
    end;
  end;
end;

procedure TAnimatorForm.DeleteBtnClick(Sender: TObject);
begin
  if FImage.FrameIndex >= 0 then
  begin
    FImage.DeleteFrame(FImage.FrameIndex);
    FModified := True;
  end;
end;

procedure TAnimatorForm.UpBtnClick(Sender: TObject);
begin
  if FImage.FrameIndex > 0 then
  begin
    FImage.MoveFrame(FImage.FrameIndex, FImage.FrameIndex - 1);
    FModified := True;
  end;
end;

procedure TAnimatorForm.DownBtnClick(Sender: TObject);
begin
  if (FImage.FrameIndex >= 0) and (FImage.FrameIndex < FImage.Count - 1) then
  begin
    FImage.MoveFrame(FImage.FrameIndex, FImage.FrameIndex + 1);
    FModified := True;
  end;
end;

procedure TAnimatorForm.AboutBtnClick(Sender: TObject);
begin
 MessageDlg(
   'RX GIF Image Format Support'+#13+#10+
   'RX Library Demo Program'+#13+#10+'1, 1, 1998', mtInformation, [mbOK], 0);
end;

procedure TAnimatorForm.ThumbnailsBoxClick(Sender: TObject);
begin
  InvalidateImage(True);
end;

procedure TAnimatorForm.ColorDepthComboChange(Sender: TObject);
begin
  DefaultMappingMethod := TMappingMethod(ColorDepthCombo.ItemIndex);
end;

procedure TAnimatorForm.BackColorButtonClick(Sender: TObject);
begin
  SelectBackColor;
end;

procedure TAnimatorForm.TransColorButtonClick(Sender: TObject);
begin
  SelectTransColor;
end;

procedure TAnimatorForm.LoopChange(Sender: TObject);
begin
  RepeatForever.Enabled := LoopBox.Checked;
  LoopLbl.Enabled := RepeatForever.Enabled and not RepeatForever.Checked;
  RepeatCnt.Enabled := LoopLbl.Enabled;
  RepeatCntBtn.Enabled := LoopLbl.Enabled;
  if not (FLoading or FUpdating) then
  begin
    if RepeatForever.Checked then
      FImage.RepeatCount := 0
    else
      FImage.RepeatCount := StrToIntDef(RepeatCnt.Text, FImage.RepeatCount);
    FImage.Looping := LoopBox.Checked;
    FModified := True;
  end;
end;

procedure TAnimatorForm.CommentChange(Sender: TObject);
begin
  if not (FLoading or FUpdating) then
  begin
    if (Sender = FrameComment) and (FImage.FrameIndex >= 0) then
      FImage.Frames[FImage.FrameIndex].Comment := FrameComment.Lines
    else
    if Sender = TrailingComment then
      FImage.Comment := TrailingComment.Lines;
    FModified := True;
  end;
end;

procedure TAnimatorForm.TopLeftChange(Sender: TObject);
begin
  if not (FLoading or FUpdating) and (FImage.FrameIndex >= 0) then
  begin
    FImage.Frames[FImage.FrameIndex].Origin := Point(
      StrToIntDef(ImageLeft.Text, ImageLeftBtn.Position),
      StrToIntDef(ImageTop.Text, ImageTopBtn.Position));
    FModified := True;
  end;
end;

procedure TAnimatorForm.DelayTimeChange(Sender: TObject);
begin
  if not (FLoading or FUpdating) and (FImage.FrameIndex >= 0) then
  begin
    FImage.Frames[FImage.FrameIndex].AnimateInterval :=
      StrToIntDef(DelayTime.Text, DelayTimeBtn.Position) * 10;
    FModified := True;
  end;
end;

procedure TAnimatorForm.DisposalComboChange(Sender: TObject);
begin
  if not (FLoading or FUpdating) and (FImage.FrameIndex >= 0) then
  begin
    if DisposalCombo.ItemIndex >= 0 then
    begin
      FImage.Frames[FImage.FrameIndex].DisposalMethod :=
        TDisposalMethod(DisposalCombo.ItemIndex);
      FModified := True;
    end;
  end;
end;

procedure TAnimatorForm.TransBoxClick(Sender: TObject);
begin
  TransColor.Enabled := TransBox.Checked;
  TransColorLabel.Enabled := TransBox.Checked;
  if not (FLoading or FUpdating) and (FImage.FrameIndex >= 0) then
  begin
    if not TransBox.Checked then
    begin
      FImage.Frames[FImage.FrameIndex].TransparentColor := clNone;
      FModified := True;
    end;
  end;
end;

procedure TAnimatorForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) and FLoading then
  begin
    FAborting := True;
    Key := #0;
  end;
end;

procedure TAnimatorForm.SpeedBarDblClick(Sender: TObject);
begin
  JvSpeedbar.Customize(0);
end;

procedure TAnimatorForm.FlatBtnsClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    JvSpeedbar.Options := JvSpeedbar.Options + [sbFlatBtns]
  else
    JvSpeedbar.Options := JvSpeedbar.Options - [sbFlatBtns];
end;

procedure TAnimatorForm.AlwaysOnTopClick(Sender: TObject);
begin
  if AlwaysOnTop.Checked then
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE)
  else
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TAnimatorForm.CheckLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    if TJvLabel(Sender).FocusControl.CanFocus then
      ActiveControl := TJvLabel(Sender).FocusControl;
    if (TJvLabel(Sender).FocusControl is TCheckBox) then
      with TCheckBox(TJvLabel(Sender).FocusControl) do
        if Enabled then
          Checked := not Checked;
  end;
end;

procedure TAnimatorForm.GrayscaleBtnClick(Sender: TObject);
begin
  GrayscaleImage;
end;

end.

