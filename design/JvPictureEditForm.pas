{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPictEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPictureEditForm;

{$I jvcl.inc}

interface

uses
  Classes, Windows, Messages, Graphics, Forms, Controls, Dialogs, Menus,
  StdCtrls, ExtCtrls, ExtDlgs, Buttons,
  JvMRUManager, JvFormPlacement, JvClipboardMonitor, JvComponent, JvAppStorage,
  {$IFDEF MSWINDOWS}
  JvAppRegistryStorage,
  {$ENDIF MSWINDOWS}
  JvMRUList;

type
  TPictureEditDialog = class(TJvForm)
    Load: TButton;
    Save: TButton;
    Copy: TButton;
    Paste: TButton;
    Clear: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    HelpBtn: TButton;
    DecreaseBox: TCheckBox;
    FormStorage: TJvFormStorage;
    GroupBox: TGroupBox;
    ImagePanel: TPanel;
    ImagePaintBox: TPaintBox;
    Bevel: TBevel;
    Paths: TButton;
    PathsBtn: TSpeedButton;
    PathsMenu: TPopupMenu;
    PathsMRU: TJvMRUManager;
    AppStorage: TJvAppRegistryStorage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormStorageRestorePlacement(Sender: TObject);
    procedure FormStorageSavePlacement(Sender: TObject);
    procedure ImagePaintBoxPaint(Sender: TObject);
    procedure PathsClick(Sender: TObject);
    procedure PathsMRUClick(Sender: TObject; const RecentName,
      Caption: string; UserData: Longint);
    procedure PathsMenuPopup(Sender: TObject);
    procedure PathsMRUChange(Sender: TObject);
    procedure PathsBtnClick(Sender: TObject);
  private
    FGraphicClass: TGraphicClass;
    FClipMonitor: TJvClipboardMonitor;
    procedure CheckEnablePaste;
    procedure DecreaseBMPColors;
    procedure SetGraphicClass(Value: TGraphicClass);
    function GetDecreaseColors: Boolean;
    procedure LoadFile(const FileName: string);
    procedure UpdatePathsMenu;
    procedure UpdateClipboard(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
  protected
    procedure CreateHandle; override;
  public
    Pic: TPicture;
    IconColor: TColor;
    FileDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    procedure ValidateImage;
    property DecreaseColors: Boolean read GetDecreaseColors;
    property GraphicClass: TGraphicClass read FGraphicClass write SetGraphicClass;
  end;

implementation

uses
  TypInfo, SysUtils,
  Clipbrd, Consts,
  {$IFDEF MSWINDOWS}
  ShellAPI, LibHelp,
  {$ENDIF MSWINDOWS}
  JvJVCLUtils, JvJCLUtils, JvConsts, JvDsgnConsts, JvDirectoryListForm, JvTypes;


{$R *.dfm}

procedure CopyPicture(Pict: TPicture; BackColor: TColor);
begin
  if Pict.Graphic <> nil then
  begin
    if Pict.Graphic is TIcon then
      CopyIconToClipboard(Pict.Icon, BackColor)
    { check other specific graphic types here }
    else
      Clipboard.Assign(Pict);
  end;
end;

procedure PastePicture(Pict: TPicture; GraphicClass: TGraphicClass);
var
  NewGraphic: TGraphic;
begin
  if Pict <> nil then
  begin
    if Clipboard.HasFormat(CF_ICON) and
      ((GraphicClass = TIcon) or (GraphicClass = TGraphic)) then
    begin
      NewGraphic := CreateIconFromClipboard;
      if NewGraphic <> nil then
        try
          Pict.Assign(NewGraphic);
        finally
          NewGraphic.Free;
        end;
    end
    { check other specific graphic types here }
    else
    if Clipboard.HasFormat(CF_PICTURE) then
      Pict.Assign(Clipboard);
  end;
end;

function EnablePaste(Graph: TGraphicClass): Boolean;
begin
  if Graph = TBitmap then
    Result := Clipboard.HasFormat(CF_BITMAP)
  else
  if Graph = TMetaFile then
    Result := Clipboard.HasFormat(CF_METAFILEPICT)
  else
  if Graph = TIcon then
    Result := Clipboard.HasFormat(CF_ICON)
  { check other graphic types here }
  //else
  //if Graph = TGraphic then
  //  Result := Clipboard.HasFormat(CF_PICTURE)
  else
    Result := Clipboard.HasFormat(CF_PICTURE);
end;

function ValidPicture(Pict: TPicture): Boolean;
begin
  Result := (Pict.Graphic <> nil) and not Pict.Graphic.Empty;
end;

//=== { TPictureEditDialog } =================================================

procedure TPictureEditDialog.SetGraphicClass(Value: TGraphicClass);
begin
  FGraphicClass := Value;
  CheckEnablePaste;
  DecreaseBox.Enabled := (GraphicClass = TBitmap) or (GraphicClass = TGraphic);
end;

procedure TPictureEditDialog.CheckEnablePaste;
begin
  Paste.Enabled := EnablePaste(GraphicClass);
end;

procedure TPictureEditDialog.ValidateImage;
var
  Enable: Boolean;
begin
  Enable := ValidPicture(Pic);
  Save.Enabled := Enable;
  Clear.Enabled := Enable;
  Copy.Enabled := Enable;
end;

procedure TPictureEditDialog.UpdateClipboard(Sender: TObject);
begin
  CheckEnablePaste;
end;

procedure TPictureEditDialog.FormCreate(Sender: TObject);
begin
  Pic := TPicture.Create;
  FileDialog := TOpenPictureDialog.Create(Self);
  SaveDialog := TSavePictureDialog.Create(Self);
  FileDialog.Title := RsLoadPicture;
  SaveDialog.Title := RsSavePictureAs;
  Bevel.Visible := False;
  Font.Style := [];
  AppStorage.Root := SDelphiKey;
  PathsMRU.RecentMenu := PathsMenu.Items;
  IconColor := clBtnFace;
  HelpContext := hcDPictureEditor;
  Save.Enabled := False;
  Clear.Enabled := False;
  Copy.Enabled := False;
  FClipMonitor := TJvClipboardMonitor.Create(Self);
  FClipMonitor.OnChange := UpdateClipboard;
  CheckEnablePaste;
end;

function TPictureEditDialog.GetDecreaseColors: Boolean;
begin
  Result := DecreaseBox.Checked;
end;

procedure TPictureEditDialog.FormDestroy(Sender: TObject);
begin
  FClipMonitor.Free;
  Pic.Free;
end;

procedure TPictureEditDialog.LoadFile(const FileName: string);
begin
  Application.ProcessMessages;
  StartWait;
  try
    Pic.LoadFromFile(FileName);
  finally
    StopWait;
  end;
  ImagePaintBox.Invalidate;
  ValidateImage;
end;

procedure TPictureEditDialog.LoadClick(Sender: TObject);
begin
  if FileDialog.Execute then
    Self.LoadFile(FileDialog.FileName);
end;

procedure TPictureEditDialog.SaveClick(Sender: TObject);
begin
  if (Pic.Graphic <> nil) and not Pic.Graphic.Empty then
    with SaveDialog do
    begin
      DefaultExt := GraphicExtension(TGraphicClass(Pic.Graphic.ClassType));
      Filter := GraphicFilter(TGraphicClass(Pic.Graphic.ClassType));
      if Execute then
      begin
        StartWait;
        try
          Pic.SaveToFile(FileName);
        finally
          StopWait;
        end;
      end;
    end;
end;

procedure TPictureEditDialog.DecreaseBMPColors;
begin
  if ValidPicture(Pic) and (Pic.Graphic is TBitmap) and DecreaseColors then
    SetBitmapPixelFormat(Pic.Bitmap, pf4bit, DefaultMappingMethod);
end;

procedure TPictureEditDialog.CopyClick(Sender: TObject);
begin
  CopyPicture(Pic, IconColor);
end;

procedure TPictureEditDialog.PasteClick(Sender: TObject);
begin
  if Pic <> nil then
  begin
    PastePicture(Pic, GraphicClass);
    DecreaseBMPColors;
    ImagePaintBox.Invalidate;
    ValidateImage;
  end;
end;

procedure TPictureEditDialog.ImagePaintBoxPaint(Sender: TObject);
var
  DrawRect: TRect;
  None: string;
  Ico: HICON;
  W, H: Integer;
begin
  with TPaintBox(Sender) do
  begin
    Canvas.Brush.Color := Color;
    DrawRect := ClientRect;
    if ValidPicture(Pic) then
    begin
      with DrawRect do
        if (Pic.Width > Right - Left) or (Pic.Height > Bottom - Top) then
        begin
          if Pic.Width > Pic.Height then
            Bottom := Top + MulDiv(Pic.Height, Right - Left, Pic.Width)
          else
            Right := Left + MulDiv(Pic.Width, Bottom - Top, Pic.Height);
          Canvas.StretchDraw(DrawRect, Pic.Graphic);
        end
        else
        begin
          with DrawRect do
          begin
            if Pic.Graphic is TIcon then
            begin
              Ico := CreateRealSizeIcon(Pic.Icon);
              try
                GetIconSize(Ico, W, H);
                DrawIconEx(Canvas.Handle, (Left + Right - W) div 2,
                  (Top + Bottom - H) div 2, Ico, W, H, 0, 0, DI_NORMAL);
              finally
                DestroyIcon(Ico);
              end;
            end
            else
              Canvas.Draw((Right + Left - Pic.Width) div 2,
                (Bottom + Top - Pic.Height) div 2, Pic.Graphic);
          end;
        end;
    end
    else
      with DrawRect, Canvas do
      begin
        None := srNone;
        TextOut(Left + (Right - Left - TextWidth(None)) div 2, Top + (Bottom -
          Top - TextHeight(None)) div 2, None);
      end;
  end;
end;

procedure TPictureEditDialog.CreateHandle;
begin
  inherited CreateHandle;
  DragAcceptFiles(Handle, True);
end;

procedure TPictureEditDialog.WMDestroy(var Msg: TMessage);
begin
  DragAcceptFiles(Handle, False);
  inherited;
end;

procedure TPictureEditDialog.WMDropFiles(var Msg: TWMDropFiles);
var
  AFileName: array [0..255] of Char;
  Num: Cardinal;
begin
  Msg.Result := 0;
  try
    Num := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
    if Num > 0 then
    begin
      DragQueryFile(Msg.Drop, 0, PChar(@AFileName), Pred(SizeOf(AFileName)));
      Application.BringToFront;
      Self.LoadFile(StrPas(AFileName));
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TPictureEditDialog.UpdatePathsMenu;
var
  I: Integer;
begin
  for I := 0 to PathsMenu.Items.Count - 1 do
    PathsMenu.Items[I].Checked :=
      CompareText(PathsMenu.Items[I].Caption, FileDialog.InitialDir) = 0;
end;

procedure TPictureEditDialog.ClearClick(Sender: TObject);
begin
  Pic.Graphic := nil;
  ImagePaintBox.Invalidate;
  Save.Enabled := False;
  Clear.Enabled := False;
  Copy.Enabled := False;
end;

procedure TPictureEditDialog.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

const
  cBackColorIdent = 'ClipboardBackColor';
  cFileDir = 'FileDialog.InitialDir';

procedure TPictureEditDialog.FormStorageRestorePlacement(Sender: TObject);
begin
  IconColor := FormStorage.ReadInteger(cBackColorIdent, clBtnFace);
  FileDialog.InitialDir := FormStorage.ReadString(cFileDir, FileDialog.InitialDir);
end;

procedure TPictureEditDialog.FormStorageSavePlacement(Sender: TObject);
begin
  FormStorage.WriteInteger(cBackColorIdent, IconColor);
  if FileDialog.InitialDir <> '' then
    FormStorage.WriteString(cFileDir, FileDialog.InitialDir);
end;

procedure TPictureEditDialog.PathsClick(Sender: TObject);
begin
  if EditFolderList(PathsMRU.Strings) then
    UpdatePathsMenu;
end;

procedure TPictureEditDialog.PathsMRUClick(Sender: TObject;
  const RecentName, Caption: string; UserData: Longint);
begin
  if DirectoryExists(RecentName) then
    {SetCurrentDir(RecentName);}
    FileDialog.InitialDir := RecentName
  else
    PathsMRU.Remove(RecentName);
  UpdatePathsMenu;
end;

procedure TPictureEditDialog.PathsMenuPopup(Sender: TObject);
begin
  UpdatePathsMenu;
end;

procedure TPictureEditDialog.PathsMRUChange(Sender: TObject);
begin
  PathsBtn.Enabled := PathsMRU.Strings.Count > 0;
end;

procedure TPictureEditDialog.PathsBtnClick(Sender: TObject);
var P:TPoint;
begin
  P := PathsBtn.ClientOrigin;
  PathsMenu.Popup(P.X,P.Y + PathsBtn.Height);
end;

end.

