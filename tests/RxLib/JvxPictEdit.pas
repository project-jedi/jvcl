{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxPictEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
Portions copyright  (c) 1995, 1997 Borland International 
Portions copyright (c) 1995, 1996 AO ROSNO     
Portions copyright (c) 1997 Master-Bank        
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxPictEdit;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Forms, Controls, Dialogs, Buttons,
  RTLConsts, DesignIntf, DesignEditors, VCLEditors, StdCtrls, ExtCtrls, JvxPlacemnt, JvxClipMon,
  {$IFDEF Delphi3_Up} ExtDlgs, ComCtrls, {$ELSE} JvxImagPrvw, {$ENDIF} Menus,
  JvxMRUList, JvxCtrls;

type

{ TJvxPictureEditDialog }

  TJvxPictureEditDialog = class(TForm)
    Load: TButton;
    Save: TButton;
    Copy: TButton;
    Paste: TButton;
    Clear: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    HelpBtn: TButton;
    DecreaseBox: TCheckBox;
    UsePreviewBox: TCheckBox;
    FormStorage: TJvxFormStorage;
    GroupBox: TGroupBox;
    ImagePanel: TPanel;
    ImagePaintBox: TPaintBox;
    Bevel: TBevel;
    Paths: TButton;
    PathsBtn: TJvxSpeedButton;
    PathsMenu: TPopupMenu;
    PathsMRU: TJvxMRUManager;
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
  private
    FGraphicClass: TGraphicClass;
    Pic: TPicture;
    FIconColor: TColor;
    FClipMonitor: TJvxClipboardMonitor;
{$IFDEF Delphi3_Up}
    FProgress: TProgressBar;
    FProgressPos: Integer;
    FileDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
{$ELSE}
    FileDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
{$ENDIF}
    procedure CheckEnablePaste;
    procedure ValidateImage;
    procedure DecreaseBMPColors;
    procedure SetGraphicClass(Value: TGraphicClass);
    function GetDecreaseColors: Boolean;
    procedure LoadFile(const FileName: string);
    procedure UpdatePathsMenu;
    procedure UpdateClipboard(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
{$IFDEF Delphi3_Up}
    procedure GraphicProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
{$ENDIF}
  protected
    procedure CreateHandle; override;
  public
    property DecreaseColors: Boolean read GetDecreaseColors;
    property GraphicClass: TGraphicClass read FGraphicClass write SetGraphicClass;
  end;

{ TJvxPictEditor }

  TJvxPictEditor = class(TComponent)
  private
    FGraphicClass: TGraphicClass;
    FPicture: TPicture;
    FPicDlg: TJvxPictureEditDialog;
    FDecreaseColors: Boolean;
    procedure SetPicture(Value: TPicture);
    procedure SetGraphicClass(Value: TGraphicClass);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property PicDlg: TJvxPictureEditDialog read FPicDlg;
    property GraphicClass: TGraphicClass read FGraphicClass write SetGraphicClass;
    property Picture: TPicture read FPicture write SetPicture;
  end;

{ TJvxPictProperty }

{ Property editor the TPicture properties (e.g. the Picture property). Brings
  up a file open dialog allowing loading a picture file. }

  TJvxPictProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TJvxGraphicPropertyEditor }

  TJvxGraphicPropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TJvxGraphicsEditor }

  TJvxGraphicsEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  end;

function EditGraphic(Graphic: TGraphic; AClass: TGraphicClass;
  const DialogCaption: string): Boolean;

implementation

uses TypInfo, SysUtils, Clipbrd, Consts, ShellApi, LibHelp, JvxClipIcon, JvxGraph,
  JvxVCLUtils, JvxAppUtils, JvxConst, JvxDirFrm, JvxFileUtil;

{$B-}
{$IFDEF WIN32}
 {$D-}
{$ENDIF}

{$R *.DFM}

procedure CopyPicture(Pict: TPicture; BackColor: TColor);
begin
  if Pict.Graphic <> nil then begin
    if Pict.Graphic is TIcon then CopyIconToClipboard(Pict.Icon, BackColor)
    { check another specific graphic types here }
    else Clipboard.Assign(Pict);
  end;
end;

procedure PastePicture(Pict: TPicture; GraphicClass: TGraphicClass);
var
  NewGraphic: TGraphic;
begin
  if (Pict <> nil) then begin
    if Clipboard.HasFormat(CF_ICON) and ((GraphicClass = TIcon) or
      (GraphicClass = TGraphic)) then
    begin
      NewGraphic := CreateIconFromClipboard;
      if NewGraphic <> nil then
        try
          Pict.Assign(NewGraphic);
        finally
          NewGraphic.Free;
        end;
    end
    { check another specific graphic types here }
    else if Clipboard.HasFormat(CF_PICTURE) then
      Pict.Assign(Clipboard);
  end;
end;

function EnablePaste(Graph: TGraphicClass): Boolean;
begin
  if (Graph = TBitmap) then Result := Clipboard.HasFormat(CF_BITMAP)
  else if (Graph = TMetafile) then Result := Clipboard.HasFormat(CF_METAFILEPICT)
  else if (Graph = TIcon) then Result := Clipboard.HasFormat(CF_ICON)
  { check another graphic types here }
  else if (Graph = TGraphic) then Result := Clipboard.HasFormat(CF_PICTURE)
  else Result := Clipboard.HasFormat(CF_PICTURE);
end;

function ValidPicture(Pict: TPicture): Boolean;
begin
  Result := (Pict.Graphic <> nil) and not Pict.Graphic.Empty;
end;

{ TJvxPictEditor }

constructor TJvxPictEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicDlg := TJvxPictureEditDialog.Create(Self);
  FGraphicClass := TGraphic;
  FPicDlg.GraphicClass := FGraphicClass;
end;

destructor TJvxPictEditor.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TJvxPictEditor.Execute: Boolean;
var
  Bmp: TBitmap;
  CurDir: string;
begin
  FPicDlg.Pic.Assign(FPicture);
  with FPicDlg.FileDialog do
  begin
    Options := [ofHideReadOnly, ofFileMustExist, ofShowHelp];
    DefaultExt := GraphicExtension(GraphicClass);
    Filter := GraphicFilter(GraphicClass);
    HelpContext := hcDLoadPicture;
  end;
  with FPicDlg.SaveDialog do
  begin
    Options := [ofHideReadOnly, ofFileMustExist, ofShowHelp,
      ofOverwritePrompt];
    DefaultExt := GraphicExtension(GraphicClass);
    Filter := GraphicFilter(GraphicClass);
    HelpContext := hcDSavePicture;
  end;
  FPicDlg.ValidateImage;
  CurDir := GetCurrentDir;
  try
    Result := FPicDlg.ShowModal = mrOK;
  finally
    SetCurrentDir(CurDir);
  end;
  FDecreaseColors := FPicDlg.DecreaseColors;
  if Result then begin
    if FPicDlg.Pic.Graphic <> nil then begin
      if (GraphicClass = TBitmap) and (FPicDlg.Pic.Graphic is TIcon) then
      begin
        Bmp := CreateBitmapFromIcon(FPicDlg.Pic.Icon, FPicDlg.FIconColor);
        try
          if FPicDlg.DecreaseColors then
            SetBitmapPixelFormat(Bmp, pf4bit, DefaultMappingMethod);
          FPicture.Assign(Bmp);
        finally
          Bmp.Free;
        end;
      end
      else FPicture.Assign(FPicDlg.Pic);
    end
    else FPicture.Graphic := nil;
  end;
end;

procedure TJvxPictEditor.SetGraphicClass(Value: TGraphicClass);
begin
  FGraphicClass := Value;
  if FPicDlg <> nil then FPicDlg.GraphicClass := Value;
end;

procedure TJvxPictEditor.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ Utility routines }

function EditGraphic(Graphic: TGraphic; AClass: TGraphicClass;
  const DialogCaption: string): Boolean;
var
  PictureEditor: TJvxPictEditor;
begin
  Result := False;
  if Graphic = nil then Exit;
  PictureEditor := TJvxPictEditor.Create(nil);
  try
    PictureEditor.FPicDlg.Caption := DialogCaption;
    PictureEditor.GraphicClass := AClass;
    if AClass = nil then
      PictureEditor.GraphicClass := TGraphicClass(Graphic.ClassType);
    PictureEditor.Picture.Assign(Graphic);
    Result := PictureEditor.Execute;
    if Result then
      if (PictureEditor.Picture.Graphic = nil) or
         (PictureEditor.Picture.Graphic is PictureEditor.GraphicClass) then
        Graphic.Assign(PictureEditor.Picture.Graphic)
      else Result := False;
  finally
    PictureEditor.Free;
  end;
end;

{ TJvxPictProperty }

procedure TJvxPictProperty.Edit;
var
  PictureEditor: TJvxPictEditor;
  Comp: TPersistent;
begin
  PictureEditor := TJvxPictEditor.Create(nil);
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      PictureEditor.FPicDlg.Caption := TComponent(Comp).Name + '.' + GetName;
    PictureEditor.Picture := TPicture(Pointer(GetOrdValue));
    if PictureEditor.Execute then
      SetOrdValue(Longint(PictureEditor.Picture));
  finally
    PictureEditor.Free;
  end;
end;

function TJvxPictProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvxPictProperty.GetValue: string;
var
  Picture: TPicture;
begin
  Picture := TPicture(GetOrdValue);
  if Picture.Graphic = nil then Result := ResStr(srNone)
  else Result := '(' + Picture.Graphic.ClassName + ')';
end;

procedure TJvxPictProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;

{ TJvxGraphicPropertyEditor }

procedure TJvxGraphicPropertyEditor.Edit;
var
  PictureEditor: TJvxPictEditor;
  Comp: TPersistent;
begin
  PictureEditor := TJvxPictEditor.Create(nil);
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      PictureEditor.FPicDlg.Caption := TComponent(Comp).Name + '.' + GetName
    else PictureEditor.FPicDlg.Caption := GetName;
    PictureEditor.GraphicClass := TGraphicClass(GetTypeData(GetPropType)^.ClassType);
    PictureEditor.Picture.Graphic := TGraphic(Pointer(GetOrdValue));
    if PictureEditor.Execute then
      if (PictureEditor.Picture.Graphic = nil) or
         (PictureEditor.Picture.Graphic is PictureEditor.GraphicClass) then
        SetOrdValue(LongInt(PictureEditor.Picture.Graphic))
      else raise Exception.Create(ResStr(SInvalidPropertyValue));
  finally
    PictureEditor.Free;
  end;
end;

function TJvxGraphicPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvxGraphicPropertyEditor.GetValue: string;
var
  Graphic: TGraphic;
begin
  Graphic := TGraphic(GetOrdValue);
  if (Graphic = nil) or Graphic.Empty then Result := ResStr(srNone)
  else Result := '(' + Graphic.ClassName + ')';
end;

procedure TJvxGraphicPropertyEditor.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;

{ TJvxGraphicsEditor }

procedure TJvxGraphicsEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'PICTURE') = 0) or
    (CompareText(PropName, 'IMAGE') = 0) or
    (CompareText(PropName, 'GLYPH') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;

{ TJvxPictureEditDialog }

procedure TJvxPictureEditDialog.SetGraphicClass(Value: TGraphicClass);
begin
  FGraphicClass := Value;
  CheckEnablePaste;
  DecreaseBox.Enabled := (GraphicClass = TBitmap) or (GraphicClass = TGraphic);
end;

procedure TJvxPictureEditDialog.CheckEnablePaste;
begin
  Paste.Enabled := EnablePaste(GraphicClass);
end;

procedure TJvxPictureEditDialog.ValidateImage;
var
  Enable: Boolean;
begin
  Enable := ValidPicture(Pic);
  Save.Enabled := Enable;
  Clear.Enabled := Enable;
  Copy.Enabled := Enable;
end;

{$IFDEF Delphi3_Up}
procedure TJvxPictureEditDialog.GraphicProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Stage in [psStarting, psEnding] then begin
    FProgressPos := 0;
    FProgress.Position := 0;
  end
  else if Stage = psRunning then begin
    if PercentDone >= FProgressPos + 10 then begin
      FProgress.Position := PercentDone;
      FProgressPos := PercentDone;
    end;
  end;
  if RedrawNow then ImagePaintBox.Update;
end;
{$ENDIF}

procedure TJvxPictureEditDialog.UpdateClipboard(Sender: TObject);
begin
  CheckEnablePaste;
end;

procedure TJvxPictureEditDialog.FormCreate(Sender: TObject);
begin
  Pic := TPicture.Create;
{$IFDEF Delphi3_Up}
  FileDialog := TOpenPictureDialog.Create(Self);
  SaveDialog := TSavePictureDialog.Create(Self);
  UsePreviewBox.Visible := False;
  FProgress := TProgressBar.Create(Self);
  with FProgress do begin
    SetBounds(UsePreviewBox.Left, UsePreviewBox.Top, UsePreviewBox.Width,
      UsePreviewBox.Height);
    Parent := Self;
    Min := 0; Max := 100;
    Position := 0;
  end;
  Pic.OnProgress := GraphicProgress;
{$ELSE}
  FileDialog := TOpenDialog.Create(Self);
  SaveDialog := TSaveDialog.Create(Self);
{$ENDIF}
  FileDialog.Title := 'Load picture';
  SaveDialog.Title := 'Save picture as';
{$IFDEF WIN32}
  Bevel.Visible := False;
  Font.Style := [];
  with FormStorage do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
{$ELSE}
  if NewStyleControls then Font.Style := [];
{$ENDIF}
  PathsMRU.RecentMenu := PathsMenu.Items;
  FIconColor := clBtnFace;
  HelpContext := hcDPictureEditor;
  Save.Enabled := False;
  Clear.Enabled := False;
  Copy.Enabled := False;
  FClipMonitor := TJvxClipboardMonitor.Create(Self);
  FClipMonitor.OnChange := UpdateClipboard;
  CheckEnablePaste;
end;

function TJvxPictureEditDialog.GetDecreaseColors: Boolean;
begin
  Result := DecreaseBox.Checked;
end;

procedure TJvxPictureEditDialog.FormDestroy(Sender: TObject);
begin
  FClipMonitor.Free;
  Pic.Free;
end;

procedure TJvxPictureEditDialog.LoadFile(const FileName: string);
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

procedure TJvxPictureEditDialog.LoadClick(Sender: TObject);
{$IFNDEF Delphi3_Up}
var
  FileName: string;
{$ENDIF}
begin
{$IFNDEF Delphi3_Up}
  if UsePreviewBox.Checked then begin
    FileName := '';
    if DirExists(FileDialog.InitialDir) then
      SetCurrentDir(FileDialog.InitialDir);
    if SelectImage(FileName, GraphicExtension(GraphicClass),
      GraphicFilter(GraphicClass)) then
    begin
      FileDialog.Filename := FileName;
      Self.LoadFile(FileName);
    end;
  end
  else begin
{$ENDIF}
    if FileDialog.Execute then begin
      Self.LoadFile(FileDialog.Filename);
    end;
{$IFNDEF Delphi3_Up}
  end;
{$ENDIF}
end;

procedure TJvxPictureEditDialog.SaveClick(Sender: TObject);
begin
  if (Pic.Graphic <> nil) and not Pic.Graphic.Empty then begin
    with SaveDialog do begin
      DefaultExt := GraphicExtension(TGraphicClass(Pic.Graphic.ClassType));
      Filter := GraphicFilter(TGraphicClass(Pic.Graphic.ClassType));
      if Execute then begin
        StartWait;
        try
          Pic.SaveToFile(Filename);
        finally
          StopWait;
        end;
      end;
    end;
  end;
end;

procedure TJvxPictureEditDialog.DecreaseBMPColors;
begin
  if ValidPicture(Pic) and (Pic.Graphic is TBitmap) and DecreaseColors then
    SetBitmapPixelFormat(Pic.Bitmap, pf4bit, DefaultMappingMethod);
end;

procedure TJvxPictureEditDialog.CopyClick(Sender: TObject);
begin
  CopyPicture(Pic, FIconColor);
end;

procedure TJvxPictureEditDialog.PasteClick(Sender: TObject);
begin
  if (Pic <> nil) then begin
    PastePicture(Pic, GraphicClass);
    DecreaseBMPColors;
    ImagePaintBox.Invalidate;
    ValidateImage;
  end;
end;

procedure TJvxPictureEditDialog.ImagePaintBoxPaint(Sender: TObject);
var
  DrawRect: TRect;
  SNone: string;
{$IFDEF WIN32}
  Ico: HIcon;
  W, H: Integer;
{$ENDIF}
begin
  with TPaintBox(Sender) do begin
    Canvas.Brush.Color := Color;
    DrawRect := ClientRect;
    if ValidPicture(Pic) then begin
      with DrawRect do
        if (Pic.Width > Right - Left) or (Pic.Height > Bottom - Top) then
        begin
          if Pic.Width > Pic.Height then
            Bottom := Top + MulDiv(Pic.Height, Right - Left, Pic.Width)
          else
            Right := Left + MulDiv(Pic.Width, Bottom - Top, Pic.Height);
          Canvas.StretchDraw(DrawRect, Pic.Graphic);
        end
        else begin
          with DrawRect do begin
{$IFDEF WIN32}
            if Pic.Graphic is TIcon then begin
              Ico := CreateRealSizeIcon(Pic.Icon);
              try
                GetIconSize(Ico, W, H);
                DrawIconEx(Canvas.Handle, (Left + Right - W) div 2,
                  (Top + Bottom - H) div 2, Ico, W, H, 0, 0, DI_NORMAL);
              finally
                DestroyIcon(Ico);
              end;
            end else
{$ENDIF}
            Canvas.Draw((Right + Left - Pic.Width) div 2,
              (Bottom + Top - Pic.Height) div 2, Pic.Graphic);
          end;
        end;
    end
    else
      with DrawRect, Canvas do begin
        SNone := ResStr(srNone);
        TextOut(Left + (Right - Left - TextWidth(SNone)) div 2, Top + (Bottom -
          Top - TextHeight(SNone)) div 2, SNone);
      end;
  end;
end;

procedure TJvxPictureEditDialog.CreateHandle;
begin
  inherited CreateHandle;
  DragAcceptFiles(Handle, True);
end;

procedure TJvxPictureEditDialog.WMDestroy(var Msg: TMessage);
begin
  DragAcceptFiles(Handle, False);
  inherited;
end;

procedure TJvxPictureEditDialog.WMDropFiles(var Msg: TWMDropFiles);
var
  AFileName: array[0..255] of Char;
  Num: Cardinal;
begin
  Msg.Result := 0;
  try
    Num := DragQueryFile(Msg.Drop, {$IFDEF WIN32} $FFFFFFFF {$ELSE}
      $FFFF {$ENDIF}, nil, 0);
    if Num > 0 then begin
      DragQueryFile(Msg.Drop, 0, PChar(@AFileName), Pred(SizeOf(AFileName)));
      Application.BringToFront;
      Self.LoadFile(StrPas(AFileName));
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TJvxPictureEditDialog.UpdatePathsMenu;
var
  I: Integer;
begin
  for I := 0 to PathsMenu.Items.Count - 1 do begin
    PathsMenu.Items[I].Checked := CompareText(PathsMenu.Items[I].Caption,
      FileDialog.InitialDir) = 0;
  end;
end;

procedure TJvxPictureEditDialog.ClearClick(Sender: TObject);
begin
  Pic.Graphic := nil;
  ImagePaintBox.Invalidate;
  Save.Enabled := False;
  Clear.Enabled := False;
  Copy.Enabled := False;
end;

procedure TJvxPictureEditDialog.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

const
  sBackColorIdent = 'ClipboardBackColor';
  sFileDir = 'FileDialog.InitialDir';

procedure TJvxPictureEditDialog.FormStorageRestorePlacement(Sender: TObject);
begin
  FIconColor := TColor(IniReadInteger(FormStorage.IniFileObject,
    FormStorage.IniSection, sBackColorIdent, clBtnFace));
  FileDialog.InitialDir := IniReadString(FormStorage.IniFileObject,
    FormStorage.IniSection, sFileDir, FileDialog.InitialDir);
end;

procedure TJvxPictureEditDialog.FormStorageSavePlacement(Sender: TObject);
begin
  IniWriteInteger(FormStorage.IniFileObject, FormStorage.IniSection,
    sBackColorIdent, FIconColor);
  IniWriteString(FormStorage.IniFileObject, FormStorage.IniSection,
    sFileDir, FileDialog.InitialDir);
end;

procedure TJvxPictureEditDialog.PathsClick(Sender: TObject);
begin
  if EditFolderList(PathsMRU.Strings) then
    UpdatePathsMenu;
end;

procedure TJvxPictureEditDialog.PathsMRUClick(Sender: TObject;
  const RecentName, Caption: string; UserData: Longint);
begin
  if DirExists(RecentName) then begin
    {SetCurrentDir(RecentName);}
    FileDialog.InitialDir := RecentName;
  end
  else begin
    PathsMRU.Remove(RecentName);
  end;
  UpdatePathsMenu;
end;

procedure TJvxPictureEditDialog.PathsMenuPopup(Sender: TObject);
begin
  UpdatePathsMenu;
end;

procedure TJvxPictureEditDialog.PathsMRUChange(Sender: TObject);
begin
  PathsBtn.Enabled := PathsMRU.Strings.Count > 0;
end;

end.
