{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvIcoLEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvIconListForm;

interface

uses
  Classes,
  {$IFDEF VCL}
  Windows, Messages, Forms, Controls, Dialogs, Graphics,
  StdCtrls, ExtCtrls, ExtDlgs, ImgList, ComCtrls, ToolWin,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms, QControls, QDialogs, QStdCtrls, QExtCtrls, QExtDlgs,
  QImgList, QComCtrls, QGraphics, QToolWin, ClxEditors,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors,
  {$IFDEF VCL}
  VCLEditors,
  {$ENDIF VCL}
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvIconList, JvComponent;

type
  TIconListDialog = class(TJvForm)
    OK: TButton;
    Cancel: TButton;
    Holder: TPanel;
    Slot0: TPanel;
    Slot1: TPanel;
    Slot2: TPanel;
    Slot3: TPanel;
    Slot4: TPanel;
    Image0: TImage;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Bevel1: TBevel;
    Label1: TLabel;
    CntLabel: TLabel;
    Label3: TLabel;
    IdxLabel: TLabel;
    ScrollBar: TScrollBar;
    ToolBar1: TToolBar;
    Load: TToolButton;
    Delete: TToolButton;
    Clear: TToolButton;
    Copy: TToolButton;
    Paste: TToolButton;
    LoadAni: TToolButton;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure UpdateClipboard(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LoadAniClick(Sender: TObject);
  private
    FIcons: TJvIconList;
    FTopIndex: Integer;
    FSelected: Integer;
    FFileDialog: TOpenPictureDialog;
    procedure SetSelectedIndex(Index: Integer; Force: Boolean);
    procedure ListChanged(Sender: TObject);
    function GetSelectedIcon: TIcon;
    procedure CheckButtons;
    procedure ValidateImage;
    procedure CheckEnablePaste;
    procedure LoadAniFile;
    {$IFDEF VCL}
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    {$ENDIF VCL}
  protected
    {$IFDEF VisualCLX}
    procedure Activate; override;
    {$ENDIF VisualCLX}
  public
    Modified: Boolean;
  end;

  TIconListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure EditIconList(IconList: TJvIconList);

implementation

uses
  SysUtils,
  {$IFDEF VCL}
  Clipbrd, Consts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QClipbrd, QConsts,
  {$ENDIF VisualCLX}
  Math,
  JvJVCLUtils, JvJCLUtils, JvDsgnConsts, JvAni;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

const
  sSlot = 'Slot%d';
  sImage = 'Image%d';

{$IFDEF VisualCLX}
type
  TOpenIcon = class(TIcon);

function Bmp2Icon(bmp: TBitmap): TIcon;
begin
  Result := TIcon.Create;
  Result.Assign(bmp);
end;

function Icon2Bmp(Ico: TIcon): TBitmap;
begin
  Result := TBitmap.Create;
  TOpenIcon(Ico).AssignTo(Result);
end;

procedure CopyIconToClipboard(Ico: TIcon; TransparentColor: TColor);
var
  bmp: TBitmap;
begin
  bmp := Icon2Bmp(Ico);
  Clipboard.Assign(Bmp);
end;

function CreateIconFromClipboard: TIcon;
var
  bmp: TBitmap;
begin
  Result := nil;
  bmp := TBitmap.create;
  try
    bmp.Assign(Clipboard);
    Result := Bmp2Icon(bmp);
  except
    bmp.Free;
  end;
end;
{$ENDIF VisualCLX}

procedure EditIconList(IconList: TJvIconList);
begin
  with TIconListDialog.Create(Application) do
  try
    FIcons.Assign(IconList);
    Modified := False;
    if (ShowModal = mrOk) and Modified then
      IconList.Assign(FIcons);
  finally
    Free;
  end;
end;

//=== { TIconListProperty } ==================================================

procedure TIconListProperty.Edit;
var
  Editor: TIconListDialog;
  Comp: TPersistent;
  CurDir: string;
  Res: Integer;
begin
  Editor := TIconListDialog.Create(nil);
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Editor.Caption := TComponent(Comp).Name + '.' + GetName;
    Editor.FIcons.Assign(TJvIconList(Pointer(GetOrdValue)));
    Editor.Modified := False;
    CurDir := GetCurrentDir;
    try
      Res := Editor.ShowModal;
    finally
      SetCurrentDir(CurDir);
    end;
    if (Res = mrOk) and Editor.Modified then
    begin
      TJvIconList(Pointer(GetOrdValue)).Assign(Editor.FIcons);
      Designer.Modified;
    end;
  finally
    Editor.Free;
  end;
end;

function TIconListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TIconListProperty.GetValue: string;
var
  List: TJvIconList;
begin
  List := TJvIconList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0) then
    Result := srNone
  else
    Result := '(' + List.ClassName + ')';
end;

procedure TIconListProperty.SetValue(const Value: string);
begin
  if Value = '' then
    SetOrdValue(0);
end;

//=== { TIconListDialog } ====================================================

procedure TIconListDialog.LoadAniFile;
var
  AniCursor: TJvAni;
begin
  AniCursor := LoadJvAniDialog;
  try
    FIcons.Assign(AniCursor);
  finally
    AniCursor.Free;
  end;
end;

function TIconListDialog.GetSelectedIcon: TIcon;
begin
  Result := nil;
  if (FIcons.Count > 0) and (FSelected < FIcons.Count) then
    Result := FIcons[FSelected];
end;

procedure TIconListDialog.CheckEnablePaste;
begin
  {$IFDEF VCL}
  Paste.Enabled := Clipboard.HasFormat(CF_ICON);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Paste.Enabled := Clipboard.Provides('image/delphi.bitmap')
  {$ENDIF VisualCLX}
end;

procedure TIconListDialog.SetSelectedIndex(Index: Integer; Force: Boolean);
begin
  if Force or (Index <> FSelected) then
  begin
    Index := Min(FIcons.Count, Max(Index, 0));
    while (FTopIndex < Index - 4) do
      Inc(FTopIndex);
    if Index < FTopIndex then
      FTopIndex := Index;
    FSelected := Index;
    if FSelected <> ScrollBar.Position then
      ScrollBar.Position := FSelected;
    ValidateImage;
  end;
end;

procedure TIconListDialog.ListChanged(Sender: TObject);
begin
  ScrollBar.Max := FIcons.Count;
  SetSelectedIndex(FSelected, True);
  Modified := True;
end;

procedure TIconListDialog.CheckButtons;
var
  Enable: Boolean;
begin
  Enable := (FIcons.Count > 0) and (FSelected < FIcons.Count) and
    (FSelected >= 0);
  Clear.Enabled := FIcons.Count > 0;
  Delete.Enabled := Enable;
  Copy.Enabled := Enable;
  CheckEnablePaste;
end;

procedure TIconListDialog.ValidateImage;
var
  Enable: Boolean;
  I: Integer;
  Image, Slot: TComponent;
begin
  for I := 0 to 4 do
  begin
    Image := FindComponent(Format(sImage, [I]));
    Slot := FindComponent(Format(sSlot, [I]));
    if Image <> nil then
      with TImage(Image).Picture do
      begin
        if FTopIndex + I < FIcons.Count then
          Assign(FIcons[FTopIndex + I])
        else
          Assign(nil);
        TImage(Image).Transparent := True;
      end;
    if Slot <> nil then
      TPanel(Slot).ParentColor := True;
  end;
  Slot := FindComponent(Format(sSlot, [FSelected - FTopIndex]));
  if Slot <> nil then
    TPanel(Slot).Color := clActiveCaption;
  CntLabel.Caption := IntToStr(FIcons.Count);
  Enable := (FIcons.Count > 0) and (FSelected <= FIcons.Count) and
    (FSelected >= 0);
  if Enable then
    IdxLabel.Caption := IntToStr(FSelected)
  else
    IdxLabel.Caption := '';
  CheckButtons;
end;

procedure TIconListDialog.FormCreate(Sender: TObject);
var
  I: Integer;
  Image: TComponent;
begin
  FFileDialog := TOpenPictureDialog.Create(Self);
  for I := 0 to 4 do
  begin
    Image := FindComponent(Format(sImage, [I]));
    if Image <> nil then
      TImage(Image).Transparent := True;
  end;
  with FFileDialog do
  begin
    Title := RsLoadIcon;
    Options := [ofHideReadOnly, ofFileMustExist];
    DefaultExt := GraphicExtension(TIcon);
    Filter := GraphicFilter(TIcon);
  end;
  FIcons := TJvIconList.Create;
  FIcons.OnChange := ListChanged;
  FTopIndex := 0;
  FSelected := 0;
  Clear.Enabled := False;
  Copy.Enabled := False;
  Delete.Enabled := False;
  CheckEnablePaste;
end;

procedure TIconListDialog.FormDestroy(Sender: TObject);
begin
  FIcons.OnChange := nil;
  FIcons.Free;
end;

procedure TIconListDialog.UpdateClipboard(Sender: TObject);
begin
  CheckEnablePaste;
end;

procedure TIconListDialog.LoadClick(Sender: TObject);
var
  Ico: TIcon;
  I: Integer;
begin
  if FFileDialog.Execute then
  begin
    Ico := TIcon.Create;
    try
      Ico.LoadFromFile(FFileDialog.Filename);
      I := Min(FSelected + 1, FIcons.Count);
      FIcons.Insert(I, Ico);
      SetSelectedIndex(I, True);
    finally
      Ico.Free;
    end;
  end;
end;

procedure TIconListDialog.CopyClick(Sender: TObject);
begin
  CopyIconToClipboard(GetSelectedIcon, clBtnFace);
  CheckEnablePaste;
end;

procedure TIconListDialog.PasteClick(Sender: TObject);
var
  Ico: TIcon;
begin
  {$IFDEF VCL}
  if Clipboard.HasFormat(CF_ICON) then
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if Clipboard.Provides('image/delphi.bitmap') then
  {$ENDIF VisualCLX}
  begin
    Ico := CreateIconFromClipboard;
    try
      FIcons[FSelected] := Ico;
    finally
      Ico.Free;
    end;
  end;
end;

{$IFDEF VCL}
procedure TIconListDialog.WMActivate(var Msg: TWMActivate);
begin
  if Msg.Active <> WA_INACTIVE then
    CheckEnablePaste;
  inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TIconListDialog.Activate;
begin
  if Focused then
    CheckEnablePaste;
  inherited;
end;
{$ENDIF VisualCLX}

procedure TIconListDialog.ClearClick(Sender: TObject);
begin
  FIcons.Clear;
end;

procedure TIconListDialog.ScrollBarChange(Sender: TObject);
begin
  SetSelectedIndex(ScrollBar.Position, False);
end;

procedure TIconListDialog.DeleteClick(Sender: TObject);
begin
  FIcons.Delete(FSelected);
end;

procedure TIconListDialog.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  if Button = mbLeft then
  begin
    for Index := 0 to 4 do
    begin
      if TComponent(Sender).Name = Format(sImage, [Index]) then
        Break;
      if TComponent(Sender).Name = Format(sSlot, [Index]) then
        Break;
    end;
    SetSelectedIndex(FTopIndex + Index, True);
  end;
end;

procedure TIconListDialog.LoadAniClick(Sender: TObject);
begin
  LoadAniFile;
end;

end.

