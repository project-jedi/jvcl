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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvIcoLEdit;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Forms, Controls, Dialogs, Buttons, JvIcoList,
{$IFDEF Delphi6_Up}
RTLConsts,  DesignIntf, DesignEditors,  VCLEditors,
{$ELSE}
  LibIntf, DsgnIntf,
{$ENDIF}

  StdCtrls, ExtCtrls, JvPlacemnt,
  {$IFDEF Delphi3_Up} ExtDlgs, {$ELSE}
  ImagPrvw, {$ENDIF} Menus, JvSpeedbar;

type

{ TIconListDialog }

  TIconListDialog = class(TForm)
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
    JvSpeedbar: TJvSpeedBar;
    Load: TJvSpeedItem;
    LoadAni: TJvSpeedItem;
    Delete: TJvSpeedItem;
    Clear: TJvSpeedItem;
    Copy: TJvSpeedItem;
    Paste: TJvSpeedItem;
    ScrollBar: TScrollBar;
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
    Icons: TJvIconList;
    FTopIndex, FSelected: Integer;
{$IFDEF Delphi3_Up}
    FileDialog: TOpenPictureDialog;
{$ELSE}
    FileDialog: TOpenDialog;
{$ENDIF}
    procedure SetSelectedIndex(Index: Integer; Force: Boolean);
    procedure ListChanged(Sender: TObject);
    function GetSelectedIcon: TIcon;
    procedure CheckButtons;
    procedure ValidateImage;
    procedure CheckEnablePaste;
    procedure LoadAniFile;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
  public
    Modified: Boolean;
  end;

{ TIconListProperty }

  TIconListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure EditIconList(IconList: TJvIconList);

implementation

uses TypInfo, SysUtils, Clipbrd, Consts, JvClipIcon, JvVCLUtils, JvAppUtils,
  JvConst, JvLConst, JvMaxMin, JvAniFile;

{$B-}
{$IFDEF WIN32}
 {$D-}
{$ENDIF}

{$R *.DFM}

const
  sSlot = 'Slot%d';
  sImage = 'Image%d';

procedure EditIconList(IconList: TJvIconList);
begin
  with TIconListDialog.Create(Application) do
  try
    Icons.Assign(IconList);
    Modified := False;
    if (ShowModal = mrOk) and Modified then
      IconList.Assign(Icons);
  finally
    Free;
  end;
end;

{ TIconListProperty }

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
    Editor.Icons.Assign(TJvIconList(Pointer(GetOrdValue)));
    Editor.Modified := False;
    CurDir := GetCurrentDir;
    try
      Res := Editor.ShowModal;
    finally
      SetCurrentDir(CurDir);
    end;
    if (Res = mrOk) and Editor.Modified then begin
      TJvIconList(Pointer(GetOrdValue)).Assign(Editor.Icons);
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
    Result := ResStr(srNone)
  else Result := '(' + List.ClassName + ')';
end;

procedure TIconListProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;

{ TIconListDialog }

procedure TIconListDialog.LoadAniFile;
var
  Dialog: TOpenDialog;
  AniCursor: TJvAnimatedCursorImage;
begin
  Dialog := TOpenDialog.Create(Application);
  try
    with Dialog do begin
      Options := [ofHideReadOnly, ofFileMustExist];
      DefaultExt := 'ani';
      Filter := LoadStr(srAniCurFilter);
      if Execute then begin
        AniCursor := TJvAnimatedCursorImage.Create;
        try
          AniCursor.LoadFromFile(FileName);
          Icons.Assign(AniCursor);
        finally
          AniCursor.Free;
        end;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

function TIconListDialog.GetSelectedIcon: TIcon;
begin
  Result := nil;
  if (Icons.Count > 0) and (FSelected < Icons.Count) then
    Result := Icons[FSelected];
end;

procedure TIconListDialog.CheckEnablePaste;
begin
  Paste.Enabled := Clipboard.HasFormat(CF_ICON);
end;

procedure TIconListDialog.SetSelectedIndex(Index: Integer; Force: Boolean);
begin
  if Force or (Index <> FSelected) then begin
    Index := Min(Icons.Count, Max(Index, 0));
    while (FTopIndex < Index - 4) do Inc(FTopIndex);
    if Index < FTopIndex then FTopIndex := Index;
    FSelected := Index;
    if FSelected <> ScrollBar.Position then ScrollBar.Position := FSelected;
    ValidateImage;
  end;
end;

procedure TIconListDialog.ListChanged(Sender: TObject);
begin
  ScrollBar.Max := Icons.Count;
  SetSelectedIndex(FSelected, True);
  Modified := True;
end;

procedure TIconListDialog.CheckButtons;
var
  Enable: Boolean;
begin
  Enable := (Icons.Count > 0) and (FSelected < Icons.Count) and
    (FSelected >= 0);
  Clear.Enabled := Icons.Count > 0;
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
  for I := 0 to 4 do begin
    Image := FindComponent(Format(sImage, [I]));
    Slot := FindComponent(Format(sSlot, [I]));
    if Image <> nil then
      with TImage(Image).Picture do begin
        if FTopIndex + I < Icons.Count then Assign(Icons[FTopIndex + I])
        else Assign(nil);
{$IFDEF Delphi3_Up}
        TImage(Image).Transparent := True;
{$ENDIF}
      end;
    if Slot <> nil then TPanel(Slot).ParentColor := True;
  end;
  Slot := FindComponent(Format(sSlot, [FSelected - FTopIndex]));
  if Slot <> nil then TPanel(Slot).Color := clActiveCaption;
  CntLabel.Caption := IntToStr(Icons.Count);
  Enable := (Icons.Count > 0) and (FSelected <= Icons.Count) and
    (FSelected >= 0);
  if Enable then IdxLabel.Caption := IntToStr(FSelected)
  else IdxLabel.Caption := '';
  CheckButtons;
end;

procedure TIconListDialog.FormCreate(Sender: TObject);
{$IFDEF Delphi3_Up}
var
  I: Integer;
  Image: TComponent;
{$ENDIF}
begin
{$IFDEF Delphi3_Up}
  FileDialog := TOpenPictureDialog.Create(Self);
  for I := 0 to 4 do begin
    Image := FindComponent(Format(sImage, [I]));
    if Image <> nil then TImage(Image).Transparent := True;
  end;
{$ELSE}
  FileDialog := TOpenDialog.Create(Self);
{$ENDIF}
  with FileDialog do begin
    Title := LoadStr(srLoadIcon);
    Options := [ofHideReadOnly, ofFileMustExist];
    DefaultExt := GraphicExtension(TIcon);
    Filter := GraphicFilter(TIcon);
  end;
  Icons := TJvIconList.Create;
  Icons.OnChange := ListChanged;
  FTopIndex := 0;
  FSelected := 0;
  Clear.Enabled := False;
  Copy.Enabled := False;
  Delete.Enabled := False;
  CheckEnablePaste;
end;

procedure TIconListDialog.FormDestroy(Sender: TObject);
begin
  Icons.OnChange := nil;
  Icons.Free;
end;

procedure TIconListDialog.UpdateClipboard(Sender: TObject);
begin
  CheckEnablePaste;
end;

procedure TIconListDialog.LoadClick(Sender: TObject);
var
  Ico: TIcon;
  I: Integer;
{$IFNDEF Delphi3_Up}
  FileName: string;
{$ENDIF}
begin
{$IFNDEF Delphi3_Up}
  FileName := '';
  if SelectImage(FileName, GraphicExtension(TIcon), GraphicFilter(TIcon)) then
  begin
    FileDialog.Filename := FileName;
{$ELSE}
  if FileDialog.Execute then begin
{$ENDIF}
    Ico := TIcon.Create;
    try
      Ico.LoadFromFile(FileDialog.Filename);
      I := Min(FSelected + 1, Icons.Count);
      Icons.Insert(I, Ico);
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
  if Clipboard.HasFormat(CF_ICON) then begin
    Ico := CreateIconFromClipboard;
    try
      Icons[FSelected] := Ico;
    finally
      Ico.Free;
    end;
  end;
end;

procedure TIconListDialog.WMActivate(var Msg: TWMActivate);
begin
  if Msg.Active <> WA_INACTIVE then CheckEnablePaste;
  inherited;
end;

procedure TIconListDialog.ClearClick(Sender: TObject);
begin
  Icons.Clear;
end;

procedure TIconListDialog.ScrollBarChange(Sender: TObject);
begin
  SetSelectedIndex(ScrollBar.Position, False);
end;

procedure TIconListDialog.DeleteClick(Sender: TObject);
begin
  Icons.Delete(FSelected);
end;

procedure TIconListDialog.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  if Button = mbLeft then begin
    for Index := 0 to 4 do begin
      if TComponent(Sender).Name = Format(sImage, [Index]) then Break;
      if TComponent(Sender).Name = Format(sSlot, [Index]) then Break;
    end;
    SetSelectedIndex(FTopIndex + Index, True);
  end;
end;

procedure TIconListDialog.LoadAniClick(Sender: TObject);
begin
  LoadAniFile;
end;

end.
