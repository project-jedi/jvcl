{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPictureEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvPictureEditors;

interface

uses
  Windows, Messages, Classes, Graphics, Forms, Controls, Dialogs, Menus,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvPictureEditForm;

type
  { Property editor the TPicture properties (e.g. the Picture property). Brings
    up a file open dialog allowing to load a picture file. }
  TJvPictProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TJvGraphicPropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TJvGraphicsEditor = class(TDefaultEditor)
  public
    {$IFDEF COMPILER6_UP}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(Prop: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
    {$ENDIF COMPILER6_UP}
  end;

  TJvPictEditor = class(TComponent)
  private
    FGraphicClass: TGraphicClass;
    FPicture: TPicture;
    FPicDlg: TPictureEditDialog;
    FDecreaseColors: Boolean;
    procedure SetPicture(Value: TPicture);
    procedure SetGraphicClass(Value: TGraphicClass);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property PicDlg: TPictureEditDialog read FPicDlg;
    property GraphicClass: TGraphicClass read FGraphicClass write SetGraphicClass;
    property Picture: TPicture read FPicture write SetPicture;
  end;

function EditGraphic(Graphic: TGraphic; AClass: TGraphicClass;
  const DialogCaption: string): Boolean;

implementation

uses
  SysUtils, TypInfo, LibHelp, Consts,
  JvTypes, JvJVCLUtils;

//=== Utility routines =======================================================

function EditGraphic(Graphic: TGraphic; AClass: TGraphicClass;
  const DialogCaption: string): Boolean;
var
  PictureEditor: TJvPictEditor;
begin
  Result := False;
  if Graphic = nil then
    Exit;
  PictureEditor := TJvPictEditor.Create(nil);
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
      else
        Result := False;
  finally
    PictureEditor.Free;
  end;
end;

//=== { TJvPictProperty } ====================================================

procedure TJvPictProperty.Edit;
var
  PictureEditor: TJvPictEditor;
  Comp: TPersistent;
begin
  PictureEditor := TJvPictEditor.Create(nil);
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

function TJvPictProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvPictProperty.GetValue: string;
var
  Picture: TPicture;
begin
  Picture := TPicture(GetOrdValue);
  if Picture.Graphic = nil then
    Result := srNone
  else
    Result := '(' + Picture.Graphic.ClassName + ')';
end;

procedure TJvPictProperty.SetValue(const Value: string);
begin
  if Value = '' then
    SetOrdValue(0);
end;

//=== { TJvGraphicPropertyEditor } ===========================================

procedure TJvGraphicPropertyEditor.Edit;
var
  PictureEditor: TJvPictEditor;
  Comp: TPersistent;
begin
  PictureEditor := TJvPictEditor.Create(nil);
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      PictureEditor.FPicDlg.Caption := TComponent(Comp).Name + '.' + GetName
    else
      PictureEditor.FPicDlg.Caption := GetName;
    PictureEditor.GraphicClass := TGraphicClass(GetTypeData(GetPropType)^.ClassType);
    PictureEditor.Picture.Graphic := TGraphic(Pointer(GetOrdValue));
    if PictureEditor.Execute then
      if (PictureEditor.Picture.Graphic = nil) or
        (PictureEditor.Picture.Graphic is PictureEditor.GraphicClass) then
        SetOrdValue(Longint(PictureEditor.Picture.Graphic))
      else
        raise EJVCLException.CreateRes(@SInvalidPropertyValue);
  finally
    PictureEditor.Free;
  end;
end;

function TJvGraphicPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvGraphicPropertyEditor.GetValue: string;
var
  Graphic: TGraphic;
begin
  Graphic := TGraphic(GetOrdValue);
  if (Graphic = nil) or Graphic.Empty then
    Result := srNone
  else
    Result := '(' + Graphic.ClassName + ')';
end;

procedure TJvGraphicPropertyEditor.SetValue(const Value: string);
begin
  if Value = '' then
    SetOrdValue(0);
end;

//=== { TJvGraphicsEditor } ==================================================

{$IFDEF COMPILER6_UP}
procedure TJvGraphicsEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
{$ELSE}
procedure TJvGraphicsEditor.EditProperty(Prop: TPropertyEditor; var Continue, FreeEditor: Boolean);
{$ENDIF COMPILER6_UP}
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

//=== { TJvPictEditor } ======================================================

constructor TJvPictEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicDlg := TPictureEditDialog.Create(Self);
  FGraphicClass := TGraphic;
  FPicDlg.GraphicClass := FGraphicClass;
end;

destructor TJvPictEditor.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TJvPictEditor.Execute: Boolean;
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
    Result := FPicDlg.ShowModal = mrOk;
  finally
    SetCurrentDir(CurDir);
  end;
  FDecreaseColors := FPicDlg.DecreaseColors;
  if Result then
  begin
    if FPicDlg.Pic.Graphic <> nil then
    begin
      if (GraphicClass = TBitmap) and (FPicDlg.Pic.Graphic is TIcon) then
      begin
        Bmp := CreateBitmapFromIcon(FPicDlg.Pic.Icon, FPicDlg.IconColor);
        try
          if FPicDlg.DecreaseColors then
            SetBitmapPixelFormat(Bmp, pf4bit, DefaultMappingMethod);
          FPicture.Assign(Bmp);
        finally
          Bmp.Free;
        end;
      end
      else
        FPicture.Assign(FPicDlg.Pic);
    end
    else
      FPicture.Graphic := nil;
  end;
end;

procedure TJvPictEditor.SetGraphicClass(Value: TGraphicClass);
begin
  FGraphicClass := Value;
  if FPicDlg <> nil then
    FPicDlg.GraphicClass := Value;
end;

procedure TJvPictEditor.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

end.
