{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAnimatedEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvAnimatedEditor;

interface

uses
  Windows, Forms, Graphics, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  SysUtils, Classes, Dialogs, Controls,
  JvAnimatedImage, JvAniFile;

type
  TJvAnimatedEditor = class(TComponentEditor)
  private
    FContinue: Boolean;
    {$IFDEF COMPILER6_UP}
    procedure CheckEdit(const PropertyEditor: IProperty);
    {$ELSE}
    procedure CheckEdit(PropertyEditor: TPropertyEditor);
    {$ENDIF}
    procedure EditImage(Image: TJvAnimatedImage);
    procedure LoadAniFile(Image: TJvAnimatedImage);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  TypInfo,
  JvConsts;

const
  cGlyphName = 'GLYPH';
  cAniFileExtension = 'ani';

//=== TJvAnimatedEditor ======================================================
  
{$IFDEF COMPILER6_UP}
procedure TJvAnimatedEditor.CheckEdit(const PropertyEditor: IProperty);
begin
{$ELSE}
procedure TJvAnimatedEditor.CheckEdit(PropertyEditor: TPropertyEditor);
begin
  try
{$ENDIF}
    if FContinue and (CompareText(PropertyEditor.GetName, cGlyphName) = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end;
  {$IFNDEF COMPILER6_UP}
  finally
    PropertyEditor.Free;
  end;
  {$ENDIF}
end;

{$IFDEF COMPILER6_UP}
type
  TDesignerSelectionList = IDesignerSelections;
{$ENDIF}

procedure TJvAnimatedEditor.EditImage(Image: TJvAnimatedImage);
var
  Components: TDesignerSelectionList;
begin
  {$IFDEF COMPILER6_UP}
  Components := TDesignerSelections.Create;
  {$ELSE}
  Components := TDesignerSelectionList.Create;
  {$ENDIF}
  {$IFNDEF COMPILER6_UP}
  try
  {$ENDIF}
    FContinue := True;
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
  {$IFNDEF COMPILER6_UP}
  finally
    Components.Free;
  end;
  {$ENDIF}
end;

procedure TJvAnimatedEditor.LoadAniFile(Image: TJvAnimatedImage);
var
  Dialog: TOpenDialog;
  AniCursor: TJvAnimatedCursorImage;
  CurDir: string;
begin
  CurDir := GetCurrentDir;
  Dialog := TOpenDialog.Create(Application);
  try
    with Dialog do
    begin
      Options := [ofHideReadOnly, ofFileMustExist];
      DefaultExt := cAniFileExtension;
      Filter := srAniCurFilter;
      if Execute then
      begin
        AniCursor := TJvAnimatedCursorImage.Create;
        try
          AniCursor.LoadFromFile(FileName);
          AniCursor.AssignToBitmap(Image.Glyph, clFuchsia, True,
            Image.Orientation = goVertical);
          Image.Interval := AniCursor.DefaultRate;
          Image.TransparentColor := clFuchsia;
          Designer.Modified;
        finally
          AniCursor.Free;
        end;
      end;
    end;
  finally
    Dialog.Free;
    SetCurrentDir(CurDir);
  end;
end;

procedure TJvAnimatedEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1 then
    LoadAniFile(TJvAnimatedImage(Component))
  else
  if Index = GetVerbCount - 2 then
    EditImage(TJvAnimatedImage(Component))
  else
    inherited ExecuteVerb(Index);
end;

function TJvAnimatedEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := srLoadAniCursor
  else
  if Index = GetVerbCount - 2 then
    Result := srEditPicture
  else
    Result := inherited GetVerb(Index);
end;

function TJvAnimatedEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

end.
