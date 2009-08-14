{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOutlookBarEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvOutlookBarEditors;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Controls, Forms, ToolWin,
  Menus, ActnList, ComCtrls, ImgList,
  DesignEditors, DesignIntf, DesignMenus, DesignWindows,
  JvDsgnEditors, JvOutlookBar;

type
  TJvOutlookBarActivePageProperty = class(TIntegerProperty)
  private
    function GetOL: TJvCustomOutlookBar;
  protected
    property OL: TJvCustomOutlookBar read GetOL;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvOutlookBarEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvOutlookBarPagesProperty = class(TPropertyEditor)
  private
    function GetOutlookBar: TJvCustomOutlookBar;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvOutlookBarButtonImageIndexProperty = class(TJvDefaultImageIndexProperty)
  protected
    function GetPage: TJvOutlookBarPage;
    function GetBar: TJvCustomOutlookBar;
    function ImageList: TCustomImageList; override;
  end;

  TJvOutlookBarPageImageIndexProperty = class(TJvDefaultImageIndexProperty)
    function ImageList: TCustomImageList; override;
  end;


implementation

uses
  JvOutlookBarForm, JvDsgnConsts;

type
  THackOutlookBar = class(TJvCustomOutlookBar);

procedure ShowEditor(Designer: IDesigner; OutlookBar: TJvCustomOutlookBar);
var
  I: Integer;
  AEditor: TFrmOLBEditor;
begin
  AEditor := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TFrmOLBEditor then
      if TFrmOLBEditor(Screen.Forms[I]).OutlookBar = OutlookBar then
      begin
        AEditor := TFrmOLBEditor(Screen.Forms[I]);
        Break;
      end;
  // Show the editor
  if Assigned(AEditor) then
  begin
    AEditor.Show;
    if AEditor.WindowState = wsMinimized then
      AEditor.WindowState := wsNormal;
  end
  else
  begin
    AEditor := TFrmOLBEditor.Create(Application);
    try
      AEditor.Designer := Designer;
      AEditor.OutlookBar := OutlookBar;
      AEditor.Show;
    except
      AEditor.Free;
      raise;
    end;
  end;
end;

//=== { TJvOutlookBarPagesProperty } =========================================

procedure TJvOutlookBarPagesProperty.Edit;
begin
  ShowEditor(Designer, GetOutlookBar);
end;

function TJvOutlookBarPagesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

type
  THackPages = class(TJvOutlookBarPages);
  THackButtons = class(TJvOutlookBarButtons);

function TJvOutlookBarPagesProperty.GetOutlookBar: TJvCustomOutlookBar;
begin
  if GetComponent(0) is TJvCustomOutlookBar then
    Result := TJvCustomOutlookBar(GetComponent(0))
  else
  if GetComponent(0) is TJvOutlookBarPage then
    Result := THackOutlookBar(THackPages(TJvOutlookBarPage(GetComponent(0)).Collection).GetOwner)
  else
    Result := nil;
end;

function TJvOutlookBarPagesProperty.GetValue: string;
begin
  Result := Format('(%s)', [GetPropType^.Name]);
end;

//=== { TJvOutlookBarEditor } ================================================

procedure TJvOutlookBarEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowEditor(Designer, Component as TJvCustomOutlookBar);
  else
    inherited ExecuteVerb(Index);
  end;
end;

function TJvOutlookBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsOLEditor;
  else
    Result := inherited GetVerb(Index);
  end;
end;

function TJvOutlookBarEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== { TJvOutlookBarActivePageProperty } ====================================

procedure TJvOutlookBarActivePageProperty.Edit;
begin
  inherited Edit;
end;

function TJvOutlookBarActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

function TJvOutlookBarActivePageProperty.GetOL: TJvCustomOutlookBar;
begin
  if GetComponent(0) is TJvCustomOutlookBar then
    Result := TJvCustomOutlookBar(GetComponent(0))
  else
    Result := nil;
end;

function TJvOutlookBarActivePageProperty.GetValue: string;
var
  I: Integer;
begin
  I := GetOrdValue;
  if I < 0 then
    Result := ''
  else
  if I < THackOutlookBar(OL).Pages.Count then
    Result := THackOutlookBar(OL).Pages[I].Caption
  else
    Result := inherited GetValue;
end;

procedure TJvOutlookBarActivePageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to THackOutlookBar(OL).Pages.Count - 1 do
    Proc(THackOutlookBar(OL).Pages[I].Caption);
end;

procedure TJvOutlookBarActivePageProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  I := StrToIntDef(Value, -1);
  if I < 0 then
  begin
    for I := 0 to THackOutlookBar(OL).Pages.Count - 1 do
      if AnsiSameText(THackOutlookBar(OL).Pages[I].Caption, Value) then
      begin
        SetOrdValue(I);
        Modified;
        Break;
      end;
  end
  else
    inherited SetValue(Value);
end;

//=== { TJvOutlookBarButtonImageIndexProperty } ==============================

function TJvOutlookBarButtonImageIndexProperty.GetBar: TJvCustomOutlookBar;
begin
  Result := THackPages(GetPage.Collection).GetOwner as TJvCustomOutlookBar;
end;

function TJvOutlookBarButtonImageIndexProperty.GetPage: TJvOutlookBarPage;
begin
  Result := TJvOutlookBarPage(THackButtons((GetComponent(0) as TJvOutlookBarButton).Collection).GetOwner);
end;

function TJvOutlookBarButtonImageIndexProperty.ImageList: TCustomImageList;
begin
  if GetPage.ButtonSize = olbsLarge then
    Result := THackOutlookBar(GetBar).LargeImages
  else
    Result := THackOutlookBar(GetBar).SmallImages;
end;


//=== { TJvOutlookBarPageImageIndexProperty } ================================

function TJvOutlookBarPageImageIndexProperty.ImageList: TCustomImageList;
begin
  Result := THackOutlookBar(THackPages(TJvOutlookBarPage(GetComponent(0)).Collection).Owner).PageImages;
end;

end.