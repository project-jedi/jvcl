{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageListEditors.PAS, released on 2004-03-31.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPageListEditors;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF VCL}
  Windows, ImgList, Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QImgList, QGraphics,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignMenus,
  {$IFDEF VCL}
  VCLEditors,
  {$ENDIF VCL}
  {$ELSE}
  DsgnIntf, Menus,
  {$ENDIF COMPILER6_UP}
  JvPageList, JvDsgnEditors;

type
  { a property editor for the ActivePage property of TJvPageList }
  TJvActivePageProperty = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvShowDesignCaptionProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

  { a component editor for the TJvPageList }
  TJvCustomPageEditor = class(TComponentEditor)
  private
    procedure InsertPage;
    procedure PrevPage;
    procedure NextPage;
    procedure RemovePage;
    function GetPageControl: TJvCustomPageList;
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TJvSettingsTreeImagesProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

  {$IFDEF COMPILER5}

  // since D5 doesn't support interface style published properties,
  // this editor is supplied to make it easier to select a specific interface
  // implementor at design-time
  // NOTE: you must derive a new editor from TJvInterfaceProperty and override
  // GetInterfaceGUID and GetInterfaceName since these are declared as virtual abstract
  TJvInterfaceProperty = class(TComponentProperty)
  private
    FOrgStrProc: TGetStrProc;
  protected
    function IntfSupported(AComponent: TComponent): Boolean; virtual;
    function GetInterfaceGUID: TGUID; virtual; abstract;
    function GetInterfaceName: string; virtual; abstract;
    procedure ProcComps(const S: string);
    property OrgStrProc: TGetStrProc read FOrgStrProc write FOrgStrProc;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TJvPageListProperty = class(TJvInterfaceProperty)
  protected
    function GetInterfaceGUID: TGUID; override;
    function GetInterfaceName: string; override;
  end;

  {$ENDIF COMPILER5}

implementation

uses
  TypInfo,
  JvDsgnConsts, JvPageListTreeView, JvPageListEditorForm;

type
  THackTreeView = class(TJvCustomPageListTreeView);

const
  cShowEditor = 0;
  cDash = 1;
  cNewPage = 2;
  cNextPage = 3;
  cPrevPage = 4;
  cDelPage = 5;

  cElementCount = 6;

procedure TJvCustomPageEditor.Edit;
begin
  ExecuteVerb(cShowEditor);
end;

procedure TJvCustomPageEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    cShowEditor:
      ShowPageListEditor(Designer, GetPageControl);
    cNextPage:
      NextPage;
    cPrevPage:
      PrevPage;
    cNewPage:
      InsertPage;
    cDelPage:
      RemovePage;
  end;
end;

function TJvCustomPageEditor.GetPageControl: TJvCustomPageList;
begin
  if Component is TJvCustomPageList then
    Result := TJvCustomPageList(Component)
  else
    Result := TJvCustomPageList(TJvCustomPage(Component).PageList);
end;

function TJvCustomPageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    cShowEditor:
      Result := RsPageListEditorEllipsis;
    cDash:
      Result := '-';
    cNewPage:
      Result := RsNewPage;
    cNextPage:
      Result := RsNextPageAmp;
    cPrevPage:
      Result := RsPrevPage;
    cDelPage:
      Result := RsDelPage;
  end;
end;

function TJvCustomPageEditor.GetVerbCount: Integer;
begin
  Result := cElementCount; // list, div, new, next, previous, delete
end;

procedure TJvCustomPageEditor.InsertPage;
var
  P: TJvCustomPage;
  C: TJvCustomPageList;
begin
  C := GetPageControl;
  P := C.GetPageClass.Create(Designer.{$IFDEF COMPILER6_UP} Root {$ELSE} Form {$ENDIF});
  try
    P.Parent := C;
    P.Name := Designer.UniqueName(C.GetPageClass.ClassName);
    P.PageList := C;
    C.ActivePage := P;
  except
    P.Free;
    raise;
  end;
end;

procedure TJvCustomPageEditor.NextPage;
begin
  GetPageControl.NextPage;
end;

procedure TJvCustomPageEditor.PrevPage;
begin
  GetPageControl.PrevPage;
end;

procedure TJvCustomPageEditor.RemovePage;
var
  AList: TJvCustomPageList;
  APage: TJvCustomPage;
begin
  AList := GetPageControl;
  if (AList <> nil) and (AList.ActivePage <> nil) then
  begin
    APage := AList.ActivePage;
    Designer.SelectComponent(APage);
    APage.PageList := nil;
    APage.Free;
    Designer.Modified;
  end;
end;

//=== { TJvActivePageProperty } ==============================================

function TJvActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TJvActivePageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  for I := 0 to Designer.GetRoot.ComponentCount - 1 do
  begin
    Component := Designer.GetRoot.Components[I];
    if (Component.Name <> '') and (Component is TJvCustomPage) and
      (TJvCustomPage(Component).PageList = GetComponent(0)) then
      Proc(Component.Name);
  end;
end;

//=== { TJvSettingsTreeImagesProperty } ======================================

function TJvSettingsTreeImagesProperty.ImageList: TCustomImageList;
var
  T: TJvCustomPageListTreeView;
begin
  if (GetComponent(0) is TJvSettingsTreeImages) and
    (TJvSettingsTreeImages(GetComponent(0)).TreeView <> nil) then
  begin
    T := TJvSettingsTreeImages(GetComponent(0)).TreeView;
    Result := THackTreeView(T).Images;
  end
  else
    Result := nil;
end;

//=== { TJvShowDesignCaptionProperty } =======================================

function TJvShowDesignCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  // we don't want sorting for this property
  Result := [paMultiSelect, paValueList, paRevertable];
end;

{$IFNDEF COMPILER6_UP}

//=== { TJvInterfaceProperty } ===============================================

function TJvInterfaceProperty.IntfSupported(AComponent: TComponent): Boolean;
var
  Obj: IUnknown;
begin
  Result := Supports(AComponent, GetInterfaceGUID, Obj);
end;

procedure TJvInterfaceProperty.ProcComps(const S: string);
var
  Comp: TComponent;
begin
  Comp := Designer.GetComponent(S);
  if (Comp <> nil) and IntfSupported(Comp) then
    OrgStrProc(S);
end;

procedure TJvInterfaceProperty.GetValues(Proc: TGetStrProc);
begin
  OrgStrProc := Proc;
  inherited GetValues(ProcComps);
end;

procedure TJvInterfaceProperty.SetValue(const Value: string);
var
  Comp: TComponent;
begin
  if Value = '' then
    Comp := nil
  else
  begin
    Comp := Designer.GetComponent(Value);
    if not (Comp is GetTypeData(GetPropType)^.ClassType) and not IntfSupported(Comp) then
      raise EPropertyError.CreateResFmt(@RsEFmtInterfaceNotSupported, [Comp.Name, GetInterfaceName]);
  end;
  SetOrdValue(Longint(Comp));
end;

//=== { TJvPageListProperty } ================================================

function TJvPageListProperty.GetInterfaceGUID: TGUID;
begin
  Result := IPageList;
end;

function TJvPageListProperty.GetInterfaceName: string;
begin
  Result := 'IPageList';
end;

{$ENDIF COMPILER6_UP}

end.
