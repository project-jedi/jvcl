{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageListTreeViewReg.PAS, released on 2003-01-22.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvPageListTreeViewReg;
interface
uses
  Windows, Classes, JvPageListTreeView, JvDsgnEditors,
  {$IFDEF COMPILER6_UP}DesignEditors, DesignIntf, DesignMenus, VCLEditors,
  {$ELSE}DsgnIntf, Menus, {$ENDIF}ImgList,
  Graphics;

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
    {$IFDEF COMPILER6_UP}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
    {$ELSE}
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;
    {$ENDIF}
  end;

  TJvSettingsTreeImagesProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

  {$IFNDEF COMPILER6_UP}

  // since D5 doesn't support interface style published properties,
  // this editor is supplied to make it easier to select a specific interface
  // implementor at design-time
  // NOTE: you must derive a new editor from TJvInterfaceProperty and override
  // GetInterfaceGUID and GetInterfaceName since these are declared as virtual abstract
  TJvInterfaceProperty = class(TComponentProperty)
  private
    FOrgStrProc: TGetStrProc;
  protected
    function IntfSupported(AComponent:TComponent):boolean;virtual;
    function GetInterfaceGUID:TGUID;virtual;abstract;
    function GetInterfaceName: string; virtual;abstract;
    procedure ProcComps(const S:String);
    property OrgStrProc: TGetStrProc read FOrgStrProc write FOrgStrProc;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TJvPageListProperty = class(TJvInterfaceProperty)
  protected
    function GetInterfaceGUID:TGUID;override;
    function GetInterfaceName: string; override;
  end;
  {$ENDIF}

procedure Register;
{$R ..\resources\JvPageListTreeViewReg.dcr}

implementation
uses
  Forms, ComCtrls, Controls, SysUtils, TypInfo, Consts,
  JvPageLinkEditor, JvTreeItemsEditor;

resourcestring
  SFmtInterfaceNotSupported = '%s does not support the required interface (%s)';
  SNextPage = 'Ne&xt Page';
  SPrevPage = '&Previous Page';
  SNewPage = '&New Page';
  SDelPage = '&Delete Page';

type
  THackTreeView = class(TJvCustomPageListTreeView);

procedure Register;
begin
  RegisterPropertyEditor(typeinfo(TTreeNodes), TCustomTreeView, 'Items', TJvTreeItemsProperty);
  RegisterPropertyEditor(typeinfo(TJvShowDesignCaption), nil, '', TJvShowDesignCaptionProperty);
  RegisterComponents('Jv Lists, Combos and Trees', [TJvSettingsTreeView, TJvPageListTreeView, TJvPageList]);
  RegisterClasses([TJvSettingsTreeView, TJvPageListTreeView, TJvPageList, TJvStandardPage]);
  RegisterComponentEditor(TJvCustomPageList, TJvCustomPageEditor);
  RegisterComponentEditor(TJvCustomPage, TJvCustomPageEditor);
  {$IFNDEF COMPILER6_UP}
  RegisterPropertyEditor(typeinfo(TComponent), TJvCustomPageListTreeView, 'PageList', TJvPageListProperty);
  {$ENDIF}
  RegisterComponentEditor(TCustomTreeView, TJvTreeViewComponentEditor);
  RegisterComponentEditor(TJvCustomPageListTreeView, TJvPageTreeViewComponentEditor);
  // register for the standard TTreeView as well
//  RegisterComponentEditor(TTreeView, TJvTreeViewComponentEditor);
  RegisterPropertyEditor(typeinfo(TJvPageLinks),
    TJvCustomPageListTreeView, '', TJvPageLinksProperty);
  RegisterPropertyEditor(typeinfo(TJvCustomPage),
    TJvCustomPageList, 'ActivePage', TJvActivePageProperty);
  RegisterPropertyEditor(typeinfo(TImageIndex), TJvSettingsTreeImages, '', TJvSettingsTreeImagesProperty);
  //  RegisterPropertyEditor(typeinfo(integer),TJvSettingsTreeImages,'CollapsedIndex',TJvSettingsTreeImagesProperty);
  //  RegisterPropertyEditor(typeinfo(integer),TJvSettingsTreeImages,'ExpandedIndex',TJvSettingsTreeImagesProperty);
  //  RegisterPropertyEditor(typeinfo(integer),TJvSettingsTreeImages,'ImageIndex',TJvSettingsTreeImagesProperty);
  //  RegisterPropertyEditor(typeinfo(integer),TJvSettingsTreeImages,'SelectedIndex',TJvSettingsTreeImagesProperty);
end;

{ TJvCustomPageEditor }

procedure TJvCustomPageEditor.Edit;
begin
  // move to next page
  if GetPageControl.PageCount >= 1 then
    ExecuteVerb(0)
  else
    ExecuteVerb(2); // create new
end;

procedure TJvCustomPageEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: NextPage;
    1: PrevPage;
    2: InsertPage;
    3: RemovePage;
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
    0: Result := SNextPage;
    1: Result := SPrevPage;
    2: Result := SNewPage;
    3: Result := SDelPage;
  end;
end;

function TJvCustomPageEditor.GetVerbCount: Integer;
begin
  Result := 4; // new, next, previous, delete,
end;

procedure TJvCustomPageEditor.InsertPage;
var
  P: TJvCustomPage;
  C: TJvCustomPageList;
begin
  C := GetPageControl;
  P := C.GetPageClass.Create(Designer.{$IFDEF COMPILER6_UP}Root{$ELSE}Form{$ENDIF});
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

{$IFDEF COMPILER6_UP}

procedure TJvCustomPageEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
{$ELSE}

procedure TJvCustomPageEditor.PrepareItem(Index: Integer; const AItem: TMenuItem);
{$ENDIF}
begin
  inherited;
  case Index of
   {$IFNDEF COMPILER6_UP}
    0:
      AItem.Default := GetPageControl.PageCount > 1;
    2:
      AItem.Default := GetPageControl.PageCount = 0;
    {$ENDIF}
    3:
      AItem.Enabled := GetPageControl.ActivePage <> nil;
  end;
end;

procedure TJvCustomPageEditor.PrevPage;
begin
  GetPageControl.PrevPage;
end;

procedure TJvCustomPageEditor.RemovePage;
var
  P: TJvCustomPageList;
  P2: TJvCustomPage;
begin
  P := GetPageControl;
  if (P <> nil) and (P.ActivePage <> nil) then
  begin
    P2 := P.ActivePage;
    Designer.SelectComponent(P2);
    P2.PageList := nil;
    P2.Free;
    Designer.Modified;
  end;
end;

{ TJvActivePageProperty }

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

{ TJvSettingsTreeImagesProperty }

function TJvSettingsTreeImagesProperty.ImageList: TCustomImageList;
var
  T: TJvCustomPageListTreeView;
begin
  if (GetComponent(0) is TJvSettingsTreeImages) and (TJvSettingsTreeImages(GetComponent(0)).TreeView <> nil) then
  begin
    T := TJvSettingsTreeImages(GetComponent(0)).TreeView;
    Result := THackTreeView(T).Images;
  end
  else
    Result := nil;
end;

{ TJvShowDesignCaptionProperty }

function TJvShowDesignCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  // we don't want sorting for this property
  Result := [paMultiSelect, paValueList, paRevertable];
end;

{$IFNDEF COMPILER6_UP}
{ TJvInterfaceProperty }

function TJvInterfaceProperty.IntfSupported(AComponent:TComponent):boolean;
var obj:IUnknown;
begin
  Result := Supports(AComponent,GetInterfaceGUID,obj);
end;

procedure TJvInterfaceProperty.ProcComps(const S:String);
var Comp:TComponent;
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
      raise EPropertyError.CreateFmt(SFmtInterfaceNotSupported,[Comp.Name,GetInterfaceName]);
  end;
  SetOrdValue(Longint(Comp));
end;

{ TJvPageListProperty }

function TJvPageListProperty.GetInterfaceGUID: TGUID;
begin
  Result := IPageList;
end;

function TJvPageListProperty.GetInterfaceName: string;
begin
  Result := 'IPageList';
end;
{$ENDIF}

end.

