{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPgMngrEd.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPgMngrEd;

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ELSE}
  WinTypes, WinProcs,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Grids,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors, DesignWindows,
  {$ELSE}
  LibIntf, DsgnIntf, DsgnWnds,
  {$ENDIF}
  StdCtrls, ExtCtrls,
  JvPageMngr, JvPlacemnt, JvVCLUtils, JvComponent;

type
  TJvProxyEditor = class(TDesignWindow)
    BtnPanel: TPanel;
    CloseBtn: TButton;
    DeleteBtn: TButton;
    ProxyGrid: TDrawGrid;
    FormStorage: TJvFormStorage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ProxyGridDrawCell(Sender: TObject; Col, Row: Longint; Rect: TRect; State: TGridDrawState);
    procedure ProxyGridSelectCell(Sender: TObject; Col, Row: Longint; var CanSelect: Boolean);
    procedure CloseBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure ProxyGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPageManager: TJvPageManager;
    FDeleting: Boolean;
    procedure SetPageManager(Value: TJvPageManager);
    function GetForm: TCustomForm;
    procedure UpdateData;
    function CheckPageManager: Boolean;
    procedure SelectProxy(Proxy: TJvPageProxy);
    function ProxyByRow(Row: Integer): TJvPageProxy;
  protected
    function UniqueName(Component: TComponent): string; override;
    procedure Activated; override;
  public
    procedure NameProxy(Sender: TObject);
    {$IFDEF COMPILER6_UP}
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
    {$ELSE}
    procedure FormModified; override;
    {$IFDEF COMPILER3_UP}
    procedure FormClosed(Form: TCustomForm); override;
    {$ELSE}
    procedure FormClosed(Form: TForm); override;
    {$ENDIF}
    {$ENDIF}
    function GetEditState: TEditState; override;
    {$IFDEF COMPILER6_UP}
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    {$ELSE}
    {$IFDEF COMPILER4_UP}
    procedure ComponentDeleted(Component: IPersistent); override;
    {$ELSE}
    procedure ComponentDeleted(Component: TComponent); override;
    {$ENDIF}
    {$ENDIF}
    property PageManager: TJvPageManager read FPageManager write SetPageManager;
    property OwnerForm: TCustomForm read GetForm;
  end;

  TJvProxyListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TJvPageManagerEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvPageNameProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvPageBtnProperty = class(TComponentProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  Consts, Buttons,
  JvxCtrls, JvConst, JvxDConst;

{$R *.DFM}

{$IFDEF WIN32}
{$D-}
{$ENDIF}

{$IFDEF COMPILER6_UP}
type
  TDesigner = DesignIntf.IDesigner;
  TFormDesigner = DesignIntf.IDesigner;
{$ELSE}
{$IFDEF COMPILER4_UP}
type
  TDesigner = IDesigner;
  TFormDesigner = IFormDesigner;
{$ENDIF}
{$ENDIF}

function FindEditor(Manager: TJvPageManager): TJvProxyEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TJvProxyEditor then
      if TJvProxyEditor(Screen.Forms[I]).PageManager = Manager then
      begin
        Result := TJvProxyEditor(Screen.Forms[I]);
        Break;
      end;
end;

procedure ShowProxyEditor(Designer: TDesigner; Manager: TJvPageManager);
var
  Editor: TJvProxyEditor;
begin
  if Manager = nil then
    Exit;
  Editor := FindEditor(Manager);
  if Editor <> nil then
  begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then
      Editor.WindowState := wsNormal;
  end
  else
  begin
    Editor := TJvProxyEditor.Create(Application);
    try
      Editor.Designer := TFormDesigner(Designer);
      Editor.PageManager := Manager;
      Editor.Show;
    except
      Editor.Free;
      raise;
    end;
  end;
end;

//=== TJvProxyListProperty ===================================================

function TJvProxyListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvProxyListProperty.GetValue: string;
var
  List: TList;
begin
  List := TList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0) then
    Result := ResStr(srNone)
  else
    FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TJvProxyListProperty.Edit;
begin
  ShowProxyEditor(Designer, TJvPageManager(GetComponent(0)));
end;

//=== TJvPageBtnProperty =====================================================

procedure TJvPageBtnProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  {$IFDEF COMPILER6_UP}
  for I := 0 to Designer.Root.ComponentCount - 1 do
  {$ELSE}
  for I := 0 to Designer.Form.ComponentCount - 1 do
  {$ENDIF}
  begin
    {$IFDEF COMPILER6_UP}
    Component := Designer.Root.Components[I];
    {$ELSE}
    Component := Designer.Form.Components[I];
    {$ENDIF}

    if (Component.InheritsFrom(TButtonControl) or
      Component.InheritsFrom(TSpeedButton) or
      Component.InheritsFrom(TJvSpeedButton)) and
      (Component.Name <> '') then
      Proc(Component.Name);
  end;
end;

//=== TJvPageNameProperty ====================================================

function TJvPageNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TJvPageNameProperty.GetValues(Proc: TGetStrProc);
var
  PageProxy: TJvPageProxy;
  I: Integer;
begin
  PageProxy := GetComponent(0) as TJvPageProxy;
  if (PageProxy <> nil) and (PageProxy.PageManager <> nil) and
    (PageProxy.PageManager.PageOwner <> nil) then
    for I := 0 to PageProxy.PageManager.PageCount - 1 do
      Proc(PageProxy.PageManager.PageNames[I]);
end;

//=== TJvPageManagerEditor ===================================================

procedure TJvPageManagerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowProxyEditor(Designer, TJvPageManager(Component));
  end;
end;

function TJvPageManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := srProxyEditor;
  end;
end;

function TJvPageManagerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== TJvProxyEditor =========================================================

procedure TJvProxyEditor.SetPageManager(Value: TJvPageManager);
begin
  if FPageManager <> Value then
  begin
    if FPageManager <> nil then
      FPageManager.OnCheckProxy := nil;
    FPageManager := Value;
    if FPageManager <> nil then
      FPageManager.OnCheckProxy := NameProxy;
    UpdateData;
  end;
end;

function TJvProxyEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
  {$IFNDEF WIN32}
  I: Integer;
  Comp: TComponent;
  {$ENDIF}
begin
  Result := '';
  if (Component <> nil) then
    Temp := Component.ClassName
  else
    Temp := TJvPageProxy.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
  {$IFDEF WIN32}
  Result := Designer.UniqueName(Temp);
  {$ELSE}
  I := 1;
  repeat
    Result := Temp + IntToStr(I);
    Comp := OwnerForm.FindComponent(Result);
    Inc(I);
  until (Comp = nil) or (Comp = Component);
  {$ENDIF}
end;

function TJvProxyEditor.GetEditState: TEditState;
begin
  Result := [];
end;

procedure TJvProxyEditor.NameProxy(Sender: TObject);
begin
  if (Sender is TJvPageProxy) and (TJvPageProxy(Sender).Name = '') then
    TJvPageProxy(Sender).Name := UniqueName(TJvPageProxy(Sender));
end;

{$IFDEF COMPILER6_UP}
procedure TJvProxyEditor.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
{$ELSE}
{$IFDEF COMPILER3_UP}
procedure TJvProxyEditor.FormClosed(Form: TCustomForm);
{$ELSE}
procedure TJvProxyEditor.FormClosed(Form: TForm);
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF COMPILER6_UP}
  if ADesigner.Root = OwnerForm then
  {$ELSE}
  if Form = OwnerForm then
  {$ENDIF}
    Free;
end;

{$IFDEF COMPILER6_UP}
procedure TJvProxyEditor.ItemsModified(const Designer: IDesigner);
{$ELSE}
procedure TJvProxyEditor.FormModified;
{$ENDIF}
begin
  if not (csDestroying in ComponentState) then
    UpdateData;
end;

procedure TJvProxyEditor.Activated;
begin
  SelectProxy(ProxyByRow(ProxyGrid.Row - 1));
end;

{$IFDEF COMPILER6_UP}
procedure TJvProxyEditor.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  if Item = FPageManager then
  begin
{$ELSE}
{$IFDEF COMPILER4_UP}
procedure TJvProxyEditor.ComponentDeleted(Component: IPersistent);
begin
  if ExtractPersistent(Component) = FPageManager then
  begin
{$ELSE}
procedure TJvProxyEditor.ComponentDeleted(Component: TComponent);
begin
  if Component = FPageManager then
  begin
{$ENDIF}
{$ENDIF}
    FPageManager := nil;
    Close;
  end;
end;

procedure TJvProxyEditor.UpdateData;
var
  ProxyCount: Integer;
begin
  if CheckPageManager then
  begin
    if not FDeleting then
      FPageManager.Resync;
    ProxyCount := FPageManager.PageProxies.Count;
    if ProxyCount = 0 then
    begin
      ProxyGrid.RowCount := 2;
      SelectProxy(nil);
    end
    else
    begin
      ProxyGrid.RowCount := 1 + ProxyCount;
    end;
    DeleteBtn.Enabled := ProxyCount > 0;
    ProxyGrid.Invalidate;
  end;
end;

function TJvProxyEditor.GetForm: TCustomForm;
begin
  {$IFDEF COMPILER6_UP}
  Result := TCustomForm(Designer.Root); { GetParentForm(FBar) }
  {$ELSE}
  Result := Designer.Form; { GetParentForm(FBar) }
  {$ENDIF}
end;

procedure TJvProxyEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if FPageManager <> nil then
    FPageManager.OnCheckProxy := nil;
end;

procedure TJvProxyEditor.FormShow(Sender: TObject);
begin
  if FPageManager.PageOwner <> nil then
  begin
    Caption := Format(srPageProxies, [FPageManager.PageOwner.Name]);
  end;
end;

function TJvProxyEditor.CheckPageManager: Boolean;
begin
  Result := (FPageManager <> nil) and (FPageManager.Owner <> nil) and
    {$IFDEF COMPILER6_UP}
    (Designer.Root <> nil);
    {$ELSE}
    (Designer.Form <> nil);
    {$ENDIF}
end;

{$IFDEF COMPILER6_UP}
type
  TDesignerSelectionList = IDesignerSelections;
{$ENDIF}

procedure TJvProxyEditor.SelectProxy(Proxy: TJvPageProxy);
var
  FComponents: TDesignerSelectionList;
begin
  if CheckPageManager and Active then
  begin
    {$IFDEF COMPILER6_UP}
    FComponents := TDesignerSelections.Create;
    {$ELSE}
    FComponents := TDesignerSelectionList.Create;
    {$ENDIF}
    if Proxy <> nil then
      FComponents.Add(Proxy)
    else
      FComponents.Add(FPageManager);
    SetSelection(FComponents);
  end;
end;

function TJvProxyEditor.ProxyByRow(Row: Integer): TJvPageProxy;
begin
  Result := nil;
  if CheckPageManager and (Row >= 0) and
    (Row < FPageManager.PageProxies.Count) then
  begin
    Result := FPageManager.PageProxies.Items[Row];
  end;
end;

procedure TJvProxyEditor.ProxyGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  CellText: string;
  Proxy: TJvPageProxy;
begin
  CellText := '';
  if gdFixed in State then
  begin
    case Col of
      0:
        CellText := srProxyName;
      1:
        CellText := srPageName;
    end;
  end
  else
  begin
    Proxy := ProxyByRow(Row - 1);
    if Proxy <> nil then
    begin
      case Col of
        0:
          CellText := Proxy.Name;
        1:
          CellText := Proxy.PageName;
      end;
    end;
  end;
  DrawCellText(ProxyGrid, Col, Row, CellText, Rect, taLeftJustify, vaCenter);
end;

procedure TJvProxyEditor.ProxyGridSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  SelectProxy(ProxyByRow(Row - 1));
end;

procedure TJvProxyEditor.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TJvProxyEditor.DeleteBtnClick(Sender: TObject);
var
  Proxy: TJvPageProxy;
begin
  Proxy := ProxyByRow(ProxyGrid.Row - 1);
  if Proxy <> nil then
  begin
    {$IFDEF COMPILER6_UP}
    TCustomForm(Designer.Root).Designer.ValidateRename(Proxy, Proxy.Name, '');
    {$ELSE}
    Designer.ValidateRename(Proxy, Proxy.Name, '');
    {$ENDIF}
    FDeleting := True;
    try
      Proxy.Free;
      Designer.Modified;
    finally
      FDeleting := False;
    end;
  end;
end;

procedure TJvProxyEditor.ProxyGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN:
        if ProxyByRow(ProxyGrid.Row - 1) <> nil then
          ActivateInspector(#0);
      VK_DELETE:
        DeleteBtnClick(nil);
    end;
  end;
end;

procedure TJvProxyEditor.FormResize(Sender: TObject);
begin
  with ProxyGrid do
  begin
    DefaultColWidth := (ClientWidth - 1) div 2;
    ColWidths[1] := ClientWidth - ColWidths[0] - 1;
  end;
end;

procedure TJvProxyEditor.FormCreate(Sender: TObject);
begin
  if NewStyleControls then
    Font.Style := [];
  {$IFDEF WIN32}
  with FormStorage do
  begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
  {$ENDIF}
end;

end.

