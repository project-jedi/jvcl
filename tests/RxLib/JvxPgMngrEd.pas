{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxPgMngrEd.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvxPgMngrEd;

interface

uses
{$IFDEF WIN32}
  Windows,
{$ELSE}
  WinTypes, WinProcs,
{$ENDIF}
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, Grids,
  RTLConsts, DesignIntf, DesignEditors, VCLEditors, JvxPageMngr, StdCtrls, JvxPlacemnt, ExtCtrls,
  JvxVCLUtils, DesignWindows;

type
  TJvxProxyEditor = class(TDesignWindow)
    FormStorage: TJvxFormStorage;
    BtnPanel: TPanel;
    CloseBtn: TButton;
    DeleteBtn: TButton;
    ProxyGrid: TDrawGrid;
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
    FPageManager: TJvxPageManager;
    FDeleting: Boolean;
    procedure SeTJvxPageManager(Value: TJvxPageManager);
    function GetForm: TCustomForm;
    procedure UpdateData;
    function CheckPageManager: Boolean;
    procedure SelectProxy(Proxy: TJvxPageProxy);
    function ProxyByRow(Row: Integer): TJvxPageProxy;
  protected
    function UniqueName(Component: TComponent): string; override;
    procedure Activated; override;
  public
    procedure NameProxy(Sender: TObject);
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    function GetEditState: TEditState; override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    property PageManager: TJvxPageManager read FPageManager write SeTJvxPageManager;
    property OwnerForm: TCustomForm read GetForm;
  end;

{ TJvxProxyListProperty }

  TJvxProxyListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ TJvxPageManagerEditor }

  TJvxPageManagerEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TJvxPageNameProperty }

  TJvxPageNameProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TJvxPageBtnProperty }

  TJvxPageBtnProperty = class(TComponentProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses Consts, Buttons, JvxCtrls, JvxConst, JvxLConst, JvxDsgn;

{$R *.DFM}

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

{$IFDEF Delphi4_Up}
type
  TDesigner = IDesigner;
  TFormDesigner = IDesigner;
{$ENDIF}

function FindEditor(Manager: TJvxPageManager): TJvxProxyEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TJvxProxyEditor then begin
      if TJvxProxyEditor(Screen.Forms[I]).PageManager = Manager then
      begin
        Result := TJvxProxyEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowProxyEditor(Designer: TDesigner; Manager: TJvxPageManager);
var
  Editor: TJvxProxyEditor;
begin
  if Manager = nil then Exit;
  Editor := FindEditor(Manager);
  if Editor <> nil then begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then Editor.WindowState := wsNormal;
  end
  else begin
    Editor := TJvxProxyEditor.Create(Application);
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

{ TJvxProxyListProperty }

function TJvxProxyListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvxProxyListProperty.GetValue: string;
var
  List: TList;
begin
  List := TList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0) then
    Result := ResStr(srNone)
  else FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TJvxProxyListProperty.Edit;
begin
  ShowProxyEditor(Designer, TJvxPageManager(GetComponent(0)));
end;

{ TJvxPageBtnProperty }

procedure TJvxPageBtnProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  for I := 0 to Designer.Root.ComponentCount - 1 do begin
    Component := Designer.Root.Components[I];
    if (Component.InheritsFrom(TButtonControl) or 
      Component.InheritsFrom(TSpeedButton) or 
      Component.InheritsFrom(TJvxSpeedButton)) and 
      (Component.Name <> '') then Proc(Component.Name);
  end;
end;

{ TJvxPageNameProperty }

function TJvxPageNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TJvxPageNameProperty.GetValues(Proc: TGetStrProc);
var
  PageProxy: TJvxPageProxy;
  I: Integer;
begin
  PageProxy := GetComponent(0) as TJvxPageProxy;
  if (PageProxy <> nil) and (PageProxy.PageManager <> nil) and
    (PageProxy.PageManager.PageOwner <> nil) then
  begin
    for I := 0 to PageProxy.PageManager.PageCount - 1 do begin
      Proc(PageProxy.PageManager.PageNames[I]);
    end;
  end;
end;

{ TJvxPageManagerEditor }

procedure TJvxPageManagerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowProxyEditor(Designer, TJvxPageManager(Component));
  end;
end;

function TJvxPageManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := LoadStr(srProxyEditor);
  end;
end;

function TJvxPageManagerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TJvxProxyEditor }

procedure TJvxProxyEditor.SeTJvxPageManager(Value: TJvxPageManager);
begin
  if FPageManager <> Value then begin
    if FPageManager <> nil then FPageManager.OnCheckProxy := nil;
    FPageManager := Value;
    if FPageManager <> nil then FPageManager.OnCheckProxy := NameProxy;
    UpdateData;
  end;
end;

function TJvxProxyEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
{$IFNDEF WIN32}
  I: Integer;
  Comp: TComponent;
{$ENDIF}
begin
  Result := '';
  if (Component <> nil) then Temp := Component.ClassName
  else Temp := TJvxPageProxy.ClassName;
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

function TJvxProxyEditor.GetEditState: TEditState;
begin
  Result := [];
end;

procedure TJvxProxyEditor.NameProxy(Sender: TObject);
begin
  if (Sender is TJvxPageProxy) and (TJvxPageProxy(Sender).Name = '') then
    TJvxPageProxy(Sender).Name := UniqueName(TJvxPageProxy(Sender));
end;

procedure TJvxProxyEditor.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  if Designer.Root = OwnerForm then Free;
end;

procedure TJvxProxyEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TJvxProxyEditor.Activated;
begin
  SelectProxy(ProxyByRow(ProxyGrid.Row - 1));
end;

procedure TJvxProxyEditor.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  if Item = FPageManager then begin
    FPageManager := nil;
    Close;
  end;
end;

procedure TJvxProxyEditor.UpdateData;
var
  ProxyCount: Integer;
begin
  if CheckPageManager then begin
    if not FDeleting then FPageManager.Resync;
    ProxyCount := FPageManager.PageProxies.Count;
    if ProxyCount = 0 then begin
      ProxyGrid.RowCount := 2;
      SelectProxy(nil);
    end
    else begin
      ProxyGrid.RowCount := 1 + ProxyCount;
    end;
    DeleteBtn.Enabled := ProxyCount > 0;
    ProxyGrid.Invalidate;
  end;
end;

function TJvxProxyEditor.GetForm: TCustomForm;
begin
  Result := GetParentForm(BtnPanel); //Designer.Form;
end;

procedure TJvxProxyEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if FPageManager <> nil then FPageManager.OnCheckProxy := nil;
end;

procedure TJvxProxyEditor.FormShow(Sender: TObject);
begin
  if FPageManager.PageOwner <> nil then begin
    Caption := Format(LoadStr(srPageProxies), [FPageManager.PageOwner.Name]);
  end;
end;

function TJvxProxyEditor.CheckPageManager: Boolean;
begin
  Result := (FPageManager <> nil) and (FPageManager.Owner <> nil) and
    (Designer.Root <> nil);
end;

procedure TJvxProxyEditor.SelectProxy(Proxy: TJvxPageProxy);
var
  FComponents: IDesignerSelections;
begin
  if CheckPageManager and Active then begin
    FComponents := CreateSelectionList;
    if Proxy <> nil then
      FComponents.Add(Proxy)
    else
      FComponents.Add(FPageManager);
    SetSelection(FComponents);
  end;
end;

function TJvxProxyEditor.ProxyByRow(Row: Integer): TJvxPageProxy;
begin
  Result := nil;
  if CheckPageManager and (Row >= 0) and
    (Row < FPageManager.PageProxies.Count) then
  begin
    Result := FPageManager.PageProxies.Items[Row];
  end;
end;

procedure TJvxProxyEditor.ProxyGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  CellText: string;
  Proxy: TJvxPageProxy;
begin
  CellText := '';
  if gdFixed in State then begin
    case Col of
      0: CellText := LoadStr(srProxyName);
      1: CellText := LoadStr(srPageName);
    end;
  end
  else begin
    Proxy := ProxyByRow(Row - 1);
    if Proxy <> nil then begin
      case Col of
        0: CellText := Proxy.Name;
        1: CellText := Proxy.PageName;
      end;
    end;
  end;
  DrawCellText(ProxyGrid, Col, Row, CellText, Rect, taLeftJustify, vaCenter);
end;

procedure TJvxProxyEditor.ProxyGridSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  SelectProxy(ProxyByRow(Row - 1));
end;

procedure TJvxProxyEditor.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TJvxProxyEditor.DeleteBtnClick(Sender: TObject);
var
  Proxy: TJvxPageProxy;
begin
  Proxy := ProxyByRow(ProxyGrid.Row - 1);
  if Proxy <> nil then begin
    Self.ValidateRename(Proxy, Proxy.Name, '');
    FDeleting := True;
    try
      Proxy.Free;
      Designer.Modified;
    finally
      FDeleting := False;
    end;
  end;
end;

procedure TJvxProxyEditor.ProxyGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then begin
    case Key of
      VK_RETURN:
        if ProxyByRow(ProxyGrid.Row - 1) <> nil then begin
          ActivateInspector(#0);
        end;
      VK_DELETE:
        DeleteBtnClick(nil);
    end;
  end;
end;

procedure TJvxProxyEditor.FormResize(Sender: TObject);
begin
  with ProxyGrid do begin
    DefaultColWidth := (ClientWidth - 1) div 2;
    ColWidths[1] := ClientWidth - ColWidths[0] - 1;
  end;
end;

procedure TJvxProxyEditor.FormCreate(Sender: TObject);
begin
  if NewStyleControls then Font.Style := [];
{$IFDEF WIN32}
  with FormStorage do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
{$ENDIF}
end;

end.
