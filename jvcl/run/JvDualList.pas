{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDUALLIST.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDualList;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, // inline
  Classes, Controls, StdCtrls,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvComponentBase, Forms;

type
  TJvDualListCustomizeEvent = procedure(Sender: TObject; Form: TCustomForm) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDualListDialog = class(TJvComponent)
  private
    FSorted: Boolean;
    FTitle: string;
    FLabel1Caption: TCaption;
    FLabel2Caption: TCaption;
    FOkBtnCaption: TCaption;
    FCancelBtnCaption: TCaption;
    FHelpBtnCaption: TCaption;
    FHelpContext: THelpContext;
    FList1: TStrings;
    FList2: TStrings;
    FShowHelp: Boolean;
    FScrollBars: TScrollStyle;
    FWidth: Integer;
    FOnCustomize: TJvDualListCustomizeEvent;
    FHeight: Integer;
    FCenterOnControl: TControl;
    FResizable: Boolean;
    procedure SetList1(Value: TStrings);
    procedure SetList2(Value: TStrings);
    function IsLabel1Custom: Boolean;
    function IsLabel2Custom: Boolean;
    function IsOkBtnCustom: Boolean;
    function IsCancelBtnCustom: Boolean;
    function IsHelpBtnCustom: Boolean;
    procedure SetCenterOnControl(const Value: TControl);
  protected
    procedure CustomizeForm(AForm: TCustomForm); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property Sorted: Boolean read FSorted write FSorted;
    property Title: string read FTitle write FTitle;
    property Label1Caption: TCaption read FLabel1Caption write FLabel1Caption stored IsLabel1Custom;
    property Label2Caption: TCaption read FLabel2Caption write FLabel2Caption stored IsLabel2Custom;
    property OkBtnCaption: TCaption read FOkBtnCaption write FOkBtnCaption stored IsOkBtnCustom;
    property CancelBtnCaption: TCaption read FCancelBtnCaption write FCancelBtnCaption stored IsCancelBtnCustom;
    property HelpBtnCaption: TCaption read FHelpBtnCaption write FHelpBtnCaption stored IsHelpBtnCustom;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property List1: TStrings read FList1 write SetList1;
    property List2: TStrings read FList2 write SetList2;
    property CenterOnControl: TControl read FCenterOnControl write SetCenterOnControl;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property ShowHelp: Boolean read FShowHelp write FShowHelp default True;
    property ScrollBars: TScrollStyle read FScrollBars write FScrollBars default ssBoth;
    property Resizable: Boolean read FResizable write FResizable default True;
    property OnCustomize: TJvDualListCustomizeEvent read FOnCustomize write FOnCustomize;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Types, Consts, SysUtils,
  JvDualListForm, JvResources, JvJVCLUtils;

constructor TJvDualListDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowHelp := True;
  FResizable := True;
  FScrollBars := ssBoth;
  FList1 := TStringList.Create;
  FList2 := TStringList.Create;
  FLabel1Caption := RsDualListSrcCaption;
  FLabel2Caption := RsDualListDestCaption;
  OkBtnCaption := SOKButton;
  CancelBtnCaption := SCancelButton;
  HelpBtnCaption := SHelpButton;
end;

destructor TJvDualListDialog.Destroy;
begin
  CenterOnControl := nil; // remove free notification
  List1.Free;
  List2.Free;
  inherited Destroy;
end;

procedure TJvDualListDialog.CustomizeForm(AForm: TCustomForm);
begin
  if Assigned(FOnCustomize) then
    FOnCustomize(Self, AForm);
end;

procedure TJvDualListDialog.SetCenterOnControl(const Value: TControl);
begin
  ReplaceComponentReference(Self, Value, TComponent(FCenterOnControl));
end;

procedure TJvDualListDialog.SetList1(Value: TStrings);
begin
  FList1.Assign(Value);
end;

procedure TJvDualListDialog.SetList2(Value: TStrings);
begin
  FList2.Assign(Value);
end;

function TJvDualListDialog.IsLabel1Custom: Boolean;
begin
  Result := AnsiCompareStr(Label1Caption, RsDualListSrcCaption) <> 0;
end;

function TJvDualListDialog.IsLabel2Custom: Boolean;
begin
  Result := AnsiCompareStr(Label2Caption, RsDualListDestCaption) <> 0;
end;

function TJvDualListDialog.IsOkBtnCustom: Boolean;
begin
  Result := AnsiCompareStr(OkBtnCaption, SOKButton) <> 0;
end;

procedure TJvDualListDialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = CenterOnControl) then
    CenterOnControl := nil;
end;

function TJvDualListDialog.IsCancelBtnCustom: Boolean;
begin
  Result := AnsiCompareStr(CancelBtnCaption, SCancelButton) <> 0;
end;

function TJvDualListDialog.IsHelpBtnCustom: Boolean;
begin
  Result := AnsiCompareStr(HelpBtnCaption, SHelpButton) <> 0;
end;

function TJvDualListDialog.Execute: Boolean;
var
  Form: TJvDualListForm;
  Pt: TPoint;
begin
  Form := TJvDualListForm.Create(Application);
  try
    if Resizable then
    begin
      Form.BorderStyle := bsSizeable;
      Form.BorderIcons := Form.BorderIcons - [biMinimize, biMaximize] + [biSystemMenu];
      Form.Icon := nil;
    end;

    Form.SrcList.ScrollBars := FScrollBars;
    Form.DstList.ScrollBars := FScrollBars;
    Form.Font.Style := [];
    Form.ShowHelp := Self.ShowHelp;
    Form.SrcList.Sorted := Sorted;
    Form.DstList.Sorted := Sorted;
    Form.SrcList.Items := List1;
    Form.DstList.Items := List2;
    if Title <> '' then
      Form.Caption := Self.Title;
    if Label1Caption <> '' then
      Form.SrcLabel.Caption := Label1Caption;
    if Label2Caption <> '' then
      Form.DstLabel.Caption := Label2Caption;
    Form.OkBtn.Caption := OkBtnCaption;
    Form.CancelBtn.Caption := CancelBtnCaption;
    Form.HelpBtn.Caption := HelpBtnCaption;
    Form.HelpContext := Self.HelpContext;
    Form.HelpBtn.HelpContext := HelpContext;

    if Width <> 0 then
      Form.Width := Width;
    if Height <> 0 then
      Form.Height := Height;

    if CenterOnControl <> nil then
    begin
      Form.Position := poDesigned;
      Pt := CenterOnControl.ClientToScreen(Point(0, 0));
      Form.Left := Pt.X + (CenterOnControl.Width - Form.Width) div 2;
      Form.Top := Pt.Y + (CenterOnControl.Height - Form.Height) div 2;
      if Form.Left < 0 then
        Form.Left := 0;
      if Form.Top < 0 then
        Form.Top := 0;
      if Form.Left >= Screen.Width - Form.Width then
        Form.Left := Screen.Width - Form.Width;
      if Form.Top >= Screen.Height - Form.Height then
        Form.Top := Screen.Height - Form.Height;
    end;

    CustomizeForm(Form);
    Result := (Form.ShowModal = mrOk);
    if Result then
    begin
      List1 := Form.SrcList.Items;
      List2 := Form.DstList.Items;
    end;
  finally
    Form.Free;
  end;
end;

{$IFDEF UNITVERSIONING}

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
