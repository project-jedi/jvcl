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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDUALLIST;

interface

uses Classes, Controls{, JvComponent};

type

{ TJvDualListDialog }

  TJvDualListDialog = class(TComponent)
  private
    FCtl3D: Boolean;
    FSorted: Boolean;
    FTitle: String;
    FLabel1Caption: TCaption;
    FLabel2Caption: TCaption;
    FOkBtnCaption: TCaption;
    FCancelBtnCaption: TCaption;
    FHelpBtnCaption: TCaption;
    FHelpContext: THelpContext;
    FList1: TStrings;
    FList2: TStrings;
    FShowHelp: Boolean;
    function GetTitle: string;
    procedure SetTitle(const ATitle: string);
    procedure SetList1(Value: TStrings);
    procedure SetList2(Value: TStrings);
    function IsLabel1Custom: Boolean;
    function IsLabel2Custom: Boolean;
    function IsOkBtnCustom: Boolean;
    function IsCancelBtnCustom: Boolean;
    function IsHelpBtnCustom: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property Ctl3D: Boolean read FCtl3D write FCtl3D default True;
    property Sorted: Boolean read FSorted write FSorted;
    property Title: string read GetTitle write SetTitle;
    property Label1Caption: TCaption read FLabel1Caption write FLabel1Caption
      stored IsLabel1Custom;
    property Label2Caption: TCaption read FLabel2Caption write FLabel2Caption
      stored IsLabel2Custom;
    property OkBtnCaption: TCaption read FOkBtnCaption write FOkBtnCaption
      stored IsOkBtnCustom;
    property CancelBtnCaption: TCaption read FCancelBtnCaption write FCancelBtnCaption
      stored IsCancelBtnCustom;
    property HelpBtnCaption: TCaption read FHelpBtnCaption write FHelpBtnCaption
      stored IsHelpBtnCustom;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property List1: TStrings read FList1 write SetList1;
    property List2: TStrings read FList2 write SetList2;
    property ShowHelp: Boolean read FShowHelp write FShowHelp default True;
  end;

implementation

uses SysUtils, Forms, JvFDualLst, Consts, JvTConst, JvVCLUtils;

{ TJvDualListDialog }

constructor TJvDualListDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCtl3D := True;
  FShowHelp := True;
  FTitle := EmptyStr;
  FList1 := TStringList.Create;
  FList2 := TStringList.Create;
  FLabel1Caption := LoadStr(SDualListSrcCaption);
  FLabel2Caption := LoadStr(SDualListDestCaption);
  OkBtnCaption := ResStr(SOKButton);
  CancelBtnCaption := ResStr(SCancelButton);
  HelpBtnCaption := ResStr(SHelpButton);
end;

destructor TJvDualListDialog.Destroy;
begin
  List1.Free;
  List2.Free;
  //if (FTitle <> nil) and (FTitle^ <> '') then Dispose(FTitle);
  inherited Destroy;
end;

function TJvDualListDialog.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TJvDualListDialog.SetTitle(const ATitle: string);
begin
  FTitle := ATitle;
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
  Result := CompareStr(Label1Caption, LoadStr(SDualListSrcCaption)) <> 0;
end;

function TJvDualListDialog.IsLabel2Custom: Boolean;
begin
  Result := CompareStr(Label2Caption, LoadStr(SDualListDestCaption)) <> 0;
end;

function TJvDualListDialog.IsOkBtnCustom: Boolean;
begin
  Result := CompareStr(OkBtnCaption, ResStr(SOKButton)) <> 0;
end;

function TJvDualListDialog.IsCancelBtnCustom: Boolean;
begin
  Result := CompareStr(CancelBtnCaption, ResStr(SCancelButton)) <> 0;
end;

function TJvDualListDialog.IsHelpBtnCustom: Boolean;
begin
  Result := CompareStr(HelpBtnCaption, ResStr(SHelpButton)) <> 0;
end;

function TJvDualListDialog.Execute: Boolean;
var
  Form: TJvDualListForm;
begin
  Form := TJvDualListForm.Create(Application);
  try
    with Form do begin
      Ctl3D := Self.Ctl3D;
{$IFDEF WIN32}
      if NewStyleControls then Font.Style := [];
{$ENDIF}
      ShowHelp := Self.ShowHelp;
      SrcList.Sorted := Sorted;
      DstList.Sorted := Sorted;
      SrcList.Items := List1;
      DstList.Items := List2;
      if Self.Title <> '' then Form.Caption := Self.Title;
      if Label1Caption <> '' then SrcLabel.Caption := Label1Caption;
      if Label2Caption <> '' then DstLabel.Caption := Label2Caption;
      OkBtn.Caption := OkBtnCaption;
      CancelBtn.Caption := CancelBtnCaption;
      HelpBtn.Caption := HelpBtnCaption;
      HelpContext := Self.HelpContext;
      HelpBtn.HelpContext := HelpContext;
    end;
    Result := (Form.ShowModal = mrOk);
    if Result then begin
      List1 := Form.SrcList.Items;
      List2 := Form.DstList.Items;
    end;
  finally
    Form.Free;
  end;
end;

end.
