{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOLBarEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Property editors for the TOutlookBar component }

unit JvOLBarEditor;
interface

uses
  JvOLBar, Classes, {$IFDEF Delphi6_UP}DesignEditors, DesignIntf, DesignMenus{$ELSE}Dsgnintf, Menus{$ENDIF};

type
  TOLBarActivePageEditor = class(TIntegerProperty)
  private
    FOL: TJvCustomOutlookBar;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: String);override;
    procedure GetValues(Proc: TGetStrProc); override;
    property OL:TJvCustomOutlookBar read FOL;
  end;

  TOLBarComponentEditor = class(TComponentEditor)
  private
    procedure DelButton(OLBar: TJvCustomOutlookBar);
    procedure DelPage(OLBar: TJvCustomOutlookBar);
    procedure NewButton(OLBar: TJvCustomOutlookBar);
    procedure NewPage(OLBar: TJvCustomOutlookBar);
    procedure NextPage(OLBar: TJvCustomOutlookBar);
    procedure PrevPage(OLBar: TJvCustomOutlookBar);
  public
    procedure Edit;override;


 {$IFDEF Delphi6_UP}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
{$ELSE}
     procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;
{$ENDIF}


    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TOLBarPagesPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;


implementation
uses
  SysUtils, JvOBEdFrm, Dialogs;

type
  THackOL = class(TJvCustomOutlookBar);

resourcestring
  SFmtPage  = 'Page %u';
  SFmtButton = 'Button %u';
  SOLEditor = 'OutlookBar Editor...';
  SOLNewPage = 'New Page';
  SOLDelPage = 'Delete Page';
  SOLNextPage = 'Next Page';
  SOLPrevPage = 'Previous Page';
  SOLSeparator  = '-';
  SOLNewButton = 'New Button';
  SOLDelButton = 'Delete Button';

{ TOLBarComponentEditor }

procedure TOLBarComponentEditor.Edit;
begin
  inherited Edit;
end;

procedure TOLBarComponentEditor.NewPage(OLBar: TJvCustomOutlookBar);
var i:integer;
begin
  if (OLBar <> nil) then
  begin
    i := THackOL(OLBar).Pages.Count;
    THackOL(OLBar).Pages.BeginUpdate;  //why ??
    try
      with THackOL(OLBar).Pages.Add do
        Caption := Format(SFmtPage,[i+1]);
      THackOL(OLBar).ActivePageIndex := i; 
    finally
      THackOL(OLBar).Pages.EndUpdate;
    end;
  end;
end;

procedure TOLBarComponentEditor.DelPage(OLBar: TJvCustomOutlookBar);
begin
  with THackOL(OLBar) do
  begin
    if ActivePage <> nil then
      Pages.Delete(ActivePage.Index);
  end;
end;

procedure TOLBarComponentEditor.NewButton(OLBar: TJvCustomOutlookBar);
var i:integer;
begin
  with THackOL(OLBar) do
    if ActivePage <> nil then
    begin
      i := ActivePage.Buttons.Count;
      with ActivePage.Buttons.Add do
        Caption := Format(SFmtButton,[i+1]);
    end;
end;

procedure TOLBarComponentEditor.DelButton(OLBar: TJvCustomOutlookBar);
begin
  if (OLBar <> nil) and (OLBar.ActivePage <> nil) and (OLBar.ActivePage.Buttons.Count > 0) then
    with OLBar.ActivePage.Buttons do
      Delete(Count - 1);
end;

procedure TOLBarComponentEditor.NextPage(OLBar: TJvCustomOutlookBar);
begin
  if (OLBar <> nil) and (OLBar.ActivePage <> nil) then
    THackOL(OLBar).ActivePageIndex := THackOL(OLBar).ActivePageIndex + 1;
end;

procedure TOLBarComponentEditor.PrevPage(OLBar: TJvCustomOutlookBar);
begin
  if (OLBar <> nil) and (OLBar.ActivePage <> nil) then
    THackOL(OLBar).ActivePageIndex := THackOL(OLBar).ActivePageIndex - 1;
end;

procedure TOLBarComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      if TfrmOLBarEditFrm.Edit(Component as TJvCustomOutlookBar) then
        Designer.Modified;
    1:; // separator
    2: NewPage(Component as TJvCustomOutlookBar);
    3: NextPage(Component as TJvCustomOutlookBar);
    4: PrevPage(Component as TJvCustomOutlookBar);
    5: DelPage(Component as TJvCustomOutlookBar);
    6:; // separator
    7: NewButton(Component as TJvCustomOutlookBar);
    8: DelButton(Component as TJvCustomOutlookBar);
  else
    inherited ExecuteVerb(Index);
  end;
end;

function TOLBarComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SOLEditor;
    1: Result := SOLSeparator;
    2: Result := SOLNewPage;
    3: Result := SOLNextPage;
    4: Result := SOLPrevPage;
    5: Result := SOLDelPage;
    6: Result := SOLSeparator;
    7: Result := SOLNewButton;
    8: Result := SOLDelButton;
  else
    Result := inherited GetVerb(Index);
  end;
end;

function TOLBarComponentEditor.GetVerbCount: Integer;
begin
//  Result := 1; // only show editor
  Result := 6; // only show page actions
//  Result := 10; / show all actions (pages and buttons)
end;



 {$IFDEF Delphi6_UP}
 procedure TOLBarComponentEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  case Index of
    3: // next page
      AItem.Enabled := (THackOL(Component).ActivePage <> nil) and (THackOL(Component).ActivePageIndex < THackOL(Component).Pages.Count -1);
    4: // prev page
      AItem.Enabled := (THackOL(Component).ActivePage <> nil) and (THackOL(Component).ActivePageIndex > 0);
    5:  // delete page
      AItem.Enabled := (THackOL(Component).Pages.Count > 0 );
  else
    inherited PrepareItem(Index,AItem);
  end;
end;
{$ELSE}
     procedure TOLBarComponentEditor.PrepareItem(Index: Integer; const AItem: TMenuItem);
begin
  case Index of
    3: // next page
      AItem.Enabled := (THackOL(Component).ActivePage <> nil) and (THackOL(Component).ActivePageIndex < THackOL(Component).Pages.Count -1);
    4: // prev page
      AItem.Enabled := (THackOL(Component).ActivePage <> nil) and (THackOL(Component).ActivePageIndex > 0);
    5:  // delete page
      AItem.Enabled := (THackOL(Component).Pages.Count > 0 );
  else
    inherited PrepareItem(Index,AItem);
  end;
end;
{$ENDIF}




{ TOLBarActivePageEditor }

procedure TOLBarActivePageEditor.Edit;
begin
  inherited Edit;
end;

function TOLBarActivePageEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

function TOLBarActivePageEditor.GetValue: string;
var i:integer;
begin
  FOL := GetComponent(0) as TJvCustomOutlookBar;
  i := GetOrdValue;
  if i = -1 then Result := ''
  else if i < THackOL(OL).Pages.Count then
    Result := THackOL(OL).Pages[i].Caption
  else
    Result := inherited GetValue;
end;


procedure TOLBarActivePageEditor.GetValues(Proc: TGetStrProc);
var i:integer;
begin
  FOL := GetComponent(0) as TJvCustomOutlookBar;
  for i := 0 to THackOL(FOL).Pages.Count -1 do
    Proc(THackOL(FOL).Pages[i].Caption);
end;

procedure TOLBarActivePageEditor.SetValue(const Value: String);
var i:integer;
begin
  FOL := GetComponent(0) as TJvCustomOutlookBar;
  i := StrToIntDef(Value,-1);
  if i = -1 then
  begin
    for i := 0 to THackOL(FOL).Pages.Count - 1 do
      if AnsiSameText(THackOL(FOL).Pages[i].Caption,Value) then
      begin
        SetOrdValue(i);
        Modified;
        Exit;
      end;
  end
  else
    inherited SetValue(Value);
end;

{ TOLBarPagesPropertyEditor }

procedure TOLBarPagesPropertyEditor.Edit;
begin

  if TfrmOLBarEditFrm.Edit(GetComponent(0) as TJvCustomOutlookBar) then
    Designer.Modified;
end;

function TOLBarPagesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog,paReadOnly];
end;

function TOLBarPagesPropertyEditor.GetValue: string;
begin
  Result := '(TOutlookBarPages)';
end;


end.

