{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockPropertyEditors.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-12-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
}
{$I JVCL.INC}
unit JvDockPropertyEditors;

interface

uses {$IFDEF COMPILER6_UP} DesignIntf, DesignEditors, VCLEditors, {$ELSE}
     Dsgnintf, {$ENDIF} JvDockControlForm, JvDockVIDStyle;


type
  {$IFNDEF USEJVCL}
  TJvDockControlEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TJvDockStyleEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;
  {$ENDIF}

  TJvDockVIDTabPageControlEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


implementation
uses
  Classes, Sysutils, Dialogs, JvDockGlobals;


{$IFNDEF USEJVCL}
procedure TJvDockControlEditor.ExecuteVerb(Index: Integer);
var ABoutStr: string;
  ProductStr: string;
begin
  inherited;
  case Index of
    0:
    begin
      if Component is TJvDockServer then
        ProductStr := RsDockServerName
      else if Component is TJvDockClient then
        ProductStr := RsDockClientName
      else Exit;
      ABoutStr := Format(RsDockManagerAbout,
        [ProductStr,
        RsDockManagerVersion,
        RsDockManagerCopyRightBegin,
        RsDockManagerCopyRightEnd,
        RsDockAuthorName,
        RsDockCompanyName,
        RsDockHomePage,
        RsDockEmail]);
      ShowMessage(ABoutStr);
    end;
  end;
end;

function TJvDockControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
    begin
      if Component is TJvDockServer then
        Result := Format('%s %s', [RsDockAbout, RsDockServerName])
      else if Component is TJvDockClient then
        Result := Format('%s %s', [RsDockAbout, RsDockClientName])
      else Exit;
    end;
  end;
end;

function TJvDockControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;



procedure TJvDockStyleEditor.ExecuteVerb(Index: Integer);
var ABoutStr: string;
begin
  inherited;
  case Index of
    0:
    begin
      ABoutStr := Format(RsDockManagerAbout,
        [TJvDockBasicStyle(Component).GetControlName,
        RsDockStyleVersion,
        RsDockStyleCopyRightBegin,
        RsDockStyleCopyRightEnd,
        RsDockAuthorName,
        RsDockCompanyName,
        RsDockHomePage,
        RsDockEmail]);
      ShowMessage(ABoutStr);
    end;
  end;
end;

function TJvDockStyleEditor.GetVerb(Index: Integer): string;
begin
  Result := Format('%s %s', [RsDockAbout, TJvDockBasicStyle(Component).GetControlName]);
end;

function TJvDockStyleEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;
{$ENDIF}


procedure TJvDockVIDTabPageControlEditor.ExecuteVerb(Index: Integer);
var Sheet: TJvDockVIDTabSheet;
  Page: TJvDockVIDTabPageControl;
begin
  inherited ExecuteVerb(Index);
  if Component is TJvDockVIDTabPageControl then
    Page := Component as TJvDockVIDTabPageControl
  else Page := TJvDockVIDTabSheet(Component).Parent as TJvDockVIDTabPageControl;
  case Index of
    0:
    begin

{$IFDEF COMPILER6_UP}
      Sheet := TJvDockVIDTabSheet.Create(Designer.Root);
{$ELSE}

      Sheet := TJvDockVIDTabSheet.Create(Designer.Form);
{$ENDIF}

      Sheet.PageControl := Page;
      Sheet.Name := Designer.UniqueName(TJvDockVIDTabSheet.ClassName);
      Sheet.Caption := Sheet.Name;
      Page.ActivePage := Sheet;
      Page.Panel.Invalidate;
    end;
    1:
    begin
      if Page.PageCount >= 0 then
      begin
        if Page.ActivePageIndex = Page.PageCount - 1 then
          Page.ActivePageIndex := 0
        else Page.ActivePageIndex := Page.ActivePageIndex + 1;
      end;
    end;
    2:
    begin
      if Page.PageCount >= 0 then
      begin
        if Page.ActivePageIndex = 0 then
          Page.ActivePageIndex := Page.PageCount - 1
        else Page.ActivePageIndex := Page.ActivePageIndex - 1;
      end;
    end;
    3:
    begin
      if Page.PageCount >= 0 then
        Page.ActivePage.Free;
    end;
  end;
end;

function TJvDockVIDTabPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Ne&w Page';
    1: Result := 'Ne&xt Page';
    2: Result := '&Previous Page';
    3: Result := '&Delete Page';
  end;
end;

function TJvDockVIDTabPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

end.

