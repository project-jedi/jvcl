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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDockPropertyEditors;

interface

uses
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDockControlForm, JvDockVIDStyle;

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

  {$ENDIF USEJVCL}

  TJvDockVIDTabPageControlEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  Classes, SysUtils, Dialogs,
  {$IFDEF USEJVCL}
  JvDsgnConsts,
  {$ENDIF USEJVCL}
  JvDockGlobals;

{$IFNDEF USEJVCL}
resourcestring
  RsDockNewPage = 'Ne&w Page';
  RsDockNextPage = 'Ne&xt Page';
  RsDockPreviousPage = '&Previous Page';
  RsDockDeletePage = '&Delete Page';
{$ENDIF USEJVCL}

{$IFNDEF USEJVCL}

//=== { TJvDockControlEditor } ===============================================

procedure TJvDockControlEditor.ExecuteVerb(Index: Integer);
var
  ProductStr: string;
begin
  inherited ExecuteVerb(Index);
  case Index of
    0:
    begin
      if Component is TJvDockServer then
        ProductStr := RsDockServerName
      else
      if Component is TJvDockClient then
        ProductStr := RsDockClientName
      else
        Exit;
      ShowMessageFmt(RsDockManagerAbout,
        [ProductStr, RsDockManagerVersion, RsDockManagerCopyrightBegin,
         RsDockManagerCopyrightEnd, RsDockAuthorName, RsDockCompanyName,
         RsDockHomePage, RsDockEmail]);
    end;
  end;
end;

function TJvDockControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      if Component is TJvDockServer then
        Result := Format('%s %s', [RsDockAbout, RsDockServerName])
      else
      if Component is TJvDockClient then
        Result := Format('%s %s', [RsDockAbout, RsDockClientName])
  end;
end;

function TJvDockControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== { TJvDockStyleEditor } =================================================

procedure TJvDockStyleEditor.ExecuteVerb(Index: Integer);
begin
  inherited ExecuteVerb(Index);
  case Index of
    0:
      ShowMessageFmt(RsDockManagerAbout,
        [TJvDockBasicStyle(Component).GetControlName,
        RsDockStyleVersion, RsDockStyleCopyrightBegin,
        RsDockStyleCopyrightEnd, RsDockAuthorName, RsDockCompanyName,
        RsDockHomePage, RsDockEmail]);
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

{$ENDIF USEJVCL}

//=== { TJvDockVIDTabPageControlEditor } =====================================

procedure TJvDockVIDTabPageControlEditor.ExecuteVerb(Index: Integer);
var
  Sheet: TJvDockVIDTabSheet;
  Page: TJvDockVIDTabPageControl;
begin
  inherited ExecuteVerb(Index);
  if Component is TJvDockVIDTabPageControl then
    Page := Component as TJvDockVIDTabPageControl
  else
    Page := TJvDockVIDTabSheet(Component).Parent as TJvDockVIDTabPageControl;
  case Index of
    0:
      begin
        {$IFDEF COMPILER6_UP}
        Sheet := TJvDockVIDTabSheet.Create(Designer.Root);
        {$ELSE}
        Sheet := TJvDockVIDTabSheet.Create(Designer.Form);
        {$ENDIF COMPILER6_UP}

        Sheet.PageControl := Page;
        Sheet.Name := Designer.UniqueName(TJvDockVIDTabSheet.ClassName);
        Sheet.Caption := Sheet.Name;
        Page.ActivePage := Sheet;
        Page.Panel.Invalidate;
      end;
    1:
      if Page.Count >= 0 then
      begin
        if Page.ActivePageIndex = Page.Count - 1 then
          Page.ActivePageIndex := 0
        else
          Page.ActivePageIndex := Page.ActivePageIndex + 1;
      end;
    2:
      if Page.Count >= 0 then
      begin
        if Page.ActivePageIndex = 0 then
          Page.ActivePageIndex := Page.Count - 1
        else
          Page.ActivePageIndex := Page.ActivePageIndex - 1;
      end;
    3:
      if Page.Count >= 0 then
        Page.ActivePage.Free;
  end;
end;

function TJvDockVIDTabPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsDockNewPage;
    1:
      Result := RsDockNextPage;
    2:
      Result := RsDockPreviousPage;
    3:
      Result := RsDockDeletePage;
  end;
end;

function TJvDockVIDTabPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

end.

