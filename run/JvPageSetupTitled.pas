{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageSetupTitled.PAS, released on 2000-07-25.

The Initial Developer of the Original Code is Pasha Sivtsov [psivtsov att mail dott ru]
Portions created by Pasha Sivtsov are Copyright (C) 2000 Pasha Sivtsov.
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPageSetupTitled;

{$I jvcl.inc}
{$I vclonly.inc}

interface

uses
  Classes, Messages,
  JvPageSetup;

type
  TJvPageSetupTitledDialog = class(TJvPageSetupDialog)
  private
    FHeader: string;
    FFooter: string;
    FHelpHeader: Integer;
    FHelpFooter: Integer;
    procedure SetEditText(EditId: Integer; Text: string);
    function GetEditText(EditId: Integer): string;
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
  protected
    procedure DoShow; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PageHeader: string read FHeader write FHeader;
    property PageFooter: string read FFooter write FFooter;
    property HelpContextHeader: Integer read FHelpHeader write FHelpHeader default 0;
    property HelpContextFooter: Integer read FHelpFooter write FHelpFooter default 0;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvPageSetupTitled.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvPageSetupTitled.res}
{$ENDIF LINUX}

uses
  Windows, Forms, SysUtils, CommDlg;

const
  // dialog controls
  DLGHEADER = 30;
  DLGFOOTER = 31;
  DLGHEADERLABEL = 32;
  DLGFOOTERLABEL = 33;

constructor TJvPageSetupTitledDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Template := MakeIntResource(14); // Template id
end;

procedure TJvPageSetupTitledDialog.SetEditText(EditId: Integer; Text: string);
begin
  SetDlgItemText(Handle, EditId, PChar(Text));
end;

function TJvPageSetupTitledDialog.GetEditText(EditId: Integer): string;
var
  Len: Integer;
begin
  Len := SendDlgItemMessage(Handle, EditId, WM_GETTEXTLENGTH, 0, 0);
  SetLength(Result, Len);
  GetDlgItemText(Handle, EditId, PChar(Result), Len + 1);
end;

procedure TJvPageSetupTitledDialog.DoShow;
begin
  SetEditText(DLGHEADER, FHeader);
  SetEditText(DLGFOOTER, FFooter);
  inherited DoShow;
end;

procedure TJvPageSetupTitledDialog.DoClose;
begin
  FHeader := GetEditText(DLGHEADER);
  FFooter := GetEditText(DLGFOOTER);
  inherited DoClose;
end;

procedure TJvPageSetupTitledDialog.WMHelp(var Msg: TWMHelp);

  procedure ShowHelp(ContextID: Integer);
  var
    Pt: TSmallPoint;
  begin
    Pt := PointToSmallPoint(Msg.HelpInfo^.MousePos);
    Application.HelpCommand(HELP_SETPOPUP_POS, LongInt(Pt));
    Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID);
  end;

begin
  case Msg.HelpInfo^.iCtrlId of
    DLGHEADER, DLGHEADERLABEL:
      if FHelpHeader <> 0 then
        ShowHelp(FHelpHeader)
      else
        inherited;
    DLGFOOTER, DLGFOOTERLABEL:
      if FHelpFooter <> 0 then
        ShowHelp(FHelpFooter)
      else
        inherited;
  else
    inherited;
  end;
end;

end.

