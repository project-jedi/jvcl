{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageSetupTitled.PAS, released on 2000-07-25.

The Initial Developer of the Original Code is Pasha Sivtsov [psivtsov@mail.ru]
Portions created by Pasha Sivtsov are Copyright (C) 2000 Pasha Sivtsov.
All Rights Reserved.

Last Modified: 2002-02-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPageSetupTitled;

interface

uses
  Classes, Messages, JvPageSetup;

type
  TJvPageSetupTitledDialog = class(TJvPageSetupDialog)
  private
    FHeader, FFooter: string;
    FHelpHeader, FHelpFooter: Integer;
    procedure SetEditText(aEditId: Integer; aText: string);
    function GetEditText(aEditId: Integer): string;
    procedure WMHelp(var aMessage: TWMHelp); message WM_HELP;
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

  ///////////////////////////////////////////////////////////////////////////////
implementation
///////////////////////////////////////////////////////////////////////////////

{.$R JvPageSetupTitledRus.res}
{$R JvPageSetupTitledEng.res}

uses
  Windows, Forms, SysUtils, CommDlg;

const
  // dialog controls
  DLGHEADER = 30;
  DLGFOOTER = 31;
  DLGHEADERLABEL = 32;
  DLGFOOTERLABEL = 33;

  { TJvPageSetupTitledDialog }

  //-----------------------------------------------------------------------------

constructor TJvPageSetupTitledDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Template := MakeIntResource(14); // Template id
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupTitledDialog.SetEditText(aEditId: Integer; aText: string);
begin
  SetDlgItemText(Handle, aEditId, PChar(aText));
end;

//-----------------------------------------------------------------------------

function TJvPageSetupTitledDialog.GetEditText(aEditId: Integer): string;
var
  eLen: Integer;
begin
  eLen := SendDlgItemMessage(Handle, aEditId, WM_GETTEXTLENGTH, 0, 0);
  SetLength(Result, eLen);
  GetDlgItemText(Handle, aEditId, PChar(Result), eLen + 1);
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupTitledDialog.DoShow;
begin
  SetEditText(DLGHEADER, FHeader);
  SetEditText(DLGFOOTER, FFooter);
  inherited;
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupTitledDialog.DoClose;
begin
  FHeader := GetEditText(DLGHEADER);
  FFooter := GetEditText(DLGFOOTER);
  inherited;
end;

//-----------------------------------------------------------------------------

procedure TJvPageSetupTitledDialog.WMHelp(var aMessage: TWMHelp);

// show popup help
//---------------------------------------------------------------------------
  procedure ShowHelp(aContextID: Integer);
  var
    ePt: TSmallPoint;
  begin
    ePt := PointToSmallPoint(aMessage.HelpInfo^.MousePos);
    Application.HelpCommand(HELP_SETPOPUP_POS, LongInt(ePt));
    Application.HelpCommand(HELP_CONTEXTPOPUP, aContextID);
  end;

begin
  case aMessage.HelpInfo^.iCtrlId of
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
