{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOpenDialog2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvOpenDialog2;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CommDlg, StdCtrls, JvDialogsEx, JVCLVer;

type
  TJvOpenDialog2 = class(TOpenDialog)
  private
    FOptions: TOpenDialogExOptions;
    FFilterIndex: Integer;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetFilterIndex(const Value: Integer);
  protected
    procedure DoTypeChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property OptionsEx: TOpenDialogExOptions read FOptions write FOptions;
    property FilterIndex: Integer read FFilterIndex write SetFilterIndex default 1;
  end;

implementation

var
  ShowPlaces: Boolean;

  {**************************************************}

function ExecuteHandler(var DialogData: TOpenFileName): Bool; stdcall;
var
  DialogDataEx: TOpenFileNameEx;
begin
  Move(DialogData, DialogDataEx, SizeOf(DialogData));
  if ShowPlaces then
    DialogDataEx.FlagsEx := 0
  else
    DialogDataEx.FlagsEx := OFN_EX_NOPLACESBAR;
  DialogDataEx.lStructSize := SizeOf(TOpenFileNameEx);
  Result := GetOpenFileNameEx(DialogDataEx);
end;

{**************************************************}

constructor TJvOpenDialog2.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TOpenDialogExOptions.Create;
  FFilterIndex := 1;
end;

{**************************************************}

destructor TJvOpenDialog2.Destroy;
begin
  FOptions.Free;
  inherited;
end;

{**************************************************}

procedure TJvOpenDialog2.DoTypeChange;
begin
  inherited;
  FFilterIndex := inherited FilterIndex;
end;

{**************************************************}

function TJvOpenDialog2.Execute: Boolean;
begin
  inherited FilterIndex := FilterIndex;
  if OsCompliant then
  begin
    ShowPlaces := FOptions.PlacesBar;
    Result := DoExecute(@ExecuteHandler);
  end
  else
    Result := inherited Execute;
end;

{**************************************************}

procedure TJvOpenDialog2.SetFilterIndex(const Value: Integer);
begin
  FFilterIndex := Value;
  inherited FilterIndex := Value;
end;

end.
